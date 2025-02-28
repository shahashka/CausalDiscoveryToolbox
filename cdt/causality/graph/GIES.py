"""GIES algorithm.

.. MIT License
..
.. Copyright (c) 2018 Diviyan Kalainathan
..
.. Permission is hereby granted, free of charge, to any person obtaining a copy
.. of this software and associated documentation files (the "Software"), to deal
.. in the Software without restriction, including without limitation the rights
.. to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
.. copies of the Software, and to permit persons to whom the Software is
.. furnished to do so, subject to the following conditions:
..
.. The above copyright notice and this permission notice shall be included in all
.. copies or substantial portions of the Software.
..
.. THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
.. IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
.. FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
.. AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
.. LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
.. OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
.. SOFTWARE.
"""
import os
import uuid
import warnings
import networkx as nx
from pathlib import Path
from shutil import rmtree
from tempfile import gettempdir
import pandas as pd

from .model import GraphModel
from pandas import DataFrame, read_csv
from ...utils.R import RPackages, launch_R_script
from ...utils.Settings import SETTINGS


def message_warning(msg, *a, **kwargs):
    """Ignore everything except the message."""
    return str(msg) + '\n'


warnings.formatwarning = message_warning


class GIES(GraphModel):
    """GIES algorithm **[R model]**.

    **Description:** Greedy Interventional Equivalence Search algorithm.
    A score-based Bayesian algorithm that searches heuristically the graph which minimizes
    a likelihood score on the data. The main difference with GES is that it
    accepts interventional data for its inference.

    **Required R packages**: pcalg

    **Data Type:** Continuous (``score='obs'``) or Categorical (``score='int'``)

    **Assumptions:** The output is a Partially Directed Acyclic Graph (PDAG)
    (A markov equivalence class). The available scores assume linearity of
    mechanisms and gaussianity of the data.

    Args:
        score (str): Sets the score used by GIES.
        verbose (bool): Defaults to ``cdt.SETTINGS.verbose``.

    Available scores:
        + int: GaussL0penIntScore
        + obs: GaussL0penObsScore

    .. note::
       Ref:
       D.M. Chickering (2002).  Optimal structure identification with greedy search.
       Journal of Machine Learning Research 3 , 507–554

       A. Hauser and P. Bühlmann (2012). Characterization and greedy learning of
       interventional Markov equivalence classes of directed acyclic graphs.
       Journal of Machine Learning Research 13, 2409–2464.

       P. Nandy, A. Hauser and M. Maathuis (2015). Understanding consistency in
       hybrid causal structure learning.
       arXiv preprint 1507.02608

       P. Spirtes, C.N. Glymour, and R. Scheines (2000).
       Causation, Prediction, and Search, MIT Press, Cambridge (MA)

    Example:
        >>> import networkx as nx
        >>> from cdt.causality.graph import GIES
        >>> from cdt.data import load_dataset
        >>> data, graph = load_dataset("sachs")
        >>> obj = GIES()
        >>> #The predict() method works without a graph, or with a
        >>> #directed or undirected graph provided as an input
        >>> output = obj.predict(data)    #No graph provided as an argument
        >>>
        >>> output = obj.predict(data, nx.Graph(graph))  #With an undirected graph
        >>>
        >>> output = obj.predict(data, graph)  #With a directed graph
        >>>
        >>> #To view the graph created, run the below commands:
        >>> nx.draw_networkx(output, font_size=8)
        >>> plt.show()
    """

    def __init__(self, score='obs', verbose=False, skeleton=False):
        """Init the model and its available arguments."""
        if not RPackages.pcalg:
            raise ImportError("R Package pcalg is not available.")

        super(GIES, self).__init__()
        self.scores = {'int': 'GaussL0penIntScore',
                       'obs': 'GaussL0penObsScore'}
        self.arguments = {'{FOLDER}': '/tmp/cdt_gies/',
                          '{FILE}': os.sep + 'data.csv',
                          '{TARGETS}':  os.sep + 'targets.csv',
                          '{INDEX}':  os.sep + 'index.csv',
                          '{SKELETON}': 'FALSE',
                          '{GAPS}': os.sep + 'fixedgaps.csv',
                          '{SCORE}': 'GaussL0penObsScore',
                          '{VERBOSE}': 'FALSE',
                          '{OUTPUT}': os.sep + 'result.csv',
                          '{INTERVENE}': os.sep + 'intervention.csv',
                          '{TRACK_INT}': 'TRUE'}
        self.verbose = SETTINGS.get_default(verbose=verbose)
        self.skeleton = SETTINGS.get_default(skeleton=skeleton)
        self.score = score

    def orient_undirected_graph(self, data, graph, targets, targets_index):
        """Run GIES on an undirected graph.

        Args:
            data (pandas.DataFrame): DataFrame containing the data
            graph (networkx.Graph): Skeleton of the graph to orient

        Returns:
            networkx.DiGraph: Solution given by the GIES algorithm.

        """
        # Building setup w/ arguments.
        self.arguments['{VERBOSE}'] = str(self.verbose).upper()
        self.arguments['{SCORE}'] = self.scores[self.score]

        fe = DataFrame(nx.adjacency_matrix(graph, nodelist=sorted(graph.nodes()), weight=None).todense())
        fg = DataFrame(1 - fe.values)

        results = self._run_gies(data, targets, targets_index, fixedGaps=fg, verbose=self.verbose)

        return nx.relabel_nodes(nx.DiGraph(results),
                                {idx: i for idx, i in enumerate(sorted(data.columns))})

    def orient_directed_graph(self, data, graph, targets, targets_index):
        """Run GIES on a directed_graph.

        Args:
            data (pandas.DataFrame): DataFrame containing the data
            graph (networkx.DiGraph): Skeleton of the graph to orient

        Returns:
            networkx.DiGraph: Solution given by the GIES algorithm.

        """
        warnings.warn("GIES is ran on the skeleton of the given graph.")
        return self.orient_undirected_graph(data, nx.Graph(graph), targets, targets_index)

    def create_graph_from_data(self, data, targets, targets_index, fixedGaps):
        """Run the GIES algorithm.

        Args:
            data (pandas.DataFrame): DataFrame containing the data

        Returns:
            networkx.DiGraph: Solution given by the GIES algorithm.
        """
        # Building setup w/ arguments.
        self.arguments['{SCORE}'] = self.scores[self.score]
        self.arguments['{VERBOSE}'] = str(self.verbose).upper()
        results = self._run_gies(data, targets, targets_index, fixedGaps=fixedGaps, verbose=self.verbose)

        return nx.relabel_nodes(nx.DiGraph(results[0]),
                                {idx: i for idx, i in enumerate(data.columns)}), results[1]

    def _run_gies(self, data, targets, targets_index, fixedGaps=None, verbose=True):
        """Setting up and running GIES with all arguments."""
        # Run gies
        self.arguments['{FOLDER}'] = Path('{0!s}/cdt_gies_{1!s}/'.format(gettempdir(), uuid.uuid4()))
        run_dir = self.arguments['{FOLDER}']
        os.makedirs(run_dir, exist_ok=True)

        def retrieve_result():
            return read_csv(Path('{}/result.csv'.format(run_dir)), delimiter=',').values,\
                   read_csv(Path('{}/intervention.csv'.format(run_dir)), delimiter=',').values

        try:
            data.to_csv(Path('{}/data.csv'.format(run_dir)), header=False, index=False)
            if targets is not None:
                targets.to_csv(Path('{}/targets.csv'.format(run_dir)), header=False, index=False)
            else:
                self.arguments['{TRACK_INT}'] = 'FALSE'
            if targets_index is not None:
                targets_index.to_csv(Path('{}/index.csv'.format(run_dir)), header=False, index=False)

            if fixedGaps is not None:
                fixedGaps.to_csv(Path('{}/fixedgaps.csv'.format(run_dir)), index=False, header=False)
                self.arguments['{SKELETON}'] = 'TRUE'
            else:
                self.arguments['{SKELETON}'] = 'FALSE'


            gies_result = launch_R_script(Path("{}/R_templates/gies.R".format(os.path.dirname(os.path.realpath(__file__)))),
                                          self.arguments, output_function=retrieve_result, verbose=verbose, debug=True)
        # Cleanup
        except Exception as e:
            rmtree(run_dir)
            raise e
        except KeyboardInterrupt:
            rmtree(run_dir)
            raise KeyboardInterrupt
        rmtree(run_dir)
        return gies_result
