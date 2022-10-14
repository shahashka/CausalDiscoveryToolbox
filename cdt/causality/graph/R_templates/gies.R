# MIT License
#
# Copyright (c) 2018 Diviyan Kalainathan
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

library(methods)
library(pcalg)

dataset <- read.csv(file='{FOLDER}{FILE}', header=FALSE, sep=",");

if({SKELETON}){
  fixedGaps <- read.csv(file='{FOLDER}{GAPS}', sep=",", header=FALSE) # NULL
  fixedGaps = (data.matrix(fixedGaps))
  rownames(fixedGaps) <- colnames(fixedGaps)
}else{
  fixedGaps = NULL
}
fun <- function(x) {
    which( x == max(x) )
}
if({TRACK_INT}){
  targets_matrix <- as.matrix(read.csv(file='{FOLDER}{TARGETS}', header=FALSE, sep=","))
  targets <- list(integer(0))
  pos_max <- apply(targets_matrix, 1, fun )
  targets <- append(targets, pos_max)
  targets.index <- as.integer(unlist(read.csv(file='{FOLDER}{INDEX}', header=FALSE, sep=",")))
}else{
  targets = list(integer(0))
  targets.index = rep(as.integer(1), nrow(dataset))
}
score <- new("{SCORE}", data = dataset, targets=targets, target.index=targets.index)
result <- pcalg::gies(score, fixedGaps=fixedGaps, targets=targets)
gesmat <- as(result$essgraph, "matrix")
next_node <- pcalg::opt.target(result$essgraph, max.size=1, use.node.names=TRUE)
write.csv(next_node, row.names=FALSE, file = '{FOLDER}{INTERVENE}');

gesmat[gesmat] <- 1
  #gesmat[!gesmat] <- 0
write.csv(gesmat, row.names=FALSE, file = '{FOLDER}{OUTPUT}');
