library('statnet')
?`ergm-terms`

library('tidyverse')

# Install ghypernet from CRAN
install.packages("ghypernet")

# Or the development version from GitHub:
#devtools::install_github("gi0na/r-ghypernet")

library('ghypernet')
data("vertexlabels","adj_karate")
blockmodel <- bccm(adj = adj_karate, labels = vertexlabels, directed = FALSE, selfloops = FALSE)
data('adj_karate')
data('vertexlabels')
bcc.model <- bccm(adj_karate, labels=vertexlabels, directed=FALSE, selfloops=FALSE)
print(bcc.model)

g.bcc = rghype(1, bcc.model)

vertexlabels_1 = as.numeric(vertexlabels == 1)
vertexlabels_2 = as.numeric(vertexlabels == 2)

(as.matrix(g.bcc) * (vertexlabels_1 %*% t(vertexlabels_1))) %>% sum()
(as.matrix(g.bcc) * (vertexlabels_2 %*% t(vertexlabels_2))) %>% sum()

mixing_values <- function(g.bcc, vertexlabels) {
  g.bcc_1 = g.bcc[which(vertexlabels == 1), which(vertexlabels == 1)]
  val1 = g.bcc_1 %>% sum()

  g.bcc_2 = g.bcc[which(vertexlabels == 2), which(vertexlabels == 2)]
  val2 = g.bcc_2 %>% sum()

  g.bcc_12 = g.bcc[which(vertexlabels == 1), which(vertexlabels == 2)]
  val12 = g.bcc_12 %>% sum()

  return(c(val1, val2, val12))
}


mixing_values(g.bcc, vertexlabels)
mixing_values(adj_karate, vertexlabels)


