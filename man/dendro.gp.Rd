\name{dendro.gp}
\alias{dendro.gp}
\title{
Dendrogram and number of groups to be chosen
}
\description{
Draw a dendrogram and an additional bar plot helping to choose the number of groups to be retained (based on the dendrogram).
}
\usage{
dendro.gp(dend)
}
\arguments{
  \item{dend}{
a dendrogram obtained from \code{\link{hclust}}.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{hclust}}
}
\examples{
data(iris)
distances <- dist(iris[,1:4],method="euclidian")
dendro <- hclust(distances,method="ward")
dendro.gp(dendro)
}