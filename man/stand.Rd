\name{stand}
\alias{stand}
\title{
Standardization of a data frame based on another data frame
}
\description{
Centers and scales a data frame. See Details.
}
\usage{
stand(tab, ref.tab=NULL, center=NULL, scale=NULL)
}
\arguments{
  \item{tab}{
data frame to scale.
}
  \item{ref.tab}{
optional reference data frame, from which centering and scaling parameters are obtained (see Details).
}
  \item{center}{
optional vector of centering parameters (one per column of \code{tab}). See Details.
}
  \item{scale}{
optional vector of scaling parameters (one per column of \code{tab}). See Details.
}
}
\details{
If \code{ref.tab} is not \code{NULL}, centering and scaling parameters are looked for into this data frame. If it has a \code{"scaled:center"} attribute, this one is used to center \code{tab}. Otherwise means of \code{ref.tab}'s columns are used. The same happens for scaling parameters (with the \code{"scaled:scale"} attribute and standard deviations).

If \code{ref.tab} is \code{NULL}, values of \code{center} and \code{scale} are used to standardize \code{tab}.

If \code{ref.tab} and \code{center} are \code{NULL}, means of \code{tab}'s columns are used for centering. If \code{ref.tab} and \code{scale} are \code{NULL}, standard deviations of \code{tab}'s columns are used for scaling.
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link{scale}}
}
\examples{
data(iris)
set.seed(1131)
iris.samp <- iris[sample(1:150,10),1:4]

# Centering parameters of the complete dataset
attr(scale(iris[,1:4]),"scaled:center")
# Centering parameters of the reduced dataset
attr(scale(iris.samp),"scaled:center")

# Standardization based on the reduced dataset only
attr(stand(iris.samp),"scaled:center")
# Standardization based on the complete dataset
attr(stand(iris.samp,iris[,1:4]),"scaled:center")
}
