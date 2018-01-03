\name{gagen}
\alias{gagen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gamma Distribution
}
\description{
This function uses k iid exponential numbers generated from expgen(theta) and add them to generate gamma random number with shape of k and scale of theta
}
\usage{
gagen(theta, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{
Shape of gamma distribution.
}
  \item{theta}{
Scale of gamma distribution
}
\value{
gagen returns a gamma random number with shape of k and rate or scale of theta.
}
\examples{
gagen(theta, k)
## returns gamma random number
}
