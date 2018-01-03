\name{pogen}
\alias{pogen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Poisson Distribution Random Generator
}
\description{
We add results from exponential distribution until the sum exceeds the time. The output is the number of times this adding process was done.
}
\usage{
pogen(lambda, time)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lambda}{
 Parameter of the underlying exponential distribution
}
  \item{time}{
 The length of time interval
}

\value{
A random number from Poisson distribution
}

\examples{

pogen(2,1)

}
