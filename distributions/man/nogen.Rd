\name{nogen}
\alias{nogen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normal Distribution
}
\description{
This function uses pogen estimation for N(λ,λ) and makes Normal random number with scale and transition. First we make N(s,s) with pogen(s) and then transit it into N(u,s) by adding u-s to previous value.
}
\usage{
nogen(u,s)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{u}{
Expected of normal distribution
}
  \item{s}{
Standard deviation of normal distribution
}
\value{
nogen returns a normal random number with expected of u and standard deviation of s.
}
\examples{
nogen(u, s)
## returns normal random number
}
