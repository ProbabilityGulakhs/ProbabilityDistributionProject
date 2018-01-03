[23:11, 1/3/2018] Ali Gorji: \name{brgen}
\alias{brgen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bernouli Distribution
}
\description{
This function uses uniform random generator (cugen) to generate bernouli random number with probability of p. It calls cugen and generate 1 in case result was less than p and 0 otherwise.
}
\usage{
brgen(p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
Probability of 1 generation in bernouli distribution.
}
\value{
brgen returns a bernouli random number with parameter of p.
}
\examples{
brgen(p)
## returns bernouli random number
}
[23:18, 1/3/2018] Ali Gorji: \name{bigen}
\alias{bigen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Binomial Distribution
}
\description{
This function uses bernouli random generator to generate binomial random generator. Sum of n iid bernouli random variables made by brgen(p) results binomial random number of n and p.
}
\usage{
bigen(n,p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Number of events.
}
  \item{p}{
Probability of  occourance of each event.
}
\value{
bigen returns a binomial random number with parameters n and p.
}
\examples{
bien(n, k)
## returns binomial random number
}
