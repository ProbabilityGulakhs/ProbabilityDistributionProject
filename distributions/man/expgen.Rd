\name{expgen}
\alias{expgen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Exponential Distribution
}
\description{
This function uses uniform random generator (cugen) to generate uniform random number between 0 and 1 and scale it to (-1\lambda)*log(X) to make exponential random generator thanks to memoryless property.
}
\usage{
expgen(lambda)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lambda}{
Parameter of exponential distribution.
}
\value{
expgen returns a exponential random number with parameter of lambda.
}
\examples{
expgen(lambda)
## returns exponential random number
}
