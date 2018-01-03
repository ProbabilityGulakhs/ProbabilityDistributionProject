\name{regnrator}
\alias{regnrator}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
pseudo random number generator
}
\description{
we use Linear congruential generator to generate random numbers, is an algorithm that yields a sequence of pseudo-randomized numbers calculated with a discontinuous piecewise linear equation. The method represents one of the oldest and best-known pseudorandom number generator algorithms. The theory behind them is relatively easy to understand, and they are easily implemented and fast, especially on computer hardware which can provide modulo arithmetic by storage-bit truncation.
}
\usage{
regnrator(m = 2^48, a = 25214903917, c = 11, floor = 0, roof = 1, n = 1, Integer = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
  algorithm parameter
}
  \item{a}{
  choose the seed of random generator
}
  \item{c}{
  algorithm parameter
}
  \item{floor}{
  choose minimum number
}
  \item{roof}{
  choose maximum number
}
  \item{n}{
  choose how many number should passed
}
  \item{Integer}{
  if be true the function will return integer values
}

\value{
  the function will return both vector and number
}

\examples{
## will return two integers between 0 and 100
regrator ( floor = 0, roof = 100,
    n = 2, Integer = true)

## The function is currently defined as
function (m = 2^48, a = 25214903917, c = 11, floor = 0, roof = 1,
    n = 1, Integer = FALSE)
{
    distance <- roof - floor
    seed <- as.numeric(Sys.time()) * 1000
    numbers <- vector(length = n)
    seed <- (a * seed + c)\%\%m
    for (i in 1:n) {
        seed <- (a * seed + c)\%\%m
        numbers[i] <- (seed/m) * distance + floor
    }
    if (Integer)
        for (i in 1:n) numbers[i] <- as.integer(numbers[i])
    return(numbers)
  }
}
