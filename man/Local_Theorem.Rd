\name{Local_Theorem}
\alias{Local_Theorem}

\title{
Local Theorem of DeMoivre-Laplace
}
\description{
Given n Bernoulli experiments, with success probability p, this function calculates the approximate probability that a successful event occurs exactly m times. 
}
\usage{
Local_Theorem(n, m, p)
}

\arguments{
  \item{n}{An integer value representing the number of repetitions of the Bernoulli experiment.
}
  \item{m}{An integer value representing the number of times that a successful event occurs in the n repetitions of the Bernoulli experiment. 
}
  \item{p}{A real value with the probability that a successful event will happen in any single Bernoulli experiment (called the probability of success).
}
}

\details{
  Bernoulli experiments are sequences of events, in which successive experiments are independent and at each experiment the probability of appearance of a "successful" event (p) remains constant. The value of n must be high and the value of p must be small.
}

\value{
A real value representing the approximate probability that a successful event occurs exactly m times in n repetitions of a Bernoulli experiment.
}

\references{
Gnedenko, B. V. (1978). The Theory of Probability. Mir Publishers. Moscow.
}

\author{
Larisa Zamora and Jorge Diaz
}

\note{
Department of Mathematics. University of Oriente. Cuba.
}

\seealso{
Integral_Theorem, Poisson_Theorem.
}

\examples{
Prob<-Local_Theorem(n=100,m=50,p=0.02)
Prob

## The function is currently defined as
function (n, m, p) 
{
    a <- n * p
    b <- sqrt(a * (1 - p))
    x <- (m - a)/b
    P <- dnorm(x, 0, 1)/b
    return(P)
  }
}
