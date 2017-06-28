\name{Integral_Theorem}
\alias{Integral_Theorem}

\title{
Integral Theorem of DeMoivre-Laplace
}

\description{
Given n Bernoulli experiments, with success probability p, this function calculates the probability that a successful event occurs between linf and lsup times.
}
\usage{
Integral_Theorem(n = 100, p = 0.5, linf = 0, lsup = 50)
}

\arguments{
  \item{n}{An integer value representing the number of repetitions of the Bernoulli experiment.
}
  \item{p}{A real value with the probability that a successful event will happen in any single Bernoulli experiment (called the probability of success).
}
  \item{linf}{An integer value representing the minimum number of times that the successful event should happen. 
} 
  \item{lsup}{An integer value representing the maximum number of times that the successful event should happen.
} 
}

\details{
 Bernoulli experiments are sequences of events, in which successive experiments are independent and at each experiment the probability of appearance of a "successful" event (p) remains constant. The value of n must be high and the value of p must be small. It is necessary that linf < lsup.
}

\value{
A real value representing the approximate probability that a successful event occurs between linf and lsup times, in n repetitions of a Bernoulli experiment.
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
Poisson_Theorem, Local_Theorem.
}

\examples{
Prob<-Integral_Theorem(n=100,p=0.5,linf=0,lsup=50)
Prob

## The function is currently defined as
function (n = 100, p = 0.5, linf = 0, lsup = 50) 
{
    A <- (linf - n * p)/sqrt(n * p * (1 - p))
    B <- (lsup - n * p)/sqrt(n * p * (1 - p))
    P <- pnorm(B) - pnorm(A)
    return(P)
  }
}
