\name{Poisson_Theorem}
\alias{Poisson_Theorem}

\title{
Poisson Theorem.
}
\description{
Given n Bernoulli experiments, with success probability p (p small), this function calculates the approximate probability that a successful event occurs exactly m times.
}
\usage{
Poisson_Theorem(n, m, p)
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
Bernoulli experiments are sequences of events, in which successive experiments are independent and at each experiment the probability of appearance of a "successful" event (p) remains constant.  The value of n must be high and the value of p must be very small.
}

\value{A numerical value representing the approximate probability that a successful event occurs exactly m times. 
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
Integral_Theorem, Local_Theorem.
}

\examples{
Prob<-Poisson_Theorem(n=100,m=50,p=0.002)
Prob

## The function is currently defined as
function (n, m, p) 
{
    landa <- n * p
    P <- dpois(m, landa)
    return(P)
  }
}