\name{ApplicIntegralTheo}
\alias{ApplicIntegralTheo}

\title{Applications of the Integral Theorem of DeMoivre-Laplace.
}
\description{
This function shows three applications of the integral theorem of DeMoivre-Laplace:
 1. To estimate the probability (beta) that the frequency of occurrence of the successful event will deviate from the probability that this event will happen in any single Bernoulli experiment (p) in a quantity not bigger than alpha. 
 2. To calculate the least number of experiments that must be carried out (n).
 3. To determine the boundary of possible variations between the frequency of occurrence of the successful event and the probability p (alpha).
}

\usage{
ApplicIntegralTheo(Applic = "alpha", n = 10000, p = 0.5, alpha = 0.01, beta = 0.9)
}

\arguments{
  \item{Applic}{
   It indicates the calculation to be carried out:
    if "n", the function calculates the number of repetitions,
    if "alpha", the function calculates the boundary of possible variations of abs(frequency-p)
    if "beta", the function calculates the probability that the frequency of occurrence of the successful event will deviate from the probability p by no more than alpha.
}
  \item{n}{
   Number of repetitions of the Bernoulli trial.
}
  \item{p}{
   Probability that a successful event happens in any single Bernoulli trial (called the probability of success).
}
  \item{alpha}{
   The boundary of possible variations of abs(frequency-p).
}
  \item{beta}{
   Probability that the frequency of occurrence of the successful event will deviate from the probability p by no more than alpha.
}
}

\value{
Numeric value representing the values of n, alpha or beta according to the value that the parameter "Applic" takes ("n", "alpha" or "beta"). 
}
\references{
Gnedenko, B. V. (1978). The Theory of Probability. Mir Publishers. Moscow.
}

\author{Larisa Zamora and Jorge Diaz
}

\note{Department of Mathematics. University of Oriente. Cuba.
}

\seealso{
Integral_Theorem.
}

\examples{
beta<-ApplicIntegralTheo(Applic="beta",n=369,p=0.4,alpha=0.05) 
beta

alpha<-ApplicIntegralTheo(Applic="alpha",n=369,p=0.4,beta=0.95) 
alpha

n<-ApplicIntegralTheo(Applic="n",p=0.4,alpha=0.05,beta=0.95) 
n

## The function is currently defined as
function (Applic = "alpha", n = 10000, p = 0.5, alpha = 0.01, 
    beta = 0.9) 
{
    Alpha <- function(n, p, beta) {
        a <- (beta + 1)/2
        alpha <- ((p * (1 - p)/n)^0.5) * qnorm(a)
        return(alpha)
    }
    Beta <- function(n, p, alpha) {
        b <- alpha * (n/(p * (1 - p)))^0.5
        beta <- 2 * pnorm(b) - 1
        return(beta)
    }
    Repetitions <- function(p, alpha, beta) {
        a <- (beta + 1)/2
        n <- (p * (1 - p) * ((qnorm(a)/alpha)^2))\%/\%1 + 1
        return(n)
    }
    options(digits = 17)
    value <- switch(Applic, alpha = Alpha(n, p, beta), beta = Beta(n, 
        p, alpha), n = Repetitions(p, alpha, beta))
    return(value)
  }
}

