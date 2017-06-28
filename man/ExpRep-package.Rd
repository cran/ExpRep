\name{ExpRep-package}
\alias{ExpRep-package}
\alias{ExpRep}
\docType{package}
\title{
Experiment Repetitions 
}
\description{
The package ExpRep, which basically responds to educational purposes, allows to calculate 
 the probabilities of occurrences of an event in a great number of repetitions of 
 Bernoulli experiment, through the application of the local and the integral theorem of 
 De Moivre Laplace, and the theorem of Poisson. It gives the possibility to show the results 
 graphically and analytically, and to compare the results obtained by the application of the 
 above theorems with those calculated by the direct application of the Binomial formula. 
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{ExpRep}
\packageIndices{ExpRep}
}

\author{
Larisa Zamora-Matamoros and Jorge Diaz-Silvera

Maintainer: Larisa Zamora-Matamoros <larisa@uo.edu.cu>
}

\references{
Gnedenko, B. V. (1978). The Theory of Probability. Mir Publishers. Moscow.
}

\examples{
ProbL<-Local_Theorem(n=100,m=50,p=0.02)
ProbL

ProbI<-Integral_Theorem(n=100,p=0.5,linf=0,lsup=50)
ProbI

ProbP<-Poisson_Theorem(n=100,m=50,p=0.002)
ProbP

beta<-ApplicIntegralTheo(Applic="beta",n=369,p=0.4,alpha=0.05) 
beta

alpha<-ApplicIntegralTheo(Applic="alpha",n=369,p=0.4,beta=0.95) 
alpha

n<-ApplicIntegralTheo(Applic="n",p=0.4,alpha=0.05,beta=0.95) 
n

S_Local_Limit_Theorem(n = 170, p = 0.5, Compare = TRUE, Table = TRUE, Graph = TRUE, 
      GraphE = TRUE)

S_Poisson_Theorem(n = 169, p = 0.002, Compare = TRUE, Table = TRUE, Graph = TRUE, 
      GraphE = TRUE)

S_Integral_Theorem(n=100, p=0.5, linf = 0, lsup = 50, Compare = TRUE, Table = TRUE, 
      Graph = TRUE, GraphE = TRUE) 

Buffon(p = 0.5, width = 0.2, r = c(100, 500, 1000, 1500))
}
