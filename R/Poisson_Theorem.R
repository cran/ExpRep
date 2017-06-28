#*************************************************************************
#***********************      Poisson Theorem           ******************
#*************************************************************************

Poisson_Theorem<-function(n,m,p)
  {# Arguments:
   #  n: Number of repetitions of the Bernoulli trial.
   #  p: Probability that a successful event will happen in any single
   #     Bernoulli trial (called the probability of success)
   #  m: Number of times that a successful event occurs in the n repetitions
   #     of the Bernoulli trial.

   # Returns:
   #  P: Approximate probability that a successful event occurs exactly m times.

  landa<-n*p
  P<-dpois(m,landa)
  return(P)
} #End function Poisson_Theorem





