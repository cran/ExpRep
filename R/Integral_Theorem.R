#*************************************************************************
#*************  Integral Theorem of DeMoivre-Laplace     *****************
#*************************************************************************

Integral_Theorem<-function(n=100,p=0.5,linf=0,lsup=50)
 {# Arguments:
  #  n   : Number of repetitions of the Bernoulli trial.
  #  p   : Probability that a successful event will happen in any single
  #        Bernoulli trial (called the probability of success).
  #  linf: Minimum number of times that the successful event should happen.
  #  lsup: Maximum number of times that the successful event should happen.

  # Returns:
  #  P   : Approximate probability that a successful event occurs between
  #        linf and lsup times, in n repetitions of a Bernoulli trial.

  A<-(linf-n*p)/sqrt(n*p*(1-p))
  B<-(lsup-n*p)/sqrt(n*p*(1-p))
  P<-pnorm(B)-pnorm(A)
  return(P)
 } #End function Integral_Theorem
