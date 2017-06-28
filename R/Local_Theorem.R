#*************************************************************************
#************* Local Theorem of DeMoivre-Laplace         *****************
#*************************************************************************

Local_Theorem<-function(n,m,p)
  {# Arguments:
   #  n: Number of repetitions of the Bernoulli trial.
   #  p: Probability that a successful event will happen in any single
   #     Bernoulli trial (called the probability of success).
   #  m: Number of times that a successful event occurs in the n repetitions
   #     of the Bernoulli trial.

   # Returns:
   #  P: Approximate probability that a successful event occurs exactly
   #     m times in n repetitions of the Bernoulli trial.

   a<-n*p; b<-sqrt(a*(1-p)); x<-(m-a)/b
   P<-dnorm(x,0,1)/b
   return(P)
  } #End function Local_Theorem
