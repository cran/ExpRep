#Applications of the Integral Theorem of DeMoivre Laplace


ApplicIntegralTheo<-function(Applic="alpha",n=10000,p=0.5,alpha=0.01,beta=0.9)
  {#Arguments:
   #  Applic: It indicates the calculation to be carried out:
   #   if "n", the function calculates the number of repetitions,
   #   if "alpha", the function calculates the boundary of possible variations of abs(frequency-p)
   #   if "beta", the function calculates the probability that the frequency of occurrence of the
   #   successful event will deviate from the probability p by no more than alpha.
   #  n     : Number of repetitions of the Bernoulli trial.
   #  p     : Probability that a successful event happens in any single
   #          Bernoulli trial (called the probability of success).
   #  alpha : The boundary of possible variations of abs(frequency-p).
   #  beta  : Probability that the frequency of occurrence of the successful
   #          event will deviate from the probability p by no more than alpha.

   # Returns:
   #  value: Numeric value representing the value of n, alpha or beta when
   #         the parameter Applic takes the value "n", "alpha" or "beta" respectively.
  
  Alpha<-function(n,p,beta)
   {# Arguments:
    #   n   : Number of repetitions of the Bernoulli trial.
    #   p   : Probability of occurrence of event A.
    #   beta: Probability that the frequency of occurrence of event A will
    #         deviate from the probability p by no more than alpha.
    
    # Returns:
    #   alpha: The boundary of possible variations of abs(frequency-p)
    
    a<-(beta+1)/2
    alpha<-((p*(1-p)/n)^0.5)*qnorm(a)
    return(alpha)
   } #End function Alpha
  
  Beta<-function(n,p,alpha)
   {# Arguments:
    #   n    : Number of repetitions of the Bernoulli trial.
    #   p    : Probability of occurrence of event A.
    #   alpha: The boundary of possible variations of abs(frequency - p)
    
    # Returns:
    #   beta: Probability that the frequency of occurrence of the successful event
    #         will deviate from the probability p by no more than alpha.
    
    b<-alpha*(n/(p*(1-p)))^0.5
    beta<-2*pnorm(b)-1
    return(beta)
   } #End function Beta
  
  Repetitions<-function(p, alpha, beta)
   {# Arguments:
    #   p    : Probability of occurrence of event A.
    #   alpha: The boundary of possible variations of abs(frequency(A)- p)
    #   beta : Probability that the frequency of occurrence of the successful event 
    #          will deviate from the probability p by no more than alpha.
    
    # Returns:
    #   n: Number of repetitions of the Bernoulli trial.
    
    a<-(beta+1)/2
    n<-(p*(1-p)*((qnorm(a)/alpha)^2))%/%1+1
    return(n)
   } #End function Repetitions
  
  options(digits=17)
  value<-switch(Applic,alpha=Alpha(n,p,beta),beta=Beta(n,p,alpha),
                       n=Repetitions(p,alpha,beta))
  return(value)
 }
