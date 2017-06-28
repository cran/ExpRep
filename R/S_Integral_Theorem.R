#***********************************************************************************
#************** Simulation of the Integral Theorem of DeMoivre Laplace *************
S_Integral_Theorem <-function(n=200, p=0.5, linf=0,lsup=100,Compare=TRUE,
                              Table=TRUE,Graph=TRUE, GraphE=TRUE)
  {# Arguments:
   # n      : Numbers of repetitions of the experiment.
   # p      : Probability that a successful event will happen in any single
   #          Bernoulli trial (called the probability of success).
   # linf   : Minimum number of times that the successful event should happen.
   # lsup   : Maximum number of times that the successful event should happen.
   # Compare: Logical value. If True, the function calculates the probability using
   #          the integral theorem, the Poisson theorem and the Binomial theorem,
   #          comparing the results.
   # Table  : Logical value. If True, the function shows the table with the calculated
   #          probabilities.
   # Graph  : Logical value. If True, the function shows the graphic of the calculated
   #          probabilities.
   # GraphE : Logical value. If True, the function shows the graphic of the errors in the approximation.
  
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
  
  
  layout(matrix(1))
  PNormal<-numeric();Dif<-numeric(); PBin<-numeric()
  k<-lsup-linf-1
  #Para calcular P(linf<=X<=lsup)
  PNormal[1]<-Integral_Theorem(n,p,linf,lsup)
  PBin[1]<-0
  for (j in linf:lsup)
    PBin[1]<-PBin[1]+dbinom(j,n,p)
  Dif[1]<-abs(PBin[1]-PNormal[1])
  #Para calcular P(linf+(1,2,3..lsup-1)<0X<=lsup)
  for (i in 1:k)
    {linf_i<-linf+i
     PNormal[i+1]<-Integral_Theorem(n,p,linf_i,lsup)
     if (Compare==TRUE)
       {PBin[i+1]<-0
        for (j in linf_i:lsup)
          PBin[i+1]<-PBin[i+1]+dbinom(j,n,p)
        Dif[i+1]<-abs(PBin[i+1]-PNormal[i+1])
       } #End if
    } #End for

  if (Graph==TRUE & GraphE==TRUE) 
    {layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))
    }
  
  if (Graph==TRUE)
   {ymini<-min(PNormal[k+1],PBin[k+1])-0.05; ymaxi<-max(PNormal[1],PBin[1])+0.05
    mfg<-c(1,1,2,2)
    plot(PNormal,ylim=c(ymini,ymaxi),type="l",main="The Integral Limit Theorem",xlab="k (linf<=k<=lsup)",ylab="Probability",
          col="red")
     mtext("Integral Theorem", line= -1, side = 1, adj = 1, col = "red")
     if (Compare==TRUE)
       {points(PBin,type="p", col="blue")
        mtext("Binomial Probability", line= -2, side = 1, adj = 1, col = "blue")
       } #End second if
    }#End first if

  if (GraphE==TRUE)
    {mfg<-c(2,1,2,2)
     dmini<-min(Dif)-0.01; dmaxi<-max(Dif)+0.01
     plot(Dif,ylim=c(dmini,dmaxi),type="b",main="Errors",xlab="m",ylab="Errors",col="green")
     abline(a=0,b=0, col="red")
    }
  
  if (Table==TRUE)
    {Ak<-array(1:(k+1))
     if (Compare==TRUE)
      TablaR<-data.frame("k"=Ak,"PBinomial"=PBin,"T_Integral"=PNormal,
                       "Difference"=Dif) else
             TablaR<-data.frame("K"=Ak,"T_Integral"=PNormal)
     TablaR
    }
  } # End of function

