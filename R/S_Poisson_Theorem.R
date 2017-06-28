#***********************************************************************************
#********************** Simulation of the Poisson Theorem **************************

S_Poisson_Theorem<-function(n=2000,p=0.002,Compare=TRUE,Table=TRUE,Graph=TRUE,
                            GraphE=FALSE)
  {# Arguments:
   # n      : Number of repetitions of the experiment.
   # p      : Probability that a successful event will happen in any single
   #          Bernoulli trial (called the probability of success).
   # Compare: Logical value. If True, the function calculates the probability using
   #          the integral theorem, the Poisson theorem and the Binomial approach,
   #          comparing the results.
   # Table  : Logical value. If True, the function shows the table with the calculated
   #          probabilities.
   # Graph  : Logical value. If True, the function shows the graph of the calculated
   #          probabilities.
   # GraphE : Logical value. If True, the function shows the graphics corresponding to 
   #          the differences between the probabilities using the Binomial approach and Poisson theorem, and 
   #          the probabilities using the Binomial approach and Local theorem.
  
  layout(matrix(1))
  m<-array(0:n); PPoisson<-numeric()
   a<-n*p
   for (mi in 1:(n+1))
     PPoisson[mi]<-dpois(mi-1,a)
   if (Compare==TRUE)
     {PBin<-numeric(); x<-numeric();PNormal<-numeric()
      Dif1<-numeric();Dif2<-numeric()
      b<-sqrt(a*(1-p))
      for (mi in 1:(n+1))
        {x[mi]<-(mi-1-a)/b
         PBin[mi]<-dbinom(mi-1,n,p)
         PNormal[mi]<-dnorm(x[mi],0,1)/b
        }
      Dif1<-abs(PBin-PPoisson)
      Dif2<-abs(PBin-PNormal)
    }
   
   if (Graph==TRUE & GraphE==TRUE) 
     {layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))
      }
   
   if (Graph==TRUE)
     {mfg<-c(1,1,2,2)
      ll<-length(which(Dif1>0.0000005))
      plot(PPoisson[1:ll],type="b",main="The Poisson Theorem",xlab="m",ylab="Probability",col="red")
      mtext("Poisson Theorem", line= -1, side = 3, adj = 1, col = "red")
      if (Compare==TRUE)
        {points(PBin[1:ll],type="b", col="green")
         points(PNormal[1:ll],type="b", col="blue")
         mtext("Local Theorem", line= -2, side = 3, adj = 1, col = "blue")
         mtext("Binomial Probability", line= -3, side = 3, adj = 1, col = "green")
        }
     }
   
   if (GraphE==TRUE)
    {mfg<-c(2,1,2,2)
     ll<-length(which(Dif1>0.0000005))
     plot(Dif2[1:ll],type="b",main="Errors",xlab="m",ylab="Differences",col="red")
     mtext("Binomial-Poisson", line= -1, side = 3, adj = 1, col = "red")
     points(Dif1[1:ll],type="b", col="green")
     mtext("Binomial-Local Theorem", line= -2, side = 3, adj = 1, col = "green")
    }
   
   if (Table==TRUE)
     {if (Compare==TRUE)
      TablaR<-data.frame("m"=m,"x"=x,"PBinomial"=PBin,"TPoisson"=PPoisson,
                     "Difference1"=Dif1,"TLocal"=PNormal,"Difference2"=Dif2) 
         else TablaR<-data.frame("m"=m,"TPoisson"=PPoisson)
      TablaR}
  }# End function S_Poisson_Theorem


