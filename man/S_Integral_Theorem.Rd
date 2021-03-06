\name{S_Integral_Theorem}
\alias{S_Integral_Theorem}

\title{
Simulations of the Integral Theorem of DeMoivre-Laplace.
}
\description{
Given n Bernoulli experiments with success probability p, this function calculates and plots the exact probability and the approximate probability that a successful event occurs between linf+i (0<=i<=lsup-linf-1) and lsup times. It also calculates the difference between these probabilities and shows all the computations in a table. 
}
\usage{
S_Integral_Theorem(n = 200, p = 0.5, linf = 0, lsup = 100, Compare = TRUE, 
     Table = TRUE, Graph = TRUE, GraphE = TRUE)
}

\arguments{
  \item{n}{An integer vector with the numbers of repetitions of the Bernoulli experiment.
}
  \item{p}{A real value with the probability that a successful event will happen in any single Bernoulli experiment (called the probability of success).
}
  \item{linf}{An integer vector, of the same length than n, with the minimum numbers of times that the successful event should happen.
}
  \item{lsup}{An integer vector, of the same length than n, with the maximum numbers of times that the successful event should happen.
}
  \item{Compare}{A logical value, if TRUE the function calculates both the exact probability and the approximate probability that a successful event occurs and compares these probabilities.
}
  \item{Table}{A logical value, if TRUE the function shows a table with the carried out computations.
}
  \item{Graph}{A logical value, if TRUE the function plots both the exact probability and the approximate probability that a successful event occurs.
}
  \item{GraphE}{A logical value, if TRUE the function shows the graphic of the errors in the approximation.}
}

\details{
Bernoulli experiments are sequences of events, in which successive experiments are independent and at each experiment the probability of appearance of a "successful" event (p) remains constant. It is necessary that linf < lsup.
}

\value{A graph and/or a table.
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
S_Integral_Theorem (n = 200, p = 0.5, linf = 0, lsup = 100, Compare = TRUE, Table = TRUE, 
    Graph = TRUE, GraphE = TRUE) 

## The function is currently defined as
function (n = 200, p = 0.5, linf = 0, lsup = 100, Compare = TRUE, 
    Table = TRUE, Graph = TRUE, GraphE = TRUE) 
  { Integral_Theorem <- function(n = 100, p = 0.5, linf = 0, 
        lsup = 50) {
        A <- (linf - n * p)/sqrt(n * p * (1 - p))
        B <- (lsup - n * p)/sqrt(n * p * (1 - p))
        P <- pnorm(B) - pnorm(A)
        return(P)
    }
    layout(matrix(1))
    PNormal <- numeric()
    Dif <- numeric()
    PBin <- numeric()
    k <- lsup - linf - 1
    PNormal[1] <- Integral_Theorem(n, p, linf, lsup)
    PBin[1] <- 0
    for (j in linf:lsup) PBin[1] <- PBin[1] + dbinom(j, n, p)
    Dif[1] <- abs(PBin[1] - PNormal[1])
    for (i in 1:k) {
        linf_i <- linf + i
        PNormal[i + 1] <- Integral_Theorem(n, p, linf_i, lsup)
        if (Compare == TRUE) {
            PBin[i + 1] <- 0
            for (j in linf_i:lsup) PBin[i + 1] <- PBin[i + 1] + 
                dbinom(j, n, p)
            Dif[i + 1] <- abs(PBin[i + 1] - PNormal[i + 1])
        }
    }
    if (Graph == TRUE & GraphE == TRUE) {
        layout(matrix(c(1, 1, 2, 2), 2, 2, byrow = TRUE))
    }
    if (Graph == TRUE) {
        ymini <- min(PNormal[k + 1], PBin[k + 1]) - 0.05
        ymaxi <- max(PNormal[1], PBin[1]) + 0.05
        mfg <- c(1, 1, 2, 2)
        plot(PNormal, ylim = c(ymini, ymaxi), type = "l", main = "The Integral Limit Theorem", 
            xlab = "k (linf<=k<=lsup)", ylab = "Probability", 
            col = "red")
        mtext("Integral Theorem", line = -1, side = 1, adj = 1, 
            col = "red")
        if (Compare == TRUE) {
            points(PBin, type = "p", col = "blue")
            mtext("Binomial Probability", line = -2, side = 1, 
                adj = 1, col = "blue")
        }
    }
    if (GraphE == TRUE) {
        mfg <- c(2, 1, 2, 2)
        dmini <- min(Dif) - 0.01
        dmaxi <- max(Dif) + 0.01
        plot(Dif, ylim = c(dmini, dmaxi), type = "b", main = "Errors", 
            xlab = "m", ylab = "Errors", col = "green")
        abline(a = 0, b = 0, col = "red")
    }
    if (Table == TRUE) {
        Ak <- array(1:(k + 1))
        if (Compare == TRUE) 
            TablaR <- data.frame(k = Ak, PBinomial = PBin, T_Integral = PNormal, 
                Difference = Dif)
        else TablaR <- data.frame(K = Ak, T_Integral = PNormal)
        TablaR
    }
  }
}
