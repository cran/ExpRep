\name{S_Local_Limit_Theorem}
\alias{S_Local_Limit_Theorem}

\title{
Simulations of Local Theorem of DeMoivre-Laplace
}

\description{
Given n Bernoulli experiments, with success probability p, this function calculates and plots the exact probability and the approximate probability that a successful event occurs exactly m times (0<=m<=n). It also calculates the difference between these probabilities and shows all the computations in a table. 
}

\usage{
S_Local_Limit_Theorem(n = 170, p = 0.5, Compare = TRUE, Table = TRUE, 
     Graph = TRUE, GraphE = TRUE)
}

\arguments{
  \item{n}{An integer value representing the number of repetitions of the experiment.
}
  \item{p}{A real value with the probability that a successful event will happen in any single Bernoulli experiment (called the probability of success).
}
  \item{Compare}{A logical value, if TRUE the function calculates both the exact probability and the approximate probability that a successful event occurs exactly m times and compares these probabilities.
}
  \item{Table}{A logical value, if TRUE the function shows a table with the carried out computations.
}
  \item{Graph}{A logical value, if TRUE the function plots both the exact probability and the approximate probability that a successful event occurs exactly m times.
}
  \item{GraphE}{A logical value, if TRUE the function shows the graphic of the errors in the approximation.}
}

\details{
  Bernoulli experiments are sequences of events, in which successive experiments are independent and at each experiment the probability of appearance of a "successful" event (p) remains constant. The value of n must be high and the value of p must be small.
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
S_Local_Limit_Theorem(n = 170, p = 0.5, Compare = TRUE, Table = TRUE, Graph = TRUE, 
     GraphE = TRUE)

## The function is currently defined as
function (n = 170, p = 0.5, Compare = TRUE, Table = TRUE, Graph = TRUE, GraphE = TRUE) 
  { layout(matrix(1))
    m <- array(0:n)
    x <- numeric()
    PNormal <- numeric()
    a <- n * p
    b <- sqrt(a * (1 - p))
    for (mi in 1:(n + 1)) {
        x[mi] <- (mi - 1 - a)/b
        PNormal[mi] <- dnorm(x[mi], 0, 1)/b
    }
    if (Compare == TRUE) {
        PBin <- numeric()
        for (mi in 1:(n + 1)) PBin[mi] <- dbinom(mi - 1, n, p)
        Dif <- abs(PBin - PNormal)
    }
    if (Graph == TRUE & GraphE == TRUE) {
        layout(matrix(c(1, 1, 2, 2), 2, 2, byrow = TRUE))
    }
    if (Graph == TRUE) {
        mfg <- c(1, 1, 2, 2)
        plot(PNormal, type = "p", main = "The Local Limit Theorem", 
            xlab = "m", ylab = "Probability", col = "red")
        mtext("Local Theorem", line = -1, side = 3, adj = 1, 
            col = "red")
        if (Compare == TRUE) {
            points(PBin, type = "p", col = "blue")
            mtext("Binomial Probability", line = -2, side = 3, 
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
        if (Compare == TRUE) 
            TablaR <- data.frame(m = m, x = x, PBinomial = PBin, 
                TLocal = PNormal, Difference = Dif)
        else TablaR <- data.frame(m = m, x = x, TLocal = PNormal)
        TablaR
    }
  }
}

