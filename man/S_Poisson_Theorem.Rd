\name{S_Poisson_Theorem}
\alias{S_Poisson_Theorem}

\title{
Simulations of Poisson Theorem
}
\description{
Given n Bernoulli experiments, with success probability p, this function calculates and plots the exact probability and the approximate probability that a successful event occurs exactly m times (0<=m<=n). It also calculates the difference between theses probabilities and shows all the computations in a table. 
}
\usage{
S_Poisson_Theorem(n = 2000, p = 0.002, Compare = TRUE, Table = TRUE, 
     Graph = TRUE, GraphE = FALSE)
}

\arguments{
  \item{n}{An integer value representing the number of repetitions of the Bernoulli experiment.
}
  \item{p}{A real value with the probability that a successful event will happen in any single Bernoulli experiment (called the probability of success)
}
  \item{Compare}{A logical value, if TRUE the function calculates both the exact probability and the approximate probability that a successful event occurs exactly m times and compares these probabilities.
}
  \item{Table}{A logical value, if TRUE the function shows a table with the carried out computations.
}
  \item{Graph}{A logical value, if TRUE the function plots both the exact probability and the approximate probability that a successful event occurs exactly m times.
}
  \item{GraphE}{A logical value, if TRUE, the function shows the graphics corresponding to the differences between the probabilities using the Binomial approach and Poisson theorem, and the probabilities using the Binomial approach and Local theorem.}
}

\details{
 Bernoulli experiments are sequences of events, in which successive experiments are independent and at each experiment the probability of appearance of a "successful" event (p) remains constant. The value of n must be high and the value of p must be very small.
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
S_Poisson_Theorem(n = 169, p = 0.002, Compare = TRUE, Table = TRUE, Graph = TRUE, 
    GraphE = TRUE)
## The function is currently defined as
function (n = 2000, p = 0.002, Compare = TRUE, Table = TRUE, 
    Graph = TRUE, GraphE = FALSE) 
  { layout(matrix(1))
    m <- array(0:n)
    PPoisson <- numeric()
    a <- n * p
    for (mi in 1:(n + 1)) PPoisson[mi] <- dpois(mi - 1, a)
    if (Compare == TRUE) {
        PBin <- numeric()
        x <- numeric()
        PNormal <- numeric()
        Dif1 <- numeric()
        Dif2 <- numeric()
        b <- sqrt(a * (1 - p))
        for (mi in 1:(n + 1)) {
            x[mi] <- (mi - 1 - a)/b
            PBin[mi] <- dbinom(mi - 1, n, p)
            PNormal[mi] <- dnorm(x[mi], 0, 1)/b
        }
        Dif1 <- abs(PBin - PPoisson)
        Dif2 <- abs(PBin - PNormal)
    }
    if (Graph == TRUE & GraphE == TRUE) {
        layout(matrix(c(1, 1, 2, 2), 2, 2, byrow = TRUE))
    }
    if (Graph == TRUE) {
        mfg <- c(1, 1, 2, 2)
        ll <- length(which(Dif1 > 5e-07))
        plot(PPoisson[1:ll], type = "b", main = "The Poisson Theorem", 
            xlab = "m", ylab = "Probability", col = "red")
        mtext("Poisson Theorem", line = -1, side = 3, adj = 1, 
            col = "red")
        if (Compare == TRUE) {
            points(PBin[1:ll], type = "b", col = "green")
            points(PNormal[1:ll], type = "b", col = "blue")
            mtext("Local Theorem", line = -2, side = 3, adj = 1, 
                col = "blue")
            mtext("Binomial Probability", line = -3, side = 3, 
                adj = 1, col = "green")
        }
    }
    if (GraphE == TRUE) {
        mfg <- c(2, 1, 2, 2)
        ll <- length(which(Dif1 > 5e-07))
        plot(Dif2[1:ll], type = "b", main = "Errors", xlab = "m", 
            ylab = "Differences", col = "red")
        mtext("Binomial-Poisson", line = -1, side = 3, adj = 1, 
            col = "red")
        points(Dif1[1:ll], type = "b", col = "green")
        mtext("Binomial-Local Theorem", line = -2, side = 3, 
            adj = 1, col = "green")
    }
    if (Table == TRUE) {
        if (Compare == TRUE) 
            TablaR <- data.frame(m = m, x = x, PBinomial = PBin, 
                TPoisson = PPoisson, Difference1 = Dif1, TLocal = PNormal, 
                Difference2 = Dif2)
        else TablaR <- data.frame(m = m, TPoisson = PPoisson)
        TablaR
    }
  }
}

