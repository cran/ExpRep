\name{Buffon}
\alias{Buffon}

\title{
Buffon
}
\description{
Simulations of the experiment of Buffon.
}

\usage{
Buffon(p = 0.5, width = 0.2, r = c(100, 500, 1000, 1500))
}

\arguments{
  \item{p}{
  Probability of occurrence of some event.
}
  \item{width}{
  Width of the band where the probabilities are represented. 
}
  \item{r}{
  Array of four values, representing the numbers of repetitions of the experiment that will be carried out.
}
}

\value{
 Four graphics, each one is the simulation of the experiment of Buffon for the number of repetitions contained in the array r.
}

\references{Gnedenko, B. V. (1978). The Theory of Probability. Mir Publishers. Moscow.
}

\author{Larisa Zamora and Jorge Diaz
}

\note{Department of Mathematics. University of Oriente. Cuba.
}
\examples{
Buffon(p = 0.5, width = 0.2, r = c(100, 500, 1000, 1500))

## The function is currently defined as
function (p = 0.5, width = 0.2, r = c(100, 500, 1000, 1500)) 
{
    Position <- function(k, colum) {
        PE <- k\%/\%colum
        Resto <- k\%\%colum
        if (Resto == 0) {
            fila <- PE
            columna <- colum
        }
        else {
            fila <- PE + 1
            columna <- Resto
        }
        Position <- list(fila, columna)
        return(Position)
    }
    nf <- layout(matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE), TRUE)
    k <- 0
    la <- p - width
    lb <- p + width
    if (la < 0) 
        la <- 0
    if (lb > 1) 
        lb <- 1
    for (j in 1:4) {
        k <- k + 1
        Probcara <- array(0, dim = r[j])
        for (i in 1:r[j]) {
            binomial <- rbinom(i, 1, p)
            cara <- length(binomial[binomial == 1])
            Probcara[i] <- cara/i
        }
        P <- Position(k, 2)
        fila <- P[[1]]
        colum <- P[[2]]
        mfg <- c(fila, colum, 2, 2)
        a <- as.character(r[j])
        plot(Probcara, type = "p", main = paste0("n=", a), xlab = "Repetitions", 
            ylab = "Probability", font.main = 3, col = "blue", 
            ylim = c(la, lb))
        abline(h = p, col = "red", lty = 1, lwd = 2)
    }
  }
}

