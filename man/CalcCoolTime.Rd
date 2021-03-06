% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CalcCoolTime_fun.R
\name{CalcCoolTime}
\alias{CalcCoolTime}
\title{Calculate time to cool from euthermic to torpor.}
\usage{
CalcCoolTime(Ta, bat.params)
}
\arguments{
\item{Ta}{ambient temperature}

\item{bat.params}{list of parameters output from \code{\link{BatLoad}}}
}
\value{
Time in hours.
}
\description{
\code{CalcCoolTime} Calculates time to cool from euthermic conditions to
torpid state given the ambient temperature.
}
\examples{
##CalcCoolTime
data("mylu.params")

e <- seq(-10:34) #temperature vector

r <- CalcCoolTime(Ta = e, bat.params = mylu.params)

plot(e, r, xlab = "Temp (C)", ylab = "Time")
}
\references{
McKechnie & Wolf 2004 equation 3
}
\author{
Katie Haase
}
