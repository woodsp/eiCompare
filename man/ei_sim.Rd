% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ei_sim.R
\name{ei_sim}
\alias{ei_sim}
\title{Importance sampling from iterative EI}
\usage{
ei_sim(ei.object, samples)
}
\arguments{
\item{ei.object}{The output of ei::ei() where simulate is set to FALSE.}

\item{samples}{The number of samples to use.}
}
\value{
an ei object from the ei package.
}
\description{
This is a modified version of the ei.sim function in the ei package. This
function is modified to enable users to enter their desired sample size,
instead of having the sample size be fixed at 99.
}
