\name{margin.lines}
\alias{margin.lines}
\title{Add Lines to Plot Margins}
\description{Draw lines parallel to a given axis at a specified margin line.}
\usage{margin.lines(from, to, from.line=1, to.line=1, side=1, ...)}
\arguments{
    \item{from}{ a vector of user coordinates lines are drawn from. }
    \item{to}{ a vector of user coordinates lines are drawn to. }
    \item{from.line}{ the margin line number lines are drawn from. }
    \item{to.line}{ the margin line number lines are drawn to. }
    \item{side}{ a single number specifying the margin (1=bottom, 2=left, 3=top, 4=right). }
    \item{...}{ other parameters to pass to \code{\link{segments}}. }
}
\details{Lines are drawn between the two given coordinates parallel to the specified axis.}
\author{Alastair Droop, York Centre for Complex Systems Analysis, The University of York, UK}
\seealso{
\code{\link{line.coordinates}} for converting margin line values to user coordinates.
\code{\link{margin.brackets}} for drawing brackets in margin lines.
}
\examples{
plot(1:10)
margin.lines(from=c(1, 6), to=c(5, 10), side=3)
mtext(text=c(expression(x<=5), expression(x>=7)), side=3, line=2, at=c(3, 8.5))
}
\keyword{aplot}
