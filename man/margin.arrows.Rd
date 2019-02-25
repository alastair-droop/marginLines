\name{margin.arrows}
\alias{margin.arrows}
\title{Add Arrows to Plot Margins}
\description{Draw arrows parallel to a given axis at a specified margin line.}
\usage{margin.arrows(from, to, from.line=1, to.line=1, side=1, ...)}
\arguments{
    \item{from}{ a vector of user coordinates arrows are drawn from. }
    \item{to}{ a vector of user coordinates arrows are drawn to. }
    \item{from.line}{ the margin line number arrows are drawn from. }
    \item{to.line}{ the margin line number arrows are drawn to. }
    \item{side}{ a single number specifying the margin (1=bottom, 2=left, 3=top, 4=right). }
    \item{...}{ other parameters to pass to \code{\link{arrows}}. }
}
\details{Arrows are drawn between the two given coordinates parallel to the specified axis.}
\author{Alastair Droop, York Centre for Complex Systems Analysis, The University of York, UK}
\seealso{
\code{\link{line.coordinates}} for converting margin line values to user coordinates.
\code{\link{margin.brackets}} for drawing brackets in margin lines.
\code{\link{arrows}} for the underlying arrow function, including how to specify head size.
}
\examples{
plot(1:10)
margin.arrows(from=c(1, 6), to=c(5, 10), side=3, code=3, length=0.1)
mtext(text=c(expression(x<=5), expression(x>=7)), side=3, line=2, at=c(3, 8.5))
}
\keyword{aplot}
