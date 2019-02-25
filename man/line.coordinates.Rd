\name{line.coordinates}
\alias{line.coordinates}
\title{Convert Margin Lines into User Coordinates}
\description{ Convert a line number on a specified plot side into user coordinates.}
\usage{line.coordinates(line, side=1)}
\arguments{
\item{line}{ a vector of margin line numbers to convert to user coordinates. }
\item{side}{ a single number specifying the margin (1=bottom, 2=left, 3=top, 4=right). }
}
\details{
The user coordinate specifying the input lines in the given margin are returned as a numeric vector.
A single coordinate is returned for each line. This will be an x-coordinate if side is 2 or 4, and a y-coordinate if side is 1 or 3.
For non-negative line numbers, the coordinates returned will be outside the plotting area.
}
\value{A vector of coordinates.}
\author{Alastair Droop, York Centre for Complex Systems Analysis, The University of York, UK}
\seealso{
\code{\link{margin.lines}} for drawing lines specified by margin line coordinates.
}
\examples{
plot(1:10)
line.coordinates(line=0:4, side=1)
}
\keyword{aplot}
\keyword{dplot}
