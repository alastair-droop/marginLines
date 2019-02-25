\name{margin.brackets}
\alias{margin.brackets}
\title{ Convert margin lines into user coordinates }
\description{ \code{margin.brackets} returns the user coordinates for the lines given in a given margin. }
\usage{margin.brackets(from, to, line=1, side=1, tick.length=0, tick.depth=0.25, ...)}
\arguments{
    \item{from}{ the start coordinate of the bracket(s). }
    \item{to}{ the end coordinate of the bracket(s). }
    \item{line}{ which line(s) to draw the brackets on. }
    \item{side}{ on which side of the plot (1=bottom, 2=left, 3=top, 4=right). }
    \item{tick.length}{ the length (perpendicular to the axis) of the tick mark (in user coordinates). }
    \item{tick.depth}{ the length (parallel to the axis) of the tick mark (in margin lines). }
    \item{...}{ further graphical parameters to pass for drawing the lines. }
}
\details{
Brackets are drawn parallel to the axis specified by \code{side}. If a vertical axis is specified, to and from specify the y-coordinates in (in user coordinates) to draw brackets between; if a horizontal axis is specified, from and t specify x-coordinates.

\code{tick.length} and \code{tick.depth} define the look of the brackets. \code{tick.length} defines how far the tick marks extend beyond the specified user coordinates. \code{tick.depth} defines the depth of the tick mark in margin lines.
}
\author{ Alastair Droop, York Centre for Complex Systems Analysis, The University of York, UK}
\seealso{
\code{\link{line.coordinates}} for converting margin lines to user coordinates.

\code{\link{margin.lines}} for drawing lines in plot margins.
}
\examples{
plot(1:10)
margin.brackets(from=c(1, 7), to=c(5, 10), side=3)
mtext(text=c(expression(x<=5), expression(x>=7)), side=3, line=2, at=c(3, 8.5))
}
