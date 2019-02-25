margin.arrows <- function(from, to, from.line=1, to.line=1, side=1, ...){
	if(length(from) != length(to)) stop("from and to are different lengths")
	if(length(from.line) != length(to.line)) stop("from.line and to.line are different lengths")
	from.line <- rep(from.line, length.out=length(from.line))
	to.line <- rep(to.line, length.out=length(to.line))
	if(!side %in% 1:4) stop("incorrect value for side")
	from.line.coordinates <- line.coordinates(from.line, side)
	to.line.coordinates <- line.coordinates(to.line, side)
	old.xpd <- par("xpd")
	par(xpd=NA)
	if(side %in% c(1, 3)) arrows(x0=from, y0=from.line.coordinates, x1=to, y1=to.line.coordinates, ...)
	else arrows(x0=from.line.coordinates, y0=from, x1=to.line.coordinates, y1=to, ...)
	par(xpd=old.xpd)
}
