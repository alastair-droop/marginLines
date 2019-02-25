margin.brackets <- function(from, to, line=1, side=1, tick.length=0, tick.depth=0.25, ...){
	if(length(from) != length(to)) stop("from and to are different lengths")
	line <- rep(line, length.out=length(from))
	if(!side %in% 1:4) stop("incorrect value for side")
	old.xpd <- par("xpd")
	par(xpd=NA)
	margin.lines(from=from, to=to, from.line=line, to.line=line, side=side, ...)
	margin.lines(from=from - tick.length, to=from, from.line=line - tick.depth, to.line=line, side=side, ...)
	margin.lines(from=to, to=to + tick.length, from.line=line, to.line=line - tick.depth, side=side, ...)
	par(xpd=old.xpd)
}
