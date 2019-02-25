line.coordinates <- function(line, side=1){
	if(length(side) != 1) stop("a single side must be specified")
	if(side %in% 1:4 == FALSE) stop("incorrect value for side")
	side <- rep(side, length.out=length(line))
	cpi <- (rep(diff(par("usr"))[c(1, 3)], each=2) / rep(par("pin"), each=2))[c(3, 1, 4, 2)]
	cpl <- (cpi * par("mai")) / par("mar")
	margin.extremes <- par("usr")[c(3, 1, 4, 2)][side]
	margin.distance <- line * cpl[side]
	margin.sign <- c(-1, -1, 1, 1)[side]
	return(margin.extremes + (margin.sign * margin.distance))
}
