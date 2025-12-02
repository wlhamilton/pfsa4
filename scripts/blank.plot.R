blank.plot <- function(
	xlim = c(0,1),
	ylim = c(0,1),
	xlab = '',
	ylab = '',
	...
) {
	plot( 0, 0, col = 'white', bty = 'n', xaxt = 'n', yaxt = 'n', xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ... )
}

