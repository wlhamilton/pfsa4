qq_plot <- function( pvalues, colour, draw_interval = FALSE ) {
	observed = sort( pvalues )
	N = length( observed )
	q = (1:N)/(N+1)
	pmax = max( -log10(observed))


	if( draw_interval ) {
		upper = -log10(qbeta( 0.975, shape1 = 1:N, shape2 = N+1-(1:N)))
		lower = -log10(qbeta( 0.025, shape1 = 1:N, shape2 = N+1-(1:N)))
		polygon(
			x = c( -log10(q), rev( -log10(q) )),
			y = c( upper, rev( lower )),
			border = NA,
			col = 'grey80'
		)
	}
	points(
		x = -log10(q), y = -log10(observed),
		pch = 19,
		cex = 0.5,
		col = colour
	)
	abline( a = 0, b = 1, lwd = 2, col = rgb( 0, 0, 0, 0.2 ))
	axis(1)
	axis(2)
	return( median(observed))
}
