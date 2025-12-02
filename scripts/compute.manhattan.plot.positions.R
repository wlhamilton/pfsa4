# utility function to generate sensible x axis locations
# for manhattan-type plots, with chromosomes separated.
compute.manhattan.plot.positions <- function(
	chromosome,
	position,
	seperation = 250000
) {
	O = order( chromosome, position, decreasing = F )
	chromosome = chromosome[O]
	position = position[O]
	plot_pos = position
	plot_pos[-1] = plot_pos[-1] - plot_pos[1:( length( plot_pos ) - 1 )]
	plot_pos[ which( plot_pos < -100 ) ] = seperation
	plot_pos = cumsum( plot_pos )
	return( plot_pos )
}
