library( dplyr )

args = list(
	loadings = snakemake@input$loadings,
	output   = snakemake@output$pdf,
	n        = as.integer(snakemake@params$n)
)

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

loadings = (
	readr::read_csv( args$loadings, comment = '#' )
	%>% arrange( chromosome, position )
)
loadings$plot.position = compute.manhattan.plot.positions( loadings$chromosome, loadings$position )
colours = c( "#6e90ca", "#292973" )
chromosomes = sprintf( "Pf3D7_%02d_v3", 1:14 )
loadings$chromosome = factor( loadings$chromosome, levels = chromosomes )

{
	npcs = args$n
	pdf( file = args$output, width = 6, height = npcs+1  )
	layout.m = rep( 0, (2*npcs)+1 )
	layout.m[ (1:npcs)*2 ] = 1:npcs
	layout(
		matrix( layout.m, ncol = 1 ),
		heights = (layout.m > 0) * 0.9 + 0.1
	)
	for( i in 1:npcs ) {
		par( mar = c( 0.5, 4.1, 0.5, 1.1 ))
		plot(
			loadings$plot.position,
			abs( loadings[[sprintf( 'eigenvector_%d', i ) ]] ),
			pch = 19,
			col = colours[ (( as.integer( loadings$chromosome ) - 1 ) %% 2 ) + 1 ],
			bty = 'n',
			xlab = "Position in genome",
			ylab = sprintf( "PC %d", i ),
			xaxt = 'n',
			yaxt = 'n'
		)
		grid()
		axis(2)
	}
	dev.off()
}
