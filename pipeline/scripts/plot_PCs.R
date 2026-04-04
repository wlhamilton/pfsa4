PCs = readr::read_delim( snakemake@input$PCs, comment = '#', delim = "\t" )

{
	pdf( file = snakemake@output$pdf, width = 6, height = 5  )
	layout(
		matrix( 1:4, byrow = T, nrow = 2 )
	)
	for( i in 1:4 ) {
		plot(
			PCs[[sprintf( 'PC_%d', i )]],
			PCs[[sprintf( 'PC_%d', i+1 )]],
			pch = 19,
			col = rgb( 0, 0, 0, 0.2),
			bty = 'n',
			xlab = sprintf( "PC %d", i ),
			ylab = sprintf( "PC %d", i+1 )
		)
		grid()
	}
	dev.off()
}
