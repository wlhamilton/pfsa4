PCs     = readr::read_delim( snakemake@input$PCs, comment = '#', delim = "\t" )
kinship = readr::read_csv(   snakemake@input$kinship, comment = '#' )

between = kinship[ kinship$sample_1 != kinship$sample_2, ]
#hist( pmin( between$value, 0.5 ), 100 )

threshold = as.numeric( snakemake@wildcards$r )

outliers = c(
	between$sample_1[ which( between$value > threshold ) ],
	between$sample_2[ which( between$value > threshold ) ]
)

PCs$colour = 'black'
PCs$colour[ PCs$sample %in% outliers ] = 'red'
{
	pdf( file = snakemake@output$pdf, width = 4, height = 3  )
	par( mar = c( 4.1, 4.1, 1.1, 1.1 ))
	plot(
		PCs[['PC_1']],
		PCs[['PC_2']],
		pch = 19,
		col = PCs$colour,
		bty = 'n',
		xlab = "PC 1",
		ylab = "PC 2"
	)
	grid()
	legend(
		"topleft",
		legend = c( "Has r > 0.5", "No r > 0.5" ),
		col = c( "red", "black" ),
		pch = 19,
		bty = 'n'
	)
	dev.off()
}
