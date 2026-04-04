# Function to write SNPTEST .sample format
write.sample.file = function( data, types, filename ) {
	stopifnot( length( types ) == ncol( data ))
	write( colnames( data ), file = filename, ncol = 1000 )
	write( types, file = filename, ncol = 1000, append = TRUE )
	write.table( data, file = filename, col.names = F, row.names = F, quote = F, append = T )
}

samples = readr::read_delim( snakemake@input$samples, comment = '#', delim = " " )
types = as.character( samples[1,] )
samples = samples[-1,]

PCs     = readr::read_delim( snakemake@input$PCs, comment = '#', delim = "\t" )

samples = cbind(
	samples,
	PCs[
		match( samples$sample_roma_id, PCs$sample ),
		grep( "PC_[0-9]*", colnames(PCs))
	]
)
types = c( types, rep( "C", 20 ))

write.sample.file(
	samples,
	types,
	snakemake@output$samples
)
