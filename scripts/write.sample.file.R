# Function to write SNPTEST .sample format
write.sample.file = function( data, types, filename ) {
	stopifnot( length( types ) == ncol( data ))
	write( colnames( data ), file = filename, ncol = 1000 )
	write( types, file = filename, ncol = 1000, append = TRUE )
	write.table( data, file = filename, col.names = F, row.names = F, quote = F, append = T )
}
