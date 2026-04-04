compute.exclusions.greedy <- function( kinship, threshold = 0.5 ) {
	# Find pairs above threshold
	# and symmetrise (so that each pair appears both ways round):
	library( dplyr )
	data = kinship[ kinship$value >= threshold & kinship$sample_1 != kinship$sample_2, ]
	data = dplyr::bind_rows(
		data,
		data %>% select( sample_1 = sample_2, sample_2 = sample_1, pairwise.complete.obs, value )
	)
	samples = unique( sort( c( data$sample_1, data$sample_2 )))

	exclusions = c()
	finished = FALSE
	while( nrow(data) > 0 ) {
		# count the number of related pairs per sample
		counts = sort(
			sapply( samples, function(s) {
				length( which( data[['sample_1']] == s ))
			}),
			decreasing = T
		)
		# sanity check
		stopifnot( counts[1] > 0 )
		name = names(counts)[1]
		exclusions = c( exclusions, name )

		# Remove that sample from the data
		data = data[
			data[['sample_1']] != name & data[['sample_2']] != name,
		]
	}
	return( exclusions )
}

samples = readr::read_table( snakemake@input$samples )[-1,]
kinship = readr::read_csv(   snakemake@input$kinship, comment = '#' )

wIn = which( samples$pf7_QC_pass == 'TRUE' & samples$eurofins_final_called == 'TRUE' )
stopifnot( length( wIn ) == 1368 )

threshold = as.numeric( snakemake@wildcards$r )
exclusions = c(
	samples$sample_roma_id[ !samples$sample_roma_id %in% samples$sample_roma_id[wIn] ],
	compute.exclusions.greedy( kinship[,1:4], threshold )
)

write( exclusions, file = snakemake@output$txt, ncol = 1 )
