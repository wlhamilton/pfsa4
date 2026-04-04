library( ggplot2 )
library( dplyr )

args = list(
	samples = snakemake@input$samples,
	PCs     = snakemake@input$PCs,
	output  = snakemake@output$pdf
)

PCs = readr::read_delim( args$PCs, comment = '#', delim = "\t" )
samples = readr::read_table( args$samples )[-1,]

w = match( PCs$sample, samples$sample_roma_id )
stopifnot( length( which( is.na(w))) == 0 )
samples = samples[w,]

X = (
	PCs
	%>% inner_join(
		samples
		%>% select(
			sample = sample_roma_id,
			sample_field_id,
			year = pf7_sample_year,
			collection_date = pf7_curated_collection_date,
			study_site = pf7_curated_study_site,
			QC_pass = pf7_QC_pass,
			HBB_called = eurofins_final_called,
			HBB_genotype = eurofins_final_Hb_call,
			HBB_fail_reason = hbb_fail_reason
		)
	)
)
stopifnot( nrow(X) == nrow(PCs ))

mytheme = function( base_size = 11 ) {
	return(
		ggplot2::theme_minimal( base_size = base_size )
		+ ggplot2::theme(
			axis.title.y = element_text( angle = 0, vjust = 0.5, hjust = 1 ),
			strip.text.y = element_text( angle = 0, vjust = 0.5, hjust = 0 )
		)
	)
}


by_HBB_genotype = (
	ggplot( data = X )
	+ geom_point(
		aes( x = PC_1, y = PC_2, colour = HBB_genotype ),
		size = 0.5
	)
	+ mytheme()
	+ xlab( "PC 1" )
	+ ylab( "PC 2" )
	+ scale_colour_manual(
		values = c( '#fb8500', '#8ecae6', '#023047', '#ffb703', '#219ebc', '#329047' ),
		name = "HBB genotype"
	)
)

by_study_site = (
	ggplot( data = X )
	+ geom_point(
		aes( x = PC_1, y = PC_2, colour = study_site ),
		size = 0.5
	)
	+ xlab( "PC 1" )
	+ ylab( "PC 2" )
	+ mytheme()
	+ scale_colour_manual(
		values = c(
			'#00876c',
			'#5b9f73',
			'#91b780',
			'#c4ce94',
			'#f4e5b0',
			'#edc185',
			'#e79a65',
			'#e06f54',
			'#d43d51'
		),
		name = "Study\nsite"
	)
)

by_year = (
	ggplot( data = X )
	+ geom_point(
		aes( x = PC_1, y = PC_2, colour = year ),
		size = 0.5
	)
	+ xlab( "PC 1" )
	+ ylab( "PC 2" )
	+ mytheme()
	+ scale_colour_manual(
		values = c(
			'#00876c',
			'#d43d51',
			'#f4e5b0',
			'#e06f54'
		),
		name = "Year"
	)
)

pdf( args$output, width = 4, height = 3 )
by_HBB_genotype
by_study_site
by_year
dev.off()

