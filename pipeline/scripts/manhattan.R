library( RSQLite )
library( dplyr )

args = list(
	hptest   = snakemake@input$hptest,
	output   = snakemake@output$pdf,
	analysis = snakemake@wildcards$analysis,
	mode     = snakemake@wildcards$mode
)
print( args )

db = dbConnect( dbDriver( "SQLite" ), args$hptest )
results = (
	dbGetQuery(
		db,
		sprintf(
			paste(
				"SELECT * FROM ResultView WHERE predictor_rsid == 'rs334' AND analysis == '%s'",
				"UNION ALL SELECT * FROM ResultView WHERE predictor_rsid == 'rs33930165' AND analysis == '%s:no-HbS'"
			),
			args$analysis,
			args$analysis
		)
	)
	%>% select(
		`analysis`,
		`predictor_rsid`,
		`outcome_chromosome`,
		`outcome_position`,
		`N`,
		`missing`,
		`outcome=0`,
		`outcome=1`,
		`predictor=0`,
		`predictor=1`,
		`predictor=2`,
		`minimum_outcome_count`,
		`minimum_predictor_count`,
		`minimum_expected_predictor_allele_count`,
		`minimum_expected_predictor_allele_count_genotype`,
		`add:converged`,
		`add:iterations`,
		`add:fit_time`,
		`add:ll`,
		`add:beta_1:add/outcome=1`,
		`add:sd_1`,
		`add:log10_bf`,
		`add:se_1`,
		`add:pvalue_1`,
		`dom:converged`,
		`dom:iterations`,
		`dom:fit_time`,
		`dom:ll`,
		`dom:beta_1:dom/outcome=1`,
		`dom:sd_1`,
		`dom:log10_bf`,
		`dom:prior_mode_1`,
		`dom:se_1`,
		`dom:pvalue_1`,
		`comment`
	)
)

print( head(( results %>% filter( predictor_rsid == 'rs334' ) %>% arrange( desc( `dom:log10_bf` )))), width = 1000 )
print( head(( results %>% filter( predictor_rsid == 'rs33930165' ) %>% arrange( desc( `dom:log10_bf` )))), width = 1000 )

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

blank.plot <- function(
	xlim = c(0,1),
	ylim = c(0,1),
	xlab = '',
	ylab = '',
	...
) {
	plot( 0, 0, col = 'white', bty = 'n', xaxt = 'n', yaxt = 'n', xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ... )
}


colours = c( "#6e90ca", "#292973" )
chromosomes = sprintf( "Pf3D7_%02d_v3", 1:14 )

{
	pdf( file = args$output, width = 8, height = 4 )

	layout(
		matrix(
			c(
				0,
				1,
				0,
				2,
				0
			),
			ncol = 1
		),
		heights = c(
			0.2, 1, 0.1, 1, 0.3
		)
	)

	bf_column = sprintf( "%s:log10_bf", args$mode )

	for( rsid in c( "rs334", "rs33930165") ) {
		if( rsid == 'rs334' ) {
			par( mar = c( 0.1, 8.1, 1.1, 1.1 ))
		} else {
			par( mar = c( 1.1, 8.1, 0.1, 1.1 ))
		}

		X = results[ results$predictor_rsid == rsid, ] %>% arrange( outcome_chromosome, outcome_position )
		X$plot.position = compute.manhattan.plot.positions( X$outcome_chromosome, X$outcome_position )
		X$outcome_chromosome = factor( X$outcome_chromosome, levels = chromosomes )
		plot(
			X$plot.position,
			X[[bf_column]],
			pch = 19,
			col = colours[ (as.integer( X$outcome_chromosome )-1) %% 2 + 1 ],
			xlab = '',
			ylab = '',
			ylim = c( 0, 12 ),
			xaxt = 'n',
			yaxt = 'n',
			bty = 'n',
			xpd = NA
		)
		yat = seq( from = 2, to = 12, by = 2 )
		abline( h = yat, lty = 3, col = 'lightgrey')
		m =  mean( range( X$plot.position ))
		text(
			x = -m/10,
			y = yat,
			sprintf( "%d", yat),
			xpd = NA,
			adj = 1,
			font = 2
		)
		mtext(
			expression( log[10]~italic(BF)),
			side = 2,
			line = 3,
			las = 1
		)
		mtext(
			switch(
				rsid,
				rs334 = "(vs. HbS)",
				rs33930165 = "(vs. HbC)"
			),
			side = 2,
			line = 3,
			las = 1,
			cex = 0.8,
			at = 4
		)
		if( rsid == 'rs334' ) {
			top = X[ X$outcome_position %in% c( 631190, 1121472, 1058035 ), ] %>% arrange( outcome_chromosome )
			top$name = c( "Pfsa1", "Putative new locus", "Pfsa3" )
			top$ref_allele = c( "T", "T", "T" )
			top$alt_allele = c( "A", "A", "A" )
			text(
				top$plot.position,
				top[[bf_column]] + 2,
				sprintf(
					"chr%d:%s %s > %s\n(%s)",
					c( 2, 4, 11 ),
					formatC( top$outcome_position, big.mark = "," ),
					top$ref_allele,
					top$alt_allele,
					top$name
				),
				xpd = NA
			)
		}
	}
	text(
		x = sapply(
			chromosomes,
			function( chr ) {
				mean( range( X$plot.position[ X$outcome_chromosome == chr ] ))
			}
		),
		-1.5,
		sprintf( "%d", 1:14 ),
		xpd = NA,
		font = 2
	)
	mtext(
		expression(Position~"in"~italic(Pf)~genome),
		side = 1,
		line = 2
	)
	dev.off()
}
