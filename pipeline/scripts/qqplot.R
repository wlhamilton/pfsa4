library( RSQLite )
library( dplyr )
options( width=180 )

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


blank.plot <- function(
	xlim = c(0,1),
	ylim = c(0,1),
	xlab = '',
	ylab = '',
	...
) {
	plot( 0, 0, col = 'white', bty = 'n', xaxt = 'n', yaxt = 'n', xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ... )
}

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

colours = c( "#6e90ca", "#292973" )
chromosomes = sprintf( "Pf3D7_%02d_v3", 1:14 )

{
	pvalue_column = sprintf( "%s:pvalue_1", args$mode )
	pdf( file = args$output, width = 7, height = 4 )
	layout( matrix( c(1:2), nrow = 1 ))
	{
		X = (results %>% filter( predictor_rsid == 'rs334' & minimum_expected_predictor_allele_count >= 10 ))
		X$pfsa = NA
		X$pfsa[ X$outcome_chromosome == 'Pf3D7_02_v3' & X$outcome_position >= 626190 & X$outcome_position <= 636190 ] = "Pfsa1"
		X$pfsa[ X$outcome_chromosome == 'Pf3D7_02_v3' & X$outcome_position >= 809288 & X$outcome_position <= 819288 ] = "Pfsa2"
		X$pfsa[ X$outcome_chromosome == 'Pf3D7_04_v3' & X$outcome_position >= 1116472 & X$outcome_position <= 1126472 ] = "Pfsa4"
		X$pfsa[ X$outcome_chromosome == 'Pf3D7_11_v3' & X$outcome_position >= 1053035 & X$outcome_position <= 1063035 ] = "Pfsa3"

		pmax = max( -log10( X[[pvalue_column]] ))
		blank.plot(
			xlim = c( 0, 10 ),
			ylim = c( 0, 18 ),
			xlab = "Expected "
		)
		median = qq_plot( X[is.na(X$pfsa),][[pvalue_column]], "darkorange3", TRUE )
		grid()
		qq_plot( X[[pvalue_column]], "black", FALSE )
	#	text( 3, 5, adj = 0, sprintf( "lambda = %.2f", -log10(median) / -log10(0.5 )))

		legend(
			"bottomright",
			legend = c( "All (vs HbS)", "(Excl. Pfsa1-4)" ),
			pch = 19,
			col = c( "black", "darkorange3" ),
			bg = 'white',
			bty = 'n'
		)
	}
	{
		X = (results %>% filter( predictor_rsid == 'rs33930165' & minimum_expected_predictor_allele_count > 10 ))
		pmax = max( -log10( X[[pvalue_column]] ))
		blank.plot(
			xlim = c( 0, 10 ),
			ylim = c( 0, 10 ),
			xlab = "Expected "
		)
		qq_plot( X[[pvalue_column]], "black", TRUE )
		grid()
	#	text( 3, 5, adj = 0, sprintf( "lambda = %.2f", -log10(median) / -log10(0.5 )))

		legend(
			"bottomright",
			legend = c( "All vs. HbC" ),
			pch = 19,
			col = c( "black" ),
			bg = 'white',
			bty = 'n'
		)
	}
	dev.off()
}
