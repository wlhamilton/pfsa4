library( RSQLite )
library( dplyr )

source( "scripts/blank.plot.R" )
source( "scripts/qq_plot.R" )

db = dbConnect( dbDriver( "SQLite" ), "results/hptest/hptest.sqlite" )
results = dbGetQuery(
	db,
	paste(
		"SELECT * FROM ResultView WHERE predictor_rsid == 'rs334' AND analysis == 'samples=qcd/pcs=3'",
		"UNION ALL SELECT * FROM ResultView WHERE predictor_rsid == 'rs33930165' AND analysis == 'samples=qcd/pcs=3/no-HbS'"
	)
)

columns = c(1, 2, 6, 7, 11, 12, 13, 14, 16:18, 19, 20, 21 )#, 45:47, 50 )
head( results[, columns ] )
head(( results %>% filter( predictor_rsid == 'rs33930165' ) %>% arrange( desc( `dom:log10_bf` )))[, columns])
colours = c( "#6e90ca", "#292973" )
chromosomes = sprintf( "Pf3D7_%02d_v3", 1:14 )

{
	pdf( file = "figures/figure_SX-qqplot.pdf", width = 7, height = 4 )
	layout( matrix( c(1:2), nrow = 1 ))
	{
		X = (results %>% filter( predictor_rsid == 'rs334' & minimum_expected_predictor_allele_count >= 10 ))
		X$pfsa = NA
		X$pfsa[ X$outcome_chromosome == 'Pf3D7_02_v3' & X$outcome_position >= 626190 & X$outcome_position <= 636190 ] = "Pfsa1"
		X$pfsa[ X$outcome_chromosome == 'Pf3D7_02_v3' & X$outcome_position >= 809288 & X$outcome_position <= 819288 ] = "Pfsa2"
		X$pfsa[ X$outcome_chromosome == 'Pf3D7_04_v3' & X$outcome_position >= 1116472 & X$outcome_position <= 1126472 ] = "Pfsa4"
		X$pfsa[ X$outcome_chromosome == 'Pf3D7_11_v3' & X$outcome_position >= 1053035 & X$outcome_position <= 1063035 ] = "Pfsa3"

		pmax = max( -log10( X[['dom:pvalue_1']] ))
		blank.plot(
			xlim = c( 0, 10 ),
			ylim = c( 0, 18 ),
			xlab = "Expected "
		)
		median = qq_plot( X[is.na(X$pfsa),][['dom:pvalue_1']], "darkorange3", TRUE )
		grid()
		qq_plot( X[['dom:pvalue_1']], "black", FALSE )
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
		pmax = max( -log10( X[['dom:pvalue_1']] ))
		blank.plot(
			xlim = c( 0, 10 ),
			ylim = c( 0, 10 ),
			xlab = "Expected "
		)
		qq_plot( X[['dom:pvalue_1']], "black", TRUE )
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
