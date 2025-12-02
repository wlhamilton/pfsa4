library( RSQLite )
library( dplyr )

source( "scripts/compute.manhattan.plot.positions.R" )
source( "scripts/blank.plot.R" )

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
	pdf( file = "figures/figure_2-manhattan-new.pdf", width = 8, height = 4 )

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
			X$`dom:log10_bf`,
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
				top$`dom:log10_bf` + 2,
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
