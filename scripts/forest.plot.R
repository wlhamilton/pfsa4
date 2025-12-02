forest.plot <- function( effects ) {
	blank.plot(
		xlim = c( -1, 5 ),
		ylim = c( c( 0, 6 ))
	)
	xat = log(c( 0.5, 1, 2, 4, 8, 16, 32, 64, 128 ))
	abline( v = xat, lty = 2, col = 'lightgrey' )
	abline( v = 0, lty = 1, lwd = 2, col = 'black' )
	effects$y = 5:1
	effects$y[5] = c(0.5)

	plot.effects = effects[ effects$se < 1000, ]
	noplot.effects = effects[ effects$se > 1000, ]
	segments(
		x0 = plot.effects$beta - 1.96 * plot.effects$se,
		x1 = plot.effects$beta + 1.96 * plot.effects$se,
		y0 = plot.effects$y,
		y1 = plot.effects$y,
		col = 'grey',
		lwd = 2
	)
	points(
		x = plot.effects$beta,
		y = plot.effects$y,
		pch = 19,
		cex = 2
	)
	text(
		x = log(8),
		y = noplot.effects$y,
		"(no estimate)",
		cex = 1,
		font = 3,
		col = 'grey'
	)
	text(
		x = plot.effects$beta,
		y = plot.effects$y - 0.5,
		sprintf( "%.2e", plot.effects$p ),
		cex = 0.75
	)

	text(
		xat,
		-0.5,
		sprintf( "%.0f", exp(xat) ),
		cex = 0.8,
		xpd = NA
	)
	mtext(
		"Odds ratio and 95% CI",
		side = 1,
		line = 3
	)
	display.names = list(
		'Gambia' = c( "Gambia",        "(severe cases)" ),
		'Kenya'  = c( "Kenya",         "(severe cases)" ),
		"Ghana"  = c( "Ghana",         "(this study)" ),
		"Mali"   = c( "Mali",          "(mild cases)" ),
		"*"      = c( "Meta-analysis", "" ),
		"**"     = c( "Meta-analysis", "(mild cases)" )
	)

	mtext(
		sapply( effects$country, function(name) { display.names[[name]][1] } ),
		side = 2,
		line = 0.5,
		adj = 1,
		las = 1,
		at = effects$y,
		cex = 0.8
	)
	mtext(
		sapply( effects$country, function(name) { display.names[[name]][2] } ),
		side = 2,
		line = 0.5,
		adj = 1,
		las = 1,
		at = effects$y-0.4,
		cex = 0.6
	)
}