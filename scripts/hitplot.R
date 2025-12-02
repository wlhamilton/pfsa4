hitplot <- function(
	data,
	region,
	genes,
	bf_column = "dom:log10_bf"
) {
	exons = genes$exons
	genes = genes$genes

	shapes = c( noncoding = 19, coding = 3 )
	plot(
		data$outcome_position,
		data[[bf_column]],
		xlim = region,
		pch = shapes[ data$coding ],
		xlab = '',
		ylab = '',
		bty = 'n',
		xaxt = 'n',
		yaxt = 'n'
	)
	axis(2,las = 1)
	abline(
		v = seq( from = 1100000, to = 1140000, by = 10000 ),
		lty = 2,
		col = 'lightgrey'
	)
	mtext(
		expression( BF[HbS] ),
		side = 2,
		line = 2,
		las = 1,
		cex = 1,
		at = 2,
		adj = 1
	)


	blank.plot(
		xlim = region,
		ylim = c( 0, max( genes$level) + 0.5 )
	)
	abline(
		v = seq( from = 1100000, to = 1140000, by = 10000 ),
		lty = 2,
		col = 'lightgrey'
	)

	segments(
		x0 = genes$start,
		x1 = genes$end,
		y0 = genes$level,
		y1 = genes$level
	)

	genes$name = genes$symbol
	genes$name[ genes$symbol == '' | is.na( genes$symbol ) ] = gsub(
		".1", "",
		genes$ID[genes$symbol == '' | is.na( genes$symbol )],
		fixed = T
	)
	genes$font = 3
	genes$font[ genes$name == 'FIKK4.2' ] = 4
	text(
		x = (genes$start + genes$end) / 2,
		y = genes$level - 0.5,
		gsub( "PF3D7_", "", genes$name ),
		cex = 0.8,
		xpd = NA,
		font = genes$font
	)
	rect(
		xleft   = exons$start,
		xright  = exons$end,
		ybottom = exons$level - 0.2,
		ytop    = exons$level + 0.2,
		border  = NA,
		col     = 'grey'
	)
	axis(1)
	mtext(
		"Position on chromosome 4",
		side = 1,
		line = 3
	)
}
