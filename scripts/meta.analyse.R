meta.analyse <- function( betas, ses ) {
	v = ses^2
	meta.w = sum(1/v)
	meta.beta = sum( betas / v ) / meta.w
	meta.v = 1 / meta.w
	meta.p = 2 * pnorm( -abs( meta.beta ), sd = sqrt( meta.v ))
	return(
		list(
			betas = betas,
			ses = ses,
			meta = list(
				beta = meta.beta,
				se = sqrt(meta.v),
				p = meta.p
			)
		)
	)
}
