include: "compute_snp_stats.smk"
include: "compute_PCs.smk"
include: "create_sample_file_with_PCs.smk"
include: "run_hptest.smk"
include: "figures.smk"

def srcdir( path ):
	import os
	return os.path.join( workflow.current_basedir, path )

print( srcdir( "data/bgen" ))

rule all:
	input:
		expand(
			"results/figures/thin={thin}bp-r={r}/figure_{fig}-thin={thin}bp-r={r}.pdf",
			fig = [ 'S1A', 'S1B', 'S1C', 'PCsxcovariates' ],
			thin = [ '1', '50', '100' ],
			r = [ '0.2', '0.5']
		),
		expand(
			"results/figures/thin={thin}bp-r={r}/figure_{fig}-thin={thin}bp-r={r}-analysis={analysis}-mode={mode}.pdf",
			fig = [ 'manhattan', 'qqplot' ],
			thin = [ '1', '50', '100' ],
			r = [ '0.2', '0.5'],
			analysis = [
				'samples=qcd:pcs=2',
				'samples=qcd:pcs=3',
				'samples=qcd:pcs=5',
			],
			mode = [ 'add', 'dom' ]
		),
		hptest = expand(
			"results/hptest/hptest-thin={thin}bp-r={r}.sqlite",
			thin = [ '1', '50', '100' ],
			r = [ '0.2', '0.5']
		)
