# Shim for snakemake versions > 7
if not 'srcdir' in dir():
	def srcdir( path ):
		import os
		return os.path.join( workflow.current_basedir, path )

print( srcdir( "pipeline/config.yaml" ))
configfile: srcdir( "pipeline/config.yaml" )

print( config )

include: "pipeline/compute_snp_stats.smk"
include: "pipeline/compute_PCs.smk"
include: "pipeline/create_sample_file_with_PCs.smk"
include: "pipeline/run_hptest.smk"
include: "pipeline/figures.smk"

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
