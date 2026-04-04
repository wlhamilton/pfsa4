rule run_hptest:
	output:
		sqlite = "results/hptest/hptest-thin={thin}bp-r={r}.sqlite"
	input:
		outcome    = "results/bgen/ghana_2015_study_1555_samples.bgen",
		predictor  = "results/vcf/ghana_2015_study_1555_samples_hbb_genotypes.vcf.gz",
		samples    = rules.create_sample_file_with_PCs.output.samples
	threads: 4
	params:
		boilerplate = lambda wildcards, input, output: (' '.join([
			"hptest_v2.2.1",
			"-threads {threads}",
			"-outcome-genotypes {outcome}",
			"-s {samples}",
			"-predictor {predictor}",
			"-o {sqlite}:Result",
			"-model add dom"
		]).format(
			outcome   = input.outcome,
			samples   = input.samples,
			predictor = input.predictor,
			sqlite    = output.sqlite,
			threads   = '8'
		).format(
			r         = wildcards.r
		)),
		noHbS = "-excl-samples-where 'eurofins_final_Hb_call=HbAS' -excl-samples-where 'eurofins_final_Hb_call=HbSS' -excl-samples-where 'eurofins_final_Hb_call=HbSC'"
	shell: """
	echo {params.boilerplate}
	{params.boilerplate} -analysis-name "samples=all:pcs=0"
	{params.boilerplate} -analysis-name "samples=qcd:pcs=0" -incl-samples-where 'PC_1!=NA'
	{params.boilerplate} -analysis-name "samples=qcd:pcs=1" -incl-samples-where 'PC_1!=NA' -covariates PC_1
	{params.boilerplate} -analysis-name "samples=qcd:pcs=2" -incl-samples-where 'PC_1!=NA' -covariates PC_1 PC_2
	{params.boilerplate} -analysis-name "samples=qcd:pcs=3" -incl-samples-where 'PC_1!=NA' -covariates PC_1 PC_2 PC_3
	{params.boilerplate} -analysis-name "samples=qcd:pcs=4" -incl-samples-where 'PC_1!=NA' -covariates PC_1 PC_2 PC_3 PC_4
	{params.boilerplate} -analysis-name "samples=qcd:pcs=5" -incl-samples-where 'PC_1!=NA' -covariates PC_1 PC_2 PC_3 PC_4 PC_5
	{params.boilerplate} -analysis-name "samples=qcd:pcs=2:no-HbS" {params.noHbS} -incl-samples-where 'PC_1!=NA' -covariates PC_1 PC_2 
	{params.boilerplate} -analysis-name "samples=qcd:pcs=3:no-HbS" {params.noHbS} -incl-samples-where 'PC_1!=NA' -covariates PC_1 PC_2 PC_3
	{params.boilerplate} -analysis-name "samples=qcd:pcs=4:no-HbS" {params.noHbS} -incl-samples-where 'PC_1!=NA' -covariates PC_1 PC_2 PC_3 PC_4
	{params.boilerplate} -analysis-name "samples=qcd:pcs=5:no-HbS" {params.noHbS} -incl-samples-where 'PC_1!=NA' -covariates PC_1 PC_2 PC_3 PC_4 PC_5
"""
