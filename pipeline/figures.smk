
rule plot_PCs_with_outliers:
	output:
		pdf = "results/figures/thin={thin}bp-r={r}/figure_S1A-thin={thin}bp-r={r}.pdf"
	input:
		PCs     = rules.compute_PCs.output.PCs,
		kinship = rules.compute_PCs.output.kinship
	script: "scripts/plot_PCs_with_outliers.R"

rule plot_PCs:
	output:
		pdf = "results/figures/thin={thin}bp-r={r}/figure_S1B-thin={thin}bp-r={r}.pdf"
	input:
		PCs = rules.recompute_PCs.output.PCs
	script: "scripts/plot_PCs.R"

rule plot_PCs_with_covariates:
	output:
		pdf = "results/figures/thin={thin}bp-r={r}/figure_PCsxcovariates-thin={thin}bp-r={r}.pdf"
	input:
		samples = rules.recompute_PCs.input.samples,
		PCs     = rules.recompute_PCs.output.PCs
	script: "scripts/plot_PCs_with_covariates.R"

rule plot_loadings:
	output:
		pdf = "results/figures/thin={thin}bp-r={r}/figure_S1C-thin={thin}bp-r={r}.pdf"
	input:
		loadings = rules.recompute_PCs.output.loadings
	params:
		n = 10
	script: "scripts/plot_loadings.R"

rule manhattan:
	output:
		pdf = "results/figures/thin={thin}bp-r={r}/figure_manhattan-thin={thin}bp-r={r}-analysis={analysis}-mode={mode}.pdf"
	input:
		hptest = rules.run_hptest.output.sqlite
	script: "scripts/manhattan.R"

rule qqplot:
	output:
		pdf = "results/figures/thin={thin}bp-r={r}/figure_qqplot-thin={thin}bp-r={r}-analysis={analysis}-mode={mode}.pdf"
	input:
		hptest = rules.run_hptest.output.sqlite
	script: "scripts/qqplot.R"
