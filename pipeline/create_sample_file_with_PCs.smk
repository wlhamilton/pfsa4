rule create_sample_file_with_PCs:
	input:
		samples = rules.recompute_PCs.input.samples,
		PCs = rules.recompute_PCs.output.PCs
	output:
		samples = "results/samples/ghana_2015_study_1555_samples.PCs-thin={thin}bp-r={r}.sample"
	script: "scripts/create_sample_file_with_PCs.R"
