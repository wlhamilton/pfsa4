rule compute_snp_stats:
	output:
		sqlite = "results/snp-stats/ghana_2015_study.snp-stats.sqlite"
	input:
		bgen = "results/bgen/ghana_2015_study_1555_samples.bgen",
		samples = "results/samples/ghana_2015_study_1555_samples.sample"
	shell: """
		qctool_v2.2.1 \
		-g {input.bgen} \
		-s {input.samples} \
		-threshold 0.9 \
		-analysis-name 1368_samples \
		-excl-samples-where 'pf7_QC_pass=FALSE' \
		-excl-samples-where 'eurofins_final_called=FALSE' \
		-snp-stats \
		-osnp sqlite://{output.sqlite}:SnpStats1368

		qctool_v2.2.1 \
		-g {input.bgen} \
		-s {input.samples} \
		-threshold 0.9 \
		-analysis-name 1368_samples \
		-excl-samples-where 'pf7_QC_pass=FALSE' \
		-excl-samples-where 'eurofins_final_called=FALSE' \
		-snp-stats \
		-osnp sqlite://{output.sqlite}:SnpStats1555
"""