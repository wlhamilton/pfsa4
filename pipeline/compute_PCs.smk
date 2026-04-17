rule find_common_snps:
	output:
		txt = "results/qc/ghana_2015_study_1368_samples_f_gt_0.01_missing_lt_0.25.txt",
		tsv = "results/qc/ghana_2015_study_1368_samples_f_gt_0.01_missing_lt_0.25.tsv"
	input:
		sqlite = rules.compute_snp_stats.output.sqlite
	params:
		query1 = "SELECT chromosome || ':' || position FROM SnpStats1368 WHERE minor_allele_frequency > 0.01 AND ((A+B)/1368.0) > 0.75",
		query2 = "SELECT chromosome || ':' || position AS SNPID, chromosome || ':' || position AS rsid, chromosome, position, alleleA, alleleB FROM SnpStats1368View WHERE minor_allele_frequency > 0.01 AND ((A+B)/1368.0) > 0.75"
	shell: """
		sqlite3 {input.sqlite} "{params.query1}" > {output.txt}
		sqlite3 -separator $'\t' {input.sqlite} "{params.query2}" > {output.tsv}
	"""

rule inthinnerate:
	output:
		tsv = "results/qc/ghana_2015_study_1368_samples_f_gt_0.01_missing_lt_0.25-thin={thin}bp.tsv",
		txt = "results/qc/ghana_2015_study_1368_samples_f_gt_0.01_missing_lt_0.25-thin={thin}bp.txt"
	input:
		tsv = rules.find_common_snps.output.tsv
	params:
		inthinnerator = config['tools']['inthinnerator']
	shell: """
		{params.inthinnerator} -g {input.tsv} -min-distance {wildcards.thin}bp -suppress-excluded -o {output.tsv}
		cat {output.tsv} | grep -v '^#' | grep -v 'alternate' | cut -f1 > {output.txt}
	"""

rule compute_PCs:
	output:
		kinship   = "results/PCs/ghana_2015_study_1368_samples-thin={thin}bp.kinship.csv",
		UDUT      = "results/PCs/ghana_2015_study_1368_samples-thin={thin}bp.UDUT.csv",
		PCs       = "results/PCs/ghana_2015_study_1368_samples-thin={thin}bp.PCs.tsv"
	input:
		vcf       = srcdir( "data/vcf/ghana_2015_study_1555_samples.fakediploid.vcf.gz" ),
		samples   = srcdir( "data/samples/ghana_2015_study_1555_samples.sample" ),
		positions = rules.inthinnerate.output.txt
	params:
		qctool = config['tools']['qctool']
	shell: """
	{params.qctool} \
		-g {input.vcf} \
		-s {input.samples} \
		-incl-positions {input.positions} \
		-excl-samples-where 'pf7_QC_pass=FALSE' \
		-excl-samples-where 'eurofins_final_called=FALSE' \
		-PCs 20 \
		-kinship {output.kinship} \
		-UDUT {output.UDUT} \
		-osample {output.PCs}
"""

rule compute_PC_exclusions:
	output:
		txt = "results/PCs/1368_relatedness_exclusions-thin={thin}bp-r={r}.txt"
	input:
		kinship = rules.compute_PCs.output.kinship,
		samples = rules.compute_PCs.input.samples
	wildcard_constraints:
		r = "[0-9]+[.][0-9]+"
	script: "scripts/compute_PC_exclusions.R"

rule recompute_PCs:
	output:
		kinship  = "results/PCs/ghana_2015_study_1368_samples-thin={thin}bp-r={r}.kinship.csv",
		UDUT     = "results/PCs/ghana_2015_study_1368_samples-thin={thin}bp-r={r}.UDUT.csv",
		PCs      = "results/PCs/ghana_2015_study_1368_samples-thin={thin}bp-r={r}.PCs.tsv",
		loadings = "results/PCs/ghana_2015_study_1368_samples-thin={thin}bp-r={r}.loadings.csv"
	input:
		vcf         = rules.compute_PCs.input.vcf,
		samples     = rules.compute_PCs.input.samples,
		exclusions  = lambda w: (
			{
				'original1309': 'results/PCs/1368_relatedness_exclusions_0.50_original_1309.txt'
			}.get(
				w.r,
				rules.compute_PC_exclusions.output.txt
			)
		),
		snps        = rules.inthinnerate.output.txt
	params:
		qctool = config['tools']['qctool']
	shell: """
		{params.qctool} \
		-g {input.vcf} \
		-s {input.samples} \
		-excl-samples {input.exclusions} \
		-incl-positions {input.snps} \
		-kinship {output.kinship} \
		-UDUT {output.UDUT} \
		-PCs 20 \
		-osample {output.PCs}

		{params.qctool} \
		-g {input.vcf} \
		-s {input.samples} \
		-excl-samples {input.exclusions} \
		-incl-positions {input.snps} \
		-load-UDUT {output.UDUT} \
		-loadings {output.loadings}
"""
