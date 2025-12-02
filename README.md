# Pfsa4 paper analysis

This folder contains scripts to conduct the analyses described in:

Hamilton W. et al, "*A fourth locus in the Plasmodium falciparum genome associated with sickle haemoglobin*", bioRxiv https://doi.org/10.1101/2023.09.14.557461

## Running the scripts

The two main scripts are expressed as R markdown files, and are as follows:

* `association.Rmd` which prepares data, computes principal components, and conductes genome-wide association tests between P.falciparum genetic variants and the host genotypes HbS and HbC

* `meta_analysis.Rmd` which meta-analyses the main signal across datasets.

**Warning** Please note some data files are not included in this repository.  These are:

`data/vcf/ghana_2015_study_1555_samples_hbb_genotypes.vcf.gz` which contains host HbS/HbC genotypes for the study samples.
`data/ghana_2015_study_1555_samples.sample` which also contains host data, including the HbS/HbC genotypes for the study samples.

