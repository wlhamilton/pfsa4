# Pfsa4 paper analysis

This folder contains scripts to conduct the analyses described in:

Hamilton W. et al, "*A fourth locus in the Plasmodium falciparum genome associated with sickle haemoglobin*", bioRxiv https://doi.org/10.1101/2023.09.14.557461

## Running the scripts

The main analysis is provided using a [snakemake](https://snakemake.readthedocs.io/en/stable/index.html) pipeline, which can be found in the `pipeline` folder.  We ran this using `snakemake` version `7.32.3`, like this:
```
snakemake -s pfsa4/pipeline/master.smk -c<n>
```
where `<n>` should be replaced with the number of threads you want the pipeline to use.  All results from the pipeline will appear in the `results/` folder.

Subsequent analysis of the results was done with two scripts:

* `association.Rmd` which post-analyses the pipeline outputs to generate results for the main text and supplementary.

* `meta_analysis.Rmd` which focusses on meta-analysing the main signal across datasets.

**Warning** Please note some data files are not included in this repository.  These are:

* `data/vcf/ghana_2015_study_1555_samples_hbb_genotypes.vcf.gz` which contains host HbS/HbC genotypes for the study samples.

* `data/ghana_2015_study_1555_samples.sample` which also contains host sample metadata (also including the HbS/HbC genotypes for the study samples.)

