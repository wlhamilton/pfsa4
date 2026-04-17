# Pfsa4 paper analysis

This folder contains scripts to conduct the analyses described in:

Hamilton W. et al, "*A fourth locus in the Plasmodium falciparum genome associated with sickle haemoglobin*", bioRxiv https://doi.org/10.1101/2023.09.14.557461

## Running the scripts

The main analysis is provided using a [snakemake](https://snakemake.readthedocs.io/en/stable/index.html) pipeline.  To run this pipeline you need a few prerequisites:

* `snakemake` (we used version `7.32.3`)
* You need [hptest](https://www.chg.ox.ac.uk/~gav/hptest/), [qctool](https://www.chg.ox.ac.uk/~gav/qctool_v2/), and [inthinnerator](https://www.chg.ox.ac.uk/~gav/inthinnerator/#overview) in your `PATH`.

You should then be able to run the pipeline from the folder containing the `pfsa4` repository, like this:

```
snakemake -s pfsa4/master.smk -c<n>
```
where `<n>` should be replaced with the number of threads you want the pipeline to use.  All results from the pipeline will appear in the `results/` folder.

Subsequent analysis of the results was done with two scripts:

* `association.Rmd` which post-analyses the pipeline outputs to generate results for the main text and supplementary.

* `meta_analysis.Rmd` which focusses on meta-analysing the main signal across datasets.

