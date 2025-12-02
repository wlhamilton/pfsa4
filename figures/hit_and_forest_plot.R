library( RSQLite )
library( dplyr )
library( stringr )
library( RSQLite )

source( "scripts/blank.plot.R" )
source( "scripts/load.genes.R" )
source( "scripts/hitplot.R" )
source( "scripts/forest.plot.R" )


# hitplot region definition
margin = 25000
region = c( (1121472-margin), (1121472+margin) )

# load association results

genes = load.genes( "data/PlasmoDB-50_Pf3D7_04-genes.sqlite", region )

functional = (
	readr::read_csv( "data/ghana_2015_study_allpf7_variant_info_biallelic_pass_snps_MAF0.001_pfsa_added.csv" )
	%>% select(
		chromosome = Chrom,
		position = Pos,
		coding = Coding
	)
)

db = dbConnect( dbDriver( "SQLite" ), "results/hptest/results.sqlite" )
data = (
	dbGetQuery(
		db,
		"SELECT * FROM ResultView WHERE analysis == 'samples=qcd/pcs=3' ORDER BY predictor_id, outcome_chromosome, outcome_position"
	)
	%>% filter(
		predictor_rsid == 'rs334'
		& outcome_chromosome == "Pf3D7_04_v3"
		& outcome_position >= region[1]
		& outcome_position <= region[2]
	)
	%>% left_join(
		functional,
		by = c( outcome_chromosome = "chromosome", outcome_position = "position" )
	)
)
data$coding[ data$coding == FALSE | is.na( data$coding )] = "noncoding"
data$coding[ data$coding == TRUE ] = "coding"

{
	pdf( file = "figures/figure_3-hitplot-v2.pdf", width = 8, height = 3 )

	layout(
		matrix(
			c(
				0, 0, 0, 0, 0,
				0, 1, 0, 3, 0,
				0, 0, 0, 3, 0,
				0, 2, 0, 3, 0,
				0, 0, 0, 0, 0
			),
			nrow = 5,
			byrow = T
		),
		widths = c(0.25, 1, 0.4, 1, 0.1 ),
		heights = c( 0.1, 1, 0.1, 0.3, 0.5 )
	)
	par( mar = c( 0, 0, 0, 0 ))

	hitplot( data, region, genes, "dom:log10_bf" )

	forest.plot(
		readr::read_tsv( "tables/table_SX-meta-analysis.tsv" )
	)

	dev.off()
}
