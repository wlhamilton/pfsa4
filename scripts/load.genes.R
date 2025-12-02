# Load genes from a SQLite database
load.genes <- function( filename, region ) {
	db = dbConnect( dbDriver( "SQLite" ), filename )
	G = as_tibble(
		dbGetQuery(
			db,
			sprintf(
				"SELECT * FROM genes WHERE sequence == 'Pf3D7_04_v3' AND end >= %d AND start <= %d",
				region[1], region[2]
			)
		)
	)
	genes = G %>% filter( feature == 'mRNA' ) %>% arrange( start )
	genes$gene_id = gsub( "Parent=", "", str_extract( genes$attributes, "Parent=[^;]+" ))
	IDs = G[ G$feature == 'gene', c( "ID", "symbol" )]
	genes$symbol = IDs$symbol[ match( genes$gene_id, IDs$ID )]
	genes$symbol[ is.na( genes$symbol )] = ""
	exons = G %>% filter( feature == 'exon' ) %>% arrange( start )
	exons$gene_id = gsub( "Parent=", "", str_extract( exons$attributes, "Parent=[^;]+" ))

	{
		stopifnot( length( which( genes$end < genes$start )) == 0 )
		genes$level = NA
		levels = c(1)
		rights = c(region[1] - 100000)
		spacer = (region[2]-region[1]) / 10
		for( i in 1:nrow(genes)) {
			print(genes[i,c("ID", "start", "end")])
			print(cbind( levels, rights ))
			print(spacer)
			for( j in 1:length( levels )) {
				if( genes$start[i] >= rights[j] + spacer ) {
					genes$level[i] = levels[j]
					rights[j] = genes$end[i]
					cat( sprintf( "Assigned %d\n", j ))
					break
				}
			}
			if( is.na( genes$level[i] )) {
				highest = tail( levels, 1 )
				genes$level[i] = highest + 1
				levels = c( levels, highest + 1 )
				rights = c( rights, genes$end[i] )
				cat( sprintf( "Added %d\n", highest+1 ))
			}
		}
		exons$level = genes$level[ match( exons$gene_id, genes$ID ) ]
	}

	return(
		list(
			genes = genes,
			exons = exons
		)
	)
}
