import( "here" )
import( "modules" )
import( "stats" )
import( "dplyr" )
import( "stats" )
import( "rstatix" )

export( "run" )

# Homogeneity of variances.
run <- function( input, model, ..., .debugmod=FALSE )
{
	if( .debugmod ) browser();
	
	# Pairwise T test to check the homogeneity of variances.
	t <- input$replacements %>%
		dplyr::filter( Vaihdettu & Erä %in% input$batches.multi$Erä ) %>%
		pairwise_t_test( PimeätTunnit ~ Erä, p.adjust.method = "bonferroni" )

	print( t )
}

