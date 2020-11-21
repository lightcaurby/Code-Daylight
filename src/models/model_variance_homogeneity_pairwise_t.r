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
		dplyr::filter( Vaihdettu & Er� %in% input$batches.multi$Er� ) %>%
		pairwise_t_test( Pime�tTunnit ~ Er�, p.adjust.method = "bonferroni" )

	print( t )
}

