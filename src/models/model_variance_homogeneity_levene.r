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
	
	# Levene's test to check the homogeneity of variances.
	# With p>0.05, there is not significant difference between variances across groups, therefore
	# we can assume the homogeneity of variances in the different treatment groups.
	t <- input$replacements %>%
		dplyr::filter( Vaihdettu & Er� %in% input$batches.multi$Er� ) %>%
		levene_test( Pime�tTunnit ~ Er� )

	print( t )
}
