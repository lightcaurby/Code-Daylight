import( "here" )
import( "modules" )
import( "stats" )
import( "dplyr" )
import( "stats" )
import( "rstatix" )

export( "run" )

# Homogeneity of variances.
run <- function( input, ..., .debugmod=FALSE )
{
	if( .debugmod) browser();
	
	# Anova test to check the homogeneity of variances.
	t <- input$replacements %>%
		dplyr::filter(Vaihdettu & Er� %in% input$batches.multi$Er�) %>%
		anova_test(Pime�tTunnit ~ Er�)

	print( t )
}

