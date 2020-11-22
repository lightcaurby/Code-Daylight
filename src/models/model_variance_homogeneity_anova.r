import( "here" )
import( "modules" )
import( "stats" )
import( "dplyr" )
import( "stats" )
import( "rstatix" )

export( "run" )

# Homogeneity of variances.
run <- function( input, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$utils_debug$run( run )
	
	# Anova test to check the homogeneity of variances.
	t <- input$replacements %>%
		dplyr::filter( Vaihdettu & Erä %in% input$batches.multi$Erä ) %>%
		anova_test( PimeätTunnit ~ Erä )

	print( t )
}

