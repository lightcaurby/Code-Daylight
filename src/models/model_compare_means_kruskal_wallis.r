import( "here" )
import( "modules" )
import( "stats" )
import( "dplyr" )
import( "broom" )
import( "stats" )
import( "rstatix" )

export( "run" )

# Homogeneity of variances.
run <- function( input, models, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$utils_debug$run( run )
	
	# Kruskal-Wallis test to compare the means.
	t <- input$replacements %>%
		dplyr::filter( Vaihdettu & Erä %in% input$batches.multi$Erä ) %>%
		kruskal.test( PimeätTunnit ~ Erä, data = . ) %>%
		tidy()
	
	# Construct the result.
	result <- list(
		model = NULL,
		table = t,
		plot = NULL
	)
	result
}

