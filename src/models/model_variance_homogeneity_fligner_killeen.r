import( "here" )
import( "modules" )
import( "stats" )
import( "dplyr" )
import( "stats" )
import( "rstatix" )

export( "run" )

# Homogeneity of variances.
run <- function( input, models, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$utils_debug$run( run )
	
	# Fligner-Killeen test to check the homogeneity of variances.
	t <- input$replacements %>%
		dplyr::filter( Vaihdettu & Erä %in% input$batches.multi$Erä ) %>%
		group_by( Erä ) %>%
		add_count() %>%
		filter( n > 3 ) %>%
		ungroup() %>%
		fligner.test( PimeätTunnit ~ Erä, data = . ) %>%
		tidy()
	
	# Construct the result.
	result <- list(
		model = NULL,
		table = t,
		plot = NULL
	)
	result
}

