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
	
	# Bartlett test to check the homogeneity of variances.
	t <- input$replacements %>%
		dplyr::filter( Vaihdettu & Er� %in% input$batches.multi$Er� ) %>%
		group_by( Er� ) %>%
		add_count() %>%
		filter( n > 3 ) %>%
		ungroup() %>%
		bartlett.test( Pime�tTunnit ~ Er�, data = . ) %>%
		tidy()
	
	# Construct the result.
	result <- list(
		model = NULL,
		table = t,
		plot = NULL
	)
	result
}

