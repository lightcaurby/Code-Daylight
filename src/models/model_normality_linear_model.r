import( "here" )
import( "modules" )
import( "dplyr" )
import( "ggpubr" )
import( "stats" )
import( "rstatix" )

export( "run" )

# Linear model for normality tests.
run <- function( input, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$utils_debug$run( run )

	# Build the linear model.
	data <- input$replacements %>%
		dplyr::filter( Vaihdettu & Er� %in% input$batches.multi$Er� )
	model  <- lm( Pime�tTunnit ~ Er�, data = data )

	# Construct the result.
	result <- list(
		model = model,
		table = NULL,
		plot = NULL
	)
	result
}

