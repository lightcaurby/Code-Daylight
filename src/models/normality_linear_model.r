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
	suppressPackageStartupMessages( modules::use( here::here( "src/utils" ) ) )$debug$run( run )

	# Build the linear model.
	data <- input$replacements %>%
		dplyr::filter( Vaihdettu & Erä %in% input$batches.multi$Erä )
	model  <- lm( PimeätTunnit ~ Erä, data = data )

	# Construct the result.
	result <- list(
		model = model,
		table = NULL,
		plot = NULL
	)
	result
}

