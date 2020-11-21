import( "here" )
import( "modules" )
import( "dplyr" )
import( "ggpubr" )
import( "stats" )
import( "rstatix" )

export( "run" )

# Linear model for normality tests.
run <- function( input, ..., .debugmod=FALSE )
{
	if( .debugmod ) browser();

	# Build the linear model.
	data <- input$replacements %>%
		dplyr::filter( Vaihdettu & Er� %in% input$batches.multi$Er� )
	model  <- lm( Pime�tTunnit ~ Er�, data = data )

	model
}

