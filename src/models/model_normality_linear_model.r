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
		dplyr::filter( Vaihdettu & Erä %in% input$batches.multi$Erä )
	model  <- lm( PimeätTunnit ~ Erä, data = data )

	model
}

