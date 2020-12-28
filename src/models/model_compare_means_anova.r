import( "here" )
import( "modules" )
import( "stats" )
import( "dplyr" )
import( "broom" )
import( "ggpubr" )
import( "stats" )
import( "rstatix" )

export( "run" )

# Homogeneity of variances.
run <- function( input, models, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$utils_debug$run( run )
	
	# Anova test to check the homogeneity of variances.
	t <- input$replacements %>%
		dplyr::filter( Vaihdettu & Erä %in% input$batches.multi$Erä ) %>%
		aov( PimeätTunnit ~ Erä, data = . ) 


	# Create a QQ plot of residuals.
	p <- ggqqplot( residuals( t ) )

	# Construct the result.
	result <- list(
		model = t,
		table = tidy( TukeyHSD( t ) ),
		plot = p
	)
	result
}

