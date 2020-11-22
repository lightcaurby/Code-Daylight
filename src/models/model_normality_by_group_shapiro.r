import( "here" )
import( "modules" )
import( "dplyr" )
import( "ggpubr" )
import( "stats" )
import( "rstatix" )

export( "run" )

# Normality test with a group-wise Shapiro-Wilk test.
# - In the QQ plot, if all the points fall approximately along the reference line, we can assume normality.
# - In the group-wise Shapiro-Wilk test, if the scores are normally distributed (p > 0.05) for each group, we can assume normality.
run <- function( input, model, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$utils_debug$run( run )

	# Create a QQ plot of residuals.
	p <- ggqqplot( input$replacements, "PimeätTunnit", facet.by = "Erä" )

	# Compute Shapiro-Wilk test of normality.
	t <- input$replacements %>%
		dplyr::filter( Vaihdettu & Erä %in% input$batches.multi$Erä ) %>%
		group_by( Erä ) %>%
		shapiro_test( PimeätTunnit )

	print( p )
	print( t )
	
}

