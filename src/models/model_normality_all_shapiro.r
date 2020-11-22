import( "here" )
import( "modules" )
import( "dplyr" )
import( "ggpubr" )
import( "stats" )
import( "rstatix" )

export( "run" )

# Normality test with an overall Shapiro-Wilk test.
# - In the QQ plot, if all the points fall approximately along the reference line, we can assume normality.
# - By the overall Shapiro-Wilk test, if the p-value is not significant (p > 0.05), we can assume normality.
run <- function( input, models, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$utils_debug$run( run )

	# Create a QQ plot of residuals.
	p <- ggqqplot( residuals( models[[ "normality_linear_model" ]]$output$model ) )

	# Compute Shapiro-Wilk test of normality.
	t <- shapiro_test( residuals( models[[ "normality_linear_model" ]]$output$model ) )
	
	# Construct the result.
	result <- list(
		model = NULL,
		table = t,
		plot = NULL
	)
	result
}

