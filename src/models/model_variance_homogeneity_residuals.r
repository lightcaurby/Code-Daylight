import( "here" )
import( "modules" )
import( "stats" )
import( "dplyr" )
import( "broom" )
import( "stats" )
import( "rstatix" )

export( "run" )

# The residuals versus fits plot can be used to check the homogeneity of variances.
# If in the plot below there is no evident relationships between residuals and fitted values 
# (the mean of each groups), we can assume the homogeneity of variances.
run <- function( input, models, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$utils_debug$run( run )
	
	# Table of the model.
	t <- tidy( models[[ "normality_linear_model" ]]$output$model )
	
	# Plot the residuals.
	p <- plot( models[[ "normality_linear_model" ]]$output$model, 1 )
	
	# Construct the result.
	result <- list(
		model = NULL,
		table = t,
		plot = p
	)
	result
}
