import( "here" )
import( "modules" )
import( "stats" )
import( "dplyr" )
import( "stats" )
import( "rstatix" )

export( "run" )

# The residuals versus fits plot can be used to check the homogeneity of variances.
# If in the plot below there is no evident relationships between residuals and fitted values 
# (the mean of each groups), we can assume the homogeneity of variances.
run <- function( input, model, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$utils_debug$run( run )
	
	# Plot the residuals.
	p <- plot( model, 1 )
	
	print( p )
}
