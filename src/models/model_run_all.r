import( "here" )
import( "modules" )
import( "dplyr" )
import( "ggpubr" )
import( "stats" )
import( "rstatix" )

export( "run" )

# Run all modeling.
run <- function( input, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$utils_debug$run( run )

	# Use the modules.
	lib.models <- suppressPackageStartupMessages( modules::use( here( "src/models" ) ) )

	# Phase 1 model sources.
	models1_src <- c(
		"normality_linear_model"
	)
	
	# Generate phase 1 models.
	models1 <- lapply( models1_src, function( p ) {
		cat( sprintf( "\tRunning phase 1 model '%s'\n", p ) )
		list( 
					name = p,
					output = lib.models[[ paste0( "model_", p ) ]]$run( input )
			)
		} )
	names( models1 ) <- models1_src

	# Phase 2 model sources.
	models2_src <- c(
		"extreme_outliers",
		"normality",
		"variance_homogeneity",
		"compare_means"
	)
	
	# Generate phase 2 models.
	models2 <- lapply( models2_src, function( p ) {
		cat( sprintf( "\tRunning phase 2 model '%s'\n", p ) )
		list( 
					name = p,
					output = lib.models[[ paste0( "model_", p ) ]]$run( input, models1 )
			)
		} )
	names( models2 ) <- models2_src

	# Construct result.
	result <- list(
		phase1 = models1,
		phase2 = models2
	)
	invisible( result )
}

