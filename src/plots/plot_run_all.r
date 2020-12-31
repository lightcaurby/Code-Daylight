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
	lib.plots <- suppressPackageStartupMessages( modules::use( here( "src/plots" ) ) )

	# All plot sources.
	plot_src <- c(
		"installation_hours",
		"installation_years",
		"batch_hours",
		"batch_years",
		"distributions_installation_hours",
		"distributions_installation_years",
		"distributions_batch_hours",
		"distributions_batch_years",
		"distribution_hours",
		"distribution_years",
		"location_days",
		"location_hours",
		"location_years",
		"density_batch_hours"
	)
	
	# Generate plots.
	plots <- lapply( plot_src, function( p ) {
			list( 
					name = p,
					plot = lib.plots[[ paste0( "plot_", p ) ]]$run( input )
			)
		} )
	names( plots ) <- plot_src

	invisible( plots )
}

