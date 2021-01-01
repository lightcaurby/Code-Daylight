import( "here" )
import( "modules" )
import( "dplyr" )
import( "ggpubr" )
import( "stats" )
import( "rstatix" )

export( "run" )

# Run all plotting.
run <- function( input, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$utils_debug$run( run )

	# Use the intermediate data file if available.
	intermediateDataPath <- here( "data/temp/plots.rds")
	
	# Check if the file needs to be cleaned first.
	myopts <- getOption( "lightcaurby.Code-Daylight", default = list() )
	if( myopts$clean & file.exists( intermediateDataPath ) ) file.remove( intermediateDataPath )
	
	# Use the intermediate file or run the modeling from scratch.	
	result = NULL
	if( file.exists( intermediateDataPath ) )
	{
		# Read the data from the file.
		result <- readRDS( intermediateDataPath )
		
		# Indicate reading from cache instead of an actual run.
		result$actualRun <- FALSE
	}
	else
	{
		# Actual run.
		result = runImpl( input, ... )
		
		# Write to a data file.
		saveRDS( result, intermediateDataPath )
	
		# Indicate actual run.		
		result$actualRun <- TRUE
	}
	
	# Return value.
	invisible( result )
}

# Run all plotting.
runImpl <- function( input, ... )
{
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
	
	# Construct result.
	result <- list(
		grobs = plots,
		actualRun = NA
	)
	
	# Return value.
	invisible( result )
	
}
