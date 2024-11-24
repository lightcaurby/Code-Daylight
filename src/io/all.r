import( "here" )
import( "modules" )
import( "dplyr" )

export( "run" )

# Run all data input.
run <- function( input, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here::here( "src/utils" ) ) )$debug$run( run )

	# Actual run.
	output <- runImpl( input, ... )
	
	# Return value.
	invisible( output )
}

# Run all data input.
runImpl <- function( input, ... )
{
	# Use the modules.
	lib.io <- suppressPackageStartupMessages( modules::use( here::here( "src/io" ) ) )
	
	# All data sources.
	data_src <- c(
		"daylight_info",
		"batches",
		"replacements"
	)
	
	# Read data.
	output <- lapply( data_src, function( d ) {
		lib.io[[ d ]]$run( input )
	} )
	names( output ) <- data_src
	
	# Return data.
	invisible( output )
	
}
