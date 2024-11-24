import( "here" )
import( "modules" )
import( "dplyr" )

export( "run" )

# Run all data transforms.
run <- function( input, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here::here( "src/utils" ) ) )$debug$run( run )

	# Actual run.
	output <- runImpl( input, ... )
	
	# Return value.
	invisible( output )
}

# Run all data transforms.
runImpl <- function( input, ... )
{
	# Use the modules.
	lib.transform <- suppressPackageStartupMessages( modules::use( here::here( "src/transforms" ) ) )
	
	# All data transforms.
	transform_src <- c(
		"daylight_info",
		"replacements",
		"batch_predictions",
		"expected_failures"
	)
	
	# Transform data.
	for( t in transform_src )
	{
		# Run this transformation.
		output <- lib.transform[[ t ]]$run( input )

		# Merge output into input.
		output.names <- names( output )
		output.names <- output.names[ nzchar( output.names ) ]
		for ( n in output.names )
		{
			input[ n ] <- output[ n ]
		}
	}
	
	# Return data.
	invisible( input )
	
}
