import( "here" )
import( "modules" )
import( "utils" )

export( "run" )

# Break with debugger if configured to do so.
run <- function( func, ... )
{
	# Get the script name for the provided function.
	bn <- basename( getSrcFilename( func, full.name = F ) )
	bn <- substr( bn, start = 1, stop = nchar( bn ) - 2 ) # Strip the trailing ".R".

	# Debug break if needed.
	myopts <- getOption( "lightcaurby.Code-Daylight", default = list() )
	if( bn %in% myopts$debug.stops ) browser()
}

