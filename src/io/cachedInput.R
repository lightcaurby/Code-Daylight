import( "here" )
import( "modules" )
import( "readr" )

export( "is.available" )
export( "read" )
export( "save" )

# Target file path.
dataPath = here::here( "data/temp/input.rds" )

# Is the cached input data available?
is.available <- function( ... )
{
	# Check if the file needs to be cleaned first.
	myopts <- getOption( "lightcaurby.Code-Daylight", default = list() )
	if( myopts$clean & file.exists( dataPath ) ) file.remove( dataPath )
	
	# Check the file existence.
	file.exists( dataPath )
}

# Reads the cached input data.
read <- function( ... )
{
	# Read the cached RDS data.
	readRDS( dataPath )
}

# Writes the cached input data.
save <- function( preparedData, ... )
{
	# save the cached RDS data.
	saveRDS( preparedData, dataPath )
}
