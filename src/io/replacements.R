import( "here" )
import( "modules" )
import( "readr" )

export( "run" )

# Read lamp replacement data.
run <- function( ... )
{
	# Status information.
	cat( sprintf( "\treplacements\n" ) )

	# Read the CSV data.
	read_csv( here::here( "data/raw/vaihdot.csv" ), col_types ="fDffc", comment="#" )
}

