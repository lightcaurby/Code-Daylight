import( "here" )
import( "modules" )
import( "readr" )

export( "run" )

# Read batch data.
run <- function( ... )
{
	# Status information.
	cat( sprintf( "\tbatches\n" ) )

	# Read the CSV data.
	read_csv( here::here( "data/raw/erät.csv" ), col_types ="ffci", comment="#" )
}

