import( "here" )
import( "modules" )
import( "readr" )

export( "read" )

# Read batch data.
read <- function( ... )
{
	# Read the CSV data.
	read_csv( here::here( "data/raw/erät.csv" ), col_types ="ffci", comment="#" )
}

