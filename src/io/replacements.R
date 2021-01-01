import( "here" )
import( "modules" )
import( "readr" )

export( "read" )

# Read lamp replacement data.
read <- function( ... )
{
	# Read the CSV data.
	read_csv( here( "data/raw/vaihdot.csv" ), col_types ="fDffc", comment="#" )
}

