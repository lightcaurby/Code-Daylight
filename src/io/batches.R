import( "here" )
import( "modules" )
import( "readr" )

export( "read" )

# Read batch data.
read <- function( ... )
{
	# Read the CSV data.
	read_csv( here( "data/raw/er�t.csv" ), col_types ="ffc", comment="#" )
}

