import( "here" )
import( "modules" )
import( "readr" )

export( "run" )

# Read daylight info,
run <- function(..., .debugmod=FALSE)
{
	# Read the CSV data.
	read_csv( here( "data/raw/erät.csv" ), col_types ="ffc", comment="#" )
}

