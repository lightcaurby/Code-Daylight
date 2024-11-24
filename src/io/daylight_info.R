import( "here" )
import( "modules" )
import( "readr" )

export( "run" )

# Read daylight info.
run <- function( ... )
{
	# Status information.
	cat( sprintf( "\tdaylight info\n" ) )

	# Read the tabular data.
	read_fwf(
		file = here::here( "data/raw/tampere.txt" ),   
		col_types = cols(
			location = col_character(),
			date = col_date( format="%d.%m.%Y" ),
			time1 = col_time(),
			tz1 = col_character(),
			time2 = col_time(),
			tz2 = col_character()
		),
		fwf_widths(
			c(25, 12, 12, 6, 12, 4), 
			c("location", "date", "time1", "tz1", "time2", "tz2")
		)
	)
}

