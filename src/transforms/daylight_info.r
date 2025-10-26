import( "here" )
import( "modules" )
import( "stats" )
import( "dplyr" )
import( "hms" )

export( "run" )

# Transform daylight info.
run <- function( input, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here::here( "src/utils" ) ) )$debug$run( run )

	# Status information.
	cat( sprintf( "\tdaylight info\n" ) )
	
	# UTC conversion.
	input <- input$daylight_info %>%
		mutate( 
			sunrise.UTC = as_hms( ifelse( dst, sunrise - (3 * 60 * 60 ), sunrise - (2 * 60 * 60 ) ) ),
			sundown.UTC = as_hms( ifelse( dst, sundown - (3 * 60 * 60 ), sundown - (2 * 60 * 60 ) ) ),
			daylight.seconds = as.numeric( sundown.UTC - sunrise.UTC ),
			dusk.seconds = as.numeric( dusk ) * 2,
			daylight.hours = daylight.seconds / 60 / 60,
			dusk.hours = dusk.seconds / 60 / 60,
			nondark.hours = daylight.hours + ( 0.8 * dusk.hours ),
			dark.hours = 24 - nondark.hours
		)
	
	# Split output.
	output.leapyear <- input %>% filter( leapyear == TRUE )
	output.normalyear <- input %>% filter( leapyear == FALSE )
	
	# Return value.
	output <- list( daylight_info = list( leapyear = output.leapyear, normalyear = output.normalyear) )
	output
}

# Transform daylight info.
run.old <- function( input, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here::here( "src/utils" ) ) )$debug$run( run )
	
	# Status information.
	cat( sprintf( "\tdaylight info\n" ) )
	
	# Calculate daylight seconds.
	input <- input$daylight_info %>%
		select( -location ) %>%
		mutate( daylight.seconds = as.numeric( time2 - time1 ) )
	
	# Construct a daily frame.
	frame <- data.frame(
		date = seq.Date( as.Date("2003-10-01"), as.Date("2005-01-05" ), by = 1 ),
		daylight.seconds = NA
	)
	
	# Combine the input and the frame.
	output <- frame %>% 
		left_join( input, by=c("date" = "date") ) %>%
		rename( daylight.seconds = daylight.seconds.y ) %>%
		select( date, daylight.seconds )
	
	# Approximate seconds of daylight for each individual day.
	n <- nrow( output )
	output$daylight.seconds.ip <- approx(
		output$date, 
		output$daylight.seconds, 
		xout = seq(output$date[1], output$date[n], "day" )
	)$y
	
	# Calculate hours.
	output$daylight.hours <- output$daylight.seconds.ip / 60 / 60
	output$dark.hours <- 24 - output$daylight.hours
	
	# Restrict to one sample month.
	output <- output %>%
		filter( date >= as.Date("2004-01-01") & date <= as.Date("2004-12-31") )
	
	# Add day and month colums.
	output <- output %>% 
		mutate( 
			day = lubridate::day( date ),
			month = lubridate::month( date )
		)
	
	# Leap year processing.
	output.leapyear <- output
	output.normalyear <- output %>% filter( date != as.Date("2004-02-29") )
	
	# Return value.
	output <- list( daylight_info = list( leapyear = output.leapyear, normalyear = output.normalyear) )
	output
}







