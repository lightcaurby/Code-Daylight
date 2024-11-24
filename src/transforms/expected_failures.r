import( "here" )
import( "modules" )
import( "stats" )
import( "rstatix" )
import( "dplyr" )
import( "purrr" )
import( "forcats" )
import( "lubridate" )

export( "run" )

# Transform daylight info.
run <- function( input, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here::here( "src/utils" ) ) )$debug$run( run )
	
	# Status information.
	cat( sprintf( "\texpected failures\n" ) )
	
	di <- input$daylight_info$normalyear %>% 
		select( date, dark.hours, day, month ) %>% 
		mutate( year = 0 )
	
	df.batches <- input$batch_predictions %>%
		map( function( l ) {
			as.data.frame( l["df" ] )
		}) %>% 	bind_rows()
	colnames( df.batches ) <- c( "EräID", "Hours.min", "Hours", "Hours.max" )
	
	expected_failures_initial <- input$replacements %>% 
		filter( Vaihdettu == F ) %>% 
		select( Huoneisto, EräID, Erä, Pvm ) %>% 
		mutate( EräID = as.integer( EräID ) ) %>%
		left_join( df.batches, by="EräID" ) %>%
		mutate( Day = day(Pvm), Month=month(Pvm), Year=year(Pvm)) %>%
		mutate( YearMonth = ( Year * 100 ) + Month ) %>%
		pmap( function( ... ) {
			df <- tibble( ... )
			lo <- cumsumThreshold( di, df$Month, df$Day, df$Hours.min )
			mid <- cumsumThreshold( di, df$Month, df$Day, df$Hours )
			hi <- cumsumThreshold( di, df$Month, df$Day, df$Hours.max )
			data.frame( 
				EräID=df$EräID, 
				Erä=df$Erä, 
				Pvm.min=make_date(df$Year + lo$year, lo$month, lo$day), 
				Pvm=make_date(df$Year + mid$year, mid$month, mid$day), 
				Pvm.max=make_date(df$Year + hi$year, hi$month, hi$day), 
				Huoneisto=df$Huoneisto )
		}) %>%
		bind_rows() %>%
		arrange( Pvm )
	
	today = Sys.Date()
	Pvm.today = make_date( year = year(today), month=month(today), day=1)
	YearMonth.today = ( year(Pvm.today) * 100 ) + month(Pvm.today)
	
	expected_failures <- 
		expected_failures_initial %>%
		mutate( 
			YearMonth.min = ( year(Pvm.min) * 100 ) + month(Pvm.min),
			YearMonth = ( year(Pvm) * 100 ) + month(Pvm),
			YearMonth.max = ( year(Pvm.max) * 100 ) + month(Pvm.max)
		) %>%
		mutate( 
			Huoneisto = fct_reorder( Huoneisto, Pvm ),
			Pvm.today = Pvm.today,
			Pvm.min.2x = as_date( if_else( YearMonth.min < YearMonth.today, NA, Pvm.min ) ),
			Pvm.2x = as_date( if_else( YearMonth < YearMonth.today, NA, Pvm ) ),
			Pvm.max.2x = as_date( if_else( YearMonth.max <= YearMonth.today, Pvm.today, Pvm.max ) ),
			Overtime.label = if_else( YearMonth.max < YearMonth.today, 
																paste0( 
																	year( Pvm.max ), ".", formatC( month ( Pvm.max ), width = 2, flag = "0" )
																),
																"" ),
		) %>%
		mutate( 
			Pvm.min.2 = make_date( year=year(Pvm.min.2x), month=month(Pvm.min.2x), day = 1),
			Pvm.2 = make_date( year=year(Pvm.2x), month=month(Pvm.2x), day = 1),
			Pvm.max.2 = make_date( year=year(Pvm.max.2x), month=month(Pvm.max.2x), day = 1),
			Pvm.min.3 = if_else( is.na( Pvm.min.2 ), Pvm.today, Pvm.min.2 ),
			Pvm.3 = if_else( is.na( Pvm.2 ), Pvm.today, Pvm.2 )
		)
	
	output <- list( expected_failures = expected_failures )
	output
}

cumsumThreshold <- function(
		di,
		m,
		d,
		thold
)
{
	y = 0
	cont <- TRUE
	while( cont )
	{
		if( y == 0 )
		{
			di.use <- di[ di$month > m | ( di$month == m & di$day > d ), ] 
		} 
		else
		{
			di.use <- di
		}
		
		satisfied <- which( cumsum( di.use$dark.hours ) >= thold )
		if( length( satisfied ) > 0 )
		{
			ind <- satisfied[ 1 ]
			res <- di.use[ ind, ] %>% select( day, month, year ) %>% mutate( year = year + y )
			cont <- FALSE
		}
		else
		{
			thold <- thold - sum( di.use$dark.hours )
			y <- y + 1
		}
	}
	res
}
