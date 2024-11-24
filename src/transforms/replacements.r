import( "here" )
import( "modules" )
import( "dplyr" )
import( "forcats" )
import( "lubridate" )
import( "stats" )
import( "rstatix" )

export( "run" )

# Transform replacements.
run <- function( input, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here::here( "src/utils" ) ) )$debug$run( run )

	# Status information.
	cat( sprintf( "\tbatches and replacements\n" ) )
	
	# Combine the replacements and the batches.
	replacements <- input$replacements %>% 
		left_join( input$batches, by=c("Erä" = "Erä") ) %>%
		rename( EräID = Erä, Erä=Aika ) %>%
		select( -Selite )
	
	replacements <- replacements %>%
		arrange( Huoneisto, Pvm ) %>%
		group_by( Huoneisto ) %>%
		mutate( 
			PvmEro = as.numeric( difftime( lead(Pvm, 1), Pvm ) )
		) %>%
		ungroup() %>% 
		mutate( 
			AsennusVuosi = as.factor( lubridate::year( replacements$Pvm ) )
			#AsennusVuosi = as.factor( lag( lubridate::year( replacements$Pvm ), 1 ) )
		) %>%
		mutate ( Erä = fct_relevel(Erä, sort) )
	
	replacements <- replacements %>%
		arrange( Huoneisto, Pvm ) %>%
		group_by( Huoneisto ) %>%
		mutate( PvmSeur = lead( Pvm, 1 ) ) %>%
		ungroup() %>%
		group_by( Huoneisto ) %>%
		rowwise() %>%
		mutate( PimeätTunnit = ff( Pvm, PvmSeur, input$daylight_info ) ) %>%
		ungroup()
	
	replacements <- replacements %>%
		mutate( 
			PvmEro = ifelse(is.na(PvmSeur), as.numeric( difftime( today(), Pvm, units="days" ) ), PvmEro ),
			Vaihdettu = ifelse( is.na(PvmSeur), FALSE, TRUE )
		) %>% 
		group_by( Huoneisto ) %>%
		rowwise() %>%
		mutate( 
			PimeätTunnit = ifelse(is.na(PvmSeur), ff( Pvm, today(), input$daylight_info ), PimeätTunnit )
		) %>%
		ungroup()

	# Further preparation.
	replacements <- replacements %>% filter( is.na( PvmEro ) == F )
	replacements$VuosiEro <- replacements$PvmEro / 365
	replacements$Tila <- replacements$Huoneisto
	replacements$ArvoVuosi <- replacements$VuosiEro
	replacements$ArvoTunnit <- replacements$PimeätTunnit
	replacements$Tyyppi = factor( replacements$Tyyppi, levels=c("pienloiste", "led" ) )

	# Find mode values.
	dms.year <- my.densMode( replacements %>% filter(Vaihdettu), "ArvoVuosi" )
	dms.hour <- my.densMode( replacements %>% filter(Vaihdettu), "ArvoTunnit" )

	# Pretty-printed values.
	py.date <- pretty(replacements$PvmEro, n=10) 
	py.year <- pretty(replacements$VuosiEro, n=10) 
	py.darkness <- pretty(replacements$PimeätTunnit, n=10) 

	# Batches with more than one sample.
	batches.multi <- replacements %>%
		dplyr::filter(Vaihdettu) %>%
		group_by(Erä) %>%
		get_summary_stats(PimeätTunnit, type = "mean_sd") %>%
		filter( n > 2 ) %>%
		select( Erä )
	
	# Return value.
	output <- list(
		replacements = replacements,
		batches.multi = batches.multi,
		dms.year = dms.year,
		dms.hour = dms.hour,
		py.date = py.date,
		py.year = py.year,
		py.darkness = py.darkness
	)
	output
}

# Determine the mode by density.
my.densMode <- function(df, colname){
	data <- df %>% 
		select( colname)
	td <- density( data[[ colname ]] )
	maxDens <- which.max( td$y )
	list( x = td$x[ maxDens ], y = td$y[ maxDens ] )
}

# Calculate dark hours.
ff <- function( dt.1, dt.2, daylight_info )
{
	year.1 <- lubridate::year( dt.1 )
	month.1 <- lubridate::month( dt.1 )
	day.1 <- lubridate::day( dt.1 )
	year.2 <- lubridate::year( dt.2 )
	month.2 <- lubridate::month( dt.2 )
	day.2 <- lubridate::day( dt.2 )
	
	years.full <- max( c( 0, year.2 - year.1 - 1) )
	ret <- NA
	if( !is.na( year.1) & !is.na( year.2 ))
	{
		if( year.1 != year.2 )
		{
			partial.1 <- daylight_info$normalyear %>%
				arrange( month, day ) %>%
				filter( month > month.1 | ( month == month.1 & day >= day.1 ) ) %>%
				summarize( total = sum( dark.hours) ) %>%
				.$total
			
			partial.2 <- daylight_info$normalyear %>%
				arrange( month, day ) %>%
				filter( month < month.2 | ( month == month.2 & day <= day.2 ) ) %>%
				summarize( total = sum( dark.hours) ) %>%
				.$total
			
		}
		else
		{
			partial.1 <- daylight_info$normalyear %>%
				arrange( month, day ) %>%
				filter( ( month > month.1 | ( month == month.1 & day >= day.1 ) )	& ( month < month.2 | ( month == month.2 & day <= day.2 ) ) )	%>%
				summarize( total = sum( dark.hours) ) %>%
				.$total
			
			partial.2 <- 0
		}
		ret <- partial.1 + partial.2 + ( sum( daylight_info$normalyear$dark.hours ) * years.full )
	}
	ret
}







