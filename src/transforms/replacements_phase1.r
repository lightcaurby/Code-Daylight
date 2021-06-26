import( "here" )
import( "modules" )
import( "dplyr" )
import( "forcats" )
import( "lubridate" )

export( "run" )

# Transform replacements.
run <- function( replacements, batches, daylight_info, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here::here( "src/utils" ) ) )$debug$run( run )

	# Combine the replacements and the batches.
	output <- replacements %>% 
		left_join( batches, by=c("Erä" = "Erä") ) %>%
		rename( EräID = Erä, Erä=Aika ) %>%
		select( -Selite )
	
	output <- output %>%
		arrange( Huoneisto, Pvm ) %>%
		group_by( Huoneisto ) %>%
		mutate( 
			PvmEro = as.numeric( difftime( lead(Pvm, 1), Pvm ) )
		) %>%
		ungroup() %>% 
		mutate( 
			AsennusVuosi = as.factor( lubridate::year( output$Pvm ) )
			#AsennusVuosi = as.factor( lag( lubridate::year( output$Pvm ), 1 ) )
		) %>%
		mutate ( Erä = fct_relevel(Erä, sort) )
	
	output <- output %>%
		arrange( Huoneisto, Pvm ) %>%
		group_by( Huoneisto ) %>%
		mutate( PvmSeur = lead( Pvm, 1 ) ) %>%
		ungroup() %>%
		group_by( Huoneisto ) %>%
		rowwise() %>%
		mutate( PimeätTunnit = ff( Pvm, PvmSeur, daylight_info ) ) %>%
		ungroup()
	
	output <- output %>%
		mutate( 
			PvmEro = ifelse(is.na(PvmSeur), as.numeric( difftime( today(), Pvm, units="days" ) ), PvmEro ),
			Vaihdettu = ifelse( is.na(PvmSeur), FALSE, TRUE )
		) %>% 
		group_by( Huoneisto ) %>%
		rowwise() %>%
		mutate( 
			PimeätTunnit = ifelse(is.na(PvmSeur), ff( Pvm, today(), daylight_info ), PimeätTunnit )
		) %>%
		ungroup()
	
	# Return value.
	output
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







