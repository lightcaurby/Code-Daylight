import( "here" )
import( "modules" )
import( "stats" )
import( "dplyr" )
import( "rstatix" )

export( "run" )

# Transform daylight info.
run <- function( input, ..., .debugmod=FALSE )
{
	if( .debugmod) browser();
	
	# Prepare data.
	output <- input %>% filter( is.na( PvmEro ) == F )
	output$VuosiEro <- output$PvmEro / 365
	output$Tila <- output$Huoneisto
	output$ArvoVuosi <- output$VuosiEro
	output$ArvoTunnit <- output$Pime�tTunnit
	output$Tyyppi = factor( output$Tyyppi, levels=c("pienloiste", "led" ) )
	
	# Find mode values.
	dms.vuosi <- my.densMode( output %>% filter(Vaihdettu), "ArvoVuosi" )
	dms.tunti <- my.densMode( output %>% filter(Vaihdettu), "ArvoTunnit" )
	
	py.pvm <- pretty(output$PvmEro, n=10) 
	py.vuosi <- pretty(output$VuosiEro, n=10) 
	py.pime� <- pretty(output$Pime�tTunnit, n=10) 
	
	# Er�t joissa enemm�n kuin 1 rivi
	er�t <- output %>%
		dplyr::filter(Vaihdettu) %>%
		group_by(Er�) %>%
		get_summary_stats(Pime�tTunnit, type = "mean_sd") %>%
		filter( n > 2 ) %>%
		select( Er� )
	
	# Return value.
	result <- list(
		replacements = output,
		batches.multi = er�t,
		dms.year = dms.vuosi,
		dms.hour = dms.tunti,
		py.date = py.pvm,
		py.year = py.vuosi,
		py.darkness = py.pime�
	)
	result

}

# Determine the mode by density.
my.densMode <- function(df, colname){
	data <- df %>% 
		select( colname)
	td <- density( data[[ colname ]] )
	maxDens <- which.max( td$y )
	list( x = td$x[ maxDens ], y = td$y[ maxDens ] )
}
