import( "here" )
import( "modules" )
import( "dplyr" )
import( "ggpubr" )
import( "rstatix" )

export( "run" )

# Extreme outliers?
run <- function( input, ..., .debugmod=FALSE )
{
	if( .debugmod ) browser();

	# Extreme outliers?
	t <- input$replacements %>%
		dplyr::filter( Vaihdettu & Er� %in% input$batches.multi$Er� ) %>%
		group_by( Er� ) %>%
		identify_outliers( Pime�tTunnit ) %>%
		select( Er�, Huoneisto, Pvm, PvmSeur, Pime�tTunnit, is.outlier, is.extreme )

	print( t )
}

