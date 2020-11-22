import( "here" )
import( "modules" )
import( "dplyr" )
import( "ggpubr" )
import( "rstatix" )

export( "run" )

# Extreme outliers?
run <- function( input, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$utils_debug$run( run )

	# Extreme outliers?
	t <- input$replacements %>%
		dplyr::filter( Vaihdettu & Erä %in% input$batches.multi$Erä ) %>%
		group_by( Erä ) %>%
		identify_outliers( PimeätTunnit ) %>%
		select( Erä, Huoneisto, Pvm, PvmSeur, PimeätTunnit, is.outlier, is.extreme )

	print( t )
}

