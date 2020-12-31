import( "here" )
import( "modules" )
import( "dplyr" )
import( "ggpubr" )
import( "rstatix" )

export( "run" )

# Use the modules.
lib.models.common <- suppressPackageStartupMessages( modules::use( here( "src/models/common" ) ) )

# Extreme outliers?
run <- function( input, models, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$utils_debug$run( run )

	# Extreme outliers?
	d <- input$replacements %>%
		dplyr::filter( Vaihdettu & Erä %in% input$batches.multi$Erä ) %>%
		group_by( Erä ) %>%
		identify_outliers( PimeätTunnit ) %>%
		select( Erä, Huoneisto, Pvm, PvmSeur, PimeätTunnit, is.outlier, is.extreme )

	# Construct the table object
	df <- d %>%
		mutate(
			PimeätTunnit = lib.models.common$helpers$my.decimal.format( ., PimeätTunnit, 2 ),
			reject = is.extreme
		)
	t <- lib.models.common$helpers$my.ggtexttable.wrapper( df, "Reject extreme outliers?", 8 )

	# Construct the result.
	result <- lib.models.common$helpers$my.construct.result(
		data = d,
		table = t,
		table.w = 8,
		table.h = 2
	)

	result
}

