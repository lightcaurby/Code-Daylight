import( "here" )
import( "modules" )
import( "dplyr" )
import( "purrr" )
import( "ggplot2" )
import( "grDevices" )

export( "run" )

# Create a PDF for each plot.
run <- function( data.plots, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here::here( "src/utils" ) ) )$debug$run( run )

	# Target directory.
	targetDir <- "output/plots/"
	dir.create( targetDir, recursive=TRUE, showWarnings=FALSE )
	cat( sprintf( "\tGenerating files to '%s'\n", here::here( targetDir ) ) )
	
	# Output all plots.
	invisible( lapply(data.plots$grob_list, function( g ) {
		
		# Output this plot.
		fnBase <- paste0( "plot_", g$name, ".pdf" )
		fn <- paste0( targetDir, fnBase )
		fn <- here::here( fn )
		
		# Check if the file needs to be cleaned first.
		myopts <- getOption( "lightcaurby.Code-Daylight", default = list() )
		if( myopts$clean & file.exists( fn ) ) file.remove( fn )
	
		# Generate the PDF file if it does not exist.
		cat( sprintf( "\tGenerating '%s'", fnBase ) )
		if( file.exists( fn ) == FALSE )
		{
			# Generate PDF.
			cat( sprintf( "\n" ) )
			pdf( file = fn, height=g$plot_list$.height, width=g$plot_list$.width)
			
			g$plot_list$plots %>%
				map( function( p ) {
					print( p )
					NULL
				} ) %>%
				invisible()
			
			dev.off()
		}
		else
		{
			cat( sprintf( " (skipped, already available)\n" ) )
		}
		

	} ) )
	
	invisible( TRUE )
}