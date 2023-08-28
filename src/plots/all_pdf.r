import( "here" )
import( "modules" )
import( "ggplot2" )
import( "grDevices" )

export( "run" )

# Create a PDF for each plot.
run <- function( data.plots, height, width, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here::here( "src/utils" ) ) )$debug$run( run )

	# Target directory.
	targetDir <- "output/plots/"
	dir.create( targetDir, recursive=TRUE )
	cat( sprintf( "\tGenerating files to '%s'\n", here::here( targetDir ) ) )
	
	# Output all plots.
	invisible( lapply(data.plots$grobs, function( p ) {
		
		# Output this plot.
		fnBase <- paste0( "plot_", p$name, ".pdf" )
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
			pdf( file = fn, height=height, width=width)
				print( p$plot )
			dev.off()
		}
		else
		{
			cat( sprintf( " (skipped, already available)\n" ) )
		}
		

	} ) )
	
	invisible( TRUE )
}