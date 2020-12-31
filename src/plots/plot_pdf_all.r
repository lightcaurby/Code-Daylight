import( "here" )
import( "modules" )
import( "ggplot2" )
import( "grDevices" )

export( "run" )

# Create a PDF for each plot.
run <- function( data.plots, height, width, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$utils_debug$run( run )

	# Output all plots.
	invisible( lapply(data.plots, function( p ) {
		
		# Output this plot.
		fn <- paste0( "output/plots/", "plot_", p$name, ".pdf" )
		fn <- here( fn )
		
		# Check if the file needs to be cleaned first.
		myopts <- getOption( "lightcaurby.Code-Daylight", default = list() )
		if( myopts$clean & file.exists( fn ) ) file.remove( fn )
	
		# Generate the PDF file if it does not exist.
		if( file.exists( fn ) == FALSE )
		{
			# Generate PDF.
			cat( sprintf( "\tGenerating PDF file '%s'\n", fn ) )
			pdf( file = fn, height=height, width=width)
				print( p$plot )
			dev.off()
		}

	} ) )
	
	invisible( TRUE )
}