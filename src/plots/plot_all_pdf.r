options(conflicts.policy = list(error = TRUE, warn = FALSE))
import( "here" )
import( "modules" )
import( "ggplot2" )
import( "grDevices" )

export( "run" )

# Create a PDF for each plot.
run <- function( data.plot, height, width, ..., .debugmod=FALSE )
{
	if( .debugmod) browser();

	# Output all plots.
	invisible( lapply(data.plot$plots, function( p ) {
		
		# Output this plot.
		fn <- paste0( "output/plots/", "_", p$name, ".pdf" )
		cat( sprintf( "\tGenerating PDF file '%s'\n", fn ) )
		pdf( file = here( fn ), height=height, width=width)
			print( p$plot )
		dev.off()

	} ) )
	
	invisible( TRUE )
}