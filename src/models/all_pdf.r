import( "here" )
import( "modules" )
import( "purrr" )
import( "ggplot2" )
import( "grid" )
import( "grDevices" )

export( "run" )

# Create a PDF for each model plot and table.
run <- function( data.models, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$debug$run( run )

	# Get the custom options.
	myopts <- getOption( "lightcaurby.Code-Daylight", default = list() )
	
	# Output all plots.
	targetDir <- "output/plots/"
	cat( sprintf( "\tGenerating files to '%s'\n", here( targetDir ) ) )
	OutputPDF( data.models, "plot", targetDir, myopts )

	# Output all tables.
	targetDir <- "output/tables/"
	cat( sprintf( "\tGenerating files to '%s'\n", here( targetDir ) ) )
	OutputPDF( data.models, "table", targetDir, myopts )

	invisible( TRUE )
}

# Determine the dimensions of the output.
DetermineDimensions <- function( source, outputType )
{
	# Determine the dimensions of the output.
	height <- 4
	width <- 8
	switch(
		outputType,
		plot = { height <- source[[ "plot.h" ]]; width <- source[[ "plot.w" ]] },
		table = { height <- source[[ "table.h" ]]; width <- source[[ "table.w" ]] }
	)
	list(
		h = height, 
		w = width
	)
}

# Process the actual PDF output.
Process <- function( rootDir, myopts, source, toplevelName, outputObject, outputType, upperLevel = NULL )
{
	# Determine the dimensions of the output.
	dims <- DetermineDimensions( source, outputType )
	
	# Determine the file name of the output.
	fnBase <- NULL
	if( ! is.null( upperLevel ) )
		fnBase <- paste0( "model_", toplevelName, ".", upperLevel, ".pdf" )
	else
		fnBase <- paste0( "model_", toplevelName, ".pdf" )
	fn <- paste0( rootDir, fnBase )
	fn <- here( fn )

	# Check if the file needs to be cleaned first.
	if( myopts$clean & file.exists( fn ) ) file.remove( fn )
	
	# Output the PDF file.
	# Generate the PDF file if it does not exist.
	cat( sprintf( "\tGenerating '%s'", fnBase ) )
	if( file.exists( fn ) == FALSE )
	{
		cat( sprintf( "\n" ) )
		pdf( file = fn, height=dims$h, width=dims$w )
		if( ! "ggplot" %in% class( outputObject ) )
			grid.newpage()
		grid.draw( outputObject )
		dev.off()
	}
	else
	{
		cat( sprintf( " (skipped, already available)\n" ) )
	}
}

# Generate the PDF files for tables and plots.
OutputPDF <- function( data.models, expect, rootDir, myopts )
{
	# Hide the return value.
	invisible( 
		
		# Loop through the main types of models.
		map( data.models$phase2, function( m )
		{
			# Prepare the expected type(s) of output.
			expected <- c( expect )
			
			# Loop through the output details.
			nos1 <- names( m$output )
			map( nos1, function( no1 )
			{
				# This output detail.
				x1 <- m$output[[ no1 ]]
				if( !is.null( x1 ) )
				{
					# Is this an expected output?
					if( no1 %in% expected )
					{
						# Process this.
						Process( rootDir, myopts, m$output, m$name, x1, no1 )
					}
					else
					{
						# Loop through the output details of the next level.
						nos2 <- names( x1 )
						map( nos2, function( no2 )
						{
							# This sub-level output detail.
							x2 <- x1[[ no2 ]]
							if( !is.null( x2 ) )
							{
								# Is this an expected output?
								if( no2 %in% expected )
								{
									# Process this.
									Process( rootDir, myopts, x1, m$name, x2, no2, no1 )
								}
							} 
						}
						)
					}
				} 
			}
			)
			NULL
		}
		)
	)
}
