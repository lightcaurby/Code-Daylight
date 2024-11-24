import( "modules" )

export(	"run" )

# Full workflow.
run <- function( ... )
{
	# Read modules.
	lib.io <- suppressPackageStartupMessages( modules::use( here::here( "src/io" ) ) )
	lib.transform <- suppressPackageStartupMessages( modules::use( here::here( "src/transforms" ) ) )
	lib.plots <- suppressPackageStartupMessages( modules::use( here::here( "src/plots" ) ) )
	lib.models <- suppressPackageStartupMessages( modules::use( here::here( "src/models" ) ) )

	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here::here( "src/utils" ) ) )$debug$run( run )

	# Result.
	result <- list()
	
	# Check if the cached input data is available.
	cachedInputAvailable <- lib.io$cachedInput$is.available()
	
	# Read input data.
	cat( sprintf( "Reading input data\n" ) )
	result$data <- lib.io$all$run( result )

	# Transform the input data.
	cat( sprintf( "Transforming input data\n" ) )
	result$data <- lib.transform$all$run( result$data )

	#	Process the cached the input data.
	# cat( sprintf( "Starting to use the prepared input data\n" ) )
	# if( cachedInputAvailable )
	# {
	# 	cat( sprintf( "\tReading the input data from the cached RDS file\n" ) )
	# 	plotting_data <- lib.io$cachedInput$read()
	# }
	# else
	# {
	# 	cat( sprintf( "\tSaving the input data to a cached RDS file\n" ) )
	# 	lib.io$cachedInput$save( plotting_data )
	# }

	# Generate plots.
	cat( sprintf( "Generating plots\n" ) )
	result$plots <- lib.plots$all$run( result$data )
	# if( result$plots$actualRun == FALSE )
	# 	cat( sprintf( "\t(skipped, already available)\n" ) )
	
	# Output plots as PDFs.
	cat( sprintf( "Generating an output file for each plot\n" ) )
	lib.plots$all_pdf$run( result$plots, 4, 8 )

	# Run the modeling.
	cat( sprintf( "Running the models\n" ) )
	result$models <- lib.models$all$run( result$data )
	# if( result$models$actualRun == FALSE )
	# 	cat( sprintf( "\t(skipped, already available)\n" ) )
	
	# Output model plots and tables as PDFs.
	cat( sprintf( "Generating an output file for each model and table\n" ) )
	lib.models$all_pdf$run( result$models )
	
	# Complete workflow.
	cat( sprintf( "Workflow complete\n" ) )
	invisible(result)
}

# Conditionally runs the specified function.
runIfNotCachedDataAvailable <- function( message, cachedInputAvailable, f, ... )
{
	result = NULL
	cat( message )
	if( cachedInputAvailable )
		cat( sprintf( "\t(skipped, already available)\n" ) )
	else
		result <- f( ... )
	result
}


