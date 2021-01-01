import( "modules" )

export(	"run" )

# Full workflow.
run <- function( ... )
{
	# Read modules.
	lib.io <- suppressPackageStartupMessages( modules::use( here( "src/io" ) ) )
	lib.transform <- suppressPackageStartupMessages( modules::use( here( "src/transforms" ) ) )
	lib.plots <- suppressPackageStartupMessages( modules::use( here( "src/plots" ) ) )
	lib.models <- suppressPackageStartupMessages( modules::use( here( "src/models" ) ) )
	
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$utils_debug$run( run )

	# Check if the cached input data is available.
	cachedInputAvailable <- lib.io$cachedInput$is.available()
	
	# Read input data.
	daylight_info <- runIfNotCachedDataAvailable(
		sprintf( "Reading daylight info\n" ),
		cachedInputAvailable,
		lib.io$daylight_info$read
	)
	replacements <- runIfNotCachedDataAvailable(
		sprintf( "Reading replacement data\n" ),
		cachedInputAvailable,
		lib.io$replacements$read
	)
	batches <- runIfNotCachedDataAvailable(
		sprintf( "Reading batch data\n" ),
		cachedInputAvailable,
		lib.io$batches$read
	)

	# Transform the input data.
	daylight_info <- runIfNotCachedDataAvailable(
		sprintf( "Wrangling daylight info\n" ),
		cachedInputAvailable,
		lib.transform$wrangle_daylight_info$run,
		daylight_info
	)
	replacements <- runIfNotCachedDataAvailable(
		sprintf( "Wrangling replacements data\n" ),
		cachedInputAvailable,
		lib.transform$wrangle_replacements$run,
		replacements,
		batches,
		daylight_info
	)
	plotting_data <- runIfNotCachedDataAvailable(
		sprintf( "Preparing plotting data\n" ),
		cachedInputAvailable,
		lib.transform$prepare_replacements_for_plotting$run,
		replacements
	)

	#	Process the cached the input data.
	cat( sprintf( "Starting to use the prepared input data\n" ) )
	if( cachedInputAvailable )
	{
		cat( sprintf( "\tReading the input data from the cached RDS file\n" ) )
		plotting_data <- lib.io$cachedInput$read()
	}
	else
	{
		cat( sprintf( "\tSaving the input data to a cached RDS file\n" ) )
		lib.io$cachedInput$save( plotting_data )
	}

	# Generate plots.
	cat( sprintf( "Generating plots\n" ) )
	plotting_data$plots <- lib.plots$plot_run_all$run( plotting_data )
	if( plotting_data$plots$actualRun == FALSE )
		cat( sprintf( "\t(skipped, already available)\n" ) )
	
	# Output plots as PDFs.
	cat( sprintf( "Generating an output file for each plot\n" ) )
	lib.plots$plot_pdf_all$run( plotting_data$plots, 4, 8 )

	# Run the modeling.
	cat( sprintf( "Running the models\n" ) )
	plotting_data$models <- lib.models$model_run_all$run( plotting_data )
	if( plotting_data$models$actualRun == FALSE )
		cat( sprintf( "\t(skipped, already available)\n" ) )
	
	# Output model plots and tables as PDFs.
	cat( sprintf( "Generating an output file for each model and table\n" ) )
	lib.models$model_pdf_all$run( plotting_data$models )
	
	# Complete workflow.
	cat( sprintf( "Workflow complete\n" ) )
	invisible(plotting_data)
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


