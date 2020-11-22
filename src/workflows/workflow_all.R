import( "modules" )

# Read modules.
lib.io <- suppressPackageStartupMessages( modules::use( here( "src/io" ) ) )
lib.transform <- suppressPackageStartupMessages( modules::use( here( "src/transforms" ) ) )
lib.plots <- suppressPackageStartupMessages( modules::use( here( "src/plots" ) ) )
lib.models <- suppressPackageStartupMessages( modules::use( here( "src/models" ) ) )

export(	"run" )

# Full workflow.
run <- function( ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$utils_debug$run( run )

	# Read input data.
	cat( sprintf( "Reading daylight info\n" ) )
	daylight_info <- lib.io$input_daylight_info$run()
	cat( sprintf( "Reading replacement data\n" ) )
	replacements <- lib.io$input_replacements$run()
	cat( sprintf( "Reading batch data\n" ) )
	batches <- lib.io$input_batches$run()

	# Transform the input data.
	cat( sprintf( "Wrangling daylight info\n" ) )
	daylight_info <- lib.transform$wrangle_daylight_info$run( daylight_info)
	cat( sprintf( "Wrangling replacements data\n" ) )
	replacements <- lib.transform$wrangle_replacements$run( replacements, batches, daylight_info )
	cat( sprintf( "Preparing plotting data\n" ) )
	plotting_data <-  lib.transform$prepare_replacements_for_plotting$run( replacements )

	# Generate plots.
	cat( sprintf( "Generating plots\n" ) )
	plotting_data$plots <- lib.plots$plot_run_all$run( plotting_data )

	# Output plots as PDFs.
	cat( sprintf( "Generating PDF from each plot\n" ) )
	lib.plots$plot_pdf_all$run( plotting_data, 4, 8 )

	# Run the modeling.
	cat( sprintf( "Running the models\n" ) )
	plotting_data$models <- lib.models$model_run_all$run( plotting_data )

	# Complete workflow.
	cat( sprintf( "Workflow complete\n" ) )
	invisible(plotting_data)
}



