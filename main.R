# Environment setup.
source( file = "src/env.R" )

# Flags.
options( "lightcaurby.Code-Daylight" = list(
	clean = FALSE,	# Whether to remove output files before generating them again.
	debug = c(			# Module names to stop in.
	)
))

# Main workflow.
lib <- suppressPackageStartupMessages( modules::use( here("src/workflows") ) )
plotting_data <- lib$workflow_all$run()


