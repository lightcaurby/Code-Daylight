# Environment setup.
source( file = "src/env.R" )

# Flags.
options( "lightcaurby.Code-Daylight" = list(
	clean = T,	# Whether to remove output files before generating them again.
	cache.models = FALSE,  # Whether to cache model data to an RDS file.
	cache.plots = FALSE,  # Whether to cache model data to an RDS file.
	debug = c(			# Module names to stop in.
	)
))

# Main workflow.
lib.workflows <- suppressPackageStartupMessages( modules::use( here::here("src/workflows") ) )
result <- lib.workflows$full$run()

#CairoWin()

