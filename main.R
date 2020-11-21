# Environment setup.
source( file = "src/env.R" )

# Main workflow.
lib <- suppressPackageStartupMessages( modules::use( here("src/workflows") ) )
plotting_data <- lib$workflow_all$run( .debugmod=F )


