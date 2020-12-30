import( "here" )
import( "modules" )
import( "dplyr" )
import( "ggplot2" )
import( "ggpubr" )
import( "stats" )
import( "rstatix" )

export( "run" )

# Use the modules.
lib.models.common <- suppressPackageStartupMessages( modules::use( here( "src/models/common" ) ) )

# Normality test with a group-wise Shapiro-Wilk test.
# - In the QQ plot, if all the points fall approximately along the reference line, we can assume normality.
# - In the group-wise Shapiro-Wilk test, if the scores are normally distributed (p > 0.05) for each group, we can assume normality.
run <- function( input, models, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$utils_debug$run( run )

	# Filter the relevant batches.
	relevantBatches <- input$replacements %>%
		dplyr::filter( Vaihdettu & Erä %in% input$batches.multi$Erä )
	
	# Create a QQ plot of residuals.
	p <- ggqqplot( relevantBatches, "PimeätTunnit", facet.by = "Erä" ) +
		labs(
			title="Is there normality within each batch?",
			subtitle="Normality seems to apply to most of the batches."
		)

	# Compute Shapiro-Wilk test of normality.
	t <- relevantBatches %>%
		group_by( Erä ) %>%
		shapiro_test( PimeätTunnit )

	# Construct the result.
	result <- list(
		model = NULL,
		table = t,
		plot = p
	)
	result
	
}

