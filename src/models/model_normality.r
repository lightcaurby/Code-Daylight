import( "here" )
import( "modules" )
import( "dplyr" )
import( "purrr" )
import( "ggplot2" )
import( "ggpubr" )
import( "stats" )
import( "rstatix" )

export( "run" )

# Use the modules.
lib.models.common <- suppressPackageStartupMessages( modules::use( here( "src/models/common" ) ) )

# Normality test.
run <- function( input, models, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$utils_debug$run( run )

	# Subroutines.
	baseName <- "run.normality."
	routines <- paste0( baseName, 
											c(
												"all",
												"by.group",
												"summary"
											) )
	
	# Run the subroutines.	
	result <- list( length( routines ) )
	map( routines, function( r )
	{
		# Get the routine.
		f <- get( r )
		subName <- substring(r, nchar( baseName ) + 1 )
		
		# Run the routine.
		result[[ subName ]] <<- f( input, models, result, ... )
		
	})
	result
}

# Normality test with an overall Shapiro-Wilk test.
# - In the QQ plot, if all the points fall approximately along the reference line, we can assume normality.
# - By the overall Shapiro-Wilk test, if the p-value is not significant (p > 0.05), we can assume normality.
run.normality.all <- function( input, models, ... )
{
	# Create a QQ plot of residuals.
	p <- ggqqplot( residuals( models[[ "normality_linear_model" ]]$output$model ) ) +
		labs( 
			title="Is there normality across all data?",
			subtitle="Data slightly deviates from the overall normality." 
		)
	
	# Compute Shapiro-Wilk test of normality.
	d <- shapiro_test( residuals( models[[ "normality_linear_model" ]]$output$model ) )
	
	# Construct the result.
	result <- lib.models.common$helpers$my.construct.result(
		data = d,
		plot = p,
		plot.w = 8,
		plot.h = 6
	)
	result
}

# Normality test with a group-wise Shapiro-Wilk test.
# - In the QQ plot, if all the points fall approximately along the reference line, we can assume normality.
# - In the group-wise Shapiro-Wilk test, if the scores are normally distributed (p > 0.05) for each group, we can assume normality.
run.normality.by.group <- function( input, models, ... )
{
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
	d <- relevantBatches %>%
		group_by( Erä ) %>%
		shapiro_test( PimeätTunnit )
	
	# Construct the result.
	result <- lib.models.common$helpers$my.construct.result(
		data = d,
		plot = p,
		plot.w = 8,
		plot.h = 8
	)
	result
	
}

# Normality test summary.
run.normality.summary <- function( input, models, result, ... )
{
	# Combine the prepared data.
	df <- bind_rows(
		
		result[[ "all" ]]$data %>%
			rename( p = p.value ) %>%
			mutate(
				Erä = "Combined",
				variable = "PimeätTunnit"
			) %>%
			mutate( 
				reject = ( p < 0.05 ),
				statistic = lib.models.common$helpers$my.decimal.format( ., statistic, 3  ),
				p = lib.models.common$helpers$my.decimal.format( ., p, 3  )
			) %>%
			select( Erä, variable, statistic, p, reject ),
		
		result[[ "by.group" ]]$data %>%
			mutate( 
				reject = ( p < 0.05 ),
				statistic = lib.models.common$helpers$my.decimal.format( ., statistic, 3  ),
				p = lib.models.common$helpers$my.decimal.format( ., p, 3  )
			) %>%
			select( Erä, variable, statistic, p, reject )
	)
	
	# Construct the table.
	t <- lib.models.common$helpers$my.ggtexttable.wrapper( df, "Reject null hypothesis of normality?", 5 )
	
	# Construct the result.
	result <- lib.models.common$helpers$my.construct.result(
		table = t,
		table.w = 5,
		table.h = 4
	)
	result
	
}
