import( "here" )
import( "modules" )
import( "stats" )
import( "dplyr" )
import( "purrr" )
import( "broom" )
import( "stats" )
import( "rstatix" )

export( "run" )

# The residuals versus fits plot can be used to check the homogeneity of variances.
# If in the plot below there is no evident relationships between residuals and fitted values 
# (the mean of each groups), we can assume the homogeneity of variances.
run <- function( input, models, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$utils_debug$run( run )
	
	# Subroutines.
	baseName <- "run.variance.homogeneity."
	routines <- paste0( baseName, 
											c(
												"residuals",
												"levene",
												"bartlett",
												"fligner.killeen"
											) )

	# Run the subroutines.	
	result <- list()
	map( routines, function( r )
	{
		# Get the routine.
		f <- get( r )
		subName <- substring(r, nchar( baseName ) + 1 )
		
		# Run the routine.
		result[[ subName ]] <<- f( input, models, ... )
		result[[ subName ]]$name <<- subName
		
	})
	result
}

# The residuals versus fits plot can be used to check the homogeneity of variances.
# If in the plot below there is no evident relationships between residuals and fitted values 
# (the mean of each groups), we can assume the homogeneity of variances.
run.variance.homogeneity.residuals <- function( input, models, ... )
{

	# Table of the model.
	t <- tidy( models[[ "normality_linear_model" ]]$output$model )
	
	# Plot the residuals.
	p <- plot( models[[ "normality_linear_model" ]]$output$model, 1 )
	
	# Construct the result.
	result <- list(
		model = NULL,
		table = t,
		plot = p
	)
	result
}


# Homogeneity of variances with Levene.
run.variance.homogeneity.levene <- function( input, models, ... )
{
	# Levene's test to check the homogeneity of variances.
	# With p>0.05, there is not significant difference between variances across groups, therefore
	# we can assume the homogeneity of variances in the different treatment groups.
	t <- input$replacements %>%
		dplyr::filter( Vaihdettu & Erä %in% input$batches.multi$Erä ) %>%
		levene_test( PimeätTunnit ~ Erä )

	# Construct the result.
	result <- list(
		model = NULL,
		table = t,
		plot = NULL
	)
	result
}

# Homogeneity of variances with Bartlett.
run.variance.homogeneity.bartlett <- function( input, models, ... )
{
	# Bartlett test to check the homogeneity of variances.
	t <- input$replacements %>%
		dplyr::filter( Vaihdettu & Erä %in% input$batches.multi$Erä ) %>%
		group_by( Erä ) %>%
		add_count() %>%
		filter( n > 3 ) %>%
		ungroup() %>%
		bartlett.test( PimeätTunnit ~ Erä, data = . ) %>%
		tidy()
	
	# Construct the result.
	result <- list(
		model = NULL,
		table = t,
		plot = NULL
	)
	result
}

# Homogeneity of variances with Fligner-Killeen.
run.variance.homogeneity.fligner.killeen <- function( input, models, ... )
{
	# Fligner-Killeen test to check the homogeneity of variances.
	t <- input$replacements %>%
		dplyr::filter( Vaihdettu & Erä %in% input$batches.multi$Erä ) %>%
		group_by( Erä ) %>%
		add_count() %>%
		filter( n > 3 ) %>%
		ungroup() %>%
		fligner.test( PimeätTunnit ~ Erä, data = . ) %>%
		tidy()
	
	# Construct the result.
	result <- list(
		model = NULL,
		table = t,
		plot = NULL
	)
	result
}

