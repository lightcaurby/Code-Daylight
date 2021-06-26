import( "here" )
import( "modules" )
import( "stats" )
import( "dplyr" )
import( "purrr" )
import( "broom" )
import( "stats" )
import( "rstatix" )
import( "grid" )
import( "gridGraphics" )
import( "grDevices" )
import( "Cairo" )
import( "graphics" )

export( "run" )

# Use the modules.
lib.models.common <- suppressPackageStartupMessages( modules::use( here::here( "src/models/common" ) ) )

# The residuals versus fits plot can be used to check the homogeneity of variances.
# If in the plot below there is no evident relationships between residuals and fitted values 
# (the mean of each groups), we can assume the homogeneity of variances.
run <- function( input, models, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here::here( "src/utils" ) ) )$debug$run( run )
	
	# Subroutines.
	baseName <- "run.variance.homogeneity."
	routines <- paste0( baseName, 
											c(
												"residuals",
												"levene",
												"bartlett",
												"fligner.killeen",
												"summary"
											) )

	# Run the subroutines.	
	result <- list()
	map( routines, function( r )
	{
		# Get the routine.
		f <- get( r )
		subName <- substring(r, nchar( baseName ) + 1 )
		
		# Run the routine.
		result[[ subName ]] <<- f( input, models, result, ... )
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
	d <- tidy( models[[ "normality_linear_model" ]]$output$model )
	
	# Plot the residuals.
	#CairoWin()
	par.old <- par( no.readonly=TRUE )
	par(mar = c(5, 6, 4, 2))
	plot( models[[ "normality_linear_model" ]]$output$model, 1, las = 1,
		main = "Horizontal zero-line indicates variance homogeneity" )
	suppressWarnings( grid.echo() )
	p <- grid.grab()
	par( par.old )
	#dev.off()
		
	# Construct the result.
	result <- lib.models.common$helpers$my.construct.result(
		data = d,
		plot = p,
		plot.w = 8,
		plot.h = 6
	)
	result
}


# Homogeneity of variances with Levene.
run.variance.homogeneity.levene <- function( input, models, ... )
{
	# Levene's test to check the homogeneity of variances.
	# With p>0.05, there is not significant difference between variances across groups, therefore
	# we can assume the homogeneity of variances in the different treatment groups.
	d <- input$replacements %>%
		dplyr::filter( Vaihdettu & Erä %in% input$batches.multi$Erä ) %>%
		levene_test( PimeätTunnit ~ Erä )

	# Construct the result.
	result <- lib.models.common$helpers$my.construct.result(
		data = d
	)
	result
}

# Homogeneity of variances with Bartlett.
run.variance.homogeneity.bartlett <- function( input, models, ... )
{
	# Bartlett test to check the homogeneity of variances.
	d <- input$replacements %>%
		dplyr::filter( Vaihdettu & Erä %in% input$batches.multi$Erä ) %>%
		group_by( Erä ) %>%
		add_count() %>%
		filter( n > 3 ) %>%
		ungroup() %>%
		bartlett.test( PimeätTunnit ~ Erä, data = . ) %>%
		tidy()
	
	# Construct the result.
	result <- lib.models.common$helpers$my.construct.result(
		data = d
	)
	result
}

# Homogeneity of variances with Fligner-Killeen.
run.variance.homogeneity.fligner.killeen <- function( input, models, ... )
{
	# Fligner-Killeen test to check the homogeneity of variances.
	d <- input$replacements %>%
		dplyr::filter( Vaihdettu & Erä %in% input$batches.multi$Erä ) %>%
		group_by( Erä ) %>%
		add_count() %>%
		filter( n > 3 ) %>%
		ungroup() %>%
		fligner.test( PimeätTunnit ~ Erä, data = . ) %>%
		tidy()
	
	# Construct the result.
	result <- lib.models.common$helpers$my.construct.result(
		data = d
	)
	result
}

# Summary of variance homogeneity.
run.variance.homogeneity.summary <- function( input, models, result, ... )
{
	# Combine the prepared data.
	df <- bind_rows(
		lib.models.common$helpers$my.preprocess.table.levene(
			result[[ "levene" ]]$data
		),
		lib.models.common$helpers$my.preprocess.table.others(
			result[[ "fligner.killeen" ]]$data
		),
		lib.models.common$helpers$my.preprocess.table.others(
			result[[ "bartlett" ]]$data
		)
	)
	
	# Construct the table.
	t <- lib.models.common$helpers$my.ggtexttable.wrapper(
		df, 
		"Reject null hypothesis of equal variances?", 
		6, 
		lib.models.common$helpers$my.modified.ggtable.theme( df ),
		subtitle="Borderline decision in both cases"
	)
	
	# Construct the result.
	result <- lib.models.common$helpers$my.construct.result(
		table = t,
		table.w = 7,
		table.h = 2
	)
	result
	
}
