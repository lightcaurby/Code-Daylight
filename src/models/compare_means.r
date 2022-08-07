import( "here" )
import( "modules" )
import( "stats" )
import( "dplyr" )
import( "purrr" )
import( "broom" )
import( "ggpubr" )
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

# Comparing the means.
run <- function( input, models, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here::here( "src/utils" ) ) )$debug$run( run )

	# Subroutines.
	baseName <- "run.compare.means."
	routines <- paste0( baseName, 
											c(
												"prepare.anova",
												"anova",
												"anova.with.contrasts",
												"kruskal.wallis",
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

# Comparing means with one-waY ANOVA.
run.compare.means.prepare.anova <- function( input, models, ... )
{
	# Anova test to compare the means.
	t <- input$replacements %>%
		dplyr::filter( Vaihdettu & Erä %in% input$batches.multi$Erä ) %>%
		aov( PimeätTunnit ~ Erä, data = . ) 
	
	
	# Construct the result.
	result <- list(
		model = t,
		data = NULL,
		table = NULL,
		plot = NULL
	)
	result
}


# Comparing means with one-waY ANOVA.
run.compare.means.anova <- function( input, models, result, ... )
{
	# Prepared Anova test to compare the means.
	t <- result[[ "prepare.anova" ]]$model

	# Create a QQ plot of residuals.
	#p <- ggqqplot( residuals( t ) )
	
	# Construct the result.
	result <- list(
		model = t,
		data = tidy( t ),
		table = NULL,
		plot = NULL
	)
	result
}

# Comparing means with one-waY ANOVA.
run.compare.means.anova.with.contrasts <- function( input, models, result, ... )
{
	# Anova test to compare the means.
	m <- TukeyHSD( result[[ "prepare.anova" ]]$model )

	# Create a plot of contrasts.
	#CairoWin()
	par.old <- par( no.readonly=TRUE )
	par(mar = c(5, 12, 4, 2))
	plot( m, las=1 )
	grid.echo()
	p <- grid.grab()
	par( par.old )
	#dev.off()
	
	# Data.
	d <- tidy( m )
	
	df <- d %>%
		mutate( 
			reject = ( adj.p.value < 0.05 ),
			estimate = lib.models.common$helpers$my.decimal.format( ., estimate, 2  ),
			conf.low = lib.models.common$helpers$my.decimal.format( ., conf.low, 2  ),
			conf.high = lib.models.common$helpers$my.decimal.format( ., conf.high, 2  ),
			adj.p.value = lib.models.common$helpers$my.decimal.format( ., adj.p.value, 3  )
		)
	t <- lib.models.common$helpers$my.ggtexttable.wrapper( df, "Reject null hypothesis of equal batch means?", 8 )
	
	
	# Construct the result.
	result <- lib.models.common$helpers$my.construct.result(
			model = m,
			data = d,
			table = t,
			table.w = 8,
			table.h = 7,
			plot = p,
			plot.w = 9,
			plot.h = 6
	)
	result
}

# Comparing means with Kruskal-Wallis.
run.compare.means.kruskal.wallis <- function( input, models, ... )
{
	# Kruskal-Wallis test to compare the means.
	d <- input$replacements %>%
		dplyr::filter( Vaihdettu & Erä %in% input$batches.multi$Erä ) %>%
		kruskal.test( PimeätTunnit ~ Erä, data = . ) %>%
		tidy()
	
	# Construct the result.
	result <- lib.models.common$helpers$my.construct.result(
		data = d
	)
	result
}

# Summary of means comparison.
run.compare.means.summary <- function( input, models, result, ... )
{
	# Combine the prepared data.
	df <- bind_rows(
		lib.models.common$helpers$my.preprocess.table.anova(
			result[[ "anova" ]]$data
		),
		lib.models.common$helpers$my.preprocess.table.others(
			result[[ "kruskal.wallis" ]]$data
		)
	)
	
	# Construct the table.
	t <- lib.models.common$helpers$my.ggtexttable.wrapper(
		df, 
		"Reject null hypothesis of equal means?", 
		6, 
		lib.models.common$helpers$my.modified.ggtable.theme( df )
	)

	# Construct the result.
	result <- lib.models.common$helpers$my.construct.result(
		table = t,
		table.w = 6,
		table.h = 2
	)
	result
	
}
