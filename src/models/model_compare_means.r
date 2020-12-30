import( "here" )
import( "modules" )
import( "stats" )
import( "dplyr" )
import( "purrr" )
import( "broom" )
import( "ggpubr" )
import( "stats" )
import( "rstatix" )

export( "run" )

# Comparing the means.
run <- function( input, models, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$utils_debug$run( run )

	# Subroutines.
	baseName <- "run.compare.means."
	routines <- paste0( baseName, 
											c(
												"anova",
												"kruskal.wallis"
											) )
	
	# Run the subroutines.	
	result <- list( length( routines ) )
	map( routines, function( r )
	{
		# Get the routine.
		f <- get( r )
		subName <- substring(r, nchar( baseName ) + 1 )
		
		# Run the routine.
		result[[ subName ]] <<- f( input, models, ... )
		
	})
	result
}

# Comparing means with one-waY ANOVA.
run.compare.means.anova <- function( input, models, ... )
{
	# Anova test to compare the means.
	t <- input$replacements %>%
		dplyr::filter( Vaihdettu & Erä %in% input$batches.multi$Erä ) %>%
		aov( PimeätTunnit ~ Erä, data = . ) 
	
	
	# Create a QQ plot of residuals.
	p <- ggqqplot( residuals( t ) )
	
	# Construct the result.
	result <- list(
		model = t,
		table = tidy( TukeyHSD( t ) ),
		plot = p
	)
	result
}

# Comparing means with Kruskal-Wallis.
run.compare.means.kruskal.wallis <- function( input, models, ... )
{
	# Kruskal-Wallis test to compare the means.
	t <- input$replacements %>%
		dplyr::filter( Vaihdettu & Erä %in% input$batches.multi$Erä ) %>%
		kruskal.test( PimeätTunnit ~ Erä, data = . ) %>%
		tidy()
	
	# Construct the result.
	result <- list(
		model = NULL,
		table = t,
		plot = NULL
	)
	result
}
