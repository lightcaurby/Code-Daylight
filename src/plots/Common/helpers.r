import( "here" )
import( "modules" )
import( "dplyr" )
import( "ggplot2" )
import( "ggpubr" )
import( "stats" )
import( "rstatix" )

export( "my.construct.result" )

my.construct.result <- function(
	plot = NULL,
	plot.w = 8,
	plot.h = 8
)
{
	list(
		plot = plot,
		plot.w = plot.w,
		plot.h = plot.h
	)
}


