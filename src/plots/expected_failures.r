import( "here" )
import( "modules" )
import( "ggplot2" )
import( "magrittr" )
import( "dplyr" )
import( "purrr" )
import( "lubridate" )

export( "run" )

# Create a plot.
run <- function( data.plot, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here::here( "src/utils" ) ) )$debug$run( run )

	# Status information.
	cat( sprintf( "\texpected failures\n" ) )
	
	# Plot.
	Pvm.today <- 	Pvm.today <- data.plot$expected_failures$Pvm.today[ 1 ]
	
	month.lines <-
		data.frame( date = seq( Pvm.today, max( data.plot$expected_failures$Pvm.max ), by = 1 ) ) %>%
		filter( day( date ) == 1 )
	year.lines <-
		data.frame( date = seq( Pvm.today, max( data.plot$expected_failures$Pvm.max ), by = 1 ) ) %>%
		filter( day( date ) == 1 & month( date ) == 1 )
	
	.pi.interval <- data.plot$batch_predictions %>%
		map( function( l ) {
			as.data.frame( l["df.params" ] )
		}) %>% 
		.[[1]] %>%
		extract("df.params.pi.interval")
	
	plot.theme <-
		theme_bw() +
		theme( legend.position="bottom", 
					 legend.title=element_blank(), 
					 panel.grid.minor=element_blank(),
					 panel.grid.major.x=element_blank(),
					 panel.border=element_blank(),
					 axis.ticks=element_blank(),
					 plot.margin = unit(c(1,1,0.5,1), "cm"),
					 plot.title = element_text(size=18, ),
					 axis.text.x = element_text(size=12, vjust=0, hjust=0.5 ),
					 axis.text.y = element_text(size=12, ),
					 axis.title.x = element_text(size=14, margin = margin(t = 10, r = 0, b = 0, l = 0) ),
					 axis.title.y = element_text(size=14, angle = -90, margin = margin(t = 0, r = 10, b = 0, l = 0)))	
	
	p <- ggplot( data=data.plot$expected_failures ) + 
		plot.theme +
		geom_segment( data = month.lines, aes(x=date, xend=date, y=0.5, yend=length(data.plot$expected_failures$Huoneisto)+0.5 ), linewidth=0.1, color="gray95" ) + 
		geom_segment( data = year.lines, aes(x=date, xend=date, y=0.5, yend=length(data.plot$expected_failures$Huoneisto)+0.5 ), linewidth=0.5, color="gray80", alpha=0.5 ) + 
		geom_segment( aes(x=Pvm.min.3, xend= Pvm.2, y=Huoneisto, yend=Huoneisto ), linewidth=1.5, color="coral" ) + 
		geom_segment( aes(x=Pvm.3, xend= Pvm.max.2, y=Huoneisto, yend=Huoneisto ), linewidth=1.5, color="coral" ) + 
		geom_point( aes(x=Pvm.min.2, y=Huoneisto ), size=3, color="coral3" ) + 
		geom_point( aes(x=Pvm.2, y=Huoneisto ), size=5, color="coral3" ) + 
		geom_point( aes(x=Pvm.max.2, y=Huoneisto ), size=3, color="coral3" ) + 
		annotate( "text", x = data.plot$expected_failures$Pvm.max.2 + 25, y = data.plot$expected_failures$Huoneisto, hjust=0, vjust=0.4,
							label=data.plot$expected_failures$Overtime.label, parse=F, color = "darkorange" ) +
		scale_x_date(
			name = element_blank(),
			labels = my_date_format(6),
			date_breaks = "2 month",
			limits = c(Pvm.today-15, max(data.plot$expected_failures$Pvm.max)+15), 
			expand = c(0, 0)) + 
		scale_y_discrete(
			name = "Location" ) +
		labs(
			title = paste0( "Expected failures with ", as.character( .pi.interval * 100 ), "% prediction interval" ) )

	list( 
		.height = 7,
		.width = 12,
		plots = list( p )
	)
}

my_date_format <- function (kk, format.major = "%m\n\n%Y", format.minor = "%m\n\n", tz = "UTC") 
{
	function(x)
	{ 
		if_else( is.na(x), 
						 "", 
						 if_else( month(x) == kk, 
						 				 format( x, format.major, tz=tz ), 
						 				 format( x, format.minor, tz=tz )
						 	)
		)
	}
}

