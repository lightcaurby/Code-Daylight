import( "here" )
import( "modules" )
import( "ggplot2" )
import( "ggridges" )
import( "dplyr" )

export( "run" )

# Create a plot.
run <- function( data.plot, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$utils_debug$run( run )

	# Plot.
	ggplot( data=data.plot$replacements %>% filter(Vaihdettu & Er� %in% data.plot$batches.multi$Er�)  ) +
		theme_bw() +
		theme(
			panel.border = element_blank(),
			axis.ticks = element_blank(),
			axis.text.y = element_text(vjust=0),
			axis.title.x = element_text(hjust=1),
			plot.subtitle = element_text( vjust=1 ),
			plot.title = element_text( vjust = 1, margin = margin(b = 0, unit="line"))
		) +
		geom_density_ridges(
			aes( x=Pime�tTunnit, y=Er� ),
			fill="skyblue3",
			color="skyblue4",
			#bandwidth = 1000,
			rel_min_height = 0.01,
			scale=1,
			size=0.25,
			alpha=0.45,
			jittered_points = TRUE,
			#point_shape = "|", 
			point_size = 3,
			#point_stroke = 3,
			position = position_points_jitter(height = 0)
		) +
		scale_x_continuous(expand=c(0.01,0), 
											 limits=c(
											 	min(data.plot$replacements$Pime�tTunnit),
											 	max(data.plot$replacements$Pime�tTunnit)
											 )) +
		scale_y_discrete(expand = c(0.025, 0))+
		ylab( "" ) +
		xlab( "duration (hours)" ) +
		labs(title="",
				 subtitle="")
	
	
}