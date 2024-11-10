import( "here" )
import( "modules" )
import( "ggplot2" )
import( "dplyr" )

export( "run" )

# Create a plot.
run <- function( data.plot, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here::here( "src/utils" ) ) )$debug$run( run )

	# Plot.
	ggplot() +
		theme_bw() +
		theme(
			panel.border = element_blank(),
			axis.ticks = element_blank(),
			axis.text.y = element_text(vjust=0),
			#axis.title.x = element_text(hjust=1),
			plot.subtitle = element_text( vjust=1 ),
			plot.title = element_text( vjust = 1, margin = margin(b = 0, unit="line")),
			axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0 )),
			axis.title.y = element_text(angle=-90, margin = margin(t = 0, r = 10, b = 0, l = 0 ))
		) +
		geom_density(
			data=data.plot$replacements %>% filter( Vaihdettu ),
			aes( x=PimeätTunnit ),
			fill = "orange",
			color = "darkorange",
			#rel_min_height = 0.001,
			alpha=0.45
			#size=0.1,
			#alpha = 0.5,
			#stat="density"
			#fill = df.Sampling$fill,
			#color = df.Sampling$fill
		) +
		geom_segment(
			data = as.data.frame(data.plot$dms.hour),
			aes(
				x=x,
				xend = x,
				y = y,
				yend = 0
			),
			linetype="dashed",
			size=1
		) +
		# geom_text(
		# 	data = as.data.frame(dms),
		# 	aes(
		# 		x=x,
		# 		y = 0
		# 	),
		# 	label ="moodi",
		# 	angle = 270,
		# 	vjust = 1.5,
		# 	hjust = 1.5
		# ) +
		scale_x_continuous(expand=c(0.01,0), breaks=data.plot$py.darkness) +
		scale_y_discrete(expand = c(0.01, 0))+ 
		ylab( "" ) +
		xlab( "Duration (hours)" ) +
		labs(title="",
				 subtitle="")

}

