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
	ggplot( data=data.plot$replacements %>% filter(Vaihdettu )) +
		theme_bw() +
		ylab("Duration (hours)") + 
		xlab("Installation year") +
		geom_boxplot(
			aes( x=AsennusVuosi, y = PimeätTunnit ),
			alpha=0.65,
			fill = "orange",
			color = "darkorange",
			outlier.color ="darkorange",
			outlier.size = 3,
			outlier.alpha = 0.5,
			size=0.75
		) +
		#scale_y_continuous(limits=c(0,100), breaks=seq(0,100,by=10), expand=c(0, 0)) +
		#scale_x_continuous(breaks=which( results$xlabel != "" ), labels=results$xlabel[ results$xlabel != ""], expand=c(0, 0)) +
		theme( legend.position="bottom", 
					 legend.title=element_blank(), 
					 panel.grid.minor=element_blank(),
					 panel.border=element_blank(),
					 axis.ticks=element_blank(),
					 plot.margin = unit(c(1,1,0.5,1), "cm"),
					 axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0 )),
					 axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0 )))
	

}
