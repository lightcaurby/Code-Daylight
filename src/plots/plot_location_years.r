import( "here" )
import( "modules" )
import( "ggplot2" )
import( "dplyr" )

export( "run" )

# Create a plot.
run <- function( data.plot, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here( "src/utils" ) ) )$utils_debug$run( run )

	# Plot.
	ggplot( data=data.plot$replacements ) +
		theme_bw() +
		ylab("duration (calendar years)") + 
		xlab("location") +
		geom_point( data = data.plot$replacements %>% filter(Vaihdettu), aes( x=Tila, y = VuosiEro, group=Tyyppi, color = Tyyppi ), size=3, alpha=0.8 ) +
		geom_point( data = data.plot$replacements %>% filter(!Vaihdettu), aes( x=Tila, y = VuosiEro, group=Tyyppi, color = Tyyppi ), shape="circle open", size=5, alpha=0.8 ) +
		scale_fill_manual(values = c("pienloiste"="#FC4E07", "led"="#00AFBB"), breaks = c("pienloiste", "led")) +
		scale_color_manual(values = c("pienloiste"="#FC4E07", "led"="#00AFBB"), breaks = c("pienloiste", "led")) +
		#scale_y_continuous(limits=c(0,100), breaks=seq(0,100,by=10), expand=c(0, 0)) +
		#scale_x_continuous(breaks=which( results$xlabel != "" ), labels=results$xlabel[ results$xlabel != ""], expand=c(0, 0)) +
		theme( legend.position="bottom", 
					 legend.title=element_blank(), 
					 panel.grid.minor=element_blank(),
					 panel.border=element_blank(),
					 axis.ticks=element_blank(),
					 plot.margin = unit(c(1,1,0.5,1), "cm"))

}