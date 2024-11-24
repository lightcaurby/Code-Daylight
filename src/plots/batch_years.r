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

	# Status information.
	cat( sprintf( "\tbatch years\n" ) )
	
	# Plot.
	p <- ggplot(data=data.plot$replacements) +
		theme_bw() +
		ylab("Duration (calendar years)") + 
		xlab("Batch") +
		geom_point(
			data = data.plot$replacements %>% filter( Tyyppi == "pienloiste" & Vaihdettu) ,
			aes( x=Erä, y = VuosiEro, group=Tyyppi, color = Tyyppi ),
			size=3,
			alpha=0.5
		) +
		geom_point(
			data = data.plot$replacements %>% filter( Tyyppi == "pienloiste" & !Vaihdettu ) ,
			aes( x=Erä, y = VuosiEro, group=Tyyppi, color = Tyyppi ),
			shape="circle open",
			size=5,
			alpha=0.5
		) +
		geom_point(
			data = data.plot$replacements %>% filter( Tyyppi != "pienloiste" & Vaihdettu ) ,
			aes( x=Erä, y = VuosiEro, group=Tyyppi, color = Tyyppi ),
			size=3,
			alpha=0.5
		) +
		geom_point(
			data = data.plot$replacements %>% filter( Tyyppi != "pienloiste" & !Vaihdettu ) ,
			aes( x=Erä, y = VuosiEro, group=Tyyppi, color = Tyyppi ),
			shape="circle open",
			size=5,
			alpha=0.5
		) +
		# geom_smooth(
		# 	aes( x=as.numeric( AsennusVuosi ), y=VuosiEro),
		# 	se = F,
		# 	method="loess",
		# 	span = 0.75,
		# 	color = "grey60",
		# 	linetype ="dashed",
		# 	size = 1
		# ) +
		scale_fill_manual(values = c("pienloiste"="#FC4E07", "led"="#00AFBB"), breaks = c("pienloiste", "led"), labels = c("fluorescent", "led")) +
		scale_color_manual(values = c("pienloiste"="#FC4E07", "led"="#00AFBB"), breaks = c("pienloiste", "led"), labels = c("fluorescent", "led")) +
		#scale_y_continuous(limits=c(0,100), breaks=seq(0,100,by=10), expand=c(0, 0)) +
		#scale_x_continuous(breaks=which( results$xlabel != "" ), labels=results$xlabel[ results$xlabel != ""], expand=c(0, 0)) +
		theme( legend.position="bottom", 
					 legend.title=element_blank(), 
					 panel.grid.minor=element_blank(),
					 panel.border=element_blank(),
					 axis.ticks=element_blank(),
					 plot.margin = unit(c(1,1,0.5,1), "cm"),
					 axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0) ),
					 axis.title.y = element_text(angle=-90, margin = margin(t = 0, r = 10, b = 0, l = 0)))

	list(
		.height = 4,
		.width = 8,
		plots = list( p )
	)
}

