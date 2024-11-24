import( "here" )
import( "modules" )
import( "stats" )
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
	cat( sprintf( "\tbatch predictions\n" ) )
	
	# Plot.
	df.batches <- data.plot$batch_predictions %>%
		map( function( l ) {
			as.data.frame( l["df" ] )
		}) %>% 	bind_rows()
	colnames( df.batches ) <- c( "EräID", "Hours.min", "Hours", "Hours.max" )
	
	output<- df.batches %>%
		pmap( function( ... ) {
			
			df <- tibble( ... )
			
			df.extracted.for.batch <- 
				data.plot$batch_predictions %>%
				extract2( which( df.batches$EräID == df$EräID ) )
			
			p <- batch.lifetimes.predictions.plot( df.extracted.for.batch )
			
			invisible( p )
			
		} )
	
	list( 
		.height = 6,
		.width = 9,
		plots = output
	)
}


batch.lifetimes.predictions.plot <- function( df.extracted.for.batch )
{
	df.nrow <- df.extracted.for.batch %>%
		extract2( "df.nrow" )
	
	df.params <- df.extracted.for.batch %>%
		extract2( "df.params" )
	
	df.batch <- df.extracted.for.batch %>%
		extract2( "df.batch" )
	
	df.pi.segment <- df.extracted.for.batch %>%
		extract2( "df.pi.segment" )
	
	df.pi.rect <- df.extracted.for.batch %>%
		extract2( "df.pi.rect" )
	
	df.fit <- df.extracted.for.batch %>%
		extract2( "df.fit" )
	
	xs <- sort( round( rlogis( df.params$led.max, location=df.params$led.loc, scale=df.params$led.sca ), 0), decreasing=T )
	df.log <- data.frame( y=seq(0, df.params$led.max, length.out = df.params$led.max),	x=xs )
	
	y.breaks = c( 0, 0.1, seq( 0.25, 0.75, by=0.25 ), 0.9, 1.0) * df.params$led.max
	y.labels = format( sort( y.breaks / df.params$led.max, decreasing=F), nsmall=2, small.mark=",") 
	
	x.breaks = seq( 0, df.params$led.max, by=10000 )
	x.labels = format( x.breaks, big.mark=" ") 
	
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
	
	lower <- as.character(round(df.pi.rect$xmin,0))
	upper <- as.character(round(df.pi.rect$xmax,0))
	p <- ggplot() + 
		plot.theme +
		geom_line( data=df.log, aes(x = x, y = y), linewidth=1.5, linetype="solid", color="darkgrey" ) + 
		geom_segment( data=df.pi.segment, aes(x = x, xend=xend, y = y, yend=yend), inherit.aes = F, color="orange", linewidth=1.5, alpha=0.5 ) +
		geom_rect( data=df.pi.rect, aes(xmin = xmin, xmax = xmax, ymin=ymin, ymax = ymax ), inherit.aes = F, color=NA, fill="orange", alpha=0.2 )
	
	if( df.nrow$w > 0 )
	{
		p <- p + 
			geom_point( data=df.fit, aes(x = time, y = status), color="coral3", alpha=0.75, size=6 )
	}
	
	p <- p +
		annotate( "text", x = df.pi.rect$xmax, y = df.params$led.max-500, hjust=0, vjust=-2.2, angle = -90,, size = 3,
							label=paste0("expected failure point: ", as.character( df.params$led.loc), " h"),
							parse=F, color = "darkorange" ) +
		annotate( "text", x = df.pi.rect$xmax, y = df.params$led.max-500, hjust=0, vjust=-0.7, angle = -90, size = 3,
							label=paste0("assumed standard deviation: ", as.character( df.params$led.std.percent ), "%"),
							parse=F, color = "darkorange" ) +
		annotate( "text", x = df.pi.rect$xmax, y = df.params$led.max-500, hjust=0, vjust=1.3, angle = -90,
							label=paste0( as.character( df.params$pi.interval * 100 ), "% PI: ", lower, "...", upper),
							parse=F, color = "darkorange" )
	
	if( df.nrow$w > 0 )
	{
		p <- p +
			annotate( "text", x = df.pi.segment$x, y = 500, hjust=1.05, vjust=3.4, angle = -90, size = 3,
								label=paste0( "n = ", as.character( df.nrow$w ), " / ", as.character( df.nrow$w.all ) ),
								parse=F, color = "darkorange" ) +
			annotate( "text", x = df.pi.segment$x, y = 500, hjust=1, vjust=1.4, angle = -90, fontface="bold",
								label=paste0( "bold(\"\u03bc = ", as.character( round( df.pi.segment$x, 0 ) ), "\")" ),
								parse=T, color = "darkorange" )
	}
	
	p <- p +
		scale_x_continuous(
			name = "Lifetime in hours",
			breaks= x.breaks,
			labels= x.labels,
			limits = c(0, df.params$led.max)) + 
		scale_y_continuous(
			name = "Probability of lifetime",
			breaks= y.breaks,
			labels= y.labels) +
		labs(
			title = paste0( "Lifetime prediction for batch #", df.batch$Erä, ": ", df.batch$Aika ) )
	
	invisible( p )
}

