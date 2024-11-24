# Environment setup.
source( file = "src/env.R" )

# Flags.
options( "lightcaurby.Code-Daylight" = list(
	clean = T,	# Whether to remove output files before generating them again.
	cache.models = FALSE,  # Whether to cache model data to an RDS file.
	cache.plots = FALSE,  # Whether to cache model data to an RDS file.
	debug = c(			# Module names to stop in.
	)
))

# Main workflow.
lib.workflows <- suppressPackageStartupMessages( modules::use( here::here("src/workflows") ) )
result <- lib.workflows$full$run()

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
			title = paste0( "Lifetime prediction for batch #", df.batch$Er‰, ": ", df.batch$Aika ) )
	
	#CairoWin()
	invisible( p )
}

generate_prediction_plots <- function( result )
{
	df.batches <- result$batch_predictions %>%
		map( function( l ) {
			as.data.frame( l["df" ] )
		}) %>% 	bind_rows()
	colnames( df.batches ) <- c( "Er‰ID", "Hours.min", "Hours", "Hours.max" )
	
	plots <- df.batches %>%
		pmap( function( ... ) {
			
			df <- tibble( ... )
			
			df.extracted.for.batch <- 
				result$batch_predictions %>%
				extract2( which( df.batches$Er‰ID == df$Er‰ID ) )
			
			p <- batch.lifetimes.predictions.plot( df.extracted.for.batch )
			
			invisible( p )
			
		} )

	plots
}

result$batch_predictions$plots <- generate_prediction_plots( result$data )

name <- "batch_lifetime_predictions"
targetDir <- "output/plots/"
fnBase <- paste0( "plot_", name, ".pdf" )
fn <- paste0( targetDir, fnBase )
fn <- here::here( fn )

pdf( file = fn, height=6, width=9)

result$batch_predictions$plots %>%
	map( function( p ) {
		print( p )
		NULL
	} ) %>%
	invisible()

dev.off()

#expected.plot

my_date_format <- function (kk, format.major = "%m\n\n%Y", format.minor = "%m\n\n", tz = "UTC") 
{
	function(x)
	{ 
		if_else( is.na(x), "", if_else( month(x) == kk, format( x, format.major, tz=tz ), format( x, format.minor, tz=tz ) ) )
	}
}

expected.failures.plot <- function( result )
{
	Pvm.today <- expected_failures$Pvm.today[ 1 ]
	
	month.lines <-
		data.frame( date = seq( Pvm.today, max( expected_failures$Pvm.max ), by = 1 ) ) %>%
		filter( day( date ) == 1 )
	year.lines <-
		data.frame( date = seq( Pvm.today, max( expected_failures$Pvm.max ), by = 1 ) ) %>%
		filter( day( date ) == 1 & month( date ) == 1 )

	.pi.interval <- result$batch_predictions %>%
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
	
	p <- ggplot( data=expected_failures ) + 
		plot.theme +
		geom_segment( data = month.lines, aes(x=date, xend=date, y=0.5, yend=length(expected_failures$Huoneisto)+0.5 ), linewidth=0.1, color="gray95" ) + 
		geom_segment( data = year.lines, aes(x=date, xend=date, y=0.5, yend=length(expected_failures$Huoneisto)+0.5 ), linewidth=0.5, color="gray80", alpha=0.5 ) + 
		geom_segment( aes(x=Pvm.min.3, xend= Pvm.2, y=Huoneisto, yend=Huoneisto ), linewidth=1.5, color="coral" ) + 
		geom_segment( aes(x=Pvm.3, xend= Pvm.max.2, y=Huoneisto, yend=Huoneisto ), linewidth=1.5, color="coral" ) + 
		geom_point( aes(x=Pvm.min.2, y=Huoneisto ), size=3, color="coral3" ) + 
		geom_point( aes(x=Pvm.2, y=Huoneisto ), size=5, color="coral3" ) + 
		geom_point( aes(x=Pvm.max.2, y=Huoneisto ), size=3, color="coral3" ) + 
		annotate( "text", x = expected_failures$Pvm.max.2 + 25, y = expected_failures$Huoneisto, hjust=0, vjust=0.4,
							label=expected_failures$Overtime.label, parse=F, color = "darkorange" ) +
		scale_x_date(
			name = element_blank(),
			labels = my_date_format(6),
			date_breaks = "2 month",
			limits = c(Pvm.today-15, max(expected_failures$Pvm.max)+15), 
			expand = c(0, 0)) + 
		scale_y_discrete(
			name = "Location" ) +
		labs(
			title = paste0( "Expected failures with ", as.character( .pi.interval * 100 ), "% prediction interval" ) )

	p
}


result$expected_failures_plot <- expected.failures.plot( result$data )


name <- "expected_failures_current"
targetDir <- "output/plots/"
fnBase <- paste0( "plot_", name, ".pdf" )
fn <- paste0( targetDir, fnBase )
fn <- here::here( fn )

pdf( file = fn, height=7, width=12)
result$expected_failures_plot
dev.off()

#CairoWin()

l <- list( a = c(1,2,3), b=c(4,5,6))
l1 <- l
l2 <- list( b = c(7,8,9), c=c(10,11,12))

modifyList( l1, l2 )
