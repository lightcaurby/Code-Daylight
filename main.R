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

print(result$replacements %>% arrange( Pvm ), n=200)
er‰kuva <- function( result2, seed, er‰id2, led.loc2, ci.limit )
{
		set.seed( seed )
		sample.size = 12
	
		led.loc <- led.loc2
		led.std.percent = 15
		led.sca <- led.loc * ( led.std.percent / 100 )
		led.max <- 30000
		er‰id <- as.integer( er‰id2 )
		
		result <- result2
		er‰t <- result$replacements %>% filter( Vaihdettu == F ) %>% distinct( Er‰ID )
		v <- result$replacements %>% filter( Vaihdettu == F ) %>% filter( Er‰ID == er‰id )
		
		w.all <- result$replacements %>% filter( Er‰ID == er‰id )
		w <- w.all %>% filter( Vaihdettu )
		#if( nrow(w) > 1 )
		#{
		#	smpl <- w$Pime‰tTunnit
		#}
		#else
		{
			smpl <- rlogis( sample.size, location=led.loc, scale=led.sca )
		}
		df <- data.frame( time = smpl, status = ( 1- plogis( smpl, location=led.loc, scale=led.sca ) ) * led.max )
		
		xs <- sort( round( rlogis( led.max, location=led.loc, scale=led.sca ), 0), decreasing=T )
		df.log <- data.frame( y=seq(0, led.max, length.out = led.max),	x=xs )
		
		y.breaks = c( 0, 0.1, seq( 0.25, 0.75, by=0.25 ), 0.9, 1.0) * led.max
		y.labels = format( sort( y.breaks / led.max, decreasing=F), nsmall=2, small.mark=",") 
		
		df2.segment <- data.frame(
			x = mean(df$time),
			xend = mean(df$time),
			y = 0,
			yend = led.max )
		
		ci <- CI( df$time, ci = ci.limit )
		df2.rect <- data.frame(
			xmin = ci[ "lower" ],
			xmax = ci[ "upper" ],
			ymin = 0,
			ymax = led.max )

		lower <- as.character(round(df2.rect$xmin,0))
		upper <- as.character(round(df2.rect$xmax,0))
		p <- ggplot() + 
			theme_bw() +
			geom_line( data=df.log, aes(x = x, y = y), size=1.5, linetype="solid", color="darkgrey" ) +
			#geom_segment( data=df, aes(x = time, xend=time, y = 0, yend=led.max), color="black" ) +
			geom_segment( data=df2.segment, aes(x = x, xend=xend, y = y, yend=yend), inherit.aes = F, color="orange", size=1.5, alpha=0.5 ) +
			geom_rect( data=df2.rect, aes(xmin = xmin, xmax = xmax, ymin=ymin, ymax = ymax ), inherit.aes = F, color=NA, fill="orange", alpha=0.2 )
		
		if( nrow(w) > 1 )
		{
			p <- p + 
				geom_point( data=df, aes(x = time, y = status), color="coral3", alpha=0.75, size=6 ) +
				annotate( "text", x = df2.segment$x, y = 500, hjust=1, vjust=1.3, angle = -90,
									label=paste0( "n = ", as.character( nrow(w) ), " / ", as.character( nrow(w.all) ) ), parse=F, color = "darkorange" )
		}
		else
		{
			p <- p + 
				annotate( "text", x = df2.segment$x, y = 500, hjust=1, vjust=1.3, angle = -90,
									label=paste0( "n = ", as.character( sample.size), " (random)" ), parse=F, color = "darkorange" )
		}
			
		p <- p +
			annotate( "text", x = df2.segment$x, y = led.max-500, hjust=0, vjust=-0.7, angle = -90, 
								label=paste0( as.character( ci.limit * 100 ), "% CI: ", lower, "...", upper), parse=F, color = "darkorange" ) +  
			annotate( "text", x = df2.segment$x, y = 500, hjust=1, vjust=-0.3, angle = -90, fontface="bold", 
								label=paste0( "bold(\"\u03bc = ", as.character( round(df2.segment$x, 0)), "\")"), parse=T, color = "darkorange" ) +  
			annotate( "text", x = df2.rect$xmax+1000, y = y.breaks[ 4 ], hjust=0, vjust=-0.2,
								label=paste0("Theoretical failure point\nat ", as.character( led.loc), " hours."), 
								parse=F, color = "black" ) +  
			annotate( "text", x = df2.rect$xmax+1000, y = y.breaks[ 4 ], hjust=0, vjust=1.2,
								label=paste0("Assumed standard\ndeviation is ", as.character(led.std.percent), "%."), 
								parse=F, color = "black" ) +  
			scale_x_continuous(
				name = "Lifetime",
				limits = c(0, led.max)) + 
			scale_y_continuous(
				name = "Probability of lifetime",
				breaks= y.breaks,
				labels= y.labels) +
			labs(
				title = paste0( "Sample-based lifetime prediction for batch #", er‰id2, ": ", result$batches[ result$batches$Er‰ == er‰id2, ]$Aika ) ) +
			theme( legend.position="bottom", 
						 legend.title=element_blank(), 
						 panel.grid.minor=element_blank(),
						 panel.grid.major.x=element_blank(),
						 panel.border=element_blank(),
						 axis.ticks=element_blank(),
						 plot.margin = unit(c(1,1,0.5,1), "cm"),
						 plot.title = element_text(size=18, ),
						 axis.text.x = element_text(size=12 ),
						 axis.text.y = element_text(size=12 ),
						 axis.title.x = element_text(size=14, margin = margin(t = 10, r = 0, b = 0, l = 0) ),
						 axis.title.y = element_text(size=14, angle = 90, margin = margin(t = 0, r = 10, b = 0, l = 0)))
							
		#CairoWin()
		print( p )
		data.frame( Er‰ID=er‰id2, Hours.min=df2.rect$xmin, Hours=round(df2.segment$x, 0), Hours.max=df2.rect$xmax )
}

name <- "expected_lifetime_batches"
targetDir <- "output/plots/"
fnBase <- paste0( "plot_", name, ".pdf" )
fn <- paste0( targetDir, fnBase )
fn <- here::here( fn )

pdf( file = fn, height=6, width=9)

cn <- colnames( result$batches )
cn[ 1 ] <- iconv( cn[ 1 ], "UTF-8", "latin1" )

er‰.hours.95 <- result$batches %>% 
	pmap( function( ... ) {
		df <- tibble( ... )
		colnames( df ) <- cn
		er‰kuva( result, 3333, df$Er‰, df$Mtbf, 0.95 )
	}) %>%
	bind_rows() 
dev.off()

cumsumThreshold <- function( di, m, d, thold )
{
	y = 0
	cont <- TRUE
	while( cont )
	{
		if( y == 0 )
		{
			di.use <- di[ di$month > m | ( di$month == m & di$day > d ), ] 
		} 
		else
		{
			di.use <- di
		}
		
		satisfied <- which( cumsum( di.use$dark.hours ) >= thold )
		if( length( satisfied ) > 0 )
		{
			ind <- satisfied[ 1 ]
			res <- di.use[ ind, ] %>% select( day, month, year ) %>% mutate( year = year + y )
			cont <- FALSE
		}
		else
		{
			thold <- thold - sum( di.use$dark.hours )
			y <- y + 1
		}
	}
	res
}

di <- result$daylight.info$normalyear %>% select( date, dark.hours, day, month ) %>% mutate( year = 0 )

v <- result$replacements %>% 
	filter( Vaihdettu == F ) %>% 
	select( Huoneisto, Er‰ID, Er‰, Pvm ) %>% 
	join( er‰.hours.95 ) %>%
	mutate( Day = day(Pvm), Month=month(Pvm), Year=year(Pvm)) %>%
	pmap( function( ... ) {
		df <- tibble( ... )
		lo <- cumsumThreshold( di, df$Month, df$Day, df$Hours.min )
		mid <- cumsumThreshold( di, df$Month, df$Day, df$Hours )
		hi <- cumsumThreshold( di, df$Month, df$Day, df$Hours.max )
		print( paste0( 
			df$Er‰ID, 
			" / ", 
			as.character(df$Pvm), 
			": " , 
			as.character(mid$day), 
			".", 
			as.character(mid$month),
			".", 
			as.character( mid$year + df$Year)))
		data.frame( 
			Er‰ID=df$Er‰ID, 
			Er‰=df$Er‰, 
			Pvm.min=make_date(df$Year + lo$year, lo$month, lo$day), 
			Pvm=make_date(df$Year + mid$year, mid$month, mid$day), 
			Pvm.max=make_date(df$Year + hi$year, hi$month, hi$day), 
			Huoneisto=df$Huoneisto )
	}) %>%
	bind_rows() %>%
	arrange( Pvm )

v

my_date_format <- function (kk, format.major = "%m\n\n%Y", format.minor = "%m\n\n", tz = "UTC") 
{
	function(x)
	{ 
		ifelse( is.na(x), "", ifelse( month(x) == kk, format( x, format.major, tz=tz ), format( x, format.minor, tz=tz ) ) )
	}
}

today = Sys.Date()
Pvm.today = make_date( year = year(today), month=month(today), day=1)

month.lines <-
	data.frame( date = seq( Pvm.today, max( v$Pvm.max ), by = 1 ) ) %>%
	filter( day( date ) == 1 )
year.lines <-
	data.frame( date = seq( Pvm.today, max( v$Pvm.max ), by = 1 ) ) %>%
	filter( day( date ) == 1 & month( date ) == 1 )

p <- ggplot( 
		v %>%
			mutate( 
				Huoneisto = fct_reorder( Huoneisto, Pvm ),
				Pvm.min.2x = as_date( ifelse( Pvm.min < Pvm.today, Pvm.today, Pvm.min ) ),
				Pvm.2x = as_date( ifelse( Pvm < Pvm.today, Pvm.today, Pvm ) ),
				Pvm.max.2x = as_date( ifelse( Pvm.max < Pvm.today, Pvm.today, Pvm.max ) )
			) %>%
			mutate( 
				Pvm.min.2 = make_date( year=year(Pvm.min.2x), month=month(Pvm.min.2x), day = 1),
				Pvm.2 = make_date( year=year(Pvm.2x), month=month(Pvm.2x), day = 1),
				Pvm.max.2 = make_date( year=year(Pvm.max.2x), month=month(Pvm.max.2x), day = 1)
			) ) + 
	theme_bw() +
	geom_segment( data = month.lines, aes(x=date, xend=date, y=0.5, yend=length(v$Huoneisto)+0.5 ), size=0.1, color="gray70" ) + 
	geom_segment( data = year.lines, aes(x=date, xend=date, y=0.5, yend=length(v$Huoneisto)+0.5 ), size=0.5, color="gray50" ) + 
	geom_segment( aes(x=Pvm.min.2, xend= Pvm.2, y=Huoneisto, yend=Huoneisto ), size=1.5, color="coral" ) + 
	geom_segment( aes(x=Pvm.2, xend= Pvm.max.2, y=Huoneisto, yend=Huoneisto ), size=1.5, color="coral" ) + 
	geom_point( aes(x=Pvm.min.2, y=Huoneisto ), size=3, color="coral3" ) + 
	geom_point( aes(x=Pvm.2, y=Huoneisto ), size=5, color="coral3" ) + 
	geom_point( aes(x=Pvm.max.2, y=Huoneisto ), size=3, color="coral3" ) + 
	scale_x_date(
		name = element_blank(),
		labels = my_date_format(6),
		date_breaks = "2 month",
		limits = c(Pvm.today-15, max(v$Pvm.max)+15), 
		expand = c(0, 0)) + 
	scale_y_discrete(
		name = "Location" ) +
	labs(
		title = "Expected failures with 95% prediction interval" ) +
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
				 axis.title.y = element_text(size=14, angle = 90, margin = margin(t = 0, r = 10, b = 0, l = 0)))
p
name <- "expected_failures_current"
targetDir <- "output/plots/"
fnBase <- paste0( "plot_", name, ".pdf" )
fn <- paste0( targetDir, fnBase )
fn <- here::here( fn )

pdf( file = fn, height=7, width=12)
p
dev.off()

#CairoWin()
