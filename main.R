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

batch.lifetimes.predictions.transform <- function( batch, replacements, seed, .pi.interval, .range.factor )
{
		set.seed( seed )
		
		er‰id <- as.integer( batch$Er‰ )
	
		.led.loc <- batch$Mtbf
		.led.std.percent <- batch$SDPercent
		df.params <- data.frame(
			Er‰ID = er‰id,
			led.loc = .led.loc,
			led.std.percent = .led.std.percent,
			led.sca = .led.loc * ( .led.std.percent / 100 ),
			led.max = 35000,
			pi.interval = .pi.interval,
			range.factor = .range.factor
		)
		
		er‰t <- replacements %>% filter( Vaihdettu == F ) %>% distinct( Er‰ID )
		v <- replacements %>% filter( Vaihdettu == F ) %>% filter( Er‰ID == er‰id )
		
		w.all <- replacements %>% filter( Er‰ID == er‰id )
		w <- w.all %>% filter( Vaihdettu )
		
		df.fit <- NULL
		if( nrow(w) > 0 )
		{
			df.fit <- data.frame(
					Er‰ID = er‰id,
					time = w$Pime‰tTunnit,
					status = ( 1- plogis( w$Pime‰tTunnit, location=df.params$led.loc, scale=df.params$led.sca ) ) * df.params$led.max )
		}
		
		df.pi <- generate_data_for_pi(
				.mean = df.params$led.loc,
				.pi.interval = df.params$pi.interval,
				.sd.percentage = df.params$led.std.percent,
				.range.factor = df.params$range.factor );
		df.pi$mean.obs <- NA
		if( nrow(w) > 0 )
		{
			df.pi$mean.obs <- mean( w$Pime‰tTunnit )
		}

		df2.segment <- data.frame(
			Er‰ID = er‰id,
			x = if_else( is.na( df.pi$mean.obs ), df.pi$mean, df.pi$mean.obs ),
			xend = if_else( is.na( df.pi$mean.obs ), df.pi$mean, df.pi$mean.obs ),
			y = 0,
			yend = df.params$led.max )

		df2.rect <- data.frame(
			Er‰ID = er‰id,
			xmin = df.pi$lpl,
			xmax = df.pi$upl,
			ymin = 0,
			ymax = df.params$led.max )

		list(
			Er‰ID = er‰id,
			df = data.frame(
				Er‰ID=er‰id,
				Hours.min=df2.rect$xmin, 
				Hours=round(df2.segment$x, 0), 
				Hours.max=df2.rect$xmax
			),
			df.nrow = data.frame(
				w.all = nrow( w.all ),
				w = nrow( w )
			),
			df.batch = batch,
			df.params = df.params,
			df.pi.segment = df2.segment,
			df.pi.rect = df2.rect,
			df.fit = df.fit
		)
}

generate_data_for_pi <- function( .n = 1e6, .mean, .pi.interval = 0.95, .sd.percentage = 15, .range.factor = 8 )
{
	.sd <- ( .sd.percentage / 100 ) * .mean
	.range <- .range.factor * .sd
	df.pi.gen <- data.frame(
		x =   round( .mean - (.range / 2 ) + ( runif( .n ) * .range ), 0 ),
		p =   runif( .n ) ) %>%
		mutate( p.limit.orig = pnorm( x, mean = .mean, sd = .sd ) ) %>%
		mutate( p.limit = if_else( p.limit.orig < 0.5, 1 - p.limit.orig, p.limit.orig  ) ) %>% 
		mutate( b = if_else( p >= p.limit, TRUE, FALSE )
		)
	
	df.pi.gen <- df.pi.gen %>% filter( b  ) %>% arrange( x )
	lo.index <- ceiling( ( 1 - .pi.interval ) * length( df.pi.gen$x ) )
	hi.index <- floor( .pi.interval * length( df.pi.gen$x ) )
	
	df.pi <- data.frame(
		mean = .mean,
		lpl = df.pi.gen$x[ lo.index ],
		upl = df.pi.gen$x[ hi.index ]
	);
	
	df.pi
}

generate_predictions <- function( result, .pi.interval, .range.factor )
{
	cn <- colnames( result$batches )
	cn[ 1 ] <- iconv( cn[ 1 ], "UTF-8", "latin1" )
	predictions <- result$batches %>% 
		pmap( function( ... ) {
			df <- tibble( ... )
			colnames( df ) <- cn
			batch.lifetimes.predictions.transform( df, result$replacements, 3333, .pi.interval, .range.factor )
		})

	predictions	
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

result$batch_predictions <- generate_predictions( result, 0.90, 8 )
result$batch_predictions$plots <- generate_prediction_plots( result )

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


expected.failures.transform <- function( result )
{
	di <- result$daylight.info$normalyear %>% 
		select( date, dark.hours, day, month ) %>% 
		mutate( year = 0 )
	
	df.batches <- result$batch_predictions %>%
		map( function( l ) {
			as.data.frame( l["df" ] )
		}) %>% 	bind_rows()
	colnames( df.batches ) <- c( "Er‰ID", "Hours.min", "Hours", "Hours.max" )
	
	expected_failures_initial <- result$replacements %>% 
		filter( Vaihdettu == F ) %>% 
		select( Huoneisto, Er‰ID, Er‰, Pvm ) %>% 
		join( df.batches ) %>%
		mutate( Day = day(Pvm), Month=month(Pvm), Year=year(Pvm)) %>%
		mutate( YearMonth = ( Year * 100 ) + Month ) %>%
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
	
	today = Sys.Date()
	Pvm.today = make_date( year = year(today), month=month(today), day=1)
	YearMonth.today = ( year(Pvm.today) * 100 ) + month(Pvm.today)
	
	expected_failures <- 
		expected_failures_initial %>%
		mutate( 
			YearMonth.min = ( year(Pvm.min) * 100 ) + month(Pvm.min),
			YearMonth = ( year(Pvm) * 100 ) + month(Pvm),
			YearMonth.max = ( year(Pvm.max) * 100 ) + month(Pvm.max)
		) %>%
		mutate( 
			Huoneisto = fct_reorder( Huoneisto, Pvm ),
			Pvm.today = Pvm.today,
			Pvm.min.2x = as_date( if_else( YearMonth.min < YearMonth.today, NA, Pvm.min ) ),
			Pvm.2x = as_date( if_else( YearMonth < YearMonth.today, NA, Pvm ) ),
			Pvm.max.2x = as_date( if_else( YearMonth.max <= YearMonth.today, Pvm.today, Pvm.max ) ),
			Overtime.label = if_else( YearMonth.max < YearMonth.today, 
															 paste0( 
																year( Pvm.max ), ".", formatC( month ( Pvm.max ), width = 2, flag = "0" )
															),
															 "" ),
		) %>%
		mutate( 
			Pvm.min.2 = make_date( year=year(Pvm.min.2x), month=month(Pvm.min.2x), day = 1),
			Pvm.2 = make_date( year=year(Pvm.2x), month=month(Pvm.2x), day = 1),
			Pvm.max.2 = make_date( year=year(Pvm.max.2x), month=month(Pvm.max.2x), day = 1),
			Pvm.min.3 = if_else( is.na( Pvm.min.2 ), Pvm.today, Pvm.min.2 ),
			Pvm.3 = if_else( is.na( Pvm.2 ), Pvm.today, Pvm.2 )
		)

	expected_failures		
}

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


result$expected_failures <- expected.failures.transform( result )
result$expected_failures_plot <- expected.failures.plot( result )


name <- "expected_failures_current"
targetDir <- "output/plots/"
fnBase <- paste0( "plot_", name, ".pdf" )
fn <- paste0( targetDir, fnBase )
fn <- here::here( fn )

pdf( file = fn, height=7, width=12)
result$expected_failures_plot
dev.off()

#CairoWin()

