import( "here" )
import( "modules" )
import( "stats" )
import( "rstatix" )
import( "dplyr" )
import( "purrr" )


export( "run" )

# Transform batch predictions.
run <- function( input, ... )
{
	# Debugger hook.
	suppressPackageStartupMessages( modules::use( here::here( "src/utils" ) ) )$debug$run( run )
	
	# Status information.
	cat( sprintf( "\tbatch predictions\n" ) )
	
	.seed <- 3333
	.pi.interval <- 0.90
	.range.factor <- 8
	
	cn <- colnames( input$batches )
	cn[ 1 ] <- iconv( cn[ 1 ], "UTF-8", "latin1" )
	batch_predictions <- input$batches %>% 
		pmap( function( ... ) {
			df <- tibble( ... )
			colnames( df ) <- cn
			batch_lifetimes_predictions(
				df,
				input$replacements,
				.seed,
				.pi.interval,
				.range.factor )
		})
	
	output <- list( batch_predictions = batch_predictions )
	output
}

batch_lifetimes_predictions <- function( 
		batch,
		replacements,
		.seed,
		.pi.interval,
		.range.factor )
{
	set.seed( .seed )
	
	eräid <- as.integer( batch$Erä )
	
	.led.loc <- batch$Mtbf
	.led.std.percent <- batch$SDPercent
	df.params <- data.frame(
		EräID = eräid,
		led.loc = .led.loc,
		led.std.percent = .led.std.percent,
		led.sca = .led.loc * ( .led.std.percent / 100 ),
		led.max = 30000,
		pi.interval = .pi.interval,
		range.factor = .range.factor
	)
	
	erät <- replacements %>% filter( Vaihdettu == F ) %>% distinct( EräID )
	v <- replacements %>% filter( Vaihdettu == F ) %>% filter( EräID == eräid )
	
	w.all <- replacements %>% filter( EräID == eräid )
	w <- w.all %>% filter( Vaihdettu )
	
	df.fit <- NULL
	if( nrow(w.all) > 0 )
	{
		df.fit <- data.frame(
			EräID = eräid,
			time = w.all$PimeätTunnit,
			sigma = plogis( w.all$PimeätTunnit, location=df.params$led.loc, scale=df.params$led.sca ) * df.params$led.max,
			tyyppi = w.all$Tyyppi,
		  status = w.all$Vaihdettu )
	}
	
	df.pi <- generate_data_for_pi(
		.mean = df.params$led.loc,
		.pi.interval = df.params$pi.interval,
		.sd.percentage = df.params$led.std.percent,
		.range.factor = df.params$range.factor );
	df.pi$mean.obs <- NA
	if( nrow(w) > 0 )
	{
		df.pi$mean.obs <- mean( w$PimeätTunnit )
	}
	
	df2.segment <- data.frame(
		EräID = eräid,
		x = if_else( is.na( df.pi$mean.obs ), df.pi$mean, df.pi$mean.obs ),
		xend = if_else( is.na( df.pi$mean.obs ), df.pi$mean, df.pi$mean.obs ),
		y = 0,
		yend = df.params$led.max )
	
	df2.rect <- data.frame(
		EräID = eräid,
		xmin = df.pi$lpl,
		xmax = df.pi$upl,
		ymin = 0,
		ymax = df.params$led.max )
	
	list(
		EräID = eräid,
		df = data.frame(
			EräID=eräid,
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

generate_data_for_pi <- function(
		.n = 1e6,
		.mean,
		.pi.interval = 0.95,
		.sd.percentage = 15,
		.range.factor = 8
)
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
