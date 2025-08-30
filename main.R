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

#CairoWin()


install.packages( "fitdistrplus" )
library( fitdistrplus )

result$data$batches
b.slice <- result$data$batches[ 1:10, ]$Aika

samples <- list()
for( b in b.slice )
{
	print( b )
	pop_mean <- result$data$batches %>%  filter( Aika == b ) %>% dplyr::select( Mtbf ) %>% unlist() %>% unname()
	s <- result$data$replacements %>% filter( Erä == b ) %>% dplyr::select( PimeätTunnit ) %>% unlist() %>% unname()
	if( length( s ) < 2 ) next
	s_sd <- sd( s )
	pop_sd_est <- sd( s ) * sqrt( ( length( s ) - 1 ) / length( s ) )
	t_test_result <- t.test( s, mu = pop_mean )
	conf_int <- t.test( s, conf.level = 0.95 )$conf.int
	pop_sd_percent <- pop_sd_est / pop_mean
	samples[[as.character( b )]] <- list( sample = s, pop_sd_percent = pop_sd_percent )
}

samples <- list()
for( b in b.slice )
{
	print( b )
	pop_mean <- result$data$batches %>%  filter( Aika == b ) %>% dplyr::select( Mtbf ) %>% unlist() %>% unname()
	s <- result$data$replacements %>% filter( Erä == b ) %>% dplyr::select( PimeätTunnit ) %>% unlist() %>% unname()
	if( length( s ) < 2 ) next
	fit_lnorm <- fitdist( s, "lnorm" )
	lognorm_sd <- sqrt(exp(fit_lnorm$estimate["sdlog"]^2) - 1) * exp(fit_lnorm$estimate["meanlog"] + (fit_lnorm$estimate["sdlog"]^2) / 2)
	s_sd <- sd( s )
	pop_sd_est <- sd( s ) * sqrt( ( length( s ) - 1 ) / length( s ) )
	s_sd <- lognorm_sd
	pop_sd_est <- sqrt((exp(fit_lnorm$estimate["sdlog"]^2) - 1) * exp(2 * fit_lnorm$estimate["meanlog"] + fit_lnorm$estimate["sdlog"]^2))
	pop_sd_percent <- pop_sd_est / pop_mean
	samples[[as.character( b )]] <- list( sample = s, pop_sd_percent = pop_sd_percent )
}


str( samples) 
qqnorm( s )
qqline( s, col="red")
shapiro.test( s )

fit_norm <- fitdist( s, "norm" )
fit_exp <- fitdist( s, "exp" )
fit_gamma <- fitdist( s, "gamma" )
fit_weibull <- fitdist( s, "weibull" )
fit_unif <- fitdist( s, "unif" )
fit_lnorm <- fitdist( s, "lnorm" )
summary( fit_norm )
summary( fit_exp )
summary( fit_gamma )
summary( fit_weibull )
summary( fit_unif )
summary( fit_lnorm )

par( mfrow = c(2,2))
plot( fit_norm)		 
plot( fit_exp)
plot( fit_gamma)
plot( fit_weibull)
plot( fit_unif)
plot( fit_lnorm)

AIC( fit_norm )
AIC( fit_exp )
AIC( fit_gamma )
AIC( fit_weibull )
AIC( fit_unif )
AIC( fit_lnorm )

# p > 0.05 --> sample could follow the distribution
ks.test( s, "pnorm", mean=fit_norm$estimate["mean"], sd=fit_norm$estimate["sd"])
ks.test( s, "pexp", rate= fit_exp$estimate )
ks.test( s, "pgamma", shape=fit_gamma$estimate["shape"], rate=fit_gamma$estimate["rate"])
ks.test( s, "pweibull", shape=fit_weibull$estimate["shape"], scale=fit_weibull$estimate["scale"])
ks.test( s, "punif", min=fit_unif$estimate["min"], max=fit_unif$estimate["max"])
ks.test( s, "plnorm", meanlog=fit_lnorm$estimate["meanlog"], sdlog=fit_lnorm$estimate["sdlog"])

#norm
t.test( s, conf.level = 0.95 )$conf.int

# exp
lower_bound <- qexp(0.025, rate=fit_exp$estimate)
upper_bound <- qexp(0.975, rate=fit_exp$estimate)
c(lower_bound, upper_bound)

#gamma
lower_bound <- qgamma(0.025, shape=fit_gamma$estimate["shape"], rate=fit_gamma$estimate["rate"])
upper_bound <- qgamma(0.975, shape=fit_gamma$estimate["shape"], rate=fit_gamma$estimate["rate"])
c(lower_bound, upper_bound)

# weibull
lower_bound <- qweibull(0.025, shape=fit_weibull$estimate["shape"], scale=fit_weibull$estimate["scale"])
upper_bound <- qweibull(0.975, shape=fit_weibull$estimate["shape"], scale=fit_weibull$estimate["scale"])
c(lower_bound, upper_bound)

#unif
mean_est <- (min(s) + max(s)) / 2
se <- (max(s) - min(s)) / sqrt(12 * length(s))
lower_bound <- mean_est - 1.96 * se
upper_bound <- mean_est + 1.96 * se
c(lower_bound, upper_bound)

# unif CI for min and max
boot_min <- function(data, indices) { return(min(data[indices])) }
boot_max <- function(data, indices) { return(max(data[indices])) }
boot_min_results <- boot(s, boot_min, R=1000)
boot_max_results <- boot(s, boot_max, R=1000)
boot.ci(boot_min_results, type="perc")  # For 'a'
boot.ci(boot_max_results, type="perc")  # For 'b'

# lnorm
log_sample <- log(s)
t_result <- t.test(log_sample, conf.level = 0.95)
lower_log <- t_result$conf.int[1]
upper_log <- t_result$conf.int[2]
lower_bound <- exp(lower_log)
upper_bound <- exp(upper_log)
c(lower_bound, upper_bound)

# Bootstrapping
library(boot)
boot_function <- function(data, indices) {
	return(mean(data[indices]))  # Can also compute median, variance, etc.
}
boot_results <- boot(s, boot_function, R=1000)
boot.ci(boot_results, type="perc")

#sd
exp_sd <- 1 / fit_exp$estimate
gamma_sd <- fit_gamma$estimate["shape"] / fit_gamma$estimate["rate"]
weibull_sd <- fit_weibull$estimate["scale"] * sqrt(gamma(1 + 2/fit_weibull$estimate["shape"]) - (gamma(1 + 1/fit_weibull$estimate["shape"]))^2)
unif_sd <- (fit_unif$estimate["max"] - fit_unif$estimate["min"]) / sqrt(12)
lognorm_sd <- sqrt(exp(fit_lnorm$estimate["sdlog"]^2) - 1) * exp(fit_lnorm$estimate["meanlog"] + (fit_lnorm$estimate["sdlog"]^2) / 2)


