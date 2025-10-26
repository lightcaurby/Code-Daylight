
#####################################################

plot( result$data$daylight_info$normalyear$dark.hours)

result$data$daylight_info$normalyear %>% filter( month == 10 & day == 26)

result$data$replacements %>% filter( EräID == "10")

######################################################
### Sigmoid function applied to survival probabilities
######################################################

df.extracted.for.batch <- result$data$batch_predictions[[1]]
df.params <- df.extracted.for.batch %>%
	extract2( "df.params" )
df.pi.segment <- df.extracted.for.batch %>%
	extract2( "df.pi.segment" )
df.pi.rect <- df.extracted.for.batch %>%
	extract2( "df.pi.rect" )
df.fit <- df.extracted.for.batch %>%
	extract2( "df.fit" )

# Parameters
mu <- df.params$led.loc        # Expected lifetime
sigma <- df.params$led.sca     # Standard deviation
samples <- seq(0, df.params$led.max, length.out = df.params$led.max)   # Current age of individual

# Sigmoid function
sigmoid <- function(t, mu, sigma) {
	1 / (1 + exp(-(t - mu) / sigma))
}

# Derivative of sigmoid (PDF-like)
sigmoid_derivative <- function(t, mu, sigma) {
	exp_term <- exp(-(t - mu) / sigma)
	(1 / sigma) * (exp_term / (1 + exp_term)^2)
}

# Probability of having died by age_now
cdf_value <- sigmoid(samples, mu, sigma)
#plot( cdf_value )

# Instantaneous probability of death at age_now
#pdf_value <- sigmoid_derivative(age_now, mu, sigma)
#pdf_value


# Compute probabilities
prob <- sigmoid(samples, mu, sigma)
prob <- plogis(samples, mu, sigma)
prob <- sigmoid_derivative(samples, mu, sigma)

# Visualization
ggplot(data.frame(samples, prob), aes(x = samples, y = prob)) +
	geom_line(color = "steelblue", size = 1.2) +
	geom_vline(xintercept = mu, linetype = "dashed", color = "red") +
	annotate("text", x = mu, y = 0.5, label = "Expected Lifetime", vjust = -1, color = "red") +
	labs(
		title = "Lifetime Prediction Using Sigmoid Function",
		x = "Age",
		y = "Cumulative Probability of Death"
	) +
	theme_minimal()


# Simulate survival status at fixed time
prob_survival <- prob
status <- rbinom(df.params$led.max, 1, prob_survival)
time <- runif(df.params$led.max, 1, df.params$led.max)  # arbitrary time points

# Kaplan-Meier from simulated logistic-based survival
library(survival)
km_fit <- survfit(Surv(time, status) ~ 1)
plot(km_fit, main = "KM Curve from Logistic-based Survival", xlab = "Time", ylab = "Survival Probability")

# Visualization
ggplot(data.frame(x=km_fit$time,y=km_fit$surv), aes(x = x, y = y)) +
	geom_line(color = "steelblue", size = 1.2) +
	geom_vline(xintercept = mu, linetype = "dashed", color = "red") +
	annotate("text", x = mu, y = 0.5, label = "Expected Lifetime", vjust = -1, color = "red") +
	labs(
		title = "KM Curve from Logistic-based Survival",
		x = "Age",
		y = "Survival Probability"
	) +
	theme_minimal()


str( km_fit )

################################################
### Draw the usual sigmoid plot from the samples
################################################

df.extracted.for.batch <- result$data$batch_predictions[[1]]
df.params <- df.extracted.for.batch %>%
	extract2( "df.params" )
df.pi.segment <- df.extracted.for.batch %>%
	extract2( "df.pi.segment" )
df.pi.rect <- df.extracted.for.batch %>%
	extract2( "df.pi.rect" )
df.fit <- df.extracted.for.batch %>%
	extract2( "df.fit" )


xs <- sort( round( rlogis( df.params$led.max, location=df.params$led.loc, scale=df.params$led.sca ), 0), decreasing=T )
df.log <- data.frame( x=seq(0, df.params$led.max, length.out = df.params$led.max),	y=xs )

y.breaks = c( 0, 0.1, seq( 0.25, 0.75, by=0.25 ), 0.9, 1.0) * df.params$led.max
y.labels = format( sort( y.breaks / df.params$led.max, decreasing=F), nsmall=2, small.mark=",") 
x.breaks = seq( 0, df.params$led.max, by=10000 )
x.labels = format( x.breaks, big.mark=" ") 

ggplot() + 
	geom_line( data=df.log, aes(x = x, y = y), linewidth=1.5, linetype="solid", color="darkgrey" ) + 
	geom_segment( data=df.pi.segment, aes(x = x, xend=xend, y = y, yend=yend), inherit.aes = F, color="orange", linewidth=1.5, alpha=0.5 ) +
	geom_rect( data=df.pi.rect, aes(xmin = xmin, xmax = xmax, ymin=ymin, ymax = ymax ), inherit.aes = F, color=NA, fill="orange", alpha=0.2 ) +
	geom_point( data=df.fit %>% filter( status == TRUE ), aes(x = time, y = sigma, group=tyyppi, color = tyyppi), alpha=0.75, size=6 ) +
	geom_point( data=df.fit %>% filter( status == FALSE ), aes(x = time, y = sigma, group=tyyppi, color = tyyppi), alpha=0.75, size=9, shape="circle open" ) +
	scale_fill_manual(values = c("pienloiste"="#FC4E07", "led"="#00AFBB"), breaks = c("pienloiste", "led"), labels = c("fluorescent", "led")) +
	scale_color_manual(values = c("pienloiste"="#FC4E07", "led"="#00AFBB"), breaks = c("pienloiste", "led"), labels = c("fluorescent", "led")) +
	theme(legend.position = "none") +
	scale_x_continuous(
		name = "Lifetime in hours",
		breaks= x.breaks,
		labels= x.labels,
		limits = c(0, df.params$led.max)) + 
	scale_y_continuous(
		name = "Probability of lifetime",
		breaks= y.breaks,
		labels= y.labels,
		limits = c(0, df.params$led.max))
	

#########################################
### Corrected standard deviation by batch
#########################################

batchcode <- function( df, group ) 
{
	pt <- df %>% filter( Vaihdettu == TRUE ) %>% pull( PimeätTunnit )
	mtbf <- result$data$batches %>% filter( Erä == group ) %>% pull( Mtbf )
	sd_corrected <- sqrt(sum((pt - mtbf)^2) / (length(pt) - 1))
	ret <- c( sd_corrected / mtbf )
	names( ret ) <- group
	ret
}

groups <- result$data$replacements %>%
	group_by( EräID ) %>%
	group_split()
keys <- result$data$replacements %>%
	group_by( EräID ) %>%
	group_keys() %>%
	pull( EräID ) %>%
	as.character()

map2( groups, keys, batchcode )

#########################################
### Distribution fitting
#########################################

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
#fit_exp <- fitdist( s/100, "exp" )
fit_gamma <- fitdist( s, "gamma" )
fit_weibull <- fitdist( s, "weibull" )
fit_unif <- fitdist( s, "unif" )
fit_lnorm <- fitdist( s, "lnorm" )
summary( fit_norm )
#summary( fit_exp )
summary( fit_gamma )
summary( fit_weibull )
summary( fit_unif )
summary( fit_lnorm )

par( mfrow = c(2,2))
plot( fit_norm)		 
#plot( fit_exp)
plot( fit_gamma)
plot( fit_weibull)
plot( fit_unif)
plot( fit_lnorm)

AIC( fit_norm )
#AIC( fit_exp )
AIC( fit_gamma )
AIC( fit_weibull )
AIC( fit_unif )
AIC( fit_lnorm )

# p > 0.05 --> sample could follow the distribution
ks.test( s, "pnorm", mean=fit_norm$estimate["mean"], sd=fit_norm$estimate["sd"])
#ks.test( s, "pexp", rate= fit_exp$estimate )
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


