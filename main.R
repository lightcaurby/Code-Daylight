# Environment setup.

library( checkpoint )
checkpoint( "2020-10-01" )

library(here)

source( file = here("src/env.R" ) )

gctorture( FALSE )

# Main workflow.
lib <- suppressPackageStartupMessages( modules::use( here("src/workflows") ) )
plotting_data <- lib$workflow_all$run( .debugmod=F )





# Extreme outliers?
plotting_data$replacements %>%
	dplyr::filter(Vaihdettu & Erä %in% plotting_data$batches.multi$Erä) %>%
	group_by(Erä) %>%
	identify_outliers(PimeätTunnit) %>%
	select( Erä, Huoneisto, Pvm, PvmSeur, PimeätTunnit, is.outlier, is.extreme )


# Normality?
# - In the QQ plot, if all the points fall approximately along the reference line, we can assume normality.
# - By the overall Shapiro-Wilk test, if the p-value is not significant (p > 0.05), we can assume normality.
# - In the group-wise Shapiro-Wilk test, if the scores are normally distributed (p > 0.05) for each group, we can assume normality.

# Build the linear model
data <- plotting_data$replacements %>%
	dplyr::filter(Vaihdettu & Erä %in% plotting_data$batches.multi$Erä)
model  <- lm(PimeätTunnit ~ Erä, data = data)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
ggqqplot(data, "PimeätTunnit", facet.by = "Erä")
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
# Check normality assumption by groups
plotting_data$replacements %>%
	dplyr::filter(Vaihdettu & Erä %in% plotting_data$batches.multi$Erä) %>%
	group_by(Erä) %>%
	shapiro_test(PimeätTunnit)


# Homogeneity of variance assumption?

# The residuals versus fits plot can be used to check the homogeneity of variances.
# If in the plot below there is no evident relationships between residuals and fitted values 
# (the mean of each groups), we can assume the homogeneity of variances.
plot(model, 1)
# Levene's test to check the homogeneity of variances.
# With p>0.05, there is not significant difference between variances across groups, therefore
# we can assume the homogeneity of variances in the different treatment groups.
plotting_data$replacements %>%
	dplyr::filter(Vaihdettu & Erä %in% plotting_data$batches.multi$Erä) %>%
	levene_test(PimeätTunnit ~ Erä)

plotting_data$replacements %>%
	dplyr::filter(Vaihdettu & Erä %in% plotting_data$batches.multi$Erä) %>%
	pairwise_t_test( PimeätTunnit ~ Erä, p.adjust.method = "bonferroni")

plotting_data$replacements %>%
	dplyr::filter(Vaihdettu & Erä %in% plotting_data$batches.multi$Erä) %>%
	anova_test(PimeätTunnit ~ Erä)


