# Environment setup.
source( file = "src/env.R" )

# Flags.
options( "lightcaurby.Code-Daylight" = list(
	clean = FALSE,	# Whether to remove output files before generating them again.
	debug = c(			# Module names to stop in.
	)
))

# Main workflow.
lib <- suppressPackageStartupMessages( modules::use( here("src/workflows") ) )
plotting_data <- lib$workflow_all$run()

my.decimal.format <- function( .data, col, decimals )
{
	as.data.frame( .data ) %>%
		select( !!rlang::ensym( col ) ) %>%
		round( decimals ) %>%
		format(
				decimal.mark=",", 
				big.mark=" ", 
				nsmall=decimals,
				scientific = FALSE,
				justify="right"
			) %>%
		pull( !!rlang::ensym( col ) )
}

my.ggtexttable.wrapper <- function( df, title, myHiliteCol, mytheme = ttheme("blank") )
{
	nay <- which( !df$reject )
	yea <- which( df$reject )
	hiliteCol <- myHiliteCol
	tab <- ggtexttable(df, rows = NULL, theme = mytheme )
	if( length( nay ) )
	{
		tab <- tab %>%
			table_cell_font(row =nay + 1, column = hiliteCol, face="bold",
											color = "darkolivegreen4")
	}
	if( length( yea ) )
	{
		tab <- tab %>%
			table_cell_bg( row = yea + 1, column = hiliteCol, linewidth = 0,
										 fill = "coral", color = "coral")  %>%
			table_cell_font(row =yea + 1, column = hiliteCol, face="bold",
											color = "black")
	}
	tab <- tab %>%
		tab_add_title( text = title, face = "bold", hjust=0, size=14, color="black" )
	tab
}


my.preprocess.table.levene <- function( table )
{
	df <- table %>%
		rename(
			p.value = p
		) %>%
		mutate( 
			method = "Levene's test of homogeneity of variances", 
			reject = ( p.value < 0.05 ),
			statistic = my.decimal.format( ., statistic, 3  ),
			p.value = my.decimal.format( ., p.value, 3  )
		) %>%
		select(
			method, statistic, df1, df2, p.value, reject
		)
	df
}

my.preprocess.table.others <- function( table )
{
	df <- table %>%
		rename( 
			df1 = parameter
		) %>%
		mutate( 
			df2 = NA, 
			reject = ( p.value < 0.05 ),
			statistic = my.decimal.format( ., statistic, 3  ),
			p.value = my.decimal.format( ., p.value, 3  )
		) %>%
		select(
			method, statistic, df1, df2, p.value, reject
		)
	df
}

my.preprocess.table.anova <- function( table )
{
	df <- table %>%
		filter(
			term == "Erä"
		) %>%
		rename( 
			df1 = df
		) %>%
		mutate( 
			df2 = NA, 
			method = "One-way ANOVA comparison of means", 
			reject = ( p.value < 0.05 ),
			statistic = my.decimal.format( ., statistic, 3  ),
			p.value = my.decimal.format( ., p.value, 3  )
		) %>%
		select(
			method, statistic, df1, df2, p.value, reject
		)
	df
}


df <- plotting_data$models$phase2$extreme_outliers$output$table %>%
	mutate(
		PimeätTunnit = my.decimal.format( ., PimeätTunnit, 2 ),
		reject = is.extreme
	)
my.ggtexttable.wrapper( df, "Reject extreme outliers?", 8 )



df <- bind_rows(
	my.preprocess.table.anova(
		tidy( plotting_data$models$phase2$compare_means$output$anova$model )
	),
	my.preprocess.table.others(
		plotting_data$models$phase2$compare_means$output$kruskal.wallis$table
	)
)
themeModified <- ttheme( "blank" )
themeModified$colhead$fg_params$hjust = c( 0, 1, 1, 1, 1, 0.5 )
themeModified$colhead$fg_params$x = c( 0.025, 0.95, 0.95, 0.95, 0.95, 0.5 )
themeModified$core$fg_params$hjust =as.vector( matrix( c( 0, 1, 1, 1, 1, 0.5 ), ncol=6, nrow=nrow(df), byrow=T))
themeModified$core$fg_params$x = as.vector( matrix( c( 0.025, 0.95, 0.95, 0.95, 0.95, 0.5 ), ncol=6, nrow=nrow(df), byrow=T))
my.ggtexttable.wrapper( df, "Reject null hypothesis of equal means?", 6, themeModified )

df <- bind_rows(
	my.preprocess.table.levene(
		plotting_data$models$phase2$variance_homogeneity$output$levene$table
	),
	my.preprocess.table.others(
		plotting_data$models$phase2$variance_homogeneity$output$fligner.killeen$table
	),
	my.preprocess.table.others(
		plotting_data$models$phase2$variance_homogeneity$output$bartlett$table
	)
)

themeModified <- ttheme( "blank" )
themeModified$colhead$fg_params$hjust = c( 0, 1, 1, 1, 1, 0.5 )
themeModified$colhead$fg_params$x = c( 0.025, 0.95, 0.95, 0.95, 0.95, 0.5 )
themeModified$core$fg_params$hjust =as.vector( matrix( c( 0, 1, 1, 1, 1, 0.5 ), ncol=6, nrow=nrow(df), byrow=T))
themeModified$core$fg_params$x = as.vector( matrix( c( 0.025, 0.95, 0.95, 0.95, 0.95, 0.5 ), ncol=6, nrow=nrow(df), byrow=T))
my.ggtexttable.wrapper( df, "Reject null hypothesis of equal variances?", 6, themeModified )


df <- plotting_data$models$phase2$compare_means$output$anova$table %>%
	mutate( 
		reject = ( adj.p.value < 0.05 ),
		estimate = my.decimal.format( ., estimate, 2  ),
		conf.low = my.decimal.format( ., conf.low, 2  ),
		conf.high = my.decimal.format( ., conf.high, 2  ),
		adj.p.value = my.decimal.format( ., adj.p.value, 3  )
	)
my.ggtexttable.wrapper( df, "Reject null hypothesis of equal batch means?", 8 )

df <- plotting_data$models$phase2$normality_by_group$output$table %>%
	mutate( 
		reject = ( p < 0.05 ),
		statistic = my.decimal.format( ., statistic, 2  ),
		p = my.decimal.format( ., p, 3  )
	)
my.ggtexttable.wrapper( df, "Reject null hypothesis of normality?", 5 )


ggplot3 <- function( data, ... )
{
	ggplot(data) + list(...)
}

mtcars %>% 
	ggplot3(
		aes(x=hp, y=mpg),
		geom_point(),
		geom_smooth(),
		ggtitle('Fantastic Plot')
	) %>%
	print()

