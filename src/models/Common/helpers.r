import( "here" )
import( "modules" )
import( "dplyr" )
import( "ggplot2" )
import( "ggpubr" )
import( "stats" )
import( "rstatix" )

export( "my.decimal.format" )
export( "my.modified.ggtable.theme" )
export( "my.ggtexttable.wrapper" )
export( "my.preprocess.table.anova" )
export( "my.preprocess.table.levene" )
export( "my.preprocess.table.others" )
export( "my.construct.result" )

# Executes decimal number formatting.
my.decimal.format <- function( .data, col, decimals )
{
	# Return value.
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

# Modified theme for ggtables.
my.modified.ggtable.theme <- function( df )
{
	# Use the blank theme as the baseline.
	themeModified <- ttheme( "blank" )
	
	# Column header alignments.
	themeModified$colhead$fg_params$hjust = c( 0, 1, 1, 1, 1, 0.5 )
	themeModified$colhead$fg_params$x = c( 0.025, 0.95, 0.95, 0.95, 0.95, 0.5 )
	
	# Column value alignments.
	themeModified$core$fg_params$hjust = as.vector( matrix( 
		c( 0, 1, 1, 1, 1, 0.5 ), 
		ncol=6, 
		nrow=nrow( df ), 
		byrow=T
	) )
	themeModified$core$fg_params$x = as.vector( matrix( 
		c( 0.025, 0.95, 0.95, 0.95, 0.95, 0.5 ),
		ncol=6, 
		nrow=nrow( df ), 
		byrow=T
	) )
	
	# Return value.
	themeModified
}

# Wraps ggtexttable with common formatting options.
my.ggtexttable.wrapper <- function( df, title, myHiliteCol, mytheme = ttheme("blank"), subtitle="" )
{
	# Determine the rejection status.
	nay <- which( !df$reject )
	yea <- which( df$reject )
	
	# Column to highlight.
	hiliteCol <- myHiliteCol
	
	# Construct the table.
	tab <- ggtexttable(df, rows = NULL, theme = mytheme )
	
	# Format the non-rejected data.
	if( length( nay ) )
	{
		tab <- tab %>%
			table_cell_font(row =nay + 1, column = hiliteCol, face="bold",
											color = "darkolivegreen4")
	}

	# Format the rejected data.
	if( length( yea ) )
	{
		tab <- tab %>%
			table_cell_bg( row = yea + 1, column = hiliteCol, linewidth = 0,
										 fill = "coral", color = "coral")  %>%
			table_cell_font(row =yea + 1, column = hiliteCol, face="bold",
											color = "black")
	}
	
	# Add the title.
	tab <- tab %>%
		tab_add_title( text = title, face = "bold", hjust=0, size=14, color="black" )

	# Return value.
	tab
}

# Preprocesses the output table of one-way ANOVA test.
my.preprocess.table.anova <- function( table )
{
	# Return value.
	table %>%
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
}

# Preprocesses the output table of Levene's test.
my.preprocess.table.levene <- function( table )
{
	# Return value.
	table %>%
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
}

# Preprocesses the output table of other tests for variances/means.
my.preprocess.table.others <- function( table )
{
	# Return value.
	table %>%
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
}

my.construct.result <- function(
	model = NULL,
	data = NULL,
	table = NULL,
	table.w = 8,
	table.h = 4,
	plot = NULL,
	plot.w = 8,
	plot.h = 8
)
{
	list(
		model = model,
		data = data,
		table = table,
		table.w = table.w,
		table.h = table.h,
		plot = plot,
		plot.w = plot.w,
		plot.h = plot.h
	)
}


