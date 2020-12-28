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



df <- plotting_data$models$phase2$extreme_outliers$output$table
df$is.extreme[ 2 ] = TRUE

nay <- which( !df$is.extreme )
yea <- which( df$is.extreme )

tab <- ggtexttable(df, rows = NULL, theme = ttheme( "blank" ) ) %>%  
	table_cell_bg( row = yea + 1, column = 7, linewidth = 0,
										fill="coral", color = "coral")  %>%
	table_cell_bg( row =nay + 1, column = 7, linewidth = 0,
										 fill="darkolivegreen1", color = "darkolivegreen1")  %>%
	tab_add_title( text = "Extreme outliers?", face = "bold", hjust=-0.75, size=20 )
tab


plotting_data$models$phase2$variance_homogeneity_levene$output$table

plotting_data$models$phase2$variance_homogeneity_fligner_killeen$output$table

plotting_data$models$phase2$variance_homogeneity_bartlett$output$table

plotting_data$models$phase2$compare_means_kruskal_wallis$output$table

plotting_data$models$phase2$compare_means_anova$output$model


EnsurePackage( "rvest" )
baseuri <- "https://bookdown.org/yihui/rmarkdown-cookbook/"
html <- xml2::read_html( paste0(baseuri, "index.html" ))
htmlfiles <- html %>%
	html_nodes(".chapter") %>%
	html_attr("data-path") %>%
	unique()

#purrr::map( htmlfiles, function (f) {
#	fn = paste0( baseuri, f )
#	xml2::download_html( fn)
#})
warnings()
