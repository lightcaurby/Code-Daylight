# Helper method for package installation.
EnsurePackage<-function(x)
{
	x <- as.character(x)
	if (!require(x,character.only=TRUE))
	{
		install.packages(pkgs=x) #,repos="http://cran.r-project.org")
		require(x,character.only=TRUE)
	}
}

# Checkpoint.
EnsurePackage( "checkpoint" )
checkpoint( "2020-10-01" )

# Packages.
EnsurePackage( "here" )
EnsurePackage( "modules" )
EnsurePackage( "tidyverse" )
EnsurePackage( "readr" )
EnsurePackage( "stringr" )
EnsurePackage( "lubridate" )
EnsurePackage( "ggplot2" )
EnsurePackage( "ggpubr" )
EnsurePackage( "ggridges" )
EnsurePackage( "ggTimeSeries" )
EnsurePackage( "grid" )
EnsurePackage( "gridExtra" )
EnsurePackage( "cowplot" )
EnsurePackage( "egg" )
EnsurePackage( "rstatix" )
EnsurePackage( "utils" )

