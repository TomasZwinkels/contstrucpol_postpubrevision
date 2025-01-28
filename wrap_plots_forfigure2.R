### putting it all together ###

install.packages("patchwork")
library(patchwork)

# set wd
	setwd("C:/Users/zwinkels/Tilburg University/TiU - StructureContent - Documents/General/content v structure polarization")
	getwd()

	list.files(pattern = ".rds")

# load
study1logic <- readRDS("study1logic.rds")
study1content <- readRDS("study1content.rds")
study2logic <- readRDS("study2logic.rds")
study2content <- readRDS("study2content.rds")


study1logic
study1content
study2logic
study2content

wrap_plots( study1logic,
			study1content,
			study2logic,
			study2content,
			ncol=2,
			nrow=2)
			
tiff(file="Wrapped marginal effect plots affective polarisation study 20250128.tif",
width=1500, height=1500, units="px", res=150)

	wrap_plots( study1logic,
				study1content,
				study2logic,
				study2content,
				ncol=2,
				nrow=2)

dev.off()