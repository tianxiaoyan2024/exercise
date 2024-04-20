# --------------------------------------------
# Script Name: data_exploration.R
# Purpose1: This scribes how to remove the sites with missing data of the  Doubs
#           dataset, and detect whether environmental factors are collinearity.
# Purpose2: Analysis on the relationships between fishes and environment
#           factors and visualize such relationships.


# Author:     Xiaoyan Tian
# Email:      xiaoyantian@mail.ustc.edu.cn
# Date:       2024-04-15
#!！！初稿，还要补充，不是最终版本
# --------------------------------------------

library(ade4)
library(vegan)
library(ape)
library(dplyr)


data("doubs")
doubs
str(doubs)

fish <- doubs$fish
env <- doubs$env
spa <- doubs$xy

# --------------------------------------------
# remove the sites with missing data 
# --------------------------------------------
# 1.Check for missing values in the entire data frame
anyNA(doubs)

any_missing_in_data <- any(is.na(doubs))
cat("Does the data frame have missing data?", any_missing_in_data)
#No NA in the "doubs"
#If the data has misssing value, we can filter it by the following
doubs_no_na <- doubs %>%
  filter_all(all_vars(!is.na(.)))
#OR another method to filter
doubs_clean <- na.omit(doubs)

# 2.Know the species richness of each site
# 2.1 Check the sturcture for data frame of species firstly
colSums(doubs.fish)
summary(doubs.fish)
rowSums(fish) #Note:there are no species in site 8 

# 2.2 Next llustrate the species richness of each site along the river

sit.pres <- apply(fish > 0, 1, sum)
sort(sit.pres)
par(mfrow=c(1,2))
plot(sit.pres,type="s", las=1, col="gray",
     main="Species Richness vs. \n Upstream-Downstream Gradient",
     xlab="Positions of sites along the river", ylab="Species richness")
text(sit.pres, row.names(doubs.spe), cex=.8, col="red")
plot(spa, asp=1, main="Map of Species Richness", pch=21, col="white",
     bg="brown", cex=5*sit.pres/max(sit.pres), xlab="x coordinate (km)",
     ylab="y coordinate (km)")
lines(spa, col="light blue")
#Only the No.8 sample has no fish along the river

# 3.remove the No.8 site to clean the missing data 
fish <- fish[-8,]
env <- env[-8,]
spa <- spa[-8,]
View(fish)
knitr::kable(spe[1:9,1:5])

# --------------------------------------------
# Delete the collinear environmental factors by R mode
# --------------------------------------------
#R-mode (analysis of relationships among variables or columns)

env.pea<-cor(environment) # Pearson r linear correlation
round(env.pearson, 2) #Rounds the coefficients to 2 decimal points 
(env.ken<-cor(environment, method="kendall")) # Kendall tau rank correlation
round(env.ken, 2)

#Method2
library(stats)
op <- par(mfrow=c(1,1), pty="s")
pairs(doubs.env, panel=panel.smooth,
      diag.panel=panel.hist,
      main="Biplots with histograms and smooth surves")
par(op)

# Method3
library(Hmisc)
corr_matrix2 <- rcorr(as.matrix(doubs.env))
corr_matrix2

library(corrplot)
corrplot(corr_matrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

#dts is highly correlated with alt, slo, flo, har,nit...

library(PerformanceAnalytics)
chart.Correlation(doubs.env, histogram=TRUE, pch=19)



# --------------------------------------------
# Analysis on the relationships between fishes and environment
# --------------------------------------------
# Selection of unconstrained ordination base on CDA analysis
dca <- decorana(fish)
print(dca)
# 3.0 < DCA1==3.855 < 4.0, RDA and CCA are both OK

# Method1: Using RDA
# The Hellinger transformation for the species to correct for the double zero problem
fish.hel <- decostand(fish, "hellinger")

env.z <- decostand(environment, "standardize")
apply(env.z, 2, mean) # means = 0
apply(env.z, 2, sd) # standard deviations = 1

env.z1 <- as.data.frame(scale(environment))# Same standardization with scale() 
env.z1env.z <- subset(env.z, select = -dfs)
spe.rda <- rda(spe.hel ~ ., data = env.z)
summary(spe.rda)
osl_vif_tol(spe.rda)

