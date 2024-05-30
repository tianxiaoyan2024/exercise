# --------------------------------------------
# Script Name: data_exploration.R
# Purpose1: This scribes how to remove the sites with missing data of the  Doubs
#           dataset, and detect whether environmental factors are collinearity.
# Purpose2: Analysis on the relationships between fishes and environment
#           factors and visualize such relationships.


# Author:     Xiaoyan Tian
# Email:      xiaoyantian@mail.ustc.edu.cn
# Date:       2024-04-15

# --------------------------------------------

cat("\014") #clears rhe console
rm(list=ls()) #remove all variales

# Load the package
library(ade4)
library(vegan)
library(ape)
library(dplyr)

# Load the required dataset
data("doubs")
doubs
str(doubs)
str(env)

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
# Result: No NA in the "doubs"

#If the data has misssing value, we can filter it by the following
doubs_no_na <- doubs %>%
  filter_all(all_vars(!is.na(.)))
#OR another method to filter
doubs_clean <- na.omit(doubs)

# 2. Know the species richness of each site
# 2.1 Check the sturcture for data frame of species firstly
colSums(fish)
summary(fish) 
rowSums(fish) # Note: there are no species in site 8 
sort(rowSums(fish))
# 2.2 Next illustrate the species richness of each site along the river to ensure the  

sit.pres <- apply(fish > 0, 1, sum)
sort(sit.pres) ## Confirm: the species richness at NO.8 site is zero
par(mfrow=c(1,2))
plot(sit.pres,type="s", las=1, col="gray",
     main="Species Richness vs. \n Upstream-Downstream Gradient",
     xlab="Positions of sites along the river", ylab="Species richness")
text(sit.pres, row.names(fish), cex=.8, col="red")
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
# Find the collinear environmental factors 
# --------------------------------------------
#R-mode (analysis of relationships among variables)
# Pearson r linear correlation
env.pearson<-cor(env)
round(env.pearson, 2) #Rounds the coefficients to 2 decimal points 

# Kendall tau rank correlation
env.kendall<-cor(env, method="kendall") 
round(env.kendall, 2)

# Generate a correlation chart containing a histogram and scatterplot matrix
# Method 1 on class
library(stats)
op <- par(mfrow=c(1,1), pty="s")
pairs(doubs.env, panel=panel.smooth,
      diag.panel=panel.hist,
      main="Biplots with histograms and smooth surves")
par(op)

# Method 2
# Reference:https://blog.csdn.net/weixin_39886469/article/details/111017010  
#           http://sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
library(Hmisc)
rcorr_env <- rcorr(as.matrix(env))
print(class(rcorr_env))
print(rcorr_env)

library(corrplot)
corrplot(rcorr_env$r, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
# The plot will only show the upper triangle, use hierarchical clustering for ordering

install.packages("PerformanceAnalytics", dependencies = TRUE)
library(PerformanceAnalytics)
chart.Correlation(env, histogram=T, pch=19)
# Note: there is a high collinearity between 'dfs' and other environmental factors
# --------------------------------------------
# Analysis on the relationships between fishes and environment
# --------------------------------------------
# Selection of unconstrained ordination base on CDA analysis
dca <- decorana(fish)
print(dca)
# 3.0 < DCA1==3.855 < 4.0, RDA and CCA are both OK

# 
# 1.Standardize and transform the data
# 1.1 The Hellinger transformation of the coummunity species to correct for the double zero problem
fish.hel <- decostand(fish, "hellinger")
# Calculate distance matrix
fish.bchel<-vegdist(fish.hel, method="bray", binary=FALSE) 

# 1.2 The Z-score standardization of the environmental factor for normal distribution
env.z <- decostand(env, "standardize")
apply(env.z, 2, mean) # means = 0
apply(env.z, 2, sd) # standard deviations = 1
# OR same standardization with scale() 
env.z1 <- as.data.frame(scale(env))

# 1.3 Remove the environmental factor of the 'dfs',showing collinearity with other factors
env.z2 <- subset(env.z, select = -which(names(env.z) == "dfs"))

# 2. Run the RDA and select environmental variables
dim(fish.hel)
dim(env.z2)
rda_fish <- rda(fish.hel ~ ., data = env.z2)
summary(rda_fish)
rda_fish$call
#rda(formula = fish.hel ~ alt + slo + flo + pH + har + pho + nit + amm + oxy + bdo, data = env.z2)

# 3.Forward selection of environmental variables
fwd.sel <- ordiR2step(rda(doubs.fish.hel ~ 1, data = doubs.env.z),
                      scope = formula(doubs.fish.rda), direction = "forward", R2scope = TRUE,
                      pstep = 1000, trace = FALSE)
fwd.sel$call
# rda(formula = doubs.fish.hel ~ alt + oxy + bdo, data = doubs.env.z)

# 4.Run the new model between species and the selected environmental factors
rda_signif_fish <- rda(fish.hel ~ alt + oxy + bdo, data = env.z2)

# 5.Check R^2 retreived from the rda result
R2 <- RsquareAdj(rda_signif_fish)$r.squared # unadjusted R^2 
R2 #0.5894243
R2adj <- RsquareAdj(rda_signif_fish)$adj.r.squared # adjusted R^2
R2adj #0.5401552

# 6.Check and test model significance.
anova.cca(rda_signif_fish, step = 1000) 

# 7.Visualize such relationships
# Biplots of RDA results
# Scaling 1
ordiplot(rda_signif_fish, scaling = 1, main = "", type = "text")
# Scaling 2
ordiplot(rda_signif_fish, scaling = 2, main = "Biplot - scaling 2", type = "text")


# Triplot of RDA  results: sites, response variables and explanatory variables
# Scaling 1
plot(rda_signif_fish, scaling=1, main="Triplot - scaling 1")
spe.sc <- scores(rda_signif_fish, choices=1:2, scaling=1, display="sp")
arrows(0,0,spe.sc[,1], spe.sc[,2], length=0, lty=1, col='red')

# Scaling 2
plot(rda_signif_fish, main="Triplot - scaling 2")
spe2.sc <- scores(rda_signif_fish, choices=1:2, display="sp")  
arrows(0,0,spe2.sc[,1], spe2.sc[,2], length=0, lty=1, col='red')

# PS: the step2 can use another method like this:
# PCA ananlysis on 'env'
pca_results <- prcomp(env.z2, scale = TRUE) 
# Generate a summary of the PCA results, including the standard deviation, proportion of variance explained, cumulative proportion
summary(pca_results)
# Create a biplot showing the observations (rows) and variables (columns) in the PCA results
biplot(pca_results, scale = 0, cex = 0.6)
#RDA analysis using principal components of PCA as new predictor variables which explain most of the variation in the 'env'
rda_results <- rda(fish.hel ~ PC1 + PC2, data = as.data.frame(pca_results$x))


