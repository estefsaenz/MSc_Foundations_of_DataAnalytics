setwd("C:/Users/estef/OneDrive/Warwick - MSc in Data Analytics/CS910 Foundations of Data Analytics/Final Project")
library(ggplot2)
library(plyr)
library(gridExtra)
library(Hmisc)
library(factoextra)
library(stringr)
library(data.table)
library(mclust)
library(clValid)
library(NbClust)

tema <- theme_bw() 
theme_set(tema)


set.seed(1856249)

business.8 <- read.csv('Bussiness/base.csv')


recod <- read.csv('Bussiness/recode_business_type.csv')

# recodif.names <- paste("'", recod$section, "'='", recod$recode, "'", sep='', collapse=';')
# names(business.8) <- car::recode(names(business.8), recodif.names)

vars.int <- setdiff(names(business.8), c('Code','X'))

# ===========
# PRINC COMP
# ===========

comps <- princomp(business.8[,vars.int])
summary(comps)

biplot(comps)
biplot(comps, expand=3)


# ==============================
# Determine number of clusters
# ==============================

# === ELBOW METHOD ===

# K-means

g1 <- fviz_nbclust(comps$scores[,1:7], kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = 'k-means') + ggtitle("Elbow Method's Optimal Number of Clusters") + ylab("")

# K-medioids

g2 <- fviz_nbclust(comps$scores[,1:7], pam, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = 'k-medoids') + ggtitle("") 

# H-clust

g3 <- fviz_nbclust(comps$scores[,1:7], hcut, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Hierarchical Clustering") + ggtitle("") + ylab("")


grid.arrange(g1, g2,g3, nrow = 3)



# === Silhouette METHOD ===

# K-means

g1 <- fviz_nbclust(comps$scores[,1:7], kmeans, method = "silhouette")+
  labs(subtitle = 'k-means') + ggtitle("Silhouette Method's Optimal Number of Clusters") + ylab("")

#  K-Medoids
g2 <- fviz_nbclust(comps$scores[,1:7], pam, method = "silhouette")+
  labs(subtitle = "k-medoids") + ggtitle("")


# H CLUST
g3 <- fviz_nbclust(comps$scores[,1:7], hcut, method = "silhouette")+
  labs(subtitle = "Hierarchical Clustering") + ggtitle("")+ ylab("")

grid.arrange(g1, g2, g3, nrow = 3)



# === GAP Statistic ===


# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.

# K means
gap_stat <- clusGap(comps$scores[,1:7], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 500)
g1 <- fviz_gap_stat(gap_stat)+
  labs(subtitle = "k-means") + ggtitle("Gap Statistic's Optimal Number of Clusters") + ylab("")

# K medoids

gap_stat <- clusGap(comps$scores[,1:7], FUN = pam, K.max = 10, B = 500)
g2 <- fviz_gap_stat(gap_stat)+
  labs(subtitle = "k-medoids") + ggtitle("")

# Hierarchical clustering
gap_stat <- clusGap(comps$scores[,1:7], FUN = hcut, K.max = 10, B = 500)
g3 <- fviz_gap_stat(gap_stat)+
  labs(subtitle = "Hierarchical Clustering") + ggtitle("")+ ylab("")

grid.arrange(g1, g2, g3, nrow = 3)






mod1 <- Mclust(as.matrix(comps$scores[,1:7]))
summary(mod1)
plot(mod1,  what = c("BIC", "classification"))

# Density Clustering

mod2 <- densityMclust(comps$scores[,1:7])
summary(mod2)
plot(mod2, what = "density")

#plot(mod2, what = "density", type = "level")
#plot(mod2, what = "density", type = "level",
#     data = faithful, points.cex = 0.5)
