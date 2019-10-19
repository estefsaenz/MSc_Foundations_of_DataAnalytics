setwd("C:/Users/estef/OneDrive/Warwick - MSc in Data Analytics/CS910 Foundations of Data Analytics/Final Project")
library(ggplot2)
library(plyr)
library(gridExtra)
library(Hmisc)
library(factoextra)

tema <- theme_bw() 
theme_set(tema)

data <- read.csv('london_wards.csv')
housing <- read.csv('london_housing.csv')
demo <- read.csv('london_demo.csv')

vars.int <-  names(data)[-c(1:4)]
lapply(data,class)

for(var in vars.int){
  data[,var] <- as.numeric(as.character(data[,var]))
}


vars.h <- names(housing)[-c(1:4)]
for(var in vars.h){
  housing[,var] <- as.numeric(as.character(housing[,var]))
}



vars.d <- names(demo)[-c(1:4)]
for(var in vars.d){
  demo[,var] <- as.numeric(as.character(demo[,var]))
}

# =====================
# SCATTER-PLOT MATRIX
# =====================

formula <-  parse(text=paste(vars.int, collapse ='+'))

pairs(~formula, data=data, main="Simple Scatterplot Matrix")

########
# SVM
########


comps <- princomp(data[,vars.int])

summary(comps)

biplot(comps)
#biplot(comps, choices=c(2,3))

comps.h <- princomp(housing[,vars.h])
summary(comps.h)
biplot(comps.h)
#biplot(comps.h, choices=c(2,3))



comps.d <- princomp(demo[,vars.d])
summary(comps.d)
biplot(comps.d)
biplot(comps.d, choices=c(2,3))



# ===========
# CLUSTERING
# ===========

hclust_methods <- c("ward.D", "single", "complete", "average")
list_plot<-list()
for(i in 1:length(hclust_methods)){
  hc <- hclust(dist(data[,vars.int],method="euclidean"), method = hclust_methods[i])
  #if(i==4){cut=4} else{cut=2}
  grp <- cutree(hc, k = 5)
  list_plot[[i]]<-fviz_cluster(list(data = data[,vars.int], cluster = grp)) + ggtitle(paste0('method :',hclust_methods[i]))
}
grid.arrange(list_plot[[1]],list_plot[[2]],
             list_plot[[3]],list_plot[[4]],ncol=2)

clust.1 <- hclust(dist(base.seg[,vars.int.1]), method='ward')