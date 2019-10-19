setwd("C:/Users/estef/OneDrive/Warwick - MSc in Data Analytics/CS910 Foundations of Data Analytics/Final Project")
library(ggplot2)
library(plyr)
library(gridExtra)
library(Hmisc)
library(factoextra)
library(stringr)
library(data.table)
library(mclust)
library(cclust)

set.seed(1856249)

tema <- theme_bw() 
theme_set(tema)

business.8 <- read.csv('Bussiness/base.csv')[,-1]
vars.int <- names(business.8)[-1]

data <- read.csv('london-borough-profiles.csv')
borough <- unique(data[,c('Code','Area_name')])

business.8 <- join(business.8, borough, by='Code')

# =====================
# PRINCIPAL COMPONENTS
# =====================

comps <- princomp(business.8[,vars.int])
summary(comps)
biplot(comps)
biplot(comps, expand=2)
biplot(comps, choices = c(3,4))


# ===========
# CLUSTERING
# ===========

data.clust <- comps$scores[,1:7]
rownames(data.clust) <- business.8$Area_name

# We selected the first 7 components to keep more than 95% 
# of the total variation

set.seed(1856249)

# k-means ===

# Elbow = 2
# Silhouette = 2
# Gap = 1

kmeans.c2 <- kmeans(data.clust, 2,nstart = 25)

g1 <- fviz_cluster(kmeans.c2, data = data.clust)

 
# k-medoids ===

# Elbow = 2
# Silhouette = 9
# Gap = 1

kmed.c2.e <- pam(data.clust, 2, metric = "euclidean")
kmed.c2.m <- pam(data.clust, 2, metric = "manhattan")

kmed.c9.e <- pam(data.clust, 9, metric = "euclidean")
kmed.c9.m <- pam(data.clust, 9, metric = "manhattan")

g1 <- fviz_cluster(kmed.c2.e, data = data.clust)
g2 <- fviz_cluster(kmed.c2.m, data = data.clust)

g3 <- fviz_cluster(kmed.c9.e, data = data.clust) + 
  theme_bw() + ggtitle('k-medoids')
g4 <- fviz_cluster(kmed.c9.m, data = data.clust) + 
  theme_bw() + theme(legend.position="none") + ggtitle('') +
  labs(subtitle = 'Manhattan Distance')

grid.arrange(g3,g4, nrow = 2)
g3


# H Clust ===

# Elbow = 4
# Silhouette = 2
# Gap = 1

hclust_methods <- c("single", "complete", "average")

list_plot_e<-list()
list_hclust_e <- list()

for(i in 1:length(hclust_methods)){
  hc <- hclust(dist(data.clust, method="euclidean"), method = hclust_methods[i])
  grp <- cutree(hc, k = 2)
  list_hclust_e[[i]] <- hc
  if(i == 1){
    list_plot_e[[i]]<-fviz_cluster(list(data = data.clust, cluster = grp)) + ggtitle(paste0('method :',hclust_methods[i])) +
                    ggtitle('Hierarchical Clustering') +labs(subtitle = "method :single") +
                    theme_bw()
               
  }
  else{
    list_plot_e[[i]]<-fviz_cluster(list(data = data.clust, cluster = grp)) + ggtitle(paste0('method :',hclust_methods[i])) +
      theme_bw() 
  }
}
grid.arrange(list_plot_e[[1]],list_plot_e[[2]],
             list_plot_e[[3]],ncol=1)

list_plot_m<-list()
list_hclust_m <- list()

for(i in 1:length(hclust_methods)){
  hc <- hclust(dist(data.clust, method="manhattan"), method = hclust_methods[i])
  grp <- cutree(hc, k = 4)
  list_hclust_m[[i]] <- hc 
  if(i == 1){
    list_plot_m[[i]]<-fviz_cluster(list(data = data.clust, cluster = grp)) + ggtitle(paste0('method :',hclust_methods[i])) +
      ggtitle('Manhattan Distance') +labs(subtitle = "method :single")
  }
  else{
    list_plot_m[[i]]<-fviz_cluster(list(data = data.clust, cluster = grp)) + ggtitle(paste0('method :',hclust_methods[i]))
  }
}

#grid.arrange(list_plot_e[[1]],list_plot_m[[1]], list_plot_e[[2]],
 #            list_plot_m[[2]],list_plot_e[[3]], list_plot_m[[3]],ncol=2)






eval <- clValid(data.clust, 2:9, clMethods=c("hierarchical","kmeans","pam"))

optimalScores(eval)

par(mfrow=c(2,2),mar=c(4,4,3,1))
plot(eval, measure=c("APN","AD","ADM"),legend=FALSE) + legend("center", clusterMethods(stab), col=1:9, lty=1:9, pch=paste(1:9))
plot(nClusters(eval),measures(eval,"APN")[,,1],type="n",axes=F,
        + xlab="",ylab="")
legend("center", clusterMethods(stab), col=1:9, lty=1:9, pch=paste(1:9))
par(op)

eval.1 <- clValid(data.clust, 2:9, clMethods=c("hierarchical","kmeans","pam"),
        validation='internal')

optimalScores(eval.1)


# Best clustering methods:
# Hierarchical with 2 clusters
# k-medoids with 9 clusters


kmed.c9.e <- pam(data.clust, 9, metric = 'euclidean')

business.8$kmed <- kmed.c9.e$clustering 
business.8$hclust <- cutree(list_hclust_e[[2]],2) # complete link

write.csv(business.8, 'Bussiness/clust.csv')

# ===============
# PERFILAMIENTO
# ===============

business.9 <- melt(business.8, id.vars=c('kmed','hclust','Code','Area_name'))


recod <- read.csv('Bussiness/recode_business_type_1.csv')
recodif.names <- paste("'", recod$type, "'='", recod$recode, "'", sep='', collapse=';')
business.9$variable.1 <- car::recode(business.9$variable, recodif.names)

gg1 <- ddply(business.9, c('kmed','variable.1'), summarise, mini=min(value), media=mean(value),
            maxi=max(value))


gg2 <- ddply(business.9, c('hclust','variable.1'), summarise, mini=min(value), media=mean(value),
             maxi=max(value))

ggplot(gg1, aes(x=variable.1, y=media, ymin=mini, ymax=maxi)) + 
  geom_errorbar(colour='gray70')+ geom_point(colour='salmon') +
  facet_wrap(~kmed) + coord_flip() + xlab('') + ylab('')

ggplot(gg2, aes(x=variable.1, y=media, ymin=mini, ymax=maxi)) + 
  geom_errorbar(colour='gray70')+ geom_point(colour='salmon') +
  facet_wrap(~hclust) + coord_flip() + xlab('') + ylab('')





# MCLUST (BAYESIAN INFERENCE CRITERION)

d_clust <- Mclust(as.matrix(data.clust), G=1:15, 
                  modelNames = mclust.options("emModelNames"))
plot(d_clust, what)
summary(d_clust)

# Dimention reduction to visualize the clusters
drmod <- MclustDR(d_clust, lambda = 1)
summary(drmod)
plot(drmod, what = 'contour')

business.8$mclust <- d_clust$classification

business.9 <- melt(business.8, id.vars=c('kmed','hclust','mclust','Code','Area_name'))


recod <- read.csv('Bussiness/recode_business_type_1.csv')
recodif.names <- paste("'", recod$type, "'='", recod$recode, "'", sep='', collapse=';')
business.9$variable.1 <- car::recode(business.9$variable, recodif.names)

gg3 <- ddply(business.9, c('mclust','variable.1'), summarise, mini=min(value), media=mean(value),
             maxi=max(value))

ggplot(gg3, aes(x=variable.1, y=media, ymin=mini, ymax=maxi)) + 
  geom_errorbar(colour='gray70')+ geom_point(colour='salmon') +
  facet_wrap(~mclust) + coord_flip() + xlab('') + ylab('')


write.csv(business.8, 'Bussiness/clust_b.csv')
