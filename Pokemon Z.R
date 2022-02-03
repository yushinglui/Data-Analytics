.libPaths("C:/Rpackages")
install.packages("readxl")
install.packages("xlsx")
install.packages("factoextra")
install.packages("clustertend")
install.packages("GGally")
install.packages("clValid")
install.packages("NbClust")
install.packages("cluster")
install.packages("fpc")
library("dplyr")
library("tidyr")
library("psych")
library("readxl")
library("xlsx")
library("factoextra")
library("clustertend")
library("GGally")
library("clValid")
library("NbClust")
library("cluster")
library("fpc")
library("ggplot2")
library("summarytools")
st_options(descr.stats = c("mean","sd"))

# ================Multiple plot function=================
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

myplots <- list()  # new empty list
for(i in 1:8){
  col <- names(pokemon_stats)[i]
  ggp <- ggplot(pokemon_stats, aes_string(x = col)) +
    geom_histogram(color = "black")
  myplots[[i]] <- ggp  # add each plot into plot list
}

multiplot(plotlist = myplots, cols = 4)

#=====Normalization function======
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#========Robust Scalar============
robust_scalar <- function(x){
  return ((x - median(x)) / (quantile(x, probs = .75) - quantile(x, probs = .25)))
}

#===== Read Data =====
pokemon_all <-read.csv("C:/MSC/pokedex.csv")
#pokemon_c <- drop_na(pokemon_all)
pokemon_all <- drop_na(pokemon_all, c("height_m", "weight_kg", "hp", "attack", "defense", "sp_attack", "sp_defense", "speed"))
pokemon_stats <- select(pokemon_all, height_m, weight_kg, hp, attack, defense, sp_attack, sp_defense, speed)
rownames(pokemon_stats) <- pokemon_all$index
pokemon_stats <- drop_na(pokemon_stats)
#Standardized Z-scores used here
pokemon_z <- as.data.frame(scale(pokemon_stats, center = TRUE, scale = TRUE))

#=====Hopkins statistic======
#============================
#== A value close to 1 tends to indicate the data 
#is highly clustered, random data will tend to result 
#in values around 0.5, and uniformly distributed data 
#will tend to result in values close to 0.==
#============================
hopkins_stat <- hopkins(pokemon_z, nrow(pokemon_z)-1)
cat("Hopkin's statistic is ", hopkins_stat$H)


#======Euclidean Distance======
dis_all<-get_dist(pokemon_z, method = "euclidean")
fviz_dist(dis_all, show_labels = F) + labs(title = "Ordered Dissimilarity Matrix - All data")

wss <- (nrow(pokemon_z)-1)*sum(apply(pokemon_z,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(pokemon_z,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# ======Elbow method with fviz=====
fviz_nbclust(pokemon_z[,c(1:8)], hcut, method = "wss") +
  labs(subtitle = "Elbow method") +
  geom_vline(xintercept = 3, linetype = 2)

# ======silhouette method with fviz=======
fviz_nbclust(pokemon_z[,c(1:8)], kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

# ======Gap statistic with fviz=======
set.seed(123)
fviz_nbclust(pokemon_z[,c(1:2)], kmeans, nstart = 25, iter.max = 30,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

# ==========NbClust===========
nb <- NbClust(pokemon_z[,c(1:8)], distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "complete", index ="all")
fviz_nbclust(nb) + theme_minimal()

#========= 1. K-means on ALL stats ==============
dis_all<-get_dist(pokemon_z, method = "euclidean")

k2 <- kmeans(pokemon_z, 2)
k3 <- kmeans(pokemon_z, 3)

k2_means <- aggregate(pokemon_z, by=list(k2$cluster), FUN=mean)
k3_means <- aggregate(pokemon_z, by=list(k3$cluster), FUN=mean)

fviz_cluster(k2, data = pokemon_z)
fviz_cluster(k3, data = pokemon_z)

pokemon_z$k2_clusters <- k2$cluster
pokemon_z$k3_clusters <- k3$cluster

#ggplot(pokemon_z, aes(x=attack, y=defense, color=factor(k2_clusters))) + geom_point() + labs(color = "Clusters") + ggtitle("Attack vs. Defense")
#ggpairs(pokemon_z, columns = 1:8, ggplot2::aes(colour=factor(k2_clusters)))
#ggpairs(pokemon_z, columns = 1:8, ggplot2::aes(colour=factor(k3_clusters)))

#-----------use eclust---------------
dis_all<-get_dist(pokemon_z[,c(1:8)], method = "euclidean")

k2_eclust <- eclust(pokemon_z[,c(1:8)], "kmeans", k=2, nstart=25, graph = TRUE)
k3_eclust <- eclust(pokemon_z[,c(1:8)], "kmeans", k=3, nstart=25, graph = TRUE)
#k5_eclust <- eclust(pokemon_z[,c(1:8)], "kmeans", k=5, nstart=25, graph = TRUE)

fviz_silhouette(k2_eclust)
fviz_silhouette(k3_eclust)
#fviz_silhouette(k5_eclust)

fviz_cluster(k2_eclust, data = pokemon_z)
fviz_cluster(k3_eclust, data = pokemon_z)
#fviz_cluster(k5_eclust, data = pokemon_z)

pokemon_z$k2_eclust <- k2_eclust$cluster
pokemon_z$k3_eclust <- k3_eclust$cluster
pokemon_stats$k2_all <- k2_eclust$cluster
pokemon_stats$k3_all <- k3_eclust$cluster
#pokemon_stats$k5_all <- k5_eclust$cluster

#ggpairs(pokemon_z, columns = 1:8, ggplot2::aes(colour=factor(k2_eclust)))
#ggpairs(pokemon_z, columns = 1:8, ggplot2::aes(colour=factor(k3_eclust)))

k2_info <- cluster.stats(dis_all, k2_eclust$cluster)
k3_info <- cluster.stats(dis_all, k3_eclust$cluster)
#k5_info <- cluster.stats(dis_all, k5_eclust$cluster)

k2_all_stats <- group_by(pokemon_stats, k2_all) %>%
  descr(pokemon_stats[,c(1:8)], transpose = TRUE) 
k3_all_stats <- group_by(pokemon_stats, k3_all) %>%
  descr(pokemon_stats[,c(1:8)], transpose = TRUE)
#k5_all_stats <- group_by(pokemon_stats, k5_all) %>%
#  descr(pokemon_stats[,c(1:8)], transpose = TRUE)

k2plots <- list()  # new empty list
for(i in 1:8){
  col <- names(pokemon_stats)[i]
  ggp <- ggplot(pokemon_stats, aes_string(y = col, x = "k2_all", group = "k2_all")) +
    stat_boxplot(geom ='errorbar') + 
    geom_boxplot(fill = c("red","blue"), notch = TRUE) + 
    scale_x_discrete(name = "Cluster", limits = c("1", "2"))
  k2plots[[i]] <- ggp  # add each plot into plot list
}

multiplot(plotlist = k2plots, cols = 4)

k3plots <- list()  # new empty list
for(i in 1:8){
  col <- names(pokemon_stats)[i]
  ggp <- ggplot(pokemon_stats, aes_string(y = col, x = "k3_all", group = "k3_all")) +
    stat_boxplot(geom ='errorbar') + 
    geom_boxplot(fill = c("red","blue","green"), notch = TRUE) + 
    scale_x_discrete(name = "Cluster", limits = c("1", "2", "3"))
  k3plots[[i]] <- ggp  # add each plot into plot list
}

multiplot(plotlist = k3plots, cols = 4)

#========= 2. K-means on HP, attack, defense, speed only ===========
pokemon_hads <- select(pokemon_z, hp, attack, defense, speed)

dis_hads<-get_dist(pokemon_hads, method = "euclidean")

wss_hads <- (nrow(pokemon_hads)-1)*sum(apply(pokemon_hads,2,var))
for (i in 2:15) wss_hads[i] <- sum(kmeans(pokemon_hads,
                                          centers=i)$withinss)
plot(1:15, wss_hads, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

hopkins_hads <- hopkins(pokemon_hads, nrow(pokemon_hads)-1)
cat("Hopkin's statistic is ", hopkins_hads$H)

k2_hads <- kmeans(pokemon_hads, 2)
k3_hads <- kmeans(pokemon_hads, 3)

fviz_cluster(k2_hads, data = pokemon_hads)
fviz_cluster(k3_hads, data = pokemon_hads)

k2_means_hads <- aggregate(pokemon_hads, by=list(k2_hads$cluster), FUN=mean)
k3_means_hads <- aggregate(pokemon_hads, by=list(k3_hads$cluster), FUN=mean)

pokemon_hads$k2_clusters <- k2_hads$cluster
pokemon_hads$k3_clusters <- k3_hads$cluster

#ggpairs(pokemon_hads, columns = 1:4, ggplot2::aes(colour=factor(k2_clusters)))
#ggpairs(pokemon_hads, columns = 1:4, ggplot2::aes(colour=factor(k3_clusters)))

#-----------use eclust---------------
pokemon_hads <- select(pokemon_z, hp, attack, defense, speed)

dis_hads<-get_dist(pokemon_hads, method = "euclidean")

k2_hads_ec <- eclust(pokemon_hads, "kmeans", k=2, nstart=25, graph = TRUE)
k3_hads_ec <- eclust(pokemon_hads, "kmeans", k=3, nstart=25, graph = TRUE)

fviz_silhouette(k2_hads_ec)
fviz_silhouette(k3_hads_ec)

fviz_cluster(k2_hads_ec, data = pokemon_hads)
fviz_cluster(k3_hads_ec, data = pokemon_hads)

pokemon_hads$k2_hads_ec <- k2_hads_ec$cluster
pokemon_hads$k3_hads_ec <- k3_hads_ec$cluster
pokemon_stats$k2_hads <- k2_hads_ec$cluster
pokemon_stats$k3_hads <- k3_hads_ec$cluster

#ggpairs(pokemon_hads, columns = 1:4, ggplot2::aes(colour=factor(k2_hads_ec)))
#ggpairs(pokemon_hads, columns = 1:4, ggplot2::aes(colour=factor(k3_hads_ec)))

k2_hads_info <- cluster.stats(dis_hads, k2_hads_ec$cluster)
k3_hads_info <- cluster.stats(dis_hads, k3_hads_ec$cluster)

k2_hads_stats <- group_by(pokemon_stats, k2_hads) %>%
  descr(pokemon_stats[,c(3:5,8)], transpose = TRUE) 
k3_hads_stats <- group_by(pokemon_stats, k3_hads) %>%
  descr(pokemon_stats[,c(3:5,8)], transpose = TRUE)

k3plots <- list()  # new empty list
for(i in c(3:5,8)){
  col <- names(pokemon_stats)[i]
  ggp <- ggplot(pokemon_stats, aes_string(y = col, x = "k3_hads", group = "k3_hads")) +
    stat_boxplot(geom ='errorbar') + 
    geom_boxplot(fill = c("red","blue","green"), notch = TRUE) + 
    scale_x_discrete(name = "Cluster", limits = c("1", "2", "3"))
  k3plots[[i]] <- ggp  # add each plot into plot list
}

multiplot(plotlist = k3plots, cols = 4)

#========= 3. K-means without height and weight ===========
pokemon_nohw <- select(pokemon_z, hp, attack, defense, sp_attack, sp_defense, speed)

dis_nohw<-get_dist(pokemon_nohw, method = "euclidean")

wss_nohw <- (nrow(pokemon_nohw)-1)*sum(apply(pokemon_nohw,2,var))
for (i in 2:15) wss_nohw[i] <- sum(kmeans(pokemon_nohw,
                                          centers=i)$withinss)
plot(1:15, wss_nohw, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

hopkins_nohw <- hopkins(pokemon_nohw, nrow(pokemon_nohw)-1)
cat("Hopkin's statistic is ", hopkins_nohw$H)

k2_nohw <- kmeans(pokemon_nohw, 2)
k3_nohw <- kmeans(pokemon_nohw, 3)

fviz_cluster(k2_nohw, data = pokemon_nohw)
fviz_cluster(k3_nohw, data = pokemon_nohw)

k2_means_nohw <- aggregate(pokemon_nohw, by=list(k2_nohw$cluster), FUN=mean)
k3_means_nohw <- aggregate(pokemon_nohw, by=list(k3_nohw$cluster), FUN=mean)

pokemon_nohw$k2_clusters <- k2_nohw$cluster
pokemon_nohw$k3_clusters <- k3_nohw$cluster

#ggpairs(pokemon_nohw, columns = 1:6, ggplot2::aes(colour=factor(k2_clusters)))
#ggpairs(pokemon_nohw, columns = 1:6, ggplot2::aes(colour=factor(k3_clusters)))

#-----------use eclust---------------
pokemon_nohw <- select(pokemon_z, hp, attack, defense, sp_attack, sp_defense, speed)

dis_nohw<-get_dist(pokemon_nohw, method = "euclidean")

k2_nohw_ec <- eclust(pokemon_nohw, "kmeans", k=2, nstart=25, graph = TRUE)
k3_nohw_ec <- eclust(pokemon_nohw, "kmeans", k=3, nstart=25, graph = TRUE)

fviz_silhouette(k2_nohw_ec)
fviz_silhouette(k3_nohw_ec)

fviz_cluster(k2_nohw_ec, data = pokemon_nohw)
fviz_cluster(k3_nohw_ec, data = pokemon_nohw)

pokemon_nohw$k2_nohw_ec <- k2_nohw_ec$cluster
pokemon_nohw$k3_nohw_ec <- k3_nohw_ec$cluster
pokemon_stats$k2_nohw <- k2_nohw_ec$cluster
pokemon_stats$k3_nohw <- k3_nohw_ec$cluster

#ggpairs(pokemon_nohw, columns = 1:6, ggplot2::aes(colour=factor(k2_nohw_ec)))
#ggpairs(pokemon_nohw, columns = 1:6, ggplot2::aes(colour=factor(k3_nohw_ec)))

k2_nohw_info <- cluster.stats(dis_nohw, k2_nohw_ec$cluster)
k3_nohw_info <- cluster.stats(dis_nohw, k3_nohw_ec$cluster)

k2_nohw_stats <- group_by(pokemon_stats, k2_nohw) %>%
  descr(pokemon_stats[,c(3:8)], transpose = TRUE) 
k3_nohw_stats <- group_by(pokemon_stats, k3_nohw) %>%
  descr(pokemon_stats[,c(3:8)], transpose = TRUE)

k3plots <- list()  # new empty list
for(i in 3:8){
  col <- names(pokemon_stats)[i]
  ggp <- ggplot(pokemon_stats, aes_string(y = col, x = "k3_nohw", group = "k3_nohw")) +
    stat_boxplot(geom ='errorbar') + 
    geom_boxplot(fill = c("red","blue","green"), notch = TRUE) + 
    scale_x_discrete(name = "Cluster", limits = c("1", "2", "3"))
  k3plots[[i]] <- ggp  # add each plot into plot list
}

multiplot(plotlist = k3plots, cols = 4)

#========= 4. K-means with only height and weight ===========
pokemon_onlyhw <- select(pokemon_z, height_m, weight_kg)

dis_onlyhw <- get_dist(pokemon_onlyhw, method = "euclidean")

wss_onlyhw <- (nrow(pokemon_onlyhw)-1)*sum(apply(pokemon_onlyhw,2,var))
for (i in 2:15) wss_onlyhw[i] <- sum(kmeans(pokemon_onlyhw,
                                          centers=i)$withinss)
plot(1:15, wss_onlyhw, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#hopkins_onlyhw <- hopkins(pokemon_onlyhw, nrow(pokemon_onlyhw)-1)
#cat("Hopkin's statistic is ", hopkins_onlyhw$H)

k2_onlyhw <- kmeans(pokemon_onlyhw, 2)
k3_onlyhw <- kmeans(pokemon_onlyhw, 3)

fviz_cluster(k2_onlyhw, data = pokemon_onlyhw)
fviz_cluster(k3_onlyhw, data = pokemon_onlyhw)

k2_means_onlyhw <- aggregate(pokemon_onlyhw, by=list(k2_onlyhw$cluster), FUN=mean)
k3_means_onlyhw <- aggregate(pokemon_onlyhw, by=list(k3_onlyhw$cluster), FUN=mean)

pokemon_onlyhw$k2_clusters <- k2_onlyhw$cluster
pokemon_onlyhw$k3_clusters <- k3_onlyhw$cluster

#ggpairs(pokemon_onlyhw, columns = 1:2, ggplot2::aes(colour=factor(k2_clusters)))
#ggpairs(pokemon_onlyhw, columns = 1:2, ggplot2::aes(colour=factor(k3_clusters)))

ggally_box(pokemon_onlyhw, ggplot2::aes(x = factor(k3_onlyhw_ec), y = height_m, colour=factor(k3_onlyhw_ec)))
result.anova <- aov(height_m ~ factor(k3_onlyhw_ec), data = pokemon_onlyhw)
summary(result.anova)
TukeyHSD(result.anova)
result.manova <- manova(cbind(height_m, weight_kg) ~ factor(k3_clusters), pokemon_onlyhw)
summary(result.manova, test="Wilks")

#-----------use eclust---------------
pokemon_onlyhw <- select(pokemon_z, height_m, weight_kg)

dis_onlyhw <- get_dist(pokemon_onlyhw, method = "euclidean")

k2_onlyhw_ec <- eclust(pokemon_onlyhw, "kmeans", k=2, nstart=25, graph = TRUE)
k3_onlyhw_ec <- eclust(pokemon_onlyhw, "kmeans", k=3, nstart=25, graph = TRUE)

fviz_silhouette(k2_onlyhw_ec)
fviz_silhouette(k3_onlyhw_ec)

fviz_cluster(k2_onlyhw_ec, data = pokemon_onlyhw)
fviz_cluster(k3_onlyhw_ec, data = pokemon_onlyhw)

pokemon_onlyhw$k2_onlyhw_ec <- k2_onlyhw_ec$cluster
pokemon_onlyhw$k3_onlyhw_ec <- k3_onlyhw_ec$cluster
pokemon_stats$k2_onlyhw <- k2_onlyhw_ec$cluster
pokemon_stats$k3_onlyhw <- k3_onlyhw_ec$cluster

#ggpairs(pokemon_onlyhw, columns = 1:2, ggplot2::aes(colour=factor(k2_onlyhw_ec)))
#ggpairs(pokemon_onlyhw, columns = 1:2, ggplot2::aes(colour=factor(k3_onlyhw_ec)))

k2_onlyhw_info <- cluster.stats(dis_onlyhw, k2_onlyhw_ec$cluster)
k3_onlyhw_info <- cluster.stats(dis_onlyhw, k3_onlyhw_ec$cluster)

k2_onlyhw_stats <- group_by(pokemon_stats, k2_onlyhw) %>%
  descr(pokemon_stats[,c(1:2)], transpose = TRUE) 
k3_onlyhw_stats <- group_by(pokemon_stats, k3_onlyhw) %>%
  descr(pokemon_stats[,c(1:2)], transpose = TRUE)

k2hwplots <- list()  # new empty list
for(i in 1:2){
  col <- names(pokemon_stats)[i]
  ggp <- ggplot(pokemon_stats, aes_string(y = col, x = "k2_onlyhw", group = "k2_onlyhw")) +
    stat_boxplot(geom ='errorbar') + 
    geom_boxplot(fill = c("red","blue"), notch = TRUE) + 
    scale_x_discrete(name = "Cluster", limits = c("1", "2"))
  k2hwplots[[i]] <- ggp  # add each plot into plot list
}

multiplot(plotlist = k2hwplots, cols = 4)

#========= 5. K-means with only SP ===========
pokemon_sp <- select(pokemon_z, sp_attack, sp_defense)

dis_sp<-get_dist(pokemon_sp, method = "euclidean")

wss_sp <- (nrow(pokemon_sp)-1)*sum(apply(pokemon_sp,2,var))
for (i in 2:15) wss_sp[i] <- sum(kmeans(pokemon_sp,
                                            centers=i)$withinss)
plot(1:15, wss_sp, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

hopkins_sp <- hopkins(pokemon_sp, nrow(pokemon_sp)-1)
cat("Hopkin's statistic is ", hopkins_sp$H)

k2_sp <- kmeans(pokemon_sp, 2)
k3_sp <- kmeans(pokemon_sp, 3)

fviz_cluster(k2_sp, data = pokemon_sp)
fviz_cluster(k3_sp, data = pokemon_sp)

k2_means_sp <- aggregate(pokemon_sp, by=list(k2_sp$cluster), FUN=mean)
k3_means_sp <- aggregate(pokemon_sp, by=list(k3_sp$cluster), FUN=mean)

pokemon_sp$k2_clusters <- k2_sp$cluster
pokemon_sp$k3_clusters <- k3_sp$cluster

#ggpairs(pokemon_sp, columns = 1:2, ggplot2::aes(colour=factor(k2_clusters)))
#ggpairs(pokemon_sp, columns = 1:2, ggplot2::aes(colour=factor(k3_clusters)))

#-----------use eclust---------------
pokemon_sp <- select(pokemon_z, sp_attack, sp_defense)

dis_sp<-get_dist(pokemon_sp, method = "euclidean")

k2_sp_ec <- eclust(pokemon_sp, "kmeans", k=2, nstart=25, graph = TRUE)
k3_sp_ec <- eclust(pokemon_sp, "kmeans", k=3, nstart=25, graph = TRUE)

fviz_silhouette(k2_sp_ec)
fviz_silhouette(k3_sp_ec)

fviz_cluster(k2_sp_ec, data = pokemon_sp)
fviz_cluster(k3_sp_ec, data = pokemon_sp)

pokemon_sp$k2_sp_ec <- k2_sp_ec$cluster
pokemon_sp$k3_sp_ec <- k3_sp_ec$cluster
pokemon_stats$k2_sp <- k2_sp_ec$cluster
pokemon_stats$k3_sp <- k3_sp_ec$cluster

#ggpairs(pokemon_sp, columns = 1:2, ggplot2::aes(colour=factor(k2_sp_ec)))
#ggpairs(pokemon_sp, columns = 1:2, ggplot2::aes(colour=factor(k3_sp_ec)))

k2_sp_info <- cluster.stats(dis_sp, k2_sp_ec$cluster)
k3_sp_info <- cluster.stats(dis_sp, k3_sp_ec$cluster)

k2_sp_stats <- group_by(pokemon_stats, k2_sp) %>%
  descr(pokemon_stats[,c(6:7)], transpose = TRUE) 
k3_sp_stats <- group_by(pokemon_stats, k3_sp) %>%
  descr(pokemon_stats[,c(6:7)], transpose = TRUE)


#========= 6. K-means with only HP attack defense ===========
pokemon_had <- select(pokemon_z, hp, attack, defense)

dis_had<-get_dist(pokemon_had, method = "euclidean")

wss_had <- (nrow(pokemon_had)-1)*sum(apply(pokemon_had,2,var))
for (i in 2:15) wss_had[i] <- sum(kmeans(pokemon_had,
                                            centers=i)$withinss)
plot(1:15, wss_had, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

hopkins_had <- hopkins(pokemon_had, nrow(pokemon_had)-1)
cat("Hopkin's statistic is ", hopkins_had$H)

k2_had <- kmeans(pokemon_had, 2)
k3_had <- kmeans(pokemon_had, 3)

fviz_cluster(k2_had, data = pokemon_had)
fviz_cluster(k3_had, data = pokemon_had)

k2_means_had <- aggregate(pokemon_had, by=list(k2_had$cluster), FUN=mean)
k3_means_had <- aggregate(pokemon_had, by=list(k3_had$cluster), FUN=mean)

pokemon_had$k2_clusters <- k2_had$cluster
pokemon_had$k3_clusters <- k3_had$cluster

#ggpairs(pokemon_had, columns = 1:3, ggplot2::aes(colour=factor(k2_clusters)))
#ggpairs(pokemon_had, columns = 1:3, ggplot2::aes(colour=factor(k3_clusters)))

#-----------use eclust---------------
pokemon_had <- select(pokemon_z, hp, attack, defense)

dis_had<-get_dist(pokemon_had, method = "euclidean")

k2_had_ec <- eclust(pokemon_had, "kmeans", k=2, nstart=25, graph = TRUE)
k3_had_ec <- eclust(pokemon_had, "kmeans", k=3, nstart=25, graph = TRUE)

fviz_silhouette(k2_had_ec)
fviz_silhouette(k3_had_ec)

fviz_cluster(k2_had_ec, data = pokemon_had)
fviz_cluster(k3_had_ec, data = pokemon_had)

pokemon_had$k2_had_ec <- k2_had_ec$cluster
pokemon_had$k3_had_ec <- k3_had_ec$cluster
pokemon_stats$k2_had <- k2_had_ec$cluster
pokemon_stats$k3_had <- k3_had_ec$cluster

#ggpairs(pokemon_had, columns = 1:3, ggplot2::aes(colour=factor(k2_had_ec)))
#ggpairs(pokemon_had, columns = 1:3, ggplot2::aes(colour=factor(k3_had_ec)))

k2_had_info <- cluster.stats(dis_had, k2_had_ec$cluster)
k3_had_info <- cluster.stats(dis_had, k3_had_ec$cluster)

k2_had_stats <- group_by(pokemon_stats, k2_had) %>%
  descr(pokemon_stats[,c(3:5)], transpose = TRUE) 
k3_had_stats <- group_by(pokemon_stats, k3_had) %>%
  descr(pokemon_stats[,c(3:5)], transpose = TRUE)

k3plots <- list()  # new empty list
for(i in 3:5){
  col <- names(pokemon_stats)[i]
  ggp <- ggplot(pokemon_stats, aes_string(y = col, x = "k3_had", group = "k3_had")) +
    stat_boxplot(geom ='errorbar') + 
    geom_boxplot(fill = c("red","blue","green"), notch = TRUE) + 
    scale_x_discrete(name = "Cluster", limits = c("1", "2", "3"))
  k3plots[[i]] <- ggp  # add each plot into plot list
}

multiplot(plotlist = k3plots, cols = 4)

#============================================================================
#============================================================================

#============ A. Hierarchical clustering All stats ==================

# hc_complete <- hclust(dis_all, method = "complete")
# grp_hcc2 <- cutree(hc_complete, k=2)
# grp_hcc3 <- cutree(hc_complete, k=3)
# pokemon_hcc <- mutate(pokemon_z, hclust2 = grp_hcc2, hclust3 = grp_hcc3)
# plot(hc_complete, cex = 0.6)
# rect.hclust(hc_complete, k = 2, border = c('red','green')) 
# rect.hclust(hc_complete, k = 3, border = c('red','green','blue')) 
# #ggpairs(pokemon_hcc, columns = 1:8, ggplot2::aes(colour=factor(hclust2)))
# #ggpairs(pokemon_hcc, columns = 1:8, ggplot2::aes(colour=factor(hclust3)))
# grp_hcc2_info <- cluster.stats(dis_all, grp_hcc2)
# grp_hcc3_info <- cluster.stats(dis_all, grp_hcc3)

#------------use eclust------------
pokemon_z <- select(pokemon_z, 1:8)

dis_all<-get_dist(pokemon_z, method = "euclidean")

hcc2_all <- eclust(pokemon_z, "hclust", k=2, hc_method="ward.D2", graph = FALSE)
hcc3_all <- eclust(pokemon_z, "hclust", k=3, hc_method="ward.D2", graph = FALSE)
pokemon_stats$hcc2_all <- hcc2_all$cluster
pokemon_stats$hcc3_all <- hcc3_all$cluster
hcc2_all_info <- cluster.stats(dis_all, hcc2_all$cluster)
hcc3_all_info <- cluster.stats(dis_all, hcc3_all$cluster)

hcc2_all_stats <- group_by(pokemon_stats, hcc2_all) %>%
  descr(pokemon_stats[,c(1:8)], transpose = TRUE) 
hcc3_all_stats <- group_by(pokemon_stats, hcc3_all) %>%
  descr(pokemon_stats[,c(1:8)], transpose = TRUE)

fviz_silhouette(hcc2_all)
fviz_silhouette(hcc3_all)

fviz_dend(hcc2_all, rect = TRUE, show_labels = FALSE)
fviz_dend(hcc3_all, rect = TRUE, show_labels = FALSE)

hcc3allplots <- list()  # new empty list
for(i in 1:8){
  col <- names(pokemon_stats)[i]
  ggp <- ggplot(pokemon_stats, aes_string(y = col, x = "hcc3_all", group = "hcc3_all")) +
    stat_boxplot(geom ='errorbar') + 
    geom_boxplot(fill = c("red","blue","green"), notch = TRUE) + 
    scale_x_discrete(name = "Cluster", limits = c("1", "2", "3"))
  hcc3allplots[[i]] <- ggp  # add each plot into plot list
}

multiplot(plotlist = hcc3allplots, cols = 4)

#============ B. Hierarchical clustering HADS ==================

#------------use eclust------------
pokemon_hads <- select(pokemon_z, hp, attack, defense, speed)

dis_hads<-get_dist(pokemon_hads, method = "euclidean")

hcc2_hads <- eclust(pokemon_hads, "hclust", k=2, hc_method="ward.D2", graph = FALSE)
hcc3_hads <- eclust(pokemon_hads, "hclust", k=3, hc_method="ward.D2", graph = FALSE)
pokemon_stats$hcc2_hads <- hcc2_hads$cluster
pokemon_stats$hcc3_hads <- hcc3_hads$cluster
hcc2_hads_info <- cluster.stats(dis_hads, hcc2_hads$cluster)
hcc3_hads_info <- cluster.stats(dis_hads, hcc3_hads$cluster)

hcc2_hads_stats <- group_by(pokemon_stats, hcc2_hads) %>%
  descr(pokemon_stats[,c(3:5,8)], transpose = TRUE) 
hcc3_hads_stats <- group_by(pokemon_stats, hcc3_hads) %>%
  descr(pokemon_stats[,c(3:5,8)], transpose = TRUE)

fviz_silhouette(hcc2_hads)
fviz_silhouette(hcc3_hads)

fviz_dend(hcc2_hads, rect = TRUE, show_labels = FALSE)
fviz_dend(hcc3_hads, rect = TRUE, show_labels = FALSE)

hcc3hadsplots <- list()  # new empty list
for(i in c(3:5,8)){
  col <- names(pokemon_stats)[i]
  ggp <- ggplot(pokemon_stats, aes_string(y = col, x = "hcc3_hads", group = "hcc3_hads")) +
    stat_boxplot(geom ='errorbar') + 
    geom_boxplot(fill = c("red","blue","green"), notch = TRUE) + 
    scale_x_discrete(name = "Cluster", limits = c("1", "2", "3"))
  hcc3hadsplots[[i]] <- ggp  # add each plot into plot list
}

multiplot(plotlist = hcc3hadsplots, cols = 4)

#============ C. Hierarchical clustering without height & weight ==================

#------------use eclust------------
pokemon_nohw <- select(pokemon_z, hp, attack, defense, sp_attack, sp_defense, speed)

dis_nohw<-get_dist(pokemon_nohw, method = "euclidean")

hcc2_nohw <- eclust(pokemon_nohw, "hclust", k=2, hc_method="ward.D2", graph = FALSE)
hcc3_nohw <- eclust(pokemon_nohw, "hclust", k=3, hc_method="ward.D2", graph = FALSE)
pokemon_stats$hcc2_nohw <- hcc2_nohw$cluster
pokemon_stats$hcc3_nohw <- hcc3_nohw$cluster
hcc2_nohw_info <- cluster.stats(dis_nohw, hcc2_nohw$cluster)
hcc3_nohw_info <- cluster.stats(dis_nohw, hcc3_nohw$cluster)

hcc2_nohw_stats <- group_by(pokemon_stats, hcc2_nohw) %>%
  descr(pokemon_stats[,c(3:8)], transpose = TRUE) 
hcc3_nohw_stats <- group_by(pokemon_stats, hcc3_nohw) %>%
  descr(pokemon_stats[,c(3:8)], transpose = TRUE)

fviz_silhouette(hcc2_nohw)
fviz_silhouette(hcc3_nohw)

fviz_dend(hcc2_nohw, rect = TRUE, show_labels = FALSE)
fviz_dend(hcc3_nohw, rect = TRUE, show_labels = FALSE)

#============ D. Hierarchical clustering only height & weight ==================

#------------use eclust------------
pokemon_onlyhw <- select(pokemon_z, height_m, weight_kg)

dis_onlyhw <- get_dist(pokemon_onlyhw, method = "euclidean")

hcc2_onlyhw <- eclust(pokemon_onlyhw, "hclust", k=2, hc_method="ward.D2", graph = FALSE)
hcc3_onlyhw <- eclust(pokemon_onlyhw, "hclust", k=3, hc_method="ward.D2", graph = FALSE)
pokemon_stats$hcc2_onlyhw <- hcc2_onlyhw$cluster
pokemon_stats$hcc3_onlyhw <- hcc3_onlyhw$cluster
hcc2_onlyhw_info <- cluster.stats(dis_onlyhw, hcc2_onlyhw$cluster)
hcc3_onlyhw_info <- cluster.stats(dis_onlyhw, hcc3_onlyhw$cluster)

hcc2_onlyhw_stats <- group_by(pokemon_stats, hcc2_onlyhw) %>%
  descr(pokemon_stats[,c(1:2)], transpose = TRUE) 
hcc3_onlyhw_stats <- group_by(pokemon_stats, hcc3_onlyhw) %>%
  descr(pokemon_stats[,c(1:2)], transpose = TRUE)

fviz_silhouette(hcc2_onlyhw)
fviz_silhouette(hcc3_onlyhw)

fviz_dend(hcc2_onlyhw, rect = TRUE, show_labels = FALSE)
fviz_dend(hcc3_onlyhw, rect = TRUE, show_labels = FALSE)

#============ E. Hierarchical clustering only SP ==================

#------------use eclust------------
pokemon_sp <- select(pokemon_z, sp_attack, sp_defense)

dis_sp<-get_dist(pokemon_sp, method = "euclidean")

hcc2_sp <- eclust(pokemon_sp, "hclust", k=2, hc_method="ward.D2", graph = FALSE)
hcc3_sp <- eclust(pokemon_sp, "hclust", k=3, hc_method="ward.D2", graph = FALSE)
pokemon_stats$hcc2_sp <- hcc2_sp$cluster
pokemon_stats$hcc3_sp <- hcc3_sp$cluster
hcc2_sp_info <- cluster.stats(dis_sp, hcc2_sp$cluster)
hcc3_sp_info <- cluster.stats(dis_sp, hcc3_sp$cluster)

hcc2_sp_stats <- group_by(pokemon_stats, hcc2_sp) %>%
  descr(pokemon_stats[,c(6:7)], transpose = TRUE) 
hcc3_sp_stats <- group_by(pokemon_stats, hcc3_sp) %>%
  descr(pokemon_stats[,c(6:7)], transpose = TRUE)

fviz_silhouette(hcc2_sp)
fviz_silhouette(hcc3_sp)

fviz_dend(hcc2_sp, rect = TRUE, show_labels = FALSE)
fviz_dend(hcc3_sp, rect = TRUE, show_labels = FALSE)

#============ F. Hierarchical clustering HAD ==================

#------------use eclust------------
pokemon_had <- select(pokemon_z, hp, attack, defense)

dis_had<-get_dist(pokemon_had, method = "euclidean")

hcc2_had <- eclust(pokemon_had, "hclust", k=2, hc_method="ward.D2", graph = FALSE)
hcc3_had <- eclust(pokemon_had, "hclust", k=3, hc_method="ward.D2", graph = FALSE)
pokemon_stats$hcc2_had <- hcc2_had$cluster
pokemon_stats$hcc3_had <- hcc3_had$cluster
hcc2_had_info <- cluster.stats(dis_had, hcc2_had$cluster)
hcc3_had_info <- cluster.stats(dis_had, hcc3_had$cluster)

hcc2_had_stats <- group_by(pokemon_stats, hcc2_had) %>%
  descr(pokemon_stats[,c(3:5)], transpose = TRUE) 
hcc3_had_stats <- group_by(pokemon_stats, hcc3_had) %>%
  descr(pokemon_stats[,c(3:5)], transpose = TRUE)

fviz_silhouette(hcc2_had)
fviz_silhouette(hcc3_had)

fviz_dend(hcc2_had, rect = TRUE, show_labels = FALSE)
fviz_dend(hcc3_had, rect = TRUE, show_labels = FALSE)

hcc3hadplots <- list()  # new empty list
for(i in 3:5){
  col <- names(pokemon_stats)[i]
  ggp <- ggplot(pokemon_stats, aes_string(y = col, x = "hcc3_had", group = "hcc3_had")) +
    stat_boxplot(geom ='errorbar') + 
    geom_boxplot(fill = c("red","blue","green"), notch = TRUE) + 
    scale_x_discrete(name = "Cluster", limits = c("1", "2", "3"))
  hcc3hadplots[[i]] <- ggp  # add each plot into plot list
}

multiplot(plotlist = hcc3hadplots, cols = 4)
#=====================================================================================

pokemon_results <- mutate(pokemon_all[,c(1:18)], pokemon_stats[,c(9:32)])
write.xlsx(pokemon_results, "pokemon_results.xlsx")

#=========== Validation (compare methods) ==============
assess_all <- clValid(pokemon_z[,c(1:8)], nClust = 2:6, clMethods = c("hierarchical", "kmeans"), validation = "internal",maxitems = 10000 )
assess_hads <- clValid(pokemon_hads, nClust = 2:6, clMethods = c("hierarchical", "kmeans"), validation = "internal",maxitems = 10000 )
assess_nohw <- clValid(pokemon_nohw, nClust = 2:6, clMethods = c("hierarchical", "kmeans"), validation = "internal",maxitems = 10000 )
assess_onlyhw <- clValid(pokemon_onlyhw, nClust = 2:6, clMethods = c("hierarchical", "kmeans"), validation = "internal",maxitems = 10000 )
assess_sp <- clValid(pokemon_sp, nClust = 2:6, clMethods = c("hierarchical", "kmeans"), validation = "internal",maxitems = 10000 )
assess_had <- clValid(pokemon_had, nClust = 2:6, clMethods = c("hierarchical", "kmeans"), validation = "internal",maxitems = 10000 )

summary(assess_all)

