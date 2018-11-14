# ----------------------------------------------------------------------------------
# ---- Packages --------------------------------------------------------------------
# ----------------------------------------------------------------------------------

{

require(factoextra)
require(cluster)
require(dbscan)
require(mclust)
require(data.table)
require(gridExtra)

}

# ----------------------------------------------------------------------------------
# ---- Functions -------------------------------------------------------------------
# ----------------------------------------------------------------------------------

{

# ---- prints the data types of each column in a data frame -------------------------

types = function(dat)
{
  dat = data.frame(dat)
  
  column = sapply(1:ncol(dat), function(i) colnames(dat)[i])
  data.type = sapply(1:ncol(dat), function(i) class(dat[,i]))
  levels = sapply(1:ncol(dat), function(i) length(levels(dat[,i])))
  
  return(data.frame(cbind(column, data.type, levels)))	
}

}

# ----------------------------------------------------------------------------------
# ---- Load Data -------------------------------------------------------------------
# ----------------------------------------------------------------------------------

{

# load the data into the work space
data("USArrests")

# get the data and remove any missing values (ie. NA's)
dat = na.omit(data.table(USArrests))

# check out the data
dat
types(dat)

# scale all variables to be on the same scale, so they can be compared fairly
dat = data.table(scale(dat))

}

# ----------------------------------------------------------------------------------
# ---- Distance Matrices -----------------------------------------------------------
# ----------------------------------------------------------------------------------

{

# choose the variables we want to consider
vars = names(dat)

# lets setup different combination of variables to compare distance matrices
combs = lapply(2:length(vars), function(i) apply(combn(vars, i), 2, list))
combs = unlist(unlist(combs, recursive = FALSE), recursive = FALSE)

# compute euclidean distance matrix of variables
dat.dis = lapply(1:length(combs), function(i) 
  get_dist(dat[, combs[[i]], with = FALSE], method = "euclidean"))

# plot the distance matrix
# blue indicates larger distance (ie. more dissimilar) (ie. more cold)
# orange indicates smaller distance (ie. more similar) (ie. more hot)
dis.plots = lapply(1:length(combs), function(i) 
  fviz_dist(dat.dis[[i]], gradient = list(low = "tomato", mid = "white", high = "cornflowerblue")) + 
  ggtitle(paste(combs[[i]], collapse = " + ")) + 
  theme(plot.title = element_text(hjust = 0.5)))

# all variable plot
dis.plots[[11]]

# three variable plots
# murder, assault, and urbanpop look like the best 3 variable model because its distance matrix looks like it had the clearest groupings
do.call(grid.arrange, c(dis.plots[7:10], nrow = 2))

# two variable plots
# murder and rape look like the best 2 variable model because its distance matrix looks like it had the most similar grouping magnitudes
do.call(grid.arrange, c(dis.plots[1:6], nrow = 3))

# lets move forward clustering on all variables
# we can always apply the same procedure for our 3 variable and 2 variable canidates if we wanted to

# remove objects we no longer need
rm(vars, combs, dat.dis, dis.plots)

}

# ----------------------------------------------------------------------------------
# ---- K-Means Clustering ----------------------------------------------------------
# ----------------------------------------------------------------------------------

# Step 0. randomly initialize K points called the cluster centroids
# Step 1. go through each of the data points and assign it to the closest cluster
# Step 2. move the centroids of each cluster to the average of the data points assigned to the cluster
# Step 3. go back to Step 1, unless there has been minimal change in the position of the K clusters, in which case stop

{

# check out how the total within-cluster sum of squares, average silhouette width, and cluster gap changes across different cluster sizes
stat.plots = lapply(c("wss", "silhouette", "gap_stat"), function(i) 
  fviz_nbclust(dat, kmeans, nstart = 50, iter.max = 50, k.max = 10, method = i) + 
  geom_point(size = 2, color = "steelblue") + 
  geom_line(size = 1.5, color = "steelblue") + 
  ggtitle("K-Means Optimal Number of Clusters") +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5)))

# plot the wss, silhouette, and gap plots
do.call(grid.arrange, c(stat.plots, nrow = 2))

# lets look at cluster sizes from 2 to 4
K = 2:4
mods = lapply(K, function(i) kmeans(dat, i, nstart = 50, iter.max = 50))

# build the cluster options
cluster.plots = lapply(1:length(mods), function(i) 
  fviz_cluster(mods[[i]], data = dat, stand = FALSE, geom = "point", pointsize = 2) + 
  ggtitle(paste0("K-Means ", K[i], " Cluster Plot")) +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top"))

# plot the cluster options
do.call(grid.arrange, c(cluster.plots, nrow = 2))

# 2 clusters have better seperatation
kmeans.mod = mods[[1]]

# remove objects we no longer need
rm(stat.plots, K, mods, cluster.plots)

}

# ----------------------------------------------------------------------------------
# ---- CLARA Clustering ------------------------------------------------------------
# ----------------------------------------------------------------------------------

# first lets understand PAM:
# PAM (Partitioning Around Medoids):
  # randomly select k of the n data points as the medoids
  # Assign each data point to the closest medoid
  # While the cost of the configuration decreases:
    # For each medoid m, for each non-medoid data point o:
      # Swap m and o, recompute the cost (sum of distances of points to their medoid)
      # If the total cost of the configuration increased in the previous step, undo the swap

# apply PAM multiple times to random samples of your data set
# then assign the entire data set to the clusters found by PAM, and evaluate the quality (sum of distances of points to their medoid)
# Of all these samples, you keep the best

{

# check out how the total within-cluster sum of squares, average silhouette width, and cluster gap changes across different cluster sizes
stat.plots = lapply(c("wss", "silhouette", "gap_stat"), function(i) 
  fviz_nbclust(dat, clara, samples = 50, k.max = 10, method = i) + 
  geom_point(size = 2, color = "steelblue") + 
  geom_line(size = 1.5, color = "steelblue") + 
  ggtitle("CLARA Optimal Number of Clusters") +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5)))

# plot the wss, silhouette, and gap plots
do.call(grid.arrange, c(stat.plots, nrow = 2))

# lets look at cluster sizes from 2 to 5
K = 2:5
mods = lapply(K, function(i) clara(dat, i, samples = 50))

# build the cluster options
cluster.plots = lapply(1:length(mods), function(i) 
  fviz_cluster(mods[[i]], data = dat, stand = FALSE, geom = "point", pointsize = 2) + 
  ggtitle(paste0("CLARA ", K[i], " Cluster Plot")) +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top"))

# plot the cluster options
do.call(grid.arrange, c(cluster.plots, nrow = 2))

# 3 clusters have better seperatation
clara.mod = mods[[2]]

# remove objects we no longer need
rm(stat.plots, K, mods, cluster.plots)

}

# ----------------------------------------------------------------------------------
# ---- Complete Hierarchical K-Means Clustering ------------------------------------
# ----------------------------------------------------------------------------------

# Hierarchical Clustering:
  # initially, each data point gets it own cluster
  # to reduce the number of clusters, any two clusters are merged if they are a pair with the smallest distance measure
  # initially a cluster is one data point, so a cluster has the the data point's distance measures (ie. the column in the distance matrix corresponding to that data point)
  # but when clusters have multiple data points the cluster's distance measures now have to be computed (this is where the Heirarchal Clustering methods differ)
  # after each clusters distance measures are computed, two clusters are combined if they are the closest pair of clusters
  # this procedure of computing cluster distances and combining them is continued until there are K clusters

# Complete Hierarchical Clustering:
  # in Complete Hierarchical Clustering, the cluster's distance measures are computed by assigning the maximum distance measures (ie. the rowMax in the distance matrix across columns corresponding to it's data points)
  # So, Complete Hierarchical Clustering combines clusters that are close according to their furthest edges

# Hybrid Hierarchical K-Means Clustering
  # 1. Compute hierarchical clustering
  # 2. Cut the tree in k-clusters
  # 3. compute the center (i.e the mean) of each cluster
  # 4. Do k-means by using the set of cluster centers (defined in step 3) as the initial cluster centers

{

# check out how the total within-cluster sum of squares, average silhouette width, and cluster gap changes across different cluster sizes
stat.plots = lapply(c("wss", "silhouette", "gap_stat"), function(i) 
  fviz_nbclust(dat, hkmeans, hc.method = "complete", iter.max = 50, k.max = 10, method = i) + 
  geom_point(size = 2, color = "steelblue") + 
  geom_line(size = 1.5, color = "steelblue") + 
  ggtitle("Complete Hierarchical K-Means Optimal Number of Clusters") +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5)))

# plot the wss, silhouette, and gap plots
do.call(grid.arrange, c(stat.plots, nrow = 2))

# lets look at cluster sizes from 2 to 4
K = 2:4
mods = lapply(K, function(i) hkmeans(dat, i, hc.method = "complete", iter.max = 50))

# build the cluster options
cluster.plots = lapply(1:length(mods), function(i) 
  fviz_cluster(mods[[i]], data = dat, stand = FALSE, geom = "point", pointsize = 2) + 
  ggtitle(paste0("Complete Hierarchical K-Means ", K[i], " Cluster Plot")) +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top"))

# plot the cluster options
do.call(grid.arrange, c(cluster.plots, nrow = 2))

# 2 clusters have better seperatation
hkmeans.complete.mod = mods[[1]]

# plot the Dendrogram
fviz_dend(hkmeans.complete.mod, rect = TRUE, rect_border = "black") +
  ggtitle("\nComplete Hierarchical K-Means 2 Cluster Dendrogram") +		
  theme_void(15) + 
  theme(plot.title = element_text(hjust = 0.5))

# remove objects we no longer need
rm(stat.plots, K, mods, cluster.plots)

}

# ----------------------------------------------------------------------------------
# ---- Single Hierarchical K-Means Clustering --------------------------------------
# ----------------------------------------------------------------------------------

# in Single Hierarchical Clustering, the cluster's distances measures are computed by assigning the minimum distance measures (ie. the rowMin in the distance matrix across columns corresponding to it's data points)
# So, Single Hierarchical Clustering combines clusters that are close according to their nearest edges

{

# check out how the total within-cluster sum of squares, average silhouette width, and cluster gap changes across different cluster sizes
stat.plots = lapply(c("wss", "silhouette", "gap_stat"), function(i) 
  fviz_nbclust(dat, hkmeans, hc.method = "single", iter.max = 50, k.max = 10, method = i) + 
  geom_point(size = 2, color = "steelblue") + 
  geom_line(size = 1.5, color = "steelblue") + 
  ggtitle("Single Hierarchical K-Means Optimal Number of Clusters") +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5)))

# plot the wss, silhouette, and gap plots
do.call(grid.arrange, c(stat.plots, nrow = 2))

# lets look at cluster sizes from 2 to 7
K = 2:7
mods = lapply(K, function(i) hkmeans(dat, i, hc.method = "single", iter.max = 50))

# build the cluster options
cluster.plots = lapply(1:length(mods), function(i) 
  fviz_cluster(mods[[i]], data = dat, stand = FALSE, geom = "point", pointsize = 2) + 
  ggtitle(paste0("Single Hierarchical K-Means ", K[i], " Cluster Plot")) +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top"))

# plot the cluster options
do.call(grid.arrange, c(cluster.plots, nrow = 2))

# 2 clusters have better seperatation
hkmeans.single.mod = mods[[1]]

# plot the Dendrogram
fviz_dend(hkmeans.single.mod, rect = TRUE, rect_border = "black") +
  ggtitle("\nSingle Hierarchical K-Means 2 Cluster Dendrogram") +		
  theme_void(15) + 
  theme(plot.title = element_text(hjust = 0.5))

# remove objects we no longer need
rm(stat.plots, K, mods, cluster.plots)

}

# ----------------------------------------------------------------------------------
# ---- Average Hierarchical K-Means Clustering -------------------------------------
# ----------------------------------------------------------------------------------

# in Average Hierarchical Clustering, the cluster's distances measures are computed by assigning the average distance measures (ie. the rowAvg in the distance matrix across columns corresponding to it's data points)
# So, Average Hierarchical Clustering combines clusters that are close according to their mean center

{

# check out how the total within-cluster sum of squares, average silhouette width, and cluster gap changes across different cluster sizes
stat.plots = lapply(c("wss", "silhouette", "gap_stat"), function(i) 
  fviz_nbclust(dat, hkmeans, hc.method = "average", iter.max = 50, k.max = 10, method = i) + 
  geom_point(size = 2, color = "steelblue") + 
  geom_line(size = 1.5, color = "steelblue") + 
  ggtitle("Average Hierarchical K-Means Optimal Number of Clusters") +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5)))

# plot the wss, silhouette, and gap plots
do.call(grid.arrange, c(stat.plots, nrow = 2))

# lets look at cluster sizes from 2 to 6
K = 2:6
mods = lapply(K, function(i) hkmeans(dat, i, hc.method = "average", iter.max = 50))

# build the cluster options
cluster.plots = lapply(1:length(mods), function(i) 
  fviz_cluster(mods[[i]], data = dat, stand = FALSE, geom = "point", pointsize = 2) + 
  ggtitle(paste0("Average Hierarchical K-Means ", K[i], " Cluster Plot")) +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top"))

# plot the cluster options
do.call(grid.arrange, c(cluster.plots, nrow = 2))

# 2 clusters have better seperatation
hkmeans.average.mod = mods[[1]]

# plot the Dendrogram
fviz_dend(hkmeans.average.mod, rect = TRUE, rect_border = "black") +
  ggtitle("\nAverage Hierarchical K-Means 2 Cluster Dendrogram") +		
  theme_void(15) + 
  theme(plot.title = element_text(hjust = 0.5))

# remove objects we no longer need
rm(stat.plots, K, mods, cluster.plots)

}

# ----------------------------------------------------------------------------------
# ---- Median Hierarchical K-Means Clustering --------------------------------------
# ----------------------------------------------------------------------------------

# in Median Hierarchical Clustering, the cluster's distances measures are computed by assigning the median distance measures (ie. the rowMedian in the distance matrix across columns corresponding to it's data points)
# So, Median Hierarchical Clustering combines clusters that are close according to their median center

{

# check out how the total within-cluster sum of squares, average silhouette width, and cluster gap changes across different cluster sizes
stat.plots = lapply(c("wss", "silhouette", "gap_stat"), function(i) 
  fviz_nbclust(dat, hkmeans, hc.method = "median", iter.max = 50, k.max = 10, method = i) + 
  geom_point(size = 2, color = "steelblue") + 
  geom_line(size = 1.5, color = "steelblue") + 
  ggtitle("Median Hierarchical K-Means Optimal Number of Clusters") +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5)))

# plot the wss, silhouette, and gap plots
do.call(grid.arrange, c(stat.plots, nrow = 2))

# lets look at cluster sizes from 2 to 4
K = 2:4
mods = lapply(K, function(i) hkmeans(dat, i, hc.method = "median", iter.max = 50))

# build the cluster options
cluster.plots = lapply(1:length(mods), function(i) 
  fviz_cluster(mods[[i]], data = dat, stand = FALSE, geom = "point", pointsize = 2) + 
  ggtitle(paste0("Median Hierarchical K-Means ", K[i], " Cluster Plot")) +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top"))

# plot the cluster options
do.call(grid.arrange, c(cluster.plots, nrow = 2))

# 2 clusters have better seperatation
hkmeans.median.mod = mods[[1]]

# plot the Dendrogram
fviz_dend(hkmeans.median.mod, rect = TRUE, rect_border = "black") +
  ggtitle("\nMedian Hierarchical K-Means 2 Cluster Dendrogram") +		
  theme_void(15) + 
  theme(plot.title = element_text(hjust = 0.5))

# remove objects we no longer need
rm(stat.plots, K, mods, cluster.plots)

}

# ----------------------------------------------------------------------------------
# ---- Wards Hierarchical K-Means Clustering ---------------------------------------
# ----------------------------------------------------------------------------------

# Ward's Method is different than the previous heirarchal clustering methods in a few ways
# So, lets redefine the Heirarchal Clustering procedure for Ward's Method

# initially, each data point gets it own cluster
# to reduce the number of clusters, any two clusters are merged if they are a pair with the smallest distance variance (ie. the variance of the entries in the distance matrix corresponding to the row-column (cluster1-cluster2) cross sections)
# this procedure of computing cluster distance variances and combining them is continued until there are K clusters

{

# check out how the total within-cluster sum of squares, average silhouette width, and cluster gap changes across different cluster sizes
stat.plots = lapply(c("wss", "silhouette", "gap_stat"), function(i) 
  fviz_nbclust(dat, hkmeans, hc.method = "ward.D2", iter.max = 50, k.max = 10, method = i) + 
  geom_point(size = 2, color = "steelblue") + 
  geom_line(size = 1.5, color = "steelblue") + 
  ggtitle("Wards Hierarchical K-Means Optimal Number of Clusters") +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5)))

# plot the wss, silhouette, and gap plots
do.call(grid.arrange, c(stat.plots, nrow = 2))

# lets look at cluster sizes from 2 to 4
K = 2:4
mods = lapply(K, function(i) hkmeans(dat, i, hc.method = "ward.D2", iter.max = 50))

# build the cluster options
cluster.plots = lapply(1:length(mods), function(i) 
  fviz_cluster(mods[[i]], data = dat, stand = FALSE, geom = "point", pointsize = 2) + 
  ggtitle(paste0("Wards Hierarchical K-Means ", K[i], " Cluster Plot")) +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top"))

# plot the cluster options
do.call(grid.arrange, c(cluster.plots, nrow = 2))

# 2 clusters have better seperatation
hkmeans.ward.mod = mods[[1]]

# plot the Dendrogram
fviz_dend(hkmeans.ward.mod, rect = TRUE, rect_border = "black") +
ggtitle("\nWards Hierarchical K-Means 2 Cluster Dendrogram") +		
theme_void(15) + 
theme(plot.title = element_text(hjust = 0.5))

# remove objects we no longer need
rm(stat.plots, K, mods, cluster.plots)

}

# ----------------------------------------------------------------------------------
# ---- DBSCAN Clustering -----------------------------------------------------------
# ----------------------------------------------------------------------------------

# DBSCAN has two key parameters:
  # eps ~ The radius of our neighborhoods around a data point p
  # minPts ~ The minimum number of data points we want in a neighborhood to define a cluster

# DBSCAN is density based clustering, where density is defined as:
  # density = mass / volume
  # mass ~ the number of data points in the neighborhood
  # volume ~ the area of the neighborhood

# why density based clustering is useful:
  # if we calculate the local density approximation for all points in our dataset, 
  # we could cluster our points by saying that points that are nearby (contained in the same neighborhood)
  # and have similar local density approximations belong in the same cluster. 
  # If we decrease the value of eps, we can construct smaller neighborhoods (less volume) that would also contain fewer data points. 
  # Ideally, we want to identify highly dense neighborhoods where most of the data points are contained in these neighborhoods, 
  # but the volume of each of these neighborhoods is relatively small.

# DBSCAN categories the data points into three categories:
  # Core Points: A data point p is a core point if Nbhd(p,eps) [eps-neighborhood of p] contains at least minPts ; |Nbhd(p,eps)| >= minPts.
  # Border Points: A data point q is a border point if Nbhd(q,eps) contains less than minPts data points, but q is reachable from some core point p.
  # Outlier: A data point o is an outlier if it is neither a core point nor a border point. Essentially, this is the "other" class.

# The steps to the DBSCAN algorithm are:
  # Pick a point at random that has not been assigned to a cluster or been designated as an outlier. Compute its neighborhood to determine if it's a core point. If yes, start a cluster around this point. If no, label the point as an outlier.
  # Once we find a core point and thus a cluster, expand the cluster by adding all directly-reachable points to the cluster. Perform "neighborhood jumps" to find all density-reachable points and add them to the cluster. If an an outlier is added, change that point's status from outlier to border point.
  # Repeat these two steps until all points are either assigned to a cluster or designated as an outlier.

{

# experiment with different proportions of the data to give to minPts
minPts = round(ceiling(nrow(dat) * seq(1 / nrow(dat), 0.025, length.out = 9)), 0)

# plot the eps plots
par(mfrow = c(3, 3))
lapply(1:length(minPts), function(i) kNNdistplot(dat, k = minPts[i]))

# build eps values
eps = c(2.5, 2, 1.9, 1.8, 1.65, 1.6, 1.5, 1.5, 1.5)

# check that our eps values look good
lapply(1:length(minPts), function(i) list(kNNdistplot(dat, k = minPts[i]), abline(h = eps[i], col = "red")))

# build cluster sizes
d = lapply(1:length(minPts), function(i) dbscan(dat, minPts = minPts[i], eps = eps[i]))

# build the cluster options
cluster.plots = lapply(1:length(d), function(i)
  fviz_cluster(d[[i]], data = dat, stand = FALSE, geom = "point", pointsize = 2) + 
  ggtitle(paste0("DBSCAN Cluster Plot\nminPts ~ ", round(minPts[i], 1), ", eps ~ ", eps[i])) +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top"))

# plot the cluster options
do.call(grid.arrange, cluster.plots)

# this type of clustering is not applicable to this data

# remove objects we no longer need
rm(K, eps, minPts, d, cluster.plots)

}

# ----------------------------------------------------------------------------------
# ---- Model-Based Clustering ------------------------------------------------------
# ----------------------------------------------------------------------------------

# Model based clustering assumes that data points in a cluster come from a multivariate distribution
# So, each cluster (aka component) is described by a density function (a multivariate distribution) and has an associated weight pulling data points towards it
# Typically this multivariate distribution is set as a multivariate normal distribution, which is the case for the function Mclust()

# The final solution would then be a combination of multivariate normal distributions (ie. a mixture distribtion)
# The mixture distribtuion is defined by the following properties:
  # volume
  # shape
  # orientation

# Mclust has a variety of mixture models based on the 3 above properties: ?mclustModelNames
# Mclust will fit all applicable mixture distribtions from mclustModelNames to the data, across all cluster options, and rank them according to BIC
# The mclustModelNames and cluster combination that yeilds the largest value for BIC is the chosen Model for clustering the data

{

# lets evaluate up to K clusters
K = 1:10

# build the cluster options
mclust.mod = Mclust(dat, G = K)

# build a plot that classifies which points belong to which cluster for the best cluster option
p1 = fviz_mclust(mclust.mod, what = "classification", geom = "point", ellipse.type = "convex") +
  ggtitle(paste0(mclust.mod$modelName, " Mclust Classification Plot")) +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
  guides(color = guide_legend(nrow = 1))

# build a plot that shows which cluster is the best option, based on BIC
p2 = fviz_mclust(mclust.mod, what = "BIC") +
  scale_x_discrete(limits = K) +
  ggtitle("Mclust BIC Plot") +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
  guides(color = guide_legend(nrow = 1))

# plot the performance measures
grid.arrange(p1, p2, nrow = 2)

# remove objects we no longer need
rm(K, p1, p2)

}

# ----------------------------------------------------------------------------------
# ---- Fuzzy Clustering ------------------------------------------------------------
# ----------------------------------------------------------------------------------

# Fuzzy c-means (FCM) is a method of clustering which allows one piece of data to belong to two or more clusters.

# Step 0. randomly initialize K points called the cluster centroids
# Step 1. go through each of the data points and assign it to the closest cluster
# Step 2. move the centroids of each cluster to the weighted average of the data points assigned to the cluster
  # this weighted average takes on a value between 0 and 1 for every data point and cluster pair which indicates the degree of membership of the data point to the cluster
# Step 3. go back to Step 1, unless there has been minimal change in the position of the K clusters, in which case stop

{

# check out how the total within-cluster sum of squares, average silhouette width, and cluster gap changes across different cluster sizes
stat.plots = lapply(c("wss", "silhouette", "gap_stat"), function(i) 
  fviz_nbclust(dat, fanny, k.max = 10, method = i) + 
  geom_point(size = 2, color = "steelblue") + 
  geom_line(size = 1.5, color = "steelblue") + 
  ggtitle("Fuzzy Optimal Number of Clusters") +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5)))

# plot the wss, silhouette, and gap plots
do.call(grid.arrange, c(stat.plots, nrow = 2))

# lets look at cluster sizes from 2 to 6
K = 2:5
mods = lapply(K, function(i) fanny(dat, i))

# build the cluster options
cluster.plots = lapply(1:length(mods), function(i) 
  fviz_cluster(mods[[i]], data = dat, stand = FALSE, geom = "point", pointsize = 2) + 
  ggtitle(paste0("Fuzzy ", K[i], " Cluster Plot")) +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top"))

# plot the cluster options
do.call(grid.arrange, c(cluster.plots, nrow = 2))

# 3 clusters have better seperatation
fuzzy.mod = mods[[2]]

# remove objects we no longer need
rm(stat.plots, K, mods, cluster.plots)

}

# ----------------------------------------------------------------------------------
# ---- Final Model Selection -------------------------------------------------------
# ----------------------------------------------------------------------------------

# ---- Compute Average Silhouette Width --------------------------------------------

{

# ---- K-Means Clustering ----

# define cluster size
K = 2

# define name
method = "kmeans"

# define metric
metric = "Average Silhouette Width"

# compute metric
value = fviz_nbclust(dat, kmeans, nstart = 50, iter.max = 50, k.max = K, method = "silhouette")$data$y[K]

# store results
DT = data.table(clusters = K, method = method, metric = metric, value = value)

# remove objects we no longer need
rm(K, method, metric, value)

# ---- CLARA Clustering ----

# define cluster size
K = 3

# define name
method = "clara"

# define metric
metric = "Average Silhouette Width"

# compute metric
value = fviz_nbclust(dat, clara, k.max = K, samples = 50, method = "silhouette")$data$y[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

# ---- Complete Hierarchical K-Means Clustering ----

# define cluster size
K = 2

# define name
method = "hkmeans.complete"

# define metric
metric = "Average Silhouette Width"

# compute metric
value = fviz_nbclust(dat, hkmeans, k.max = K, iter.max = 50, hc.method = "complete", method = "silhouette")$data$y[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

# ---- Single Hierarchical K-Means Clustering ----

# define cluster size
K = 2

# define name
method = "hkmeans.single"

# define metric
metric = "Average Silhouette Width"

# compute metric
value = fviz_nbclust(dat, hkmeans, k.max = K, iter.max = 50, hc.method = "single", method = "silhouette")$data$y[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

# ---- Average Hierarchical K-Means Clustering ----

# define cluster size
K = 2

# define name
method = "hkmeans.average"

# define metric
metric = "Average Silhouette Width"

# compute metric
value = fviz_nbclust(dat, hkmeans, k.max = K, iter.max = 50, hc.method = "average", method = "silhouette")$data$y[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

# ---- Median Hierarchical K-Means Clustering ----

# define cluster size
K = 2

# define name
method = "hkmeans.median"

# define metric
metric = "Average Silhouette Width"

# compute metric
value = fviz_nbclust(dat, hkmeans, k.max = K, iter.max = 50, hc.method = "median", method = "silhouette")$data$y[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

# ---- Wards Hierarchical K-Means Clustering ----

# define cluster size
K = 2

# define name
method = "hkmeans.ward"

# define metric
metric = "Average Silhouette Width"

# compute metric
value = fviz_nbclust(dat, hkmeans, k.max = K, iter.max = 50, hc.method = "ward.D2", method = "silhouette")$data$y[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

# ---- Fuzzy Clustering ----

# define cluster size
K = 3

# define name
method = "fuzzy"

# define metric
metric = "Average Silhouette Width"

# compute metric
value = fviz_nbclust(dat, fanny, k.max = K, method = "silhouette")$data$y[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

}

# ---- Compute Within Cluster Sums of Squares --------------------------------------

{

# ---- K-Means Clustering ----

# define cluster size
K = 2

# define name
method = "kmeans"

# define metric
metric = "Within Cluster Sums of Squares"

# compute metric
value = fviz_nbclust(dat, kmeans, nstart = 50, iter.max = 50, k.max = K, method = "wss")$data$y[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

# ---- CLARA Clustering ----

# define cluster size
K = 3

# define name
method = "clara"

# define metric
metric = "Within Cluster Sums of Squares"

# compute metric
value = fviz_nbclust(dat, clara, k.max = K, samples = 50, method = "wss")$data$y[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

# ---- Complete Hierarchical K-Means Clustering ----

# define cluster size
K = 2

# define name
method = "hkmeans.complete"

# define metric
metric = "Within Cluster Sums of Squares"

# compute metric
value = fviz_nbclust(dat, hkmeans, k.max = K, iter.max = 50, hc.method = "complete", method = "wss")$data$y[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

# ---- Single Hierarchical K-Means Clustering ----

# define cluster size
K = 2

# define name
method = "hkmeans.single"

# define metric
metric = "Within Cluster Sums of Squares"

# compute metric
value = fviz_nbclust(dat, hkmeans, k.max = K, iter.max = 50, hc.method = "single", method = "wss")$data$y[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

# ---- Average Hierarchical K-Means Clustering ----

# define cluster size
K = 2

# define name
method = "hkmeans.average"

# define metric
metric = "Within Cluster Sums of Squares"

# compute metric
value = fviz_nbclust(dat, hkmeans, k.max = K, iter.max = 50, hc.method = "average", method = "wss")$data$y[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

# ---- Median Hierarchical K-Means Clustering ----

# define cluster size
K = 2

# define name
method = "hkmeans.median"

# define metric
metric = "Within Cluster Sums of Squares"

# compute metric
value = fviz_nbclust(dat, hkmeans, k.max = K, iter.max = 50, hc.method = "median", method = "wss")$data$y[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

# ---- Wards Hierarchical K-Means Clustering ----

# define cluster size
K = 2

# define name
method = "hkmeans.ward"

# define metric
metric = "Within Cluster Sums of Squares"

# compute metric
value = fviz_nbclust(dat, hkmeans, k.max = K, iter.max = 50, hc.method = "ward.D2", method = "wss")$data$y[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

# ---- Fuzzy Clustering ----

# define cluster size
K = 3

# define name
method = "fuzzy"

# define metric
metric = "Within Cluster Sums of Squares"

# compute metric
value = fviz_nbclust(dat, fanny, k.max = K, method = "wss")$data$y[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

}

# ---- Compute Average Cluster Gap -------------------------------------------------

{

# ---- K-Means Clustering ----

# define cluster size
K = 2

# define name
method = "kmeans"

# define metric
metric = "Average Cluster Gap"

# compute metric
value = fviz_nbclust(dat, kmeans, nstart = 50, iter.max = 50, k.max = K, method = "gap_stat")$data$gap[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

# ---- CLARA Clustering ----

# define cluster size
K = 3

# define name
method = "clara"

# define metric
metric = "Average Cluster Gap"

# compute metric
value = fviz_nbclust(dat, clara, k.max = K, samples = 50, method = "gap_stat")$data$gap[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

# ---- Complete Hierarchical K-Means Clustering ----

# define cluster size
K = 2

# define name
method = "hkmeans.complete"

# define metric
metric = "Average Cluster Gap"

# compute metric
value = fviz_nbclust(dat, hkmeans, k.max = K, iter.max = 50, hc.method = "complete", method = "gap_stat")$data$gap[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

# ---- Single Hierarchical K-Means Clustering ----

# define cluster size
K = 2

# define name
method = "hkmeans.single"

# define metric
metric = "Average Cluster Gap"

# compute metric
value = fviz_nbclust(dat, hkmeans, k.max = K, iter.max = 50, hc.method = "single", method = "gap_stat")$data$gap[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

# ---- Average Hierarchical K-Means Clustering ----

# define cluster size
K = 2

# define name
method = "hkmeans.average"

# define metric
metric = "Average Cluster Gap"

# compute metric
value = fviz_nbclust(dat, hkmeans, k.max = K, iter.max = 50, hc.method = "average", method = "gap_stat")$data$gap[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

# ---- Median Hierarchical K-Means Clustering ----

# define cluster size
K = 2

# define name
method = "hkmeans.median"

# define metric
metric = "Average Cluster Gap"

# compute metric
value = fviz_nbclust(dat, hkmeans, k.max = K, iter.max = 50, hc.method = "median", method = "gap_stat")$data$gap[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

# ---- Wards Hierarchical K-Means Clustering ----

# define cluster size
K = 2

# define name
method = "hkmeans.ward"

# define metric
metric = "Average Cluster Gap"

# compute metric
value = fviz_nbclust(dat, hkmeans, k.max = K, iter.max = 50, hc.method = "ward.D2", method = "gap_stat")$data$gap[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

# ---- Fuzzy Clustering ----

# define cluster size
K = 3

# define name
method = "fuzzy"

# define metric
metric = "Average Cluster Gap"

# compute metric
value = fviz_nbclust(dat, fanny, k.max = K, method = "gap_stat")$data$gap[K]

# store results
update.DT = data.table(clusters = K, method = method, metric = metric, value = value)
DT = rbind(DT, update.DT)

# remove objects we no longer need
rm(K, method, metric, value, update.DT)

}

# ---- Plot Stats ------------------------------------------------------------------

{

# check out DT
DT
types(DT)

# make clusters, method, and metric factors for plotting purposes
DT[, clusters := as.factor(clusters)]
DT[, method := factor(method, levels = unique(method))]
DT[, metric := factor(metric, levels = unique(metric))]

# plot a barplot of model metrics
ggplot(DT, aes(x = method, y = value, fill = clusters)) +
geom_bar(stat = "identity", position = "dodge", color = "white") +
facet_wrap(~metric, scales = "free_y") +
theme_bw(15) + 
theme(plot.title = element_text(hjust = 0.5), legend.position = "top", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# the K-Means and Hierarchical K-Means Models have the same performance
# lets move forward with CLARA, Fuzzy, and Ward's Hierarchical K-Means for further comparision

}

# ---- Compare Cluster Plots -------------------------------------------------------

{

# setup a list for the plots
cluster.plots = list()

# K-Means
#cluster.plots[[length(cluster.plots) + 1]] = 
#  fviz_cluster(kmeans.mod, data = dat, stand = FALSE, geom = "point", pointsize = 2) + 
#  ggtitle("K-Means Cluster Plot") +
#  theme_bw(15) + 
#  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")

# CLARA
cluster.plots[[length(cluster.plots) + 1]] = 
  fviz_cluster(clara.mod, stand = FALSE, geom = "point", pointsize = 2) + 
  ggtitle("CLARA Cluster Plot") +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")

# Complete Hierarchical K-Means
#cluster.plots[[length(cluster.plots) + 1]] = 
#  fviz_cluster(hkmeans.complete.mod, stand = FALSE, geom = "point", pointsize = 2) + 
#  ggtitle("Complete Hierarchical K-Means Cluster Plot") +
#  theme_bw(15) + 
#  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")

# Single Hierarchical K-Means
#cluster.plots[[length(cluster.plots) + 1]] = 
#  fviz_cluster(hkmeans.single.mod, stand = FALSE, geom = "point", pointsize = 2) + 
#  ggtitle("Single Hierarchical K-Means Cluster Plot") +
#  theme_bw(15) + 
#  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")

# Average Hierarchical K-Means
#cluster.plots[[length(cluster.plots) + 1]] = 
#  fviz_cluster(hkmeans.average.mod, stand = FALSE, geom = "point", pointsize = 2) + 
#  ggtitle("Average Hierarchical K-Means Cluster Plot") +
#  theme_bw(15) + 
#  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")

# Median Hierarchical K-Means
#cluster.plots[[length(cluster.plots) + 1]] = 
#  fviz_cluster(hkmeans.median.mod, stand = FALSE, geom = "point", pointsize = 2) + 
#  ggtitle("Median Hierarchical K-Means Cluster Plot") +
#  theme_bw(15) + 
#  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")

# Ward's Hierarchical K-Means
cluster.plots[[length(cluster.plots) + 1]] = 
  fviz_cluster(hkmeans.ward.mod, stand = FALSE, geom = "point", pointsize = 2) + 
  ggtitle("Ward's Hierarchical K-Means Cluster Plot") +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")

# DBSCAN
#cluster.plots[[length(cluster.plots) + 1]] = 
#  fviz_cluster(dbscan.mod, data = dat, stand = FALSE, geom = "point", pointsize = 2) + 
#  ggtitle("DBSCAN Cluster Plot") +
#  theme_bw(15) + 
#  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")

# Model-Based
cluster.plots[[length(cluster.plots) + 1]] = 
  fviz_cluster(mclust.mod, stand = FALSE, geom = "point", pointsize = 2) + 
  ggtitle("Model-Based Cluster Plot") +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")

# Fuzzy
cluster.plots[[length(cluster.plots) + 1]] = 
  fviz_cluster(fuzzy.mod, stand = FALSE, geom = "point", pointsize = 2) + 
  ggtitle("Fuzzy Cluster Plot") +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")

# plot the model options
do.call(grid.arrange, cluster.plots)

# interpretation:
  # there's one point at (-0.75, -0.25) that CLARA classifies differently than the other 3 models
  # The 2nd and 3rd clusters in fuzzy look too close to each other
  # the model-based cluster has a tighter look to it than wards hkmeans

# mclust.mod looks best
mod = mclust.mod

}



