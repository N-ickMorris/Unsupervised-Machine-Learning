# ---- CLARA Clustering -------------------------------------------------------------

{

# check out how the total within-cluster sum of squares, average silhouette width, and cluster gap changes across different cluster sizes
stat.plots = lapply(c("wss", "silhouette"), function(i) 
  fviz_nbclust(dat, clara, samples = 50, k.max = 10, method = i) + 
    geom_point(size = 2, color = "steelblue") + 
    geom_line(size = 1.5, color = "steelblue") + 
    ggtitle("CLARA Optimal Number of Clusters") +
    theme_bw(15) + 
    theme(plot.title = element_text(hjust = 0.5)))

# plot the wss, silhouette, and gap plots
do.call(grid.arrange, c(stat.plots, nrow = 1))

# lets look at cluster sizes from 4 to 7
k = 4:7
mods = lapply(k, function(i) clara(dat, i, samples = 50))

# build the cluster options
cluster.plots = lapply(1:length(mods), function(i) 
  fviz_cluster(mods[[i]], data = dat, stand = FALSE, geom = "point", pointsize = 2) + 
    ggtitle(paste0("CLARA ", k[i], " Cluster Plot")) +
    theme_bw(15) + 
    theme(plot.title = element_text(hjust = 0.5), legend.position = "top"))

# plot the cluster options
do.call(grid.arrange, c(cluster.plots, nrow = 2))

# look at how the data is spread across clusters
par(mfrow = c(2, 2))
lapply(1:length(k), function(i) barplot(table(mods[[i]]$clustering), main = paste(k[i], "Clusters")))

# 4 clusters look good
clara.mod = mods[[which(k == 4)]]

# remove objects we no longer need
rm(stat.plots, k, mods, cluster.plots)

}

# ---- Model-Based Clustering -------------------------------------------------------

{

# lets evaluate up to k clusters
k = 1:10

# build the cluster options
mclust.mod = Mclust(dat, G = k)

# build a plot that classifies which points belong to which cluster for the best cluster option
p1 = fviz_mclust(mclust.mod, what = "classification", geom = "point", ellipse.type = "convex") +
  ggtitle(paste0(mclust.mod$modelName, " Mclust Classification Plot")) +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
  guides(color = guide_legend(nrow = 1))

# build a plot that shows which cluster is the best option, based on BIC
p2 = fviz_mclust(mclust.mod, what = "BIC") +
  scale_x_discrete(limits = k) +
  ggtitle("Mclust BIC Plot") +
  theme_bw(15) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
  guides(color = guide_legend(nrow = 1))

# plot the performance measures
grid.arrange(p1, p2, nrow = 1)

# remove objects we no longer need
rm(k, p1, p2)

}

# ---- DBSCAN Clustering ------------------------------------------------------------

{

# experiment with different proportions of the data to give to minPts
minPts = round(seq(2, nrow(dat) * 0.025, length.out = 9), 0)
minPts = 3:10

# plot kNN distances to find a good value for eps
par(mfrow = c(3, 3))
lapply(1:length(minPts), function(i) kNNdistplot(dat, k = minPts[i]))

# choose eps values
eps = c(0.35, 0.4, 0.4, 0.4, 0.4, 0.5, 0.5, 0.5)
eps = rep(0.01, 8)

# check that our eps values look good
par(mfrow = c(3, 3))
lapply(1:length(minPts), function(i) list(kNNdistplot(dat, k = minPts[i]), abline(h = eps[i], col = "red")))

# build models
mods = lapply(1:length(minPts), function(i) dbscan(dat, minPts = minPts[i], eps = eps[i]))

# build the cluster options
cluster.plots = lapply(1:length(mods), function(i)
  fviz_cluster(mods[[i]], data = dat, stand = FALSE, geom = "point", pointsize = 2) + 
    ggtitle(paste0("DBSCAN ", max(mods[[i]]$cluster), " Cluster Plot\nminPts: ", round(minPts[i], 1), ", eps: ", eps[i])) +
    theme_bw(15) + 
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none"))

# plot the cluster options
do.call(grid.arrange, cluster.plots)

# look at how the data is spread across clusters
par(mfrow = c(3, 3))
lapply(1:length(mods), function(i) barplot(table(mods[[i]]$cluster), main = paste(max(mods[[i]]$cluster), "Clusters")))

# 11 clusters look good
dbscan.mod = mods[[which(sapply(1:length(mods), function(i) max(mods[[i]]$cluster)) == 11)[1]]]

# remove objects we no longer need
rm(eps, minPts, mods, cluster.plots)

}
