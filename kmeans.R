# -----------------------------------------------------------------------------------
# ---- Input Information ------------------------------------------------------------
# -----------------------------------------------------------------------------------

# set the path of where the input files are
mywd = "C:/Users/Nick Morris/Downloads/Data-Science/Model-Scripts/Unsupervised/K-Means"

# create a name for a .txt file to log progress information while parallel processing
myfile = "log.txt"

# -----------------------------------------------------------------------------------
# ---- Packages ---------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# data handling
require(data.table)

# plotting
require(VIM)
require(ggplot2)
require(gridExtra)
require(GGally)
require(scales)
require(scatterplot3d)
require(gridGraphics)

# modeling
require(fpc)
require(factoextra)
require(caret)
require(cluster)

# parallel computing
require(foreach)
require(parallel)
require(doSNOW)

}

# -----------------------------------------------------------------------------------
# ---- Functions --------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# these are functions i like to use

# ---- prints the data types of each column in a data frame -------------------------

types = function(dat)
{
  # make dat into a data.frame
  dat = data.frame(dat)
  
  # get the column names
  column = sapply(1:ncol(dat), function(i) colnames(dat)[i])
  
  # get the class of the columns
  data.type = sapply(1:ncol(dat), function(i) class(dat[,i]))
  
  # compute the number of levels for each column
  levels = sapply(1:ncol(dat), function(i) ifelse(data.type[i] == "factor", length(levels(droplevels(dat[,i]))), 0))
  
  return(data.frame(column, data.type, levels))
}

# ---- emulates the default ggplot2 color scheme ------------------------------------

ggcolor = function(n, alpha = 1)
{
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# ---- plots various cluster plots --------------------------------------------------

plot.clusters = function(dat, cluster.column.name = NULL, distance.matrix = NULL, DC.title = "Discriminant Coordinate Cluster Plot", pairs.title = "Cluster Pairs Plot", silhouette.title = "Silhouette Width", font.size = 20, pairs.plot.font.size = 15, rotate = 0)
{
  # load packages we need
  require(data.table)
  require(ggplot2)
  require(GGally)
  require(scatterplot3d)
  require(gridGraphics)
  require(grid)
  require(scales)
  require(cluster)
  require(fpc)
  
  # this function emulates the default ggplot2 color scheme
  ggcolor = function(n)
  {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
  # error check
  if(is.null(cluster.column.name))
  {
    print("you must specify a value for the parameter: cluster.column.name")
    
  } else
  {
    # ---- computing discriminant coordiantes -------------------------------
    
    # make dat into a data table
    dat = data.table(dat)
    
    # extract the cluster column from dat
    clusters = as.numeric(unname(unlist(dat[, cluster.column.name, with = FALSE])))
    
    # remove the cluster column from dat
    dat = dat[, !cluster.column.name, with = FALSE]
    
    # compute the discriminant coordinates, and extract the first 3
    dat.dc = data.table(discrproj(x = dat, 
                                  clvecd = clusters,
                                  method = "dc")$proj[,1:3])
    
    # rename the columns appropriately
    setnames(dat.dc, paste0("DC", 1:3))
    
    # give dat.dc the cluster column, and make it into a factor for plotting purposes
    dat.dc[, Cluster := factor(clusters, levels = sort(unique(clusters)))]
    
    # ---- plotting 2D discriminant coordiantes -----------------------------
    
    # create a 2D cluster plot across the first 2 discriminant coordinates
    plot.2D = ggplot(dat.dc, aes(x = DC1, y = DC2, fill = Cluster)) +
      stat_density_2d(geom = "polygon", color = NA, alpha = 1/3) +
      theme_bw(font.size) +
      ggtitle(DC.title) +
      theme(legend.position = "top", 
            legend.key.size = unit(.25, "in"), 
            plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))
    
    # store this plot in a list
    output = list("plot.2D" = plot.2D)
    
    # ---- plotting 3D discriminant coordiantes -----------------------------
    
    # create a table indicating which cluster should get which ggplot color
    color.dat.dc = data.table(Cluster = levels(dat.dc$Cluster),
                              Color = ggcolor(length(levels(dat.dc$Cluster))))
    
    # set Cluster as the key column in color.dat.dc and dat.dc
    setkey(dat.dc, Cluster)
    setkey(color.dat.dc, Cluster)
    
    # join color.dat.dc onto dat.dc
    dat.dc = color.dat.dc[dat.dc]
    
    # convert Cluster back into a factor data type
    dat.dc[, Cluster := factor(Cluster, levels = sort(unique(Cluster)))]
    
    # here is my default font size for base R plotting
    font.default = 20
    
    # compute the desired adjustment according to the specified value of font.size
    font.adjust = font.size / font.default
    
    # adjust the font of the title, axis titles, axis labels, and legend
    font.title = 2 * font.adjust
    font.axis.title = 1.125 * font.adjust
    font.axis.label = 1.125 * font.adjust
    font.legend = 1.75 * font.adjust
    
    # here are my 4 default angles for viewing a 3D scatterplot
    angles = c(45, 135, 225, 315)
    
    # apply the specified rotation
    angles = angles + rotate
    
    # set up 4 plot ID numbers so each plot angle has a position in the plot window
    plot.id = 2:5
    
    # set up a legend ID number so the legend has a postion across the top of the plot window
    legend.id = c(1, 1)
    
    # set up a matrix that defines the plot layout
    plot.layout = matrix(c(legend.id, plot.id), nrow = 3, ncol = 2, byrow = TRUE)
    
    # create a new plot window
    windows()
    plot.new()
    
    # define plot margins
    par(mar = c(0, 0, 3.5, 0))
    
    # apply the layout to the plot window
    layout(mat = plot.layout, heights = c(1, 2, 2))
    
    # produce a dummy plot as a place holder for the legend and title
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    
    # produce the title
    title(main = DC.title, cex.main = font.title)
    
    # produce the legend
    legend("top", inset = 0, bty = "n", cex = font.legend, horiz = TRUE,
           title = "Clusters", legend = levels(dat.dc$Cluster), fill = ggcolor(length(levels(dat.dc$Cluster))))
    
    # create 3D cluster plots across the first 3 discriminant coordinates
    for(angle in angles)
    {
      # build the scatterplot
      scatterplot3d(x = dat.dc$DC1, y = dat.dc$DC2, z = dat.dc$DC3, color = alpha(dat.dc$Color, 1/3), 
                    xlab = "DC1", ylab = "DC2", zlab = "DC3", mar = c(3, 3, 0, 3), cex = 1.5,
                    pch = 16, cex.lab = font.axis.title, cex.axis = font.axis.label, angle = angle)
    }
    
    # save this plot as a grid object
    grid.echo()
    plot.3D = grid.grab()
    
    # add this plot to our output list
    output$plot.3D = plot.3D
    
    # close the graphics window
    graphics.off()
    
    # ---- plotting clusters across variable pairs ---------------------------
    
    # give dat the cluster column, and make it into a factor for plotting purposes
    dat[, Cluster := factor(clusters, levels = sort(unique(clusters)))]
    
    # plot the clusters across all variable pairs
    plot.pairs = ggpairs(dat,
                         mapping = aes(color = Cluster, fill = Cluster),
                         columns = which(names(dat) != "Cluster"),
                         lower = list(continuous = wrap(ggally_points, size = 1.5, alpha = 1/3)), 
                         upper = list(continuous = wrap(ggally_cor, size = 5)),
                         diag = list(continuous = wrap(ggally_densityDiag, alpha = 1/3)),
                         title = pairs.title,
                         legend = grab_legend(plot.2D)) + 
      theme_classic(base_size = pairs.plot.font.size) +
      theme(legend.position = "top")
    
    # remove the cluster column from dat
    dat[, Cluster := NULL]
    
    # add this plot to our output list
    output$plot.pairs = plot.pairs
    
    # ---- plotting silhouette widths ----------------------------------------
    
    # if the user gave a distance matrix then lets compute silhouette widths
    if(!is.null(distance.matrix))
    {
      # compute the silhouette widths
      dat.sil = silhouette(x = clusters,
                           dist = distance.matrix)
      
      # compute the summary of dat.sil
      dat.sil.sum = summary(dat.sil)
      
      # extract the avg widths from dat.sil.sum
      dat.sil.avg = data.table(Cluster = as.numeric(names(dat.sil.sum$clus.avg.widths)),
                               Average_Width = round(unname(dat.sil.sum$clus.avg.widths), 2))
      
      # order dat.sil.avg by Cluster
      dat.sil.avg = dat.sil.avg[order(Cluster)]
      
      # extract the cluster sizes from dat.sil.sum
      dat.sil.size = data.table(Cluster = as.numeric(names(dat.sil.sum$clus.sizes)),
                                Size = as.numeric(unname(dat.sil.sum$clus.sizes)))
      
      # order dat.sil.size by Cluster
      dat.sil.size = dat.sil.size[order(Cluster)]
      
      # combine dat.sil.avg and dat.sil.size into a table that will go on our plot
      dat.sil.tab = cbind(dat.sil.avg, dat.sil.size[,!"Cluster"])
      
      # convert dat.sil into a data table
      dat.sil = data.table(dat.sil[1:nrow(dat.sil), 1:ncol(dat.sil)])
      
      # sort dat.sil by cluster and sil_width
      dat.sil = dat.sil[order(cluster, -sil_width)]
      
      # give dat.sil an ID column for plotting purposes
      dat.sil[, ID := 1:nrow(dat.sil)]
      
      # convert cluster to a factor for plotting purposes
      dat.sil[, cluster := factor(cluster, levels = sort(unique(cluster)))]
      
      # aggregate sil_width by cluster in dat.sil to determine where to place dat.sil.tab in the plot 
      dat.agg = dat.sil[, .(sil_width.min = min(sil_width),
                            sil_width.max = max(sil_width)),
                        by = cluster]
      
      # build the four corners of the dat.sil.tab to place it in the plot
      # find the cluster with the smallest peak and set the peak's sil_width as the ymin
      ymin = as.numeric(min(dat.agg$sil_width.max))
      
      # find the sil_width of the max peak and set it as the ymax
      ymax = as.numeric(max(dat.sil$sil_width))
      
      # extract the cluster with the smallest peak from dat.agg
      small.peak = dat.agg[which.min(sil_width.max), cluster]
      
      # find the first ID number in dat.sil for the cluster with the smallest peak, and set that as xmin
      xmin = min(dat.sil[cluster == small.peak, ID])
      
      # find the last ID number in dat.sil for the cluster with the smallest peak, and set that as xmax
      xmax = max(dat.sil[cluster == small.peak, ID])
      
      # plot the silhouette width and add the dat.sil.tab to it
      plot.sil.width = ggplot(dat.sil, aes(x = ID, y = sil_width, fill = cluster, color = cluster)) + 
        geom_bar(stat = "identity", position = "dodge") +
        annotation_custom(tableGrob(as.matrix(dat.sil.tab), rows = NULL, 
                                    theme = ttheme_default(base_size = font.size,
                                                           colhead = list(fg_params = list(col = "black"), bg_params = list(fill = "lightgray", col = "black")),
                                                           core = list(fg_params = list(hjust = 0.5), bg_params = list(fill = c("white"), col = "black")))),
                          xmin = xmin, 
                          xmax = xmax, 
                          ymin = ymin, 
                          ymax = ymax) +
        ggtitle(silhouette.title) +
        labs(x = "Observation", y = "Silhouette Width", fill = "Cluster", color = "Cluster") +
        theme_bw(base_size = font.size) +
        theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) + 
        guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1),
               color = guide_legend(nrow = 1))
      
      # add this plot to our output list
      output$plot.sil.width = plot.sil.width
    }
    
    # add dat.dc to output
    output$DC = dat.dc
    
    # helpful message
    print("use grid.draw() to see the 3D cluster plot, for example: grid.draw(my.plot.clusters$plot.3D)")
    
    return(output)
  }
}

}

# -----------------------------------------------------------------------------------
# ---- Set Up Data ------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# set the work directory
setwd(mywd)

# create the file
file.create(myfile)

# read in the data
train = data.table(read.csv("train.csv"))
info = data.table(read.csv("input-unsupervised.csv", stringsAsFactors = FALSE))

# get the minimum number of clusters to evaluate
k.min = info[Metric == "k.min", Value]

# get the maximum number of clusters to evaluate
k.max = info[Metric == "k.max", Value]

# get the proportion of cores that will be used for model building
cores = as.numeric(info[Metric == "cores", Value])

# remove any NA's in train and test
train = na.omit(train)

# update train to have just numeric predictor variables
train = data.table(model.matrix(~., data = train)[,-1])

# free memory
gc()

}

# -----------------------------------------------------------------------------------
# ---- Model Building ---------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# lets set up an experiment to try different clusters
doe = data.table(expand.grid(clusters = k.min:k.max))

# choose the number of workers/threads and tasks for parallel processing
# specifying a value > 1 for workers means that multiple models in doe will be built in parallel
workers = max(1, floor(cores * detectCores()))
tasks = nrow(doe)

# set up a cluster if workers > 1, otherwise don't set up a cluster
if(workers > 1)
{
  # setup parallel processing
  cl = makeCluster(workers, type = "SOCK", outfile = "")
  registerDoSNOW(cl)
  
  # define %dopar%
  `%fun%` = `%dopar%`
  
  # write out start time to log file
  sink(myfile, append = TRUE)
  cat("\n------------------------------------------------\n")
  cat("kmeans clustering\n")
  cat(paste(workers, "workers started at", Sys.time(), "\n"))
  sink()
  
} else
{
  # define %do%
  `%fun%` = `%do%`
  
  # write out start time to log file
  sink(myfile, append = TRUE)
  cat("\n------------------------------------------------\n")
  cat("kmeans clustering\n")
  cat(paste("task 1 started at", Sys.time(), "\n"))
  sink()
}

# build each model in doe
mods = foreach(i = 1:tasks) %fun%
{
  # build the model
  output = kmeans(x = train, 
                  centers = doe$clusters[i], 
                  nstart = 32, 
                  iter.max = 64)
  
  # free memory
  gc()
  
  # export progress information
  sink(myfile, append = TRUE)
  cat(paste("task", i, "of", tasks, "finished at", Sys.time(), "\n"))
  sink()
  
  return(output)
}

# write out end time to log file
sink(myfile, append = TRUE)
cat(paste(tasks, "tasks finished at", Sys.time(), "\n"))
sink()

# end the cluster if it was set up
if(workers > 1)
{
  stopCluster(cl)
}

# extract the cluster solutions from each model
dat = foreach(i = 1:tasks, .combine = "cbind") %do%
{
  return(as.numeric(mods[[i]]$cluster))
}

# give each column better names
colnames(dat) = paste0("Clusters-", k.min:k.max)

# add each of the cluster solutions to our data set
dat = cbind(dat, train)

# convert the cluster solution columns into long format with 2 columns: Model and Cluster
dat = melt(dat, 
           measure.vars = names(dat)[1:nrow(doe)], 
           variable.name = "Model", 
           value.name = "Cluster")

# compute the distance matrix used by kmeans
dmat = dist(train)

# lets create cluster plots for each model
plots = lapply(levels(dat$Model), function(i)
{
  # extract model i from dat
  DT = data.table(dat[Model == i])
  
  # remove Model from DT
  DT[, Model := NULL]
  
  # build cluster plots
  output = plot.clusters(dat = DT, 
                         cluster.column.name = "Cluster",
                         distance.matrix = dmat,
                         DC.title = paste("Discriminant Coordinate Cluster Plot\nK-Means", i), 
                         pairs.title = paste("Cluster Pairs Plot\nK-Means", i), 
                         silhouette.title = paste("Silhouette Width\nK-Means", i))
  
  return(output)
})

# export cluster data and plots
for(i in 1:tasks)
{
  # set up a pdf file to capture the next graphic
  pdf(paste0("kmeans-cluster-plot-", levels(dat$Model)[i], ".pdf"), width = 16, height = 10, paper = "special") 
  
  # call the plot
  print(plots[[i]]$plot.2D)
  
  # close off the connection
  dev.off()
  
  # set up a pdf file to capture the next graphic
  pdf(paste0("kmeans-silhouette-plot-", levels(dat$Model)[i], ".pdf"), width = 16, height = 10, paper = "special") 
  
  # call the plot
  print(plots[[i]]$plot.sil.width)
  
  # close off the connection
  dev.off()
  
  if(i == 1)
  {
    # get the discriminant coordinates
    dat.dc = plots[[i]]$DC[,.(DC1, DC2, DC3)]
    
  } else
  {
    # get the discriminant coordinates
    dat.dc = rbind(dat.dc,
                   plots[[i]]$DC[,.(DC1, DC2, DC3)])
  }
}

# export dat & dat.dc
write.csv(cbind(dat, dat.dc), "kmeans-data.csv", row.names = FALSE)

# free memory
gc()

}




















