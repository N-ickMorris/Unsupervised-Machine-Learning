# -----------------------------------------------------------------------------------
# ---- Input Information ------------------------------------------------------------
# -----------------------------------------------------------------------------------

# set the path of where the input files are
mywd = "C:/ ... /Unsupervised/Variable-Selection"

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
require(randomUniformForest)

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

}

# -----------------------------------------------------------------------------------
# ---- Set Up Data ------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# set the work directory
setwd(mywd)

# read in the data
train = data.table(read.csv("train.csv"))
info = data.table(read.csv("input-var-imp-unsupervised.csv", stringsAsFactors = FALSE))

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
# ---- Variable Importance ----------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# ---- Correlation ------------------------------------------------------------------

{

# lets remove any highly correlated variables
# compute the correlation matrix
train.cor = cor(train)

# replace any NA's with 0's
train.cor[is.na(train.cor)] = 0

# find out which variables are highly correlated (by magnitude) and remove them
find.vars = findCorrelation(train.cor, cutoff = 0.9, names = TRUE, exact = TRUE)

# remove columns from train according to find.vars
if(length(find.vars) > 0) train = train[, !find.vars, with = FALSE]

}

# ---- Importance -------------------------------------------------------------------

{

# choose how many threads to use
uthreads = max(1, floor(cores * detectCores()))

# set the ntree parameter to a standard value
ntree = 500

# set the mtry parameter to a standard value
mtry = floor(sqrt(ncol(train)))

# set the nodesize parameter to a standard value for classification
nodesize = 1

# build an unsupervised random forest model on all of the data to interpret the importance of each variable
mod = unsupervised.randomUniformForest(object = train,
                                       ntree = ntree,
                                       mtry = mtry,
                                       nodesize = nodesize,
                                       baseModel = "importanceThenDistance",
                                       seed = 42,
                                       uthreads = uthreads)

# extract the variable importance
var.imp = data.table(mod$rUFObject$forest$variableImportance)

# keep the two columns of interest, variables and percent
var.imp = var.imp[,.(variable = variables, value = percent / 100)]

# order var.imp by value
var.imp = var.imp[order(value, decreasing = TRUE)]

# make variable a factor for plotting purposes
var.imp[, variable := factor(variable, levels = unique(variable))]

# plot a barplot of variable importance
var.imp.plot = ggplot(var.imp, aes(x = variable, y = value, fill = value, color = value)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Variable Importance\nRandom Forests") +
  labs(x = "Variable", y = "Scaled Importance") +
  scale_y_continuous(labels = percent) +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_color_gradient(low = "yellow", high = "red") +
  theme_dark(25) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid.major.x = element_blank())

# set up a graphics window
# windows()

# set up a pdf file to capture the next graphic
pdf("var-imp-plot.pdf", width = 16, height = 10, paper = "special") 

# call the plot
print(var.imp.plot)

# close off the connection
dev.off()

# add a rank column to var.imp
var.imp[, rank := 1:nrow(var.imp)]

# export var.imp
write.csv(var.imp, "var-imp-rank.csv", row.names = FALSE)

}

# ---- Cluster Strength -------------------------------------------------------------

{

# compute the distance matrix used by hclust
dmat = dist(train)

# choice of hclust methods
methods = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")

# build cluster methods
hc = lapply(methods, function(j) hclust(dmat, method = j))

# compute correlations
cors = rbindlist(lapply(1:length(methods), function(j) 
  data.table(method = methods[j], value = cor(dmat, cophenetic(hc[[j]])))))

# order by value
cors = cors[order(value, decreasing = TRUE)]

# make method a factor for plotting purposes
cors[, method := factor(method, levels = unique(method))]

# plot cophenetic correlations
cophenetic.plot = ggplot(cors, aes(x = method, y = value, fill = value)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Method", y = "Value") +
  ggtitle("Cophenetic Correlations\nHierarchical Clustering") + 
  scale_fill_gradient(low = "yellow", high = "red") +
  theme_dark(25) +
  theme(legend.position = "none", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# set up a pdf file to capture the next graphic
pdf("cophenetic-plot.pdf", width = 16, height = 10, paper = "special") 

# call the plot
print(cophenetic.plot)

# close off the connection
dev.off()

# lets go with the best method
method = as.character(cors$method[1])

# lets compute the prediction strength of having a certain number of clusters
predict.strength = prediction.strength(train,
                                       Gmin = k.min, 
                                       Gmax = k.max, 
                                       M = 100,
                                       clustermethod = hclustCBI,
                                       method = method)

# export cluster prediction strength
file.create("cluster-prediction-strength.txt")
sink("cluster-prediction-strength.txt")
print(predict.strength)
sink()

# free memory
gc()

}

}



