library(viridis, quietly = T)
#install.packages("hrbrthemes")
library(hrbrthemes, quietly = T)
library(tidyverse, quietly = T)
require(igraph, quietly = TRUE)

set.seed(1234)
load("C:/Users/Alessandra/Desktop/sds/HW3/hw3_data.RData")

# Data Exploration: ASD subjects -------------------------------------------


#rename the name of the subjects for convenience 
names(asd_sel) <- c(paste("subj", seq(1,9), sep = '_0'), 
                    paste("subj", seq(10,12), sep = '_'))

#build a dataframe of min and max values for all the region
# for all the 12 subjects

range_dataframe <- function(data_frame) {
  #given a df of m columns and n rows return a df with 2 columns and
  #m columns, containing the min and max values of each column
  return (data.frame(min = sapply(data_frame, min), max = sapply(data_frame, max)))
}

range_all_subjects <- lapply(asd_sel, range_dataframe)
range_all_subjects_df <- data.frame(range_all_subjects)

#view all the ranges 
range_all_subjects_df

#sample 20 regions to print out, for tidiness
indeces.samples <- sample(nrow(range_all_subjects_df), size = 20, replace = F)
range_all_subjects_df[indeces.samples, ]

#extract the ROI n°1 from all the subjects
get_roi <-function(dataframe, roi_id) {
  return(dataframe[, roi_id])
}

#dataframe that contains the only ROI NUMBER 1 of all the 
#subject. 12 columns x 145 obs
roi1_all_subjects <- data.frame(sapply(asd_sel, get_roi, 1))

#add the times column, a numeric column of values from 1 to 145
roi1_all_subjects <- cbind(times = seq(1:145), roi1_all_subjects)

#pivoting the dataframe to make a plot
roi1_all_subjects_long <- roi1_all_subjects %>% 
  pivot_longer(names(roi1_all_subjects)[-1], names_to = "subject_ID", values_to = "values")

#spaghetti plot of the ROI 2001 of 12 subjects
roi1_all_subjects_long %>%
  ggplot( aes(x=times, y=values, group=subject_ID, color=subject_ID)) +
  geom_line(size = 0.8) +
  scale_color_viridis(discrete = TRUE) +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("A spaghetti plot of Region of Interest '2001' of the 12 ASD") +
  theme_ipsum()

#plot of the ROI "2001" of 12 subjects with individual subject highlighted
tmp <- roi1_all_subjects_long %>%
  mutate(name2=subject_ID)

tmp %>%
  ggplot( aes(x=times, y=values)) +
  geom_line( data=tmp %>% dplyr::select(-subject_ID), aes(group=name2), color="grey", size=1, alpha=0.5) +
  geom_line( aes(color=subject_ID), color="#69b3a2", size= 1.1)+
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    panel.grid = element_blank()
  ) +
  labs(title = "Region of Interest 2001 of the 12 ASD subjects",
       subtitle = "the individual subject is highlighted",
       caption = "" ) +
  facet_wrap(~subject_ID)

#Concusions: subject1 is an outlyer



# Data Exploration: TD subjects -------------------------------------------

#Let's see that in the TD dataset the first and sencond subjects are outliers

#rename the name of the subjects for convenience 
names(td_sel) <- c(paste("subj", seq(1,9), sep = '_0'), 
                    paste("subj", seq(10,12), sep = '_'))

#min e max value in all the ROI of all the TD subjects
range_all_subjects_TD <- lapply(td_sel, range_dataframe)
range_all_subjects_TD_df <- data.frame(range_all_subjects_TD)

#sample 20 regions to print out, for tidiness
range_all_subjects_TD_df[sample(nrow(range_all_subjects_df),20, replace = F), ]


#function that merge all the columns (ROI's) in a sigle column
flat_df <- function(data_frame, name) {
  return (data.frame(name = c(t(data_frame)), stringsAsFactors=FALSE))
}

#flat all the TD subjects
m <- mapply(flat_df, td_sel, names(td_sel))
all_ROI_per_subjects <- data.frame(m)

#rename the subjects for convenience
names(all_ROI_per_subjects) <- names(td_sel)

#pivot just for plot with ggplot
subjects_ROI <- all_ROI_per_subjects %>%
  pivot_longer(names(all_ROI_per_subjects), names_to = "subjects", values_to = "ROI_values")

#Boxplot

subjects_ROI %>%
  ggplot( aes(x=subjects, y=ROI_values, fill=subjects)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=16)
  ) +
  labs(title = "What is the range of values of each subject's time series",
       caption = "Boxplot built considering all the ROIs of the subject \n 
       for a total of 16820 values per subject.") +
  xlab("")


# the threshold -----------------------------------------------------------

load("C:/Users/Alessandra/Desktop/sds/HW3/hw3_data.RData")

#rename 
names(asd_sel) <- c(paste("subj", seq(1,9), sep = '_0'), 
                    paste("subj", seq(10,12), sep = '_'))
names(td_sel) <- c(paste("subj", seq(1,9), sep = '_0'), 
                    paste("subj", seq(10,12), sep = '_'))

#not consider outliers 
asd <- asd_sel[-1]
td  <- td_sel[c(-1,-2)]

asd.cor <- lapply(asd, cor)
corr.obs <- c(sapply(asd.cor, function(x) x[lower.tri(x)], simplify = T))

td.cor <- lapply(td, cor)
corr.obs.td <- c(sapply(td.cor, function(x) x[lower.tri(x)], simplify = T))
corr.obs <- c(corr.obs, corr.obs.td)
hist(corr.obs, probability = T, col = "orange", border = "white", 
     main = "Histogram of correlation values observed in the 2 groups")
th <- quantile(corr.obs, probs = 0.80)
th <- round(th, 2)
abline(v = thr, lwd = 2, col = "red")
text(x = thr,y = 0, labels = "80th perc", col = "red", cex = 1)
# Pull togheter the data: Mean Value -------------------------------------

#reference : 
#https://stackoverflow.com/questions/31465415/combine-multiple-data-frames-and-calculate-average

library(data.table, quietly = T)
load("C:/Users/Alessandra/Desktop/sds/HW3/hw3_data.RData")

#rename the subjects ASD
names(asd_sel) <- c(paste("subj", seq(1,9), sep = '_0'), 
                   paste("subj", seq(10,12), sep = '_'))

#not consider the outlier
asd <- asd_sel[-1]

#add a column called index, with number from 1 to 145
for (i in 1 : length(asd)) {
  asd[[i]] <- cbind(index = seq(1, 145), asd[[i]])
}

# build a dataframe containg the average of the original cells, 
# combining the cells with the same index and same roi
asd_mean <- rbindlist(asd)[,lapply(.SD,mean), index]

#remove the index column
asd_mean <- subset(asd_mean, select = -c(index))

#rename the subjects TD 
names(td_sel) <- c(paste("subj", seq(1,9), sep = '_0'), 
                    paste("subj", seq(10,12), sep = '_'))

#not consider the two outliers 
td <- td_sel[c(-1,-2)]

#add a column called index, with number from 1 to 145
for (i in 1 : length(td)) {
  td[[i]] <- cbind(index = seq(1, 145), td[[i]])
}

# build a dataframe containg the average of the original cells, 
# combining the cells with the same index and same roi
td_mean <- rbindlist(td)[,lapply(.SD,mean), index]

#remove the index column
td_mean <- subset(td_mean, select = -c(index))

# Correlation Test: Functions -------------------------------------------------------------

?corrplot::cor.mtest

my.cor.test <- function(x,y, r0, conf.level = 0.95) {
  #r0 is the correlation according to null hypothesis
  
  n <- length(x)
  r <- cor(x, y)
  
  r <- abs(r) #test is |r| > r0
  z.r <- atanh(r)
  z.r0 <- atanh(r0)
  sigma <- 1 / sqrt(n - 3)
  z <- (z.r - z.r0)/sigma
  
  cint <- c(z.r - sigma * qnorm(conf.level), Inf)
  cint <- tanh(cint)
  pval <- pnorm(z, lower.tail=FALSE)
  
  list(conf.int = cint, p.value = pval)
}


my.cor.mtest <- function(mat, r0, conf.level = 0.95) 
{
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 1
  diag(lowCI.mat) <- diag(uppCI.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- my.cor.test(x = mat[, i], y = mat[, j], r0 = r0, conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      if (!is.null(tmp$conf.int)) {
        lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
        uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
      }
    }
  }
  list(p = p.mat, lowCI = lowCI.mat, uppCI = uppCI.mat)
}


# Correlation Test witout control -----------------------------------------------

#116x116 corelation matrix
cor.asd.matrix <- cor(asd_mean)

alpha <- 0.05
n <- ncol(asd_mean)
m <- choose(n, 2) # binomial coefficients
#to_delete <- diagonal_indeces <- seq(1, n*n, n + 1) # index of diagonal elements 
th <- 0.21

asd_multi_matrix <- my.cor.mtest(asd_mean, r0 = th, conf.level = 0.95)

asd_edges_index <- which(asd_multi_matrix$p < alpha)
length(asd_edges_index)

#check if [-t, t] does not intersect confidence interval is equal to the empy set 
#that is when t < ci.low 
asd_index <- which(th < asd_multi_matrix$lowCI)
length(asd_index)

# TD SUBJECTS

td_multi_matrix <- my.cor.mtest(td_mean, r0 = th, conf.level = 0.95)

td_edges_index <- which(td_multi_matrix$p < alpha)
length(td_edges_index)

#check if [-t, t] does not intersect confidence interval is equal to the empy set 
#that is when t < ci.low 
td_index <- which(th < td_multi_matrix$lowCI)
length(td_index)


# Bonferroni control ------------------------------------------------------

t_bonf <- alpha/m
bon_asd_edges_index <- which(asd_multi_matrix$p < alpha/m)  # the p value not depens on conf.level
length(bon_asd_edges_index)

bon_asd_multi_matrix <- my.cor.mtest(asd_mean, r0 = th, conf.level = 1 - (alpha/m))

#check if [-t, t] does not intersect confidence interval is equal to the empy set 
#that is when   t < lowCI 
bon_asd_index <- which(th < bon_asd_multi_matrix$lowCI)
length(bon_asd_index)

# TD SUBJECTS 

bon_td_edges_index <- which(td_multi_matrix$p < alpha/m)  # the p value not depens on conf.level
length(bon_td_edges_index)

bon_td_multi_matrix <- my.cor.mtest(td_mean, r0 = th, conf.level = 1 - (alpha/m))

#check if [-t, t] does not intersect confidence interval is equal to the empy set 
#that is when   t < lowCI  
bon_td_index <- which(th < bon_td_multi_matrix$lowCI)
length(bon_td_index)


# ASD and TD Graph  ------------------------------------------------------------------

asd_bon_pmat <- bon_asd_multi_matrix$p
asd_bon_pmat[diagonal_indeces] <- 1
asd_bon_adj_mat <- matrix(0, nrow = n, ncol = n, dimnames = dimnames(cor.asd.matrix))
asd_bon_adj_mat[ which(asd_bon_pmat < t_bonf) ] <- 1

G_asd_bon <-graph_from_adjacency_matrix(asd_bon_adj_mat, mode = "undirected")

# TD SUBJECTS 

td_bon_pmat <- bon_td_multi_matrix$p
td_bon_pmat[diagonal_indeces] <- 1
td_bon_adj_mat <- matrix(0, nrow = n, ncol = n, dimnames = dimnames(cor.asd.matrix))
td_bon_adj_mat[ which(td_bon_pmat < t_bonf) ] <- 1

G_td_bon <-graph_from_adjacency_matrix(td_bon_adj_mat, mode = "undirected")

# Plot
par(mfrow=c(1,2), oma = c(0,0,0,0), family = "sans", font.sub = 2, cex.sub = 0.8) #sans" and "mono
plot(G_asd_bon, 
     vertex.size = 12, 
     vertex.color = "royalblue",
     vertex.shape = "sphere",
     vertex.label.cex = 0.8,
     vertex.label.font = 2,
     vertex.label.color=grey(level = .9),
     edge.width = 2, 
     edge.color = "darkgreen",
     curved = TRUE, 
     main = "Correlation Graph ASD",
     sub = "Bonferroni adjustment"
)
plot(G_td_bon, 
     vertex.size = 12, 
     vertex.color = "royalblue",
     vertex.shape = "sphere",
     vertex.label.cex = 0.8,
     vertex.label.font = 2,
     vertex.label.color=grey(level = .9),
     edge.width = 2, 
     edge.color = "darkgreen",
     curved = TRUE, 
     main = "Correlation Graph TD",
     sub = "Bonferroni adjustment"
)
par(mfrow = c(1,1))

print(asd_graph_bon)
length(E(asd_graph_bon))
length(V(asd_graph_bon))


# Difference between independent correlation: Functions -------------------

#reference : http://davidmlane.com/hyperstat/B8712.html

my.cor.dif.test <- function(x1, y1, x2, y2, r0, conf.level = 0.95) {
  #r0 is the correlation according to null hypothesis
  #x1 : value x of group 1   #y1 : value y of group 1 
  #x2 : value x of group 2   #y2 : value y of group 2
  
  n1 <- length(x1)
  n2 <- length(x2)
  r1 <- cor(x1, y1)
  r2 <- cor(x2, y2)
  
  if (r2 > r1) {
    tp <- r1
    r1 <- r2
    r2 <- tp 
  }
  
  z1 <- atanh(r1)
  z2 <- atanh(r2)
  var1 <- (1 / (n1 - 3)) 
  var2 <- (1 / (n2 - 3)) 
  sigma <- sqrt(var1 + var2) # standard error of the statistic
  
  z.r  <- (z1 - z2)
  z.r0 <- atanh(r0)
  z <- (z.r - z.r0)/sigma
  
  cint <- c(z.r - sigma * qnorm(conf.level), Inf)
  cint <- tanh(cint)
  pval <- pnorm(z, lower.tail=FALSE)
  
  list(conf.int = cint, p.value = pval)
}

my.dif.cor.mtest <- function(mat_g1, mat_g2, r0, conf.level = 0.95) 
{
  mat_g1 <- as.matrix(mat_g1)
  mat_g2 <- as.matrix(mat_g2)
  n <- ncol(mat_g1)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 1
  diag(lowCI.mat) <- diag(uppCI.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- my.cor.dif.test(x1 = mat_g1[, i], y1 = mat_g1[, j],
                             x2 = mat_g2[, i], y2 = mat_g2[, j],
                             r0 = r0, conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      if (!is.null(tmp$conf.int)) {
        lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
        uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
      }
    }
  }
  list(p = p.mat, lowCI = lowCI.mat, uppCI = uppCI.mat)
}


# Difference between Correlation: without control -------------

dif_multi <- my.dif.cor.mtest(asd_mean, td_mean, r0 = th)
dif.pmat <- dif_multi$p
sum(dif_multi$p < alpha)
sum(dif_multi$lowCI > th)

# which edges are linked
dif.low <- dif_multi$lowCI
dimnames(dif.low) <- dimnames(cor.mat)
dif.low[lower.tri(dif.low, diag = T)] <- 0 
idx.nc <- which(dif.low > th, arr.ind=TRUE)
edges.nc <- cbind(rownames(dif.low)[idx[,"row"]], colnames(dif.low)[idx[,"col"]])
edges.nc.labels <- apply(X = edges.nc, MARGIN = 1, function(x) paste(x[1], x[2], sep = "-"))

results.no.control <- data.frame(
  edges = edges.nc.labels,
  corr_ASD = round(asd.cor.mat[idx.nc],3),
  corr_TD = round(td.cor.mat[idx.nc],3),
  delta_corr = round(delta.corr.mat[idx],3),
  delta_CI95_low = round(dif.low[idx],3)
)

results.no.control

# Difference between correlation: Bonferoni Control -----------------------

th <- 0.21
t_bonf <- (alpha/m)
dif_bon_multi <- my.dif.cor.mtest( asd_mean, td_mean, r0 = th, conf.level = 1 - (alpha/m))
dif.bon.pmat <- dif_bon_multi$p
D.bon.lowCI <- dif_bon_multi$lowCI
dimnames(D.bon.lowCI) <- dimnames(cor.mat)

#which ROI are linked  
verteces.liked.p <- which(dif_bon_multi$p < t_bonf)
verteces.liked   <- which(dif_bon_multi$lowCI > th)
verteces.liked.p
verteces.liked

# which edges are linked
length(verteces.liked)
dif.bon.low <- dif_bon_multi$lowCI
dif.bon.low[lower.tri(dif.bon.low, diag = T)] <- 0 
idx <- which(dif.bon.low > th, arr.ind=TRUE)
edges.bon <- cbind(rownames(D.bon.lowCI)[idx[,"row"]], colnames(D.bon.lowCI)[idx[,"col"]])
edges.bon
verteces.liked.name <- union(rownames(D.bon.lowCI)[idx[,"row"]], colnames(D.bon.lowCI)[idx[,"col"]])
edges.bon.labels <- apply(X = edges.bon, MARGIN = 1, function(x) paste(x[1], x[2], sep = "-"))

#correlation matrices 
#difference corr matrix
asd.cor.mat <- cor(asd_mean)
td.cor.mat <- cor(td_mean)
delta.corr.mat <- abs(asd.cor.mat - td.cor.mat)

results.bonf <- data.frame(
  edges = edges.bon.labels,
  corr_ASD = round(asd.cor.mat[idx],3),
  corr_TD = round(td.cor.mat[idx],3),
  delta_corr = round(delta.corr.mat[idx],3),
  delta_CI_low = round(dif.bon.low[idx],3)
)


# Difference between correlation: Graphs ----------------------------------

cor.mat <- cor(asd_mean)
dif_bon_pmat <- dif_bon_multi$p
dif_bon_pmat[diagonal_indeces] <- 1
dif_bon_adj_mat <- matrix(0, nrow = n, ncol = n, dimnames = dimnames(cor.asd.matrix))
dif_bon_adj_mat [which(dif_bon_pmat < t_bonf)] <- 1

G_delta_bon <-graph_from_adjacency_matrix(dif_bon_adj_mat, mode = "undirected") 

mask <- which(V(G_td_bon)$name %in% verteces.liked.name)
vertx.size <- rep(8, 116)
vertx.size[mask] <- 16

vertx.col <- rep(rgb(0,0,1,.3), 116)
vertx.col[mask] <- c(rgb(1,0,0,.3))

plot(G_delta_bon, 
     vertex.size = vertx.size, 
     vertex.color = vertx.col,
     vertex.shape = "circle",
     vertex.label.cex =(V(G_delta_bon)$size)/15,
     vertex.label.font = 2,
     vertex.label.color="black",
     edge.curved=0.2,
     edge.width = 4, 
     edge.color = "darkgreen",
     curved = TRUE, 
     main = "Difference Correlation Graph",
     sub = "Bonferroni adjustment",
     frame = T,
     vertex.frame.color = vertx.col
)
# vertex.label.dist=1.4

# 3 graph plot 

mask <- which(V(G_td_bon)$name %in% verteces.liked.name)
vertx.size <- rep(6, 116)
vertx.size[mask] <- 16

labels <- V(G_asd_bon)$name
vertx.label <- rep(NA, 116)
vertx.label[mask] <- labels[mask]

vertx.col <- rep(rgb(0,0,1,.3), 116)
vertx.col[mask] <- c(rgb(1,0,0,.3))
  
par(mfrow=c(1,3),  mar=c(1,1,1,1), family = "sans", font.sub = 2, cex.sub = 0.8)

plot(G_asd_bon, 
     vertex.size = vertx.size, 
     vertex.shape = "circle",
     vertex.label = vertx.label,
     vertex.label.cex = 1,
     vertex.label.font = 2,
     vertex.label.color="black",
     vertex.color = vertx.col,
     edge.curved=0.2,
     edge.width = 2, 
     edge.color = "darkgreen",
     curved = TRUE, 
     main = "ASD Correlation Graph",
     sub = "Bonferroni adjustment",
     frame = T,
     vertex.frame.color = vertx.col
)

plot(G_td_bon, 
     vertex.size = vertx.size, 
     vertex.shape = "circle",
     vertex.label = vertx.label,
     vertex.label.cex = 1,
     vertex.label.font = 2,
     vertex.label.color="black",
     vertex.color = vertx.col,
     edge.curved=0.2,
     edge.width = 2, 
     edge.color = "darkgreen",
     curved = TRUE, 
     main = "TD Correlation Graph",
     sub = "Bonferroni adjustment",
     frame = T,
     vertex.frame.color = vertx.col
)

plot(G_delta_bon, 
     vertex.size = vertx.size, 
     vertex.shape = "circle",
     vertex.label = vertx.label,
     vertex.label.cex = 1,
     vertex.label.font = 2,
     vertex.label.color="black",
     vertex.color = vertx.col,
     edge.curved=0.2,
     edge.width = 2, 
     edge.color = "darkgreen",
     curved = TRUE, 
     main = "Difference Correlation Graph",
     sub = "Bonferroni adjustment",
     frame = T,
     vertex.frame.color = vertx.col
)

par(mfrow=c(1,1))



