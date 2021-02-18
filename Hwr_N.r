install.packages("Hmisc")
require(Hmisc)
require(igraph)
load(file.choose())
set.seed(122)

# Set the Threshold --------------------------------------------------------
asd.overall.corr= lapply(asd_sel, cor)
td.overall.corr= lapply(td_sel,cor)
total.corr= c(do.call(rbind,asd.overall.corr),do.call(rbind,td.overall.corr))

th= quantile(total.corr, probs = 0.8)

# KMean ASD ---------------------------------------------------------------


asd.mat= as.matrix(do.call(rbind,asd_sel))  #concatenate lists in asd_sel

rownames(asd.mat)=NULL #remove row names 
rownames(asd.mat)
asd.scaled=scale(asd.mat) # scale the range of the values
View(asd.scaled)
#KMeans 
clusters <- kmeans(asd.scaled, 145)  
asd.new= cbind(clusters$cluster,asd.scaled) # add the cluster column to the matrix 
View(clusters$cluster)


asd.final= matrix(NA,nrow = 145,ncol = 116) # result matrix
# for each cluster we take the mean of the columns, creating a new sample that contains the infos
# about the most similar samples 
for (i in 1:145){
  idx=which(asd.new[,1]==i)
  
  row= colMeans(as.matrix(asd.new[idx,2:117]))
  
  asd.final[i,]= t(row)
  
}



View(asd.final)

# TD Kmean ----------------------------------------------------------------

td.mat= as.matrix(do.call(rbind,td_sel))  #concatenate lists in asd_sel

rownames(td.mat)=NULL #remove row names 
rownames(td.mat)
td.scaled=scale(td.mat) # scale the range of the values
View(td.scaled)
#KMeans 
clusters <- kmeans(td.scaled, 145)  
td.new= cbind(clusters$cluster,td.scaled) # add the cluster column to the matrix 
View(clusters$cluster)


td.final= matrix(NA,nrow = 145,ncol = 116) # result matrix
# for each cluster we take the mean of the columns, creating a new sample that contains the infos
# about the most similar samples 
for (i in 1:145){
  idx=which(td.new[,1]==i)
  
  row= colMeans(as.matrix(td.new[idx,2:117]))
  
  td.final[i,]= t(row)
  
}


View(td.final)

# Test Functions ----------------------------------------------------------

#def test function 
cor.mtest <- function(mat,th, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.mytest(mat[,i], mat[,j],th, conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(pval = p.mat, low = lowCI.mat, up = uppCI.mat))
}

cor.mytest<- function(x,y,th,conf.level=0.95){
  n= length(x)
  r=abs(cor(x,y))
  z= atanh(r)
  z.th= atanh(th)
  sigma <- 1 / sqrt(n - 3)
  test= (z-z.th)/sigma
  conf.int <- c(z - sigma * qnorm(conf.level), Inf)
  conf.int <- tanh(conf.int)
  p.value= pnorm(test,lower.tail = F)
  return(list('p.value'=p.value,'conf.int'= conf.int))
}


# Correlation Analysis ASD ----------------------------------------------------

#get the correlation matrix
asd.cor= cor(asd.final)

#plot asd matrix and add a normal distribution curve 
hist(asd.final,col = 'Red',probability = T)

curve(dnorm(x,mean = 0,sd = 1),from = -4, to= 4,add=T,col= 'Blue',lwd= 4)
hist(asd.cor,col='Orchid',probability = T)

asd.result= cor.mtest(asd.final,th)

hist(asd.result$pval,col='Orchid', probability = T)  
alpha=0.05
m= choose(116,2)
p_val.mat  = matrix(0, nrow = nrow(asd.cor), ncol = ncol(asd.cor))

#bonferroni correction
p_val.mat[which(asd.result$pval<(alpha/m))]=1  


View(p_val.mat)
diag(p_val.mat)=0
sum(p_val.mat)

G1 <- graph_from_adjacency_matrix(p_val.mat, mode = "undirected")

plot(G1)
#without correction
p_val.mat.uncor=matrix(0, nrow = nrow(asd.cor), ncol = ncol(asd.cor))
p_val.mat.uncor[which(asd.result$pval<(alpha))]=1  
diag(p_val.mat.uncor)=0
sum(p_val.mat.uncor)
G1.uncorr <- graph_from_adjacency_matrix(p_val.mat.uncor, mode = "undirected")

plot(G1.uncorr)

# Correlation Analysis TD -------------------------------------------------

td.cor= cor(td.final)
hist(td.final,col = 'Red',probability = T)
curve(dnorm(x,mean = 0,sd = 1),from = -4, to= 4,add=T,col= 'Blue',lwd= 4)
hist(td.cor,col='Orchid',probability = T)

td.result= cor.mtest(td.cor,th)

hist(td.result$pval,col='Orchid', probability = T)  
alpha=0.05
m= choose(116,2)
p_val.mat.td  = matrix(0, nrow = nrow(td.cor), ncol = ncol(td.cor))

#bonferroni correction
p_val.mat.td[which(td.result$pval<(alpha/m))]=1  


View(p_val.mat.td)
diag(p_val.mat.td)=0
sum(p_val.mat.td)

G2 <- graph_from_adjacency_matrix(p_val.mat.td, mode = "undirected")

plot(G2)
#without correction
p_val.mat.td.uncorr  = matrix(0, nrow = nrow(td.cor), ncol = ncol(td.cor))

p_val.mat.td.uncorr[which(td.result$pval<(alpha))]=1  
diag(p_val.mat.td.uncorr)=0
sum(p_val.mat.td.uncorr)

G2.uncorr <- graph_from_adjacency_matrix(p_val.mat.td, mode = "undirected")

plot(G2.uncorr)


# Difference Test ---------------------------------------------------------

#define function
cor.mtest.diff <- function(mat,mat2,th, conf.level = 0.95){
  mat <- as.matrix(mat)
  mat2<- as.matrix(mat2)
  n <- ncol(mat)
 
  
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.mytest.diff(mat[,i], mat[,j],mat2[,i],mat2[,j],th, conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(pval = p.mat, low = lowCI.mat, up = uppCI.mat))
}

cor.mytest.diff<- function(x1,x2,y1,y2,th,conf.level=0.95){
  n= length(x1)
  r1=abs(cor(x1,x2))
  r2= abs(cor(y1,y2))
  z1= atanh(r1)
  z2= atanh(r2)
  z.th= atanh(th)
  sigma= 1/( n-3)
  sigma <-sqrt(sigma+ sigma)
  z= z1-z2
  test= (z-z.th)/sigma
  conf.int <- c(z - sigma * qnorm(conf.level), Inf)
  conf.int <- tanh(conf.int)
  p.value= pnorm(test,lower.tail = F)
  return(list(p.value=p.value,conf.int= conf.int))
}

diff.result=cor.mtest.diff(asd.final,td.final,th)
View(diff.result$pval)
diag(diff.result$pval)=1
sum(diff.result$pval<(alpha/m)) # 4 significant differences 
