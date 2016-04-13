install.packages("inline")
install.packages("RcppArmadillo")
install.packages("caret")


rm(list=ls())
library(MASS)
library(GPLTR)
library(caret)
# library(Rcpp)
# library(inline)
# library(RcppArmadillo)

data(Boston)

####confounding covariates


pltr.boston <-pltr.glm(Boston,Y.name = "medv", X.names = "rm", 
                     G.names = names(Boston)[-c(6,14)], 
                     family = "gaussian", args.rpart = list(minbucket = 10, maxdepth = 4,
                                                            cp = 0, maxcompete = 0, 
                                                            maxsurrogate = 0),
                     iterMax = 10, iterMin = 1, verbose = T)
plot(pltr.boston$tree)
text(pltr.boston$tree,minlength = 0L, xpd = TRUE, cex=0.6)
summary(pltr.boston)

K=5
index<-createFolds(1:dim(Boston)[1],k=K)

sapply(1:K,function(i){
  
  
  
})

QLF<-function(dat,tao){
  
  qtao<-quantile(dat,tao)
  return(1/2*abs(dat-qtao)+(tao-1/2)*(dat-qtao))
  
}


x = subset(Boston,select = -medv)
y = Boston$medv
names<-names(x)
ord.x<-sapply(1:dim(x)[2],function(i) order(x[,i]),simplify = T)
x.ord<-sapply(1:dim(x)[2],function(i) x[ord.x[,i],i],simplify = T)
y.ordx<-sapply(1:dim(x)[2],function(i) y[ord.x[,i]],simplify = T)
ordy<-sapply(1:dim(x)[2],function(i) order(y.ordx[,i]),simplify = T)
# y.ordy<-sapply(1:dim(x)[2],function(i) order(y.ordx[,i]),simplify = T)y.ordx[ordy]
# r<-order(ordy)


QLF.org<-sum(QLF(y,tao=0.5))

y.split<-as.list(NULL)
y.split[[1]]<-as.matrix(y.ordx)
y.split.new<-as.matrix(y.ordx)
node<-as.list(NULL)

l=1
while(length(y.split)<dim(Boston)[1]){
  
  node[[l]]=0
  for (k in 1:2^(l-1)){
    
    best<-NULL
    best.index<-NULL
    y.new<-y.split[[k]]
    y.split.new<-as.list(NULL)
    for ( j in 1:dim(x)[2]){
      
      y.right<-y.new[,j]
      y.left<-NULL
      n.right<-length(y.right)
      
      
      QLF.new<-sapply(1:n.right,function(i){
        
        y.left<-c(y.left,y.right[1:i])
        y.right<-y.right[-(1:i)]
        QLF.left<-sum(QLF(y.left,tao=0.5))
        QLF.right<-sum(QLF(y.right,tao=0.5))
        QLF.new<-QLF.org-QLF.left-QLF.right
        
        return(QLF.new)
        
      })
      
      best[j]<-max(QLF.new)
      best.index[j]<-which.max(QLF.new)
      
    }
    
    
    BEST.index<-which.max(best)
    split.index<-best.index[BEST.index]
    
    
    node[[l]]<-c(node[[l]],paste(names[BEST.index],"<",x[split.index,BEST.index],sep=" "))
    
    y.split.new[[2^k-1]]<-as.matrix(y.split[[k]][1:split.index,])
    y.split.new[[2^k]]<-as.matrix(y.split[[k]][-(1:split.index),])
    
    
  }
  y.split<-y.split.new
  
  l=l+1
}




x<-as.list(rep(0,9))

for (l in 1:9){
  
  
  for (k in 1:2^(l-1)){
    
    x[[l]][2^k-1]=2^k-1
    
    x[[l]][2^k]=2^k
  }
  
}
