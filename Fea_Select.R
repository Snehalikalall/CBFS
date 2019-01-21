library('copula')
library('infotheo')
library(foreach)
library(doParallel)

# Output is informative feature subset


setwd("C:/Users/Snehalika/Desktop")       #set your dataset path here 
nf=80                                     # Number of Feature to be selected
test_lymphoma_s3<-read.csv("test_lymphoma_s3.csv")
set.seed(456)
datas2<-test_lymphoma_s3[,2:4027]      # Dataset without class label
datas2<-discretize(datas2)            # Discretize dataset
classd<-as.matrix(test_lymphoma_s3[,1])   # Class label
n <- nrow(datas2)
col<-ncol(datas2)
count<-1:ncol(datas2)
u <- matrix(runif(n*2), n, 2)
cl <- makeCluster(7)                   # (no of core-1) in machine
registerDoParallel(cl)
mimc1<-foreach(j=count, .combine=c,.packages='copula') %dopar%
{res<-mean(C.n(u, cbind(classd,datas2[,j])))
if(res==0)
{abs((1+res)*log(1+res))}
else{abs(res*log(res))}
}
mimc1<-as.matrix(mimc1)
stopCluster(cl)
fea<- matrix(0, nrow=1,ncol =nf)
idx1<-which.max(mimc1)
cmax<-which.max(mimc1)
fea[1,1]<- idx1
for (m in 1:(nf-1))
{u1<- matrix(runif(n*(m+1)), n, (m+1))
if(m==1)
{cl <- makeCluster(7)
registerDoParallel(cl)
parl<-foreach(j=count, .combine=c,.packages='copula') %dopar%
{res<-mean(C.n(u1, cbind(datas2[,idx1],datas2[,j])))
if(res==0)
{abs((1+res)*log(1+res))}
else{abs(res*log(res))}
}
stopCluster(cl)
red<-as.matrix(parl) 
result<-(mimc1-red)
result[fea]<-Inf
idx1<-which.min(result)
cd=cbind(datas2[,cmax],datas2[,idx1])}
else{
  cl <- makeCluster(7)
  registerDoParallel(cl)
  parl<-foreach(j=count, .combine=c,.packages='copula') %dopar%
  {res<-mean(C.n(u1, cbind(cd,datas2[,j])))
  if(res==0)
  {abs((1+res)*log(1+res))}
  else{abs(res*log(res))}
  }
  stopCluster(cl)
  red<-as.matrix(parl) 
  result<-(mimc1-red)
  result[fea]<-Inf
  idx1<-which.min(result) 
  cd=cbind(cd,datas2[,idx1])
}
fea[1,(m+1)]<-idx1
}
registerDoSEQ()