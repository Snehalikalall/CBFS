library('copula')
library('infotheo')
library(foreach)
library(doParallel)

# Output(fea) is informative feature subset


setwd("/home/snehalika/Desktop")       #set your dataset path here 
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
mimc1<-foreach(j=count, .combine=c,.packages='copula') %dopar%  #Find the most relevant feature
{res<-mean(C.n(u, cbind(classd,datas2[,j])))
if(res==0)
{abs((1+res)*log(1+res))}
else{abs(res*log(res))}
}
mimc1<-as.matrix(mimc1)
stopCluster(cl)
fea<- matrix(0, nrow=1,ncol =nf)        #Find the most redundant feature
fea[1,1]<-which.max(mimc1)
for (m in 2:nf)
{u1<- matrix(runif(n*m), n,m)
cl <- makeCluster(7)
registerDoParallel(cl)
feas<-fea[1,1:(m-1)]
parl<-foreach(j=count, .combine=c,.packages='copula') %dopar%
{
  res<-mean(C.n(u1, cbind(datas2[,feas],datas2[,j])))
  if(res==0)
  {abs((1+res)*log(1+res))}
  else{abs(res*log(res))}
}
stopCluster(cl)
red<-as.matrix(parl) 
result<-(mimc1-red)
result[fea]<-Inf
fea[1,m]<-which.min(result)
}
registerDoSEQ()