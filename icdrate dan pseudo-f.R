Datahasil = cbind(df.no.NA,clust.list[[2]]$cluster)

icdrate = function(Data, nc, c)
{
  n = dim(Data)[1]
  p = dim(Data)[2]
  X = Data[,1:(p-1)]
  Group = Data[,p]
  
  p = dim(X)[2]
  Mean.X = matrix(ncol = p, nrow = (nc+1))
  for (i in 1:nc)
  {
    for (j in 1:p)
    {
      Mean.X[i,j] = mean(X[which(Group==i),j])
      Mean.X[(nc+1),j] = mean(X[,j])
    }
  }
  
  SST = matrix(ncol=p, nrow=n)
  for (i in 1:n)
  {
    for (j in 1:p)
    {
      SST[i,j] = (X[i,j] - Mean.X[(nc+1),j])^2
    }
  }
  SST = sum(sum(SST))
  
  SSE = matrix(ncol=p, nrow=n)
  for (i in 1:n)
  {
    for (j in 1:p)
    {
      for (k in 1:nc)
      {
        if (Group[i]==k)
        {
          SSE[i,j] = (X[i,j] - Mean.X[k,j])^2
        }
      }
    }
  }
  SSE = sum(sum(SSE))
  
  Rsq = (SST-SSE)/SST
  pseudof = (Rsq/(c-1))/((1-Rsq)/(nc-c))
  icdrate = 1-Rsq
  list(Rsq = Rsq, pseudof = pseudof, icdrate = icdrate)
}
Datahasil3=cbind(df.no.NA,clust.list[[3]]$cluster)
Datahasil4=cbind(df.no.NA,clust.list[[4]]$cluster)
Datahasil5=cbind(df.no.NA,clust.list[[5]]$cluster)
pf1= icdrate(Datahasil,8636,2)
pf2= icdrate(Datahasil3,8636,3)
pf3= icdrate(Datahasil4,8636,4)
pf4= icdrate(Datahasil5,8636,5)
plot(x=seq(1:4),y=c(pf1$pseudof,pf2$pseudof,pf3$pseudof,pf4$pseudof),
     type='l',lwd='2',col='blue')
pf1
Datahasil$`clust.list[[2]]$cluster`<-as.factor(Datahasil$`clust.list[[2]]$cluster`)
summary(Datahasil$`clust.list[[2]]$cluster`)
clust.list[[2]]$centers
#scale df
cn<-colnames(df.scale)
Datahasil.scale<-cbind(df.scale,clust.list.)
colnames(Datahasil.scale)<-c(cn,'label')
pf.scale<-icdrate(Datahasil.scale,8636,2)
pf.scale
Datahasil.scale$label<-as.factor(Datahasil.scale$label)
summary(Datahasil.scale$label)
