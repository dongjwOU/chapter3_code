rasterstack=ndvi_crop
layers=nlayers(rasterstack)
ncell=ncell(rasterstack)
ncol=ncol(rasterstack)
nrow=nrow(rasterstack)
crs=crs(rasterstack)
extent=extent(rasterstack)

mtrx <- as.matrix(rasterstack,ncol=layers)
empt <- matrix(nrow=ncell, ncol=12)

names(rasterstack)<-c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011")


(rasterstack,start,end)

for (i in 1:nrow(rasterstack)){
  for (j in 1:ncol(rasterstack)){
    x <- rasterstack[i,j]
    xx<- ts(x, start=2000, end=2011 , frequency=1)
    r <-Trend(Yt = xx,mosum.pval = 0.05,h = 0.15,breaks = 2,sample.method = All)
    
    ## error is due to not knowing the name of each year as X2000
    
    
    empt[i,1] <- as.numeric(r)
    trend <- raster(nrows=nrow,ncols=ncol,crs=crs)
    extent(trend) <- extent
    values(trend) <- empt[,1]
    
    
    output<-empt[xxx,1]  
}}

r <-Trend(Yt = xx,mosum.pval = 0.05,h = 0.15,breaks = 2,sample.method = All)

x<- rasterstack[20,20]
xx<- ts(x, start="x2000", end= "x2011",frequency = 1) 
r <-Trend(Yt = xx,mosum.pval = 0.05,h = 0.15,breaks = 2,sample.method = All)


1x <- x[1]
xx <- ts(data = x)
xxx <- xx*100
xxx
xx
x
