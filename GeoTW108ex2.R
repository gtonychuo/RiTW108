###############################################################
# connect `sf` to `spatstat` for event pattern analysis
###############################################################
pacman::p_load(readr,dplyr,sf,spatstat,tmap,stringr,maptools)
pacman::p_load(data.table,raster,tidyverse,splancs,ggplot2,spatialkernel)

load("geotw108.rdata")
load("kaoh/house_price.rdata")
D = st_as_sf(D, coords=c("E","N"), crs=4326) %>% 
  st_transform(crs=3826)

area = c("鹽埕區","三民區","新興區","前金區","苓雅區")
types = c("店面", "套房", "華廈", "公寓")
Towns = subset(twTown,COUNTY=="高雄市" & TOWN %in% area) 
Window = st_union(Towns)
Houses = subset(D, st_within(D,Window,sparse=F) & type%in%types) %>% 
  sample_n(600)
types = factor(as.character(Houses$type))
plot(st_geometry(Towns),col='#00808040',border='red',lwd=2)
plot(st_geometry(Houses),col=types,pch=16,add=T)

PPP = as.ppp(
  X = t(sapply(st_geometry(Houses),c)), 
  W = as_Spatial(Window))
marks(PPP) = types
plot(PPP)
plot(st_geometry(Towns),col='#00800020',border='orange',lwd=2,add=T)

###################################################################
# Get some summary information on the dataset
summary(PPP)
table(marks(PPP))

# Use the split function to show the 4 point patterns
splits <- split(PPP)
plot(splits)

# Compute the densities of both sets of points
den <- density(splits)
plot(den)

# Calc & plot the porb of type 
DX = den[[1]]+den[[2]]+den[[3]]+den[[4]] 
tx = levels(types)
prob = lapply(den, function(d) d/DX) %>% setNames(tx)
p0 = par(mfrow=c(2,2), mar=c(1,1,2,2), cex=0.8)
for(i in 1:length(prob)) {
  plot(prob[[i]], main=tx[i])
  plot(st_geometry(Towns),col='#00800000',border='lightgreen',lwd=1,add=T)
  }
par(p0)

# Scan from 500m to 1000m in steps of 50m
bw_choice <- spseg(PPP, h = seq(50, 800, by = 50), opt = 1)
plotcv(bw_choice); abline(v = bw_choice$hcv, lty = 2, col = "red")
print(bw_choice$hcv)

# Set the correct bandwidth and run for 10/100 simulations only
seg10 = spseg(pts=PPP,h=400,opt=3,ntest=10,proc=T)
plotmc(seg10, "套房")

t0 = Sys.time()
seg200 = spseg(pts=PPP,h=400,opt=3,ntest=200,proc=T)
Sys.time() - t0  # 1.580724 mins
p0 = par(mfrow=c(2,2), mar=c(3,4,2,2))
lapply(tx, function(z) plotmc(seg200, z))
par(p0)





