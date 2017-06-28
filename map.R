#Kemal Burcak Kaplan
library(maps)
library(geosphere)
library(mapdata)
library(extrafont)

edges <- read.csv("post80_perc_avg.csv",header=T, as.is=T,sep=",")
lat_lon <- read.csv("lat_lon.csv",header=T, as.is=T,sep=",")

edges_f <- edges[which(edges$post80>=0.01),]  

par(mar=c(0,0,2,0))
map("worldHires", col="grey20", fill=TRUE, bg="black",
    lwd=0.05,border="black",ylim=c(-60,90))
title("-World Bilateral Trade Network post 1980-", col.main= "#3b5998",
      font.main = 4,cex.main = 1)

col.1 <- adjustcolor("#3b5998", alpha=0.3)
col.2 <- adjustcolor("#dfe3ee", alpha=0.3)
edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
edge.col <- edge.pal(100)

for(i in 1:nrow(edges_f))  {
    node1 <- lat_lon[lat_lon$coun== edges_f[i,]$exp,]
    node2 <- lat_lon[lat_lon$coun== edges_f[i,]$imp,]
    
    arc <- gcIntermediate( c(node1[1,]$lon, node1[1,]$lat), 
                           c(node2[1,]$lon, node2[1,]$lat), 
                           n=100, addStartEnd=TRUE,breakAtDateLine = TRUE)
    
    if(length(arc)!=2){
    edge.ind <- round(100*edges_f[i,]$post80 / max(edges_f$post80))
    lines(arc, col=edge.col[edge.ind], lwd=log10(edge.ind))
    }
}


