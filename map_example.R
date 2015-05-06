data.tab <- read.csv("repeat_data_20150505.csv", header = TRUE)


# Required package
library(maps)
library("mapdata")
library(RColorBrewer)
library(geosphere)
# library(mapproj)

addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

unique((data.tab$Group))

data(worldHiresMapEnv)

pdf("test.map3.pdf")

c.xlim = c(-180,180)
c.ylim = c(-90,90)
map('worldHires', xlim = c.xlim,
    ylim = c.ylim, col="grey", bg = "white",
    fill = TRUE,
#     projection = "sinusoidal",
    main = "Map of study locations/n linked to furthest track point"
    )
# ?map
g.num <- as.numeric(data.tab$Group)

# col.line <- rainbow(length(unique(g.num)), alpha = 0.6)
col.line <- addalpha(brewer.pal(length(unique(g.num)),"Paired"), alpha = 0.8)
g.col <- col.line[g.num]


points(data.tab$long_start, data.tab$lat_start, bg = g.col,
       col = addalpha(g.col, alpha = 0.9),
       pch = 21,
       cex = 1)
# ?points
# segments(y0 = data.tab$lat_start, x0 = data.tab$long_start,
#          y1 = data.tab$lat_end, x1 = data.tab$long_end,
#          col = addalpha(g.col, alpha = 0.5),
#          lty = 1,
#          lwd = 5)
# ?segments
i <- 2
x <- !is.na(data.tab$lat_start) & !is.na(data.tab$lat_end)
for(i in 1:sum(x)){
lines((((gcIntermediate(cbind(data.tab$long_start[x][i], data.tab$lat_start[x][i]),
                     cbind(data.tab$long_end[x][i], data.tab$lat_end[x][i]),
                     breakAtDateLine = TRUE)))),
                     col = addalpha(g.col[x][i], alpha = 0.5), lwd = 3)
}
# ?gcIntermediate
# ?matplot
# z <- gcIntermediate(cbind(data.tab$long_start[x][i], data.tab$lat_start[x][i]),
#                     cbind(data.tab$long_end[x][i], data.tab$lat_end[x][i]),
#                     breakAtDateLine = TRUE)
# length(z)

box()
axis(side=(1),las=1)
axis(side=(2),las=1)

legend("bottomleft", # position
       legend = unique((data.tab$Group)), 
       title = "Group",
       fill = col.line[as.numeric(unique((data.tab$Group)))],
       cex = 0.56,
       bty = "n")

dev.off()