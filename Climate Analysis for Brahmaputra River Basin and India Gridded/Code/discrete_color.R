setwd("C:/Users/mmouk/Desktop/Data vis _lab")
par(mar = c(10,10,10,10))
#par(mfrow=c(1,4))
library(ggplot2)
library(grid)
library(gridExtra)
library(extrafont)
library(ggmap)
library(maptools)
library(plyr)
library(maps)
counter=0;
summ=0;
ro=1;
i=0;
p <- read.csv("India_gridded_data.csv") ## Import the data
##mat<-matrix(0, nrow = t[2],ncol=2, byrow = FALSE)
mat<-matrix(0, nrow = 114,ncol=354, byrow = FALSE)
for(i in 1:354)
{
  for(j in 1:1368)
  {
    counter=counter+1;
    summ=summ+p[j,i];
    if(counter==12)
    {
      mat[ro,i]=summ;
      summ=0;
      ro=ro+1;
      counter=0;
      
    }
    
  }
  ro=1;
  counter=0;
  summ=0;
  

}
mat;
write.csv(mat,'India_sum_annual.csv')

avg<-read.csv("India_sum_annual.csv")

t=dim(avg)
mat2<-matrix(0, nrow = t[2],ncol=2, byrow = FALSE)
for(i in 1:(t[2]-1))
{
  m = mean(avg[,i+1])   ##  mean
  mat2[i,1]<-m
  s<-sd(avg[,i+1])
  mat2[i,2]<-s        ##   standard deviation
}
write.csv(mat2,'India_mean_sd_annual.csv')

q<-read.csv("India_mean_sd_annual.csv")
chart<-list()
w <- c(62,681,977,1.32e+03,4.35e+03)

cols <- c("[62,681)" = "yellow", "[681,977)" = "red", "[977,1.32e+03)" = "blue", "[1.32e+03,4.35e+03)" = "purple")
q$A1 <- cut(q$Mean,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[1]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Annual Long Term Mean of India gridded ")


 cols <- c("[62,681)" = "yellow", "[681,977)" = "red", "[977,1.32e+03)" = "blue", "[1.32e+03,4.35e+03)" = "purple")
 q$A2 <- cut(q$Mean,breaks = w,right = FALSE)
 h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


 River_B <- readShapeSpatial("Ind")
 ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
 cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
 cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[2]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Annual Long Term Mean of India gridded ")


##### Multiple Plot Function #####

multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols)    # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}
tiff(filename = "India_annual.tiff", pointsize =8, res = 600, units = "in", width = 8, height = 3, restoreConsole = TRUE, type = "cairo")
multiplot(chart[[2]], cols = 2)
dev.off()
jpeg(filename = "India_annual.jpeg", pointsize =8, res = 600, units = "in", width = 8, height = 2, restoreConsole = TRUE, type = "windows")
multiplot(chart[[2]] ,cols = 2)
dev.off()
######################PArt A OVER##############################################




########################PArt B Begins#####################################################

counter=0;
summ=0;
ro=1;
i=0;

avg<-read.csv("India_sum_annual.csv")

t=dim(avg)
mat1970<-matrix(0, nrow = 354,ncol=1, byrow = FALSE)
for(i in 2:355)
{
  
  for(j in 1:70)
  {
    summ=summ+avg[j,i];
  }
  mat1970[i-1,1]<-summ/70;
  summ=0;
}
write.csv(mat1970,'India_mean_1970.csv')


counter=0;
summ=0;
ro=1;
i=0;

avg<-read.csv("India_sum_annual.csv")

t=dim(avg)
mat2015<-matrix(0, nrow = 354,ncol=1, byrow = FALSE)
for(i in 2:355)
{
  
  for(j in 71:114)
  {
    summ=summ+avg[j,i];
  }
  mat2015[i-1,1]<-summ/44;
  summ=0;
}

write.csv(mat2015,'India_mean_2015.csv')

  



q<-read.csv("India_mean_2014.csv")
chart<-list()
w <- c(62,681,977,1.32e+03,4.35e+03)

cols <- c("[62,681)" = "yellow", "[681,977)" = "red", "[977,1.32e+03)" = "blue", "[1.32e+03,4.35e+03)" = "purple")
q$A1 <- cut(q$Mean,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[1]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Annual Average Rainfall India (1971-2014)")


cols <- c("[62,681)" = "yellow", "[681,977)" = "red", "[977,1.32e+03)" = "blue", "[1.32e+03,4.35e+03)" = "purple")
q$A2 <- cut(q$Mean,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[2]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Annual Average Rainfall India (1971-2014)")



##### Multiple Plot Function #####

multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols)    # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}
tiff(filename = "India_2014.tiff", pointsize =8, res = 600, units = "in", width = 8, height = 3, restoreConsole = TRUE, type = "cairo")
multiplot(chart[[2]], cols = 2)
dev.off()
jpeg(filename = "India_2014.jpeg", pointsize =8, res = 600, units = "in", width = 8, height = 2, restoreConsole = TRUE, type = "windows")
multiplot(chart[[2]] ,cols = 2)
dev.off()




q<-read.csv("India_mean_1970.csv")
chart<-list()
w <- c(62,681,977,1.32e+03,4.35e+03)

cols <- c("[62,681)" = "yellow", "[681,977)" = "red", "[977,1.32e+03)" = "blue", "[1.32e+03,4.35e+03)" = "purple")
q$A1 <- cut(q$Mean,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[1]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Annual Average Rainfall India (1901-1970)")


cols <- c("[62,681)" = "yellow", "[681,977)" = "red", "[977,1.32e+03)" = "blue", "[1.32e+03,4.35e+03)" = "purple")
q$A2 <- cut(q$Mean,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[2]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Annual Average Rainfall India (1901-1970)")



##### Multiple Plot Function #####

multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols)    # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}
tiff(filename = "India_1970.tiff", pointsize =8, res = 600, units = "in", width = 8, height = 3, restoreConsole = TRUE, type = "cairo")
multiplot(chart[[2]], cols = 2)
dev.off()
jpeg(filename = "India_1970.jpeg", pointsize =8, res = 600, units = "in", width = 8, height = 2, restoreConsole = TRUE, type = "windows")
multiplot(chart[[2]] ,cols = 2)
dev.off()









q<-read.csv("India_Annual_Mean_Difference.csv")
chart<-list()
w <- c(-215,-34,-2,66,280)

cols <- c("[-215,-34)" = "yellow", "[-34,-2)" = "red", "[-2,66)" = "blue", "[66,280)" = "purple")
q$A1 <- cut(q$Mean,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[1]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Annual Mean Difference of India for Global Warming (1970,2014) ")


cols <- c("[-215,-34)" = "yellow", "[-34,-2)" = "red", "[-2,66)" = "blue", "[66,280)" = "purple")
q$A2 <- cut(q$Mean,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[2]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Annual Mean Difference of India for Global Warming (1970,2014) ")



##### Multiple Plot Function #####

multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols)    # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}
tiff(filename = "India_Annualdiff.tiff", pointsize =8, res = 600, units = "in", width = 8, height = 3, restoreConsole = TRUE, type = "cairo")
multiplot(chart[[2]], cols = 2)
dev.off()
jpeg(filename = "India_Annualdiff.jpeg", pointsize =8, res = 600, units = "in", width = 8, height = 2, restoreConsole = TRUE, type = "windows")
multiplot(chart[[2]] ,cols = 2)
dev.off()


#################################Part B Completed##################################################
#########################
###################################################################################################



#############################Part C ##########################################################################


counter=0;
seasonalcheck=0;
summ=0;
ro=1;
i=0;
p <- read.csv("India_gridded_data.csv") ## Import the data
##mat<-matrix(0, nrow = t[2],ncol=2, byrow = FALSE)
mat<-matrix(0, nrow = 114,ncol=354, byrow = FALSE)
for(i in 1:354)
{
  for(j in 1:1368)
  {
    seasonalcheck=seasonalcheck+1;
    counter=counter+1;
    if(seasonalcheck>=6 && seasonalcheck<=9){
      summ=summ+p[j,i];
    }
    if(counter==12)
    {
      mat[ro,i]=summ;
      summ=0;
      ro=ro+1;
      counter=0;
      seasonalcheck=0;
      
    }
    
  }
  seasonalcheck=0;
  ro=1;
  counter=0;
  summ=0;
  
  
}
mat;
write.csv(mat,'India_sum_seasonal.csv')




counter=0;
summ=0;
ro=1;
i=0;

avg<-read.csv("India_sum_seasonal.csv")

t=dim(avg)
mat2<-matrix(0, nrow = t[2],ncol=2, byrow = FALSE)
for(i in 1:(t[2]-1))
{
  m = mean(avg[,i+1])   ##  mean
  mat2[i,1]<-m
  s<-sd(avg[,i+1])
  mat2[i,2]<-s        ##   standard deviation
}
write.csv(mat2,'India_mean_sd_seasonal.csv')

t=dim(avg)
mat1970<-matrix(0, nrow = 354,ncol=1, byrow = FALSE)
for(i in 2:355)
{
  
  for(j in 1:70)
  {
    summ=summ+avg[j,i];
  }
  mat1970[i-1,1]<-summ/70;
  summ=0;
}
write.csv(mat1970,'India_mean_1970_seasonal.csv')


counter=0;
summ=0;
ro=1;
i=0;

avg<-read.csv("India_sum_seasonal.csv")

t=dim(avg)
mat2015<-matrix(0, nrow = 354,ncol=1, byrow = FALSE)
for(i in 2:355)
{
  
  for(j in 71:114)
  {
    summ=summ+avg[j,i];
  }
  mat2015[i-1,1]<-summ/44;
  summ=0;
}
write.csv(mat2015,'India_mean_2014_seasonal.csv')


q<-read.csv("India_mean_2014_seasonal.csv")
chart<-list()
w <- c(0,439,776,1.08e+03,3.05e+03)

cols <- c("[0,439)" = "yellow", "[439,776)" = "red", "[776,1.08e+03)" = "blue", "[1.08e+03,3.05e+03)" = "purple")
q$A1 <- cut(q$Mean,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[1]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Average Seasonal Rainfall of India for 1971 to 2014")


cols <- c("[0,439)" = "yellow", "[439,776)" = "red", "[776,1.08e+03)" = "blue", "[1.08e+03,3.05e+03)" = "purple")
q$A2 <- cut(q$Mean,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[2]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Average Seasonal Rainfall of India for 1971 to 2014")



##### Multiple Plot Function #####

multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols)    # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}
tiff(filename = "India_seasonal_2014.tiff", pointsize =8, res = 600, units = "in", width = 8, height = 3, restoreConsole = TRUE, type = "cairo")
multiplot(chart[[2]], cols = 2)
dev.off()
jpeg(filename = "India_seasonal_2014.jpeg", pointsize =8, res = 600, units = "in", width = 8, height = 2, restoreConsole = TRUE, type = "windows")
multiplot(chart[[2]] ,cols = 2)
dev.off()


q<-read.csv("India_mean_1970_seasonal.csv")
chart<-list()
w <- c(0,439,776,1.08e+03,3.05e+03)

cols <- c("[0,439)" = "yellow", "[439,776)" = "red", "[776,1.08e+03)" = "blue", "[1.08e+03,3.05e+03)" = "purple")
q$A1 <- cut(q$Mean,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[1]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Average Seasonal Rainfall of India for 1901 to 1970 ")


cols <- c("[0,439)" = "yellow", "[439,776)" = "red", "[776,1.08e+03)" = "blue", "[1.08e+03,3.05e+03)" = "purple")
q$A2 <- cut(q$Mean,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[2]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Average Seasonal Rainfall of India for 1901 to 1970 ")



##### Multiple Plot Function #####

multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols)    # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}
tiff(filename = "India_seasonal_1970.tiff", pointsize =8, res = 600, units = "in", width = 8, height = 3, restoreConsole = TRUE, type = "cairo")
multiplot(chart[[2]], cols = 2)
dev.off()
jpeg(filename = "India_seasonal_1970.jpeg", pointsize =8, res = 600, units = "in", width = 8, height = 2, restoreConsole = TRUE, type = "windows")
multiplot(chart[[2]] ,cols = 2)
dev.off()
















q<-read.csv("India_Mean_Seasonal_Difference.csv")
chart<-list()
w <- c(-197,-19,8.7,58,275)

cols <- c("[-197,-19)" = "yellow", "[-19,8.7)" = "red", "[8.7,58)" = "blue", "[58,275)" = "purple")
q$A1 <- cut(q$Mean,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[1]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Seasonal Avg Difference of India for Global Warming (1970,2014)")


cols <- c("[-197,-19)" = "yellow", "[-19,8.7)" = "red", "[8.7,58)" = "blue", "[58,275)" = "purple")
q$A2 <- cut(q$Mean,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[2]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Seasonal Avg Difference of India for Global Warming (1970,2014)")



##### Multiple Plot Function #####

multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols)    # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}
tiff(filename = "India_Seasonaldiff.tiff", pointsize =8, res = 600, units = "in", width = 8, height = 3, restoreConsole = TRUE, type = "cairo")
multiplot(chart[[2]], cols = 2)
dev.off()
jpeg(filename = "India_Seasonaldiff.jpeg", pointsize =8, res = 600, units = "in", width = 8, height = 2, restoreConsole = TRUE, type = "windows")
multiplot(chart[[2]] ,cols = 2)
dev.off()



##########################################Part C Complete#############################################################

##############################################Part D begins##################################################

avg<-read.csv("India_sum_seasonal.csv")
mat1920<-matrix(0, nrow = 354,ncol=1, byrow = FALSE)
for(i in 2:355)
{
  
  for(j in 1:20)
  {
    summ=summ+avg[j,i];
  }
  mat1920[i-1,1]<-summ/20;
  summ=0;
}
write.csv(mat1920,'India_mean_1901-1920_seasonal.csv')


counter=0;
summ=0;
ro=1;
i=0;

avg<-read.csv("India_sum_seasonal.csv")

t=dim(avg)
mat1990<-matrix(0, nrow = 354,ncol=1, byrow = FALSE)
for(i in 2:355)
{
  
  for(j in 90:114)
  {
    summ=summ+avg[j,i];
  }
  mat1990[i-1,1]<-summ/25;
  summ=0;
}
write.csv(mat1990,'India_mean_1990-2015_seasonal.csv')









q<-read.csv("India_mean_1901-1920_seasonal.csv")
chart<-list()
w <- c(0,439,776,1.08e+03,3.05e+03)

cols <- c("[0,439)" = "yellow", "[439,776)" = "red", "[776,1.08e+03)" = "blue", "[1.08e+03,3.05e+03)" = "purple")
q$A1 <- cut(q$Mean,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[1]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Average Seasonal(June to Sept) Rainfall of India for 1901 to 1920")


cols <- c("[0,439)" = "yellow", "[439,776)" = "red", "[776,1.08e+03)" = "blue", "[1.08e+03,3.05e+03)" = "purple")
q$A2 <- cut(q$Mean,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[2]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Average Seasonal(June to Sept) Rainfall of India for 1901 to 1920 ")



##### Multiple Plot Function #####

multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols)    # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}
tiff(filename = "India_seasonal_1901-1920.tiff", pointsize =8, res = 600, units = "in", width = 8, height = 3, restoreConsole = TRUE, type = "cairo")
multiplot(chart[[2]], cols = 2)
dev.off()
jpeg(filename = "India_seasonal_1901-1920.jpeg", pointsize =8, res = 600, units = "in", width = 8, height = 2, restoreConsole = TRUE, type = "windows")
multiplot(chart[[2]] ,cols = 2)
dev.off()




q<-read.csv("India_mean_1990-2015_seasonal.csv")
chart<-list()
w <- c(0,439,776,1.08e+03,3.05e+03)

cols <- c("[0,439)" = "yellow", "[439,776)" = "red", "[776,1.08e+03)" = "blue", "[1.08e+03,3.05e+03)" = "purple")
q$A1 <- cut(q$Mean,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[1]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Average Seasonal (June to Sept) Rainfall of India for 1990 to 2014")


cols <- c("[0,439)" = "yellow", "[439,776)" = "red", "[776,1.08e+03)" = "blue", "[1.08e+03,3.05e+03)" = "purple")
q$A2 <- cut(q$Mean,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[2]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Average Seasonal(June to Sept) Rainfall of India for 1990 to 2014")



##### Multiple Plot Function #####

multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols)    # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}
tiff(filename = "India_seasonal_1990-2014.tiff", pointsize =8, res = 600, units = "in", width = 8, height = 3, restoreConsole = TRUE, type = "cairo")
multiplot(chart[[2]], cols = 2)
dev.off()
jpeg(filename = "India_seasonal_1990-2014.jpeg", pointsize =8, res = 600, units = "in", width = 8, height = 2, restoreConsole = TRUE, type = "windows")
multiplot(chart[[2]] ,cols = 2)
dev.off()















q<-read.csv("Brahmaputra_Seasonal_Difference_1920-1990.csv")
chart<-list()
w <- c(4,42,71,106,235)

cols <- c("[4,42)" = "yellow", "[42,71)" = "red", "[71,106)" = "blue", "[106,235)" = "purple")
q$A1 <- cut(q$Mean,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("11-9-2019-3772456")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[1]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="(June-Sept)Mean Difference of Brahmaputra River basin (1901-1920,1990-2015)")


cols <- c("[4,42)" = "yellow", "[42,71)" = "red", "[71,106)" = "blue", "[106,235)" = "purple")
q$A2 <- cut(q$Mean,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("11-9-2019-3772456")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[2]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="(June-Sept) Mean Difference of Brahmaputra River basin (1901-1920,1990-2015) ")



##### Multiple Plot Function #####

multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols)    # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}
tiff(filename = "Brahmputa_river_1920-1990diff.tiff", pointsize =8, res = 600, units = "in", width = 8, height = 3, restoreConsole = TRUE, type = "cairo")
multiplot(chart[[2]], cols = 2)
dev.off()
jpeg(filename = "Brahmaputa_river_1920-1990diff.jpeg", pointsize =8, res = 600, units = "in", width = 8, height = 2, restoreConsole = TRUE, type = "windows")
multiplot(chart[[2]] ,cols = 2)
dev.off()



########################################Complete##########################################################################


