library(caret)
library(dplyr)
library(plyr)
library(RANN)
library(stringr)
library(gridExtra)
rm(list = ls())
setwd("/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/graphics/acc")
set.seed(123)
x2 <-rnorm(50,0.1,0.4) 
y2 <-rnorm(50,0.1,0.4)  
z2 <- 1
minority <- cbind(x2,y2,z2)
rm(x2,y2,z2)
minority <- as.data.frame(minority)
names(minority) <- c("x1", "x2", "class")
minority$class <- factor(minority$class, levels = c('0', '1', '2', '3', '4'))

#keep in mind, point is nn to itself. Choose k one higher as actual nb
visualizeSMOTE <- function(i, minority, k = 6, n = 3){ 
  original_point <- i
  nearest <- nn2(minority[minority$class == 1,c('x1', 'x2')],
                 minority[minority$class == 1,c('x1', 'x2')], k = k)
  
  minority[original_point,"class"] <- 2
  minority[nearest$nn.idx[original_point, ],][-1,"class"] <- 3
  
  points <- minority[minority$class == 3 | minority$class == 2, c('x1', 'x2', 'class')]
  
  interpolate <- function(df, nvariables){
    random_neighbors <- sample(2:nrow(df), n, replace = TRUE)
    output <- matrix(ncol=nvariables, nrow=length(random_neighbors))
    index <- 1
    for (i in (random_neighbors)){
      output[index,] <- as.matrix(cbind(points[1,c('x1', 'x2')]+runif(1,0,1)*(points[i,c('x1', 'x2')]
                                                                              -points[1,c('x1', 'x2')]),4))
      index <- index + 1
    }
    output <- as.data.frame(output)
    names(output) <- c("x1", "x2", "class")
    output <- rbind(df, output)
    
    return(output)
  }
  interpolations <- interpolate(points, 3)
  minority <- rbind(minority, interpolations[interpolations$class == 4,])
  
  set.seed(123)
  x1 <-rnorm(5000,.6,0.3) 
  x2 <-rnorm(5000,.6,0.3)
  class <- 0
  random_data <- as.data.frame(rbind(cbind(x1, x2, class)))
  names(random_data) <- c('x1', 'x2', 'class')
  combined <- rbind(random_data, minority)
  
  combined$class <- as.factor(combined$class)
  levels(combined$class) <- c('Majority Class', 'Minority Class', 'Random Min. Point', 'Nearest Neighbors', 'SMOTE Interpolations')
  library(ggplot2)
  ggplot(data = combined, aes(x = x1, y = x2, color = as.factor(class)))+
    geom_point(data = subset(combined, class == 'Majority Class'))+
    geom_point(data = subset(combined, class == 'Minority Class'))+
    geom_point(data = subset(combined, class == 'Random Min. Point'), size = 2)+
    geom_point(data = subset(combined, class == 'Nearest Neighbors'), size = 2)+
    geom_point(data = subset(combined, class == 'SMOTE Interpolations'), size = 2)+
    scale_color_manual(values = c('grey', 'black', 'orange', '#1f78b4','#b2df8a'))+
    labs(colour = "Class", title = "SMOTE on Random Normally Distributed Data", subtitle = paste('Step ',i,', Minority ~ N(.1,.4), Majority ~ N(.6,.3),',
                                                                                                 ' k =',k,', n =',n,sep=""))+
    theme_dark()
  ggsave(paste('knn',str_pad(original_point, 3, pad = "0"),'.png', sep = ""), device = 'png', dpi = 100, height = 6, width = 7)
  
  ggplot(data = combined, aes(x = x1, y = x2, color = class))+
    geom_point(data = subset(combined, class == 'Majority Class'))+
    geom_point(data = subset(combined, class == 'Minority Class'))+
    geom_point(data = subset(combined, class == 'Random Min. Point'), size = 2)+
    geom_point(data = subset(combined, class == 'Nearest Neighbors'), size = 2)+
    geom_point(data = subset(combined, class == 'SMOTE Interpolations'), size = 2)+
    scale_color_manual(values = c('grey', 'black', 'orange', '#1f78b4','#b2df8a'))+
    coord_cartesian(xlim = c(min(interpolations$x1)-0.05, max(interpolations$x1)+0.05),
                    ylim = c(min(interpolations$x2)-0.05,max(interpolations$x2)+0.05))+
    labs(colour = "Class", title = "SMOTE on Random Normally Distributed Data (zoomed)", subtitle = paste('Step ',i,', Minority ~ N(.1,.4), Majority ~ N(.6,.3),',
                                                                                                          ' k =',k,', n =',n,sep=""))+
    theme_dark()
  ggsave(paste('knn_zoom',str_pad(original_point, 3, pad = "0"),'.png', sep = ""), device = 'png', dpi = 200, height = 6, width = 7)
  minority$class <- as.factor(1)
  minority$class <- factor(minority$class, levels = c(0,1,2,3,4))
  return(minority)
}

for (x in seq(1,50)){
  minority <- visualizeSMOTE(x, minority, k = 5, n = 4)
}

