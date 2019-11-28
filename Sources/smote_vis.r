library(caret)
library(dplyr)
library(plyr)
library(sf)
library(RANN)
library(gridExtra)
setwd("/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/graphics/")
rm(list = ls())
visualizeSMOTE <- function(i){
original_point <- i

set.seed(123)
x2 <-rnorm(50,0.1,0.1) 
y2 <-rnorm(50,0.1,0.1)  
z2 <- 1

random_data <- cbind(x2,y2,z2)


random_data <- as.data.frame(random_data)
names(random_data) <- c("x1", "x2", "class")
random_data$class <- factor(random_data$class, levels = c('0', '1', '2', '3', '4'))

head(random_data)


nearest <- nn2(random_data[random_data$class == 1,c('x1', 'x2')],
               random_data[random_data$class == 1,c('x1', 'x2')], k = 5)

random_data[original_point,"class"] <- 2
random_data[nearest$nn.idx[original_point, ],][-1,"class"] <- 3

points <- random_data[nearest$nn.idx[original_point, ], c('x1', 'x2')]
points$class <- 3
points[1,"class"] <- 2
points

interpolate <- function(df, nvariables){
  output <- matrix(ncol=nvariables, nrow=nrow(df)-1)
  for (i in (2:nrow(df))){
    output[i-1,] <- as.matrix(cbind(points[1,c('x1', 'x2')]+runif(1,0,1)*(points[i,c('x1', 'x2')]-points[1,c('x1', 'x2')]),4))
  }
  output <- as.data.frame(output)
  names(output) <- c("x1", "x2", "class")
  output <- rbind(df, output)
  return(output)
}
interpolations <- interpolate(points, 3)

table(random_data$class)
table(interpolations$class)

minority <- rbind(random_data, interpolations[interpolations$class == 4,])

set.seed(123)
x1 <-rnorm(5000,.4,0.1) 
x2 <-rnorm(5000,.4,0.1)
class <- 0
random_data <- rbind(cbind(x1, x2, class))

combined <- rbind(random_data, minority)

ggplot(data = interpolations, aes(x = x1, y = x2, group = as.factor(class), color = as.factor(class)))+
  geom_point()+
  geom_line(aes(group = class, colour = as.factor(class)))
  #geom_point(aes(group = adjuster, color = adjuster, shape = adjuster))

table(combined$class)
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
  labs(colour = "Class", title = "SMOTE on Random Normally Distributed Data")+
  theme_dark()
ggsave(paste('knn',original_point,'.png', sep = ""), device = 'png', dpi = 300, width = 10, height = 10)

ggplot(data = combined, aes(x = x1, y = x2, color = class))+
  geom_point(data = subset(combined, class == 'Majority Class'))+
  geom_point(data = subset(combined, class == 'Minority Class'))+
  geom_point(data = subset(combined, class == 'Random Min. Point'), size = 2)+
  geom_point(data = subset(combined, class == 'Nearest Neighbors'), size = 2)+
  geom_point(data = subset(combined, class == 'SMOTE Interpolations'), size = 2)+
  scale_color_manual(values = c('grey', 'black', 'orange', '#1f78b4','#b2df8a'))+
  coord_cartesian(xlim = c(min(interpolations$x1)-0.05, max(interpolations$x1)+0.05),
                  ylim = c(min(interpolations$x2)-0.05,max(interpolations$x2)+0.05))+
  labs(colour = "Class")+
  theme_dark()
ggsave(paste('knn_zoom',original_point,'.png', sep = ""), device = 'png', dpi = 300, width = 10, height = 10)
}

for (x in seq(1,6)){
  visualizeSMOTE(x)
}

            