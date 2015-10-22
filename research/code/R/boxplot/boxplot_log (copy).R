library(reshape2)
library(ggplot2)
library(gridExtra)

rm(list=ls())
project <- c('ambari','camel','derby','wicket','all')
path = "/home/keisukefujino/Desktop/R_data"
setwd("/home/keisukefujino/Desktop/R_data")
table.all<-{}
table.dev<-{}
table.chu<-{}
table.time1<-{}
table.time2<-{}
df.table.all<-{}
df.table.dev<-{}
df.table.chu<-{}
df.table.time1<-{}
df.table.time2<-{}
g.dev<-{}
g.chu<-{}
g.time1<-{}
g.time2<-{}


#create table which entitled each project name
file.names <- dir(path, pattern=".csv")
for (i in 1:length(file.names)){
  table.all[[i]] <- read.csv(file.names[i],sep = ",",header=TRUE)[,c(2,6,10,15,16)]
  table.dev[[i]] <- read.csv(file.names[i],sep = ",",header=TRUE)[,c(2,6)]
  table.chu[[i]] <- read.csv(file.names[i],sep = ",",header=TRUE)[,c(2,10)]
  table.time1[[i]] <- read.csv(file.names[i],sep = ",",header=TRUE)[,c(2,15)]
  table.time2[[i]] <- read.csv(file.names[i],sep = ",",header=TRUE)[,c(2,16)]
  df.table.all[[i]] <- melt(table.all[[i]],id="bug_type")
  df.table.dev[[i]] <- melt(table.dev[[i]],id="bug_type")
  df.table.chu[[i]] <- melt(table.chu[[i]],id="bug_type")
  df.table.time1[[i]] <- melt(table.time1[[i]],id="bug_type")
  df.table.time2[[i]] <- melt(table.time2[[i]],id="bug_type")
}
####Log scale
for (j in 1:length(file.names)){
  j
gg<-NULL
  g.dev[[j]] <- ggplot(df.table.dev[[j]],aes (x = bug_type,y = log(1+value))) + ggtitle("# of Developer who changed files")+ geom_boxplot(outlier.colour = "black",outlier.shape = 4,outlier.size = 4)
  g.chu[[j]] <- ggplot(df.table.chu[[j]],aes (x = bug_type,y = log(1+value))) + ggtitle("Code churn")+ geom_boxplot(outlier.colour = "black",outlier.shape = 4,outlier.size = 4)
  g.time1[[j]] <- ggplot(df.table.time1[[j]],aes (x = bug_type,y = log(1+value))) + ggtitle("Open to Last Commit (Days)")+ geom_boxplot(outlier.colour = "black",outlier.shape = 4,outlier.size = 4)                                      
  g.time2[[j]] <- ggplot(df.table.time2[[j]],aes (x = bug_type,y = log(1+value))) + ggtitle("Open to Resolved (Days)")+ geom_boxplot(outlier.colour = "black",outlier.shape = 4,outlier.size = 4)
  pdf(paste(paste(path,project[j],sep="/"),"_boxplot_hours_log.pdf",sep=""),width=12,height=10)
  gg <- grid.arrange(g.dev[[j]],g.chu[[j]],g.time1[[j]],g.time2[[j]],ncol=2,nrow=2,top=project[j])
  dev.off()
}

