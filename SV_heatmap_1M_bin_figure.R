#Pengze Ma
setwd()
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
library(gtools)
library(dplyr)
library(plyr)
library("rowr")
sv=list()
meltSV<- list()
p<-list()
sampName<-list()
mm<-list()
samname<-list()
meltMean<-list()
Line<- list()
meltMeantmp<-list()
files <- dir(pattern = 'chr.*out.*.txt', full.names = TRUE)
files<- mixedsort(files)


for(i in 1:length(files)){
  sv[[i]]=read.table(files[[i]],header = TRUE,check.names = FALSE)
  meltSV[[i]]=melt(sv[[i]])
  names(meltSV[[i]])[2:3] <- c("bin", "SV")
  
  #add chr name
  pattern <- "chr([0-9]|[0-9][0-9]|X|Y)"
  mm[[i]] <- regexpr(pattern, files[[i]])
  samname[[i]]<-regmatches(files[[i]], mm[[i]])
  
  p[[i]]<-ggplot(meltSV[[i]], aes(bin,Sample,fill=SV))+geom_tile()+
    theme(axis.text=element_blank(),
          axis.title.x = element_text(size=10),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.text = element_blank(),
          legend.position="none",
          panel.grid=element_blank(),
          axis.ticks=element_blank(),
          panel.background=element_blank(),
          plot.margin = unit(c(0,0,10,0),"mm"))+
    scale_x_discrete(name=samname[[i]])+
    scale_fill_gradient(low = "lightskyblue1", high = "red")
  
  #draw line
  meltMean[[i]]<-aggregate(meltSV[[i]]$SV, by=list(meltSV[[i]]$bin), FUN=sum)
  
  meltMeantmp[[i]]<-as.data.frame(aggregate(meltSV[[i]]$SV, by=list(meltSV[[i]]$bin), FUN=sum))
  names(meltMean[[i]])<- c("bin","mean")

    Line[[i]]<-ggplot(meltMean[[i]],aes(x=bin,y=mean,group=1))+
    geom_line(colour="blue",size=0.2)+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title = element_blank(),
          legend.title = element_blank(),
          legend.text = element_blank(),
          legend.position="none",
          panel.grid=element_blank(),
          axis.ticks=element_blank(),
          panel.background=element_blank(),
          plot.margin = unit(c(3,0,0,0),"mm"))+
    scale_y_continuous(limits=c(0, 500))
}



#arrangeGrob将所有 list里的图合并在一起
svGrob=do.call(arrangeGrob,c(p,nrow=1))
#grid.draw,将合并的图画出
#grid.draw(svGrob)

lineGrob=do.call(arrangeGrob,c(Line,nrow=1))
#grid.draw(lineGrob)
Grob=arrangeGrob(lineGrob,svGrob,nrow=2,heights=c(1,10),padding = unit(0,"line"))
grid.draw(Grob)

