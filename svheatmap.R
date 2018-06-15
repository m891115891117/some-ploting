setwd("D:////Working/百度项目/SV hotspots stats/")
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
library(gtools)

files <- dir(pattern = 'chr.*10M.txt', full.names = TRUE)
#natural sort
files<- mixedsort(files)
#files2 <- dir(path = "1Mbin/",pattern = 'chr.*out.*.txt', full.names = TRUE)
sv=list()
meltSV<- list()
p<-list()
sampName<-list()
mm<-list()
samname<-list()
meltMean<-list()
Line<- list()
for(i in 1:length(files)){
  sv[[i]]=read.table(files[[i]],header = TRUE,sep = "\t",check.names = FALSE)
  meltSV[[i]]=melt(sv[[i]])
  meltSV[[i]]<-meltSV[[i]][,-2]
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
  meltMean[[i]]<-aggregate(meltSV[[i]]$SV, by=list(meltSV[[i]]$bin), FUN=mean)
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
          plot.margin = unit(c(3,0,0,0),"mm"))
}
#arrangeGrob将所有 list里的图合并在一起
svGrob=do.call(arrangeGrob,c(p,nrow=1))
#grid.draw,将合并的图画出
#grid.draw(svGrob)

lineGrob=do.call(arrangeGrob,c(Line,nrow=1))
#grid.draw(lineGrob)
Grob=arrangeGrob(lineGrob,svGrob,nrow=2,heights=c(1,10),padding = unit(0,"line"))
grid.draw(Grob)

sv <- read.table("chr16outtest10M.txt",header = TRUE,sep = "\t",check.names = FALSE)

meltsv <- melt(sv)
meltsv <- meltsv[,-2]
names(meltsv)[2:3] <- c("bin", "SV")
mm<-aggregate(meltsv$SV, by=list(meltsv$bin), FUN=mean)
meltsv <- merge(meltsv,mm)
meltsv <- meltsv[,-4]

ggplot(mm,aes(x=bin,y=m,group=1))+
  geom_line(colour="blue",size=0.2)


+geom_abline(linetype="dashed",aes(slope=0,intercept=0))




 
# ggplot(meltsv, aes(bin,Sample,fill=SV))+geom_tile()+
#   theme(axis.text=element_blank(),
#         axis.title = element_blank(),
#         legend.title = element_blank(),
#         legend.text = element_blank(),
#         legend.position="none",
#         panel.grid=element_blank(),
#         axis.ticks=element_blank(),
#         panel.background=element_blank(),
#         plot.margin = unit(c(0,0,10,0),"mm"))
# 
# p<-ggplot(meltsv, aes(bin,Sample,fill=SV))+geom_tile()+
#   ylab("List of Sample ") +
#   xlab("List of Bin") +
#   theme(legend.title = element_text(size = 10),
#         legend.text = element_text(size = 0.1),
#         plot.title = element_text(size=10),
#         axis.text.y=element_text(size=1.8),
#         axis.text.x = element_text(size = 3,angle = 15),
#         panel.grid=element_blank(),
#         axis.ticks=element_blank(),
#         panel.background=element_blank())
# ggsave(p,filename="sv.pdf", width=4, height=10,limitsize = FALSE)
# 
# 
# theme(legend.title = element_text(size = 10),
#       legend.text = element_text(size = 0.1),
#       plot.title = element_text(size=16),
#       axis.title=element_text(size=1,face="bold"),
#       axis.text.x = element_text(angle = 40, hjust = 0.1)) +
#   labs(fill = "SV count")
# 
# 
# ggplot(meltsv, aes(bin,Sample,fill=SV))+geom_raster(size=0.25)+
#   theme(axis.text=element_blank(),
#         panel.grid=element_blank(),
#         axis.ticks=element_blank(),
#         panel.background=element_blank())
# 
# 
# 
# p<- ggplot(meltsv, aes(bin, Sample,fill=SV )) +
#   geom_tile(aes(size=10), color = "white") +
#   scale_fill_gradient(low = "white", high = "steelblue") +
#   ylab("List of Sample ") +
#   xlab("List of Bin") +
#   theme(legend.title = element_text(size = 10),
#         legend.text = element_text(size = 0.1),
#         plot.title = element_text(size=16),
#         axis.title=element_text(size=1,face="bold"),
#         axis.text.x = element_text(angle = 40, hjust = 0.1)) +
#   labs(fill = "SV count")
# 
# 
# ggplot(meltsv, aes(bin, Sample)) + geom_tile(aes(fill = SV),colour = "white")+
#   scale_fill_gradient(low ="white",high ="steelblue")+
#   theme(axis.text=element_blank(),
#         panel.grid=element_blank(),
#         axis.ticks=element_blank(),
#         panel.background=element_blank())
#   
#   
#   
#   theme(axis.text.y =element_text(size=3),axis.title=element_text(size=14,face="bold"))
# 
#   dev.off()
#   ###
#   
#   for(i in 1:length(files)){
#     sv[[i]]=read.table(files[[i]],header = TRUE,sep = "\t",check.names = FALSE)
#     meltSV[[i]]=melt(sv[[i]])
#     meltSV[[i]]<-meltSV[[i]][,-2]
#     names(meltSV[[i]])[2:3] <- c("bin", "SV")
#     p[[i]]<-ggplot(meltSV[[i]], aes(bin,Sample,fill=SV))+geom_tile()+
#       ylab("List of Sample ") +
#       xlab("List of Bin") +
#       theme(legend.title = element_text(size = 10),
#             legend.text = element_text(size = 0.1),
#             plot.title = element_text(size=10),
#             axis.text.y=element_text(size=1.8),
#             axis.text.x = element_text(size = 3,angle = 15),
#             panel.grid=element_blank(),
#             axis.ticks=element_blank(),
#             panel.background=element_blank())
#   }
# 
