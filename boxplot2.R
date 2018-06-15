#Pengze Ma
#Draw Boxplot of mean depth for tumor/normal sample


library("xlsx")
library(ggplot2)


mytable <- read.xlsx("516pairs_QC.xlsx",header = T,sheetIndex = 1)
wgcid <- as.character(mytable$WGC.ID)
mytable$type=substr(wgcid,11,11)
mytable$type[mytable$type == "T"] <- "Tumor"
mytable$type[mytable$type == "N"] <- "Normal"
mytable$coverage<-mytable$Total.throughput.Gb.*1000000000/(3137161264-239850717)



myplot<-ggplot(mytable,aes(x=type,y=coverage,fill=type))+
  geom_boxplot(colour="grey38")+
  scale_y_continuous(name = "Depth",limits=c(0, 170),breaks = seq(0, 170, 20))+
  scale_x_discrete(name = "516 Normal Samples/516 Tumor Samples")+
  ggtitle("Boxplot of average depth between Normal/Tumor samples")+
  theme_classic()

ggsave(myplot,filename="Tumo_normal_mean_depth_boxplot.pdf", width=10, height=6)
#ggsave(myplot,filename="aaa.pdf", width=10, height=6)

######joint boxplot
library(reshape2)


mydata<- mytable[,18:22]
mymelt<- melt(mydata)
mymelt$variable<-as.character(mymelt$variable)

mymelt$variable[mymelt$variable == "Coverage...1x.."] <- "1x"
mymelt$variable[mymelt$variable == "Coverage...4x"] <- "4x"
mymelt$variable[mymelt$variable == "Coverage...10x"] <- "10x"
mymelt$variable[mymelt$variable == "Coverage...20x"] <- "20x"
mymelt$variable<-factor(mymelt$variable,levels = unique(mymelt$variable))

coverageplot <-ggplot(mymelt, aes(variable, value,fill=type))+geom_boxplot(colour="grey38")+
  scale_y_continuous(name = "coverage rate",limits=c(0.85, 1),breaks = seq(0.85, 1,0.05 ))+
  scale_x_discrete(name = "")+
  ggtitle("Boxplot of coverage rate in defferent level")+
  theme_classic()


ggsave(coverageplot,filename="allcoverage_boxplot.pdf", width=10, height=6)








