

library("xlsx")
library(ggplot2)
library(reshape2)
myqc <- read.table("combine.txt",sep="\t",header = T)

myqct <- as.data.frame(t(myqc))

myqct <- as.data.frame(t(myqc),col.names=as.character(myqc[,1]))

colnames(myqct) <- as.character(unlist(myqct[1,]))
myqct<-myqct[-1,]

myframe<- as.data.frame(cbind(as.character(myqct$`Coverage >=1x`),
                              as.character(myqct$`Coverage >=4x`),
                              as.character(myqct$`Coverage >=10x`),
                              as.character(myqct$`Coverage >=20x`)),
                        make.row.names=as.character(row.names(myqct)))

names(myframe) <- c("Coverage >=1x","Coverage >=4x","Coverage >=10x","Coverage >=20x")


myframe$`Coverage >=1x`<- as.numeric(sub("%", "",myframe$`Coverage >=1x`,fixed=TRUE))/100
myframe$`Coverage >=4x`<- as.numeric(sub("%", "",myframe$`Coverage >=4x`,fixed=TRUE))/100
myframe$`Coverage >=10x`<- as.numeric(sub("%", "",myframe$`Coverage >=10x`,fixed=TRUE))/100
myframe$`Coverage >=20x`<- as.numeric(sub("%", "",myframe$`Coverage >=20x`,fixed=TRUE))/100


mymeltframe<- melt(myframe)
mymeltframe$variable<-as.character(mymeltframe$variable)

mymeltframe$variable[mymeltframe$variable=="Coverage >=1x"] <- "1x"
mymeltframe$variable[mymeltframe$variable=="Coverage >=4x"] <- "4x"
mymeltframe$variable[mymeltframe$variable=="Coverage >=10x"] <- "10x"
mymeltframe$variable[mymeltframe$variable=="Coverage >=20x"] <- "20x"
mymeltframe$variable<- factor(mymeltframe$variable,levels = unique(mymeltframe$variable))


covplot <-ggplot(mymeltframe, aes(variable, value,fill=variable))+geom_boxplot(colour="grey38")+
  scale_y_continuous(name = "coverage rate",limits=c(0.80, 1),breaks = seq(0.80, 1,0.05 ))+
  scale_x_discrete(name = "")+
  ggtitle("Boxplot of coverage rate in defferent level")+
  theme_classic()
covplot

#ggsave(covplot,filename="coverage_boxplot.pdf", width=10, height=6)



