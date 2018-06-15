
library(ggplot2)
setwd("D://Working//西南医院QC/")

aa <- read.table("R17010975LU03.insertSize.txt_onlyinsert.txt",header=TRUE,sep="\t")

files <- dir(pattern = '*insertSize.txt_onlyinsert.txt', full.names = TRUE)
names(aa)<- c("insertSize","count")
bb<-ggplot(data=aa)+geom_bar(aes(x=aa$insertSize,y=count),stat="identity",fill="red")+
  scale_y_continuous(name = "Count",limits=c(0, 320000),
                     breaks = seq(0, 320000, 50000),expand = c(0,0))+
  scale_x_continuous(name = "Insert Size",limits=c(0, 500),
                     breaks = seq(0, 500, 50),expand = c(0,0))+theme_classic()+
  ggtitle("Insert Size for all read in R17010975LU03")+
  geom_vline(data=aa, aes(xintercept=168),
             linetype="dashed")
  
ggsave(bb,filename="Insert Size R17010975LU03.pdf", width=10, height=6)


aa<-list()
p<- list()
for(i in 1:length(files)){
  aa[[i]]=read.table(files[[i]],header = TRUE,sep = "\t",check.names = FALSE)
  names(aa[[i]]) <- c("insertSize","count")
  p[[i]]<- ggplot(data=aa[[i]])+
    geom_bar(aes(x=aa[[i]]$insertSize,y=aa[[i]]count),stat="identity",fill="red")+
    scale_y_continuous(name = "Count",limits=c(0, 300000),breaks = seq(0, 310000, 50000),expand = c(0,0))+
    scale_x_continuous(name = "Insert Size",limits=c(0, 500), breaks = seq(0, 500, 50),expand = c(0,0))+
    theme_classic()+ggtitle("Insert Size for all read in ")
}





