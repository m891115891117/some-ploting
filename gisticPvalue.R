setwd("D://Working/百度项目/recurrent focal CNV/516_all_GISTIC_recurrent/example_results")

#mm <- read.table("amp_genes.conf_90.txt",sep = "\t",fill = T,header = T)
mm <- read.table("amp_genes.conf_90.txt",sep = "\t",fill = T,header = F,check.names = FALSE)
nn<- as.data.frame(t(mm))
nn<- nn[-nrow(nn),]
nn<- nn[-1,]
nn$V19<-format(as.numeric(as.character(nn$V2)),scientific = FALSE)

cc <- read.table("del_genes.conf_90.txt",sep = "\t",fill = T,header = F,check.names = FALSE)
dd<- as.data.frame(t(cc))
dd<- dd[-nrow(dd),]
dd<-dd[-1,]
dd$V19<-format(as.numeric(as.character(dd$V2)),scientific = FALSE)




