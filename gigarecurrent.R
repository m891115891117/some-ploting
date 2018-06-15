
source("http://bioconductor.org/biocLite.R")
biocLite("gaia")


source("http://bioconductor.org/biocLite.R")
biocLite("qvalue")



files <- dir("recurrent focal CNV/",pattern = ".*male.CNV*",full.names = T)


filelist=list()

for(i in 1:length(files)){
  filelist[[i]]= read.table(files[[i]],header = T)
  if(filelist[[i]]$log2>=0.585){
    filelist[[i]]$log2<-"0"
  }
  if(filelist[[i]]$log2<-1.1){
    filelist[[i]]$log2<-"0"
  }
}

rb<- do.call(rbind,filelist)






for(i in 1:length(files)){
  filelist[[i]]= read.table(files[[i]],header = T)
}





################################




