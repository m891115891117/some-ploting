#Pengze Ma

library(ggplot2)
library(dplyr)
#import base sequence quality data and divide to R1 R2  
#files <- dir('.', pattern = '.Per_base_sequence_quality.txt', full.names = TRUE)
R1 <- dir('.', pattern = 'R1_fastqc.Per_base_sequence_quality.txt', full.names = TRUE)
R2 <- dir('.', pattern = 'R2_fastqc.Per_base_sequence_quality.txt', full.names = TRUE)


#MultiQC plot function
MultiQC_Plot <- function(arg1,inputRead,breaks){
  if(inputRead=="R1"){
    inputRead<- "forward reads"
  }
  else if(inputRead=="R2"){
    inputRead<- "reverse reads"
  }
  ggplot()+geom_line(data=arg1,aes(x=V1,y=as.numeric(as.character(V2)),group=arg1$Sample,colour=Sample))+
    geom_rect(aes(xmin = 0, ymin = 10, ymax = 20, xmax = Inf),fill = "red", alpha = 0.2)+
    geom_rect(aes(xmin = 0, ymin = 20, ymax = 28, xmax = Inf),fill = "yellow", alpha = 0.2)+
    geom_rect(aes(xmin = 0, ymin = 28, ymax = Inf, xmax = Inf),fill = "#00AFBB", alpha = 0.2)+
    scale_x_discrete(breaks = breaks)+
    labs(title = paste("Per base sequence quality for ",inputRead), x = "Position in read (pb)",
         y = "Mean quality scores",
         subtitle = "Red: low quality zone")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color=NULL))+
    scale_y_continuous(expand = c(0, 0))
  #ggsave(arg2,filename=paste("MultiQCplot",j,".pdf",sep=""),width=10, height=6)
}

#defined empty lists
res=list()
filenames<- list()


MultiQC_R1_R2_separator <-function(input){
#get the dynamic input name in order to assign the output file name
get_Vname<-substitute(input)  
#redefine input 
files<-input
#read all base quality data in list res,then 
#trim header,convert V1 into string,generate only first two column
#add a 3rd column,which is each sample name, as a group column



for(i in 1:length(files))
{
  res[[i]]=read.table(files[[i]], header = F, fill = NA,stringsAsFactors = F)
  res[[i]]=res[[i]][-1,]
  res[[i]]$V1<- factor(res[[i]]$V1, levels=unique(res[[i]]$V1))
  res[[i]] <- res[[i]][,1:2]
  filenames[[i]] <- strsplit(files[[i]],"/")[[1]]
  filenames[[i]] <- grep("txt$",filenames[[i]],value = TRUE,perl = TRUE)
  filenames[[i]]<-strsplit(filenames[[i]],"\\.")[[1]]
  filenames[[i]] <-grep("fastqc$",filenames[[i]],value = TRUE,perl=TRUE)
  res[[i]]$Sample<- rep(filenames[[i]])
}

#define two file groups: x*10 & y+10, to
#make x figures, each figure contain 10 files
#make 2 figures, each figure contain:
#ceiling((y+10)/2) & y+10-ceiling((y+10)/2) respectively 
n<-length(files)  

#give x axis break scope
nlev <- nlevels(res[[1]]$V1)
breaks<-scales::extended_breaks()(1:nlev)[-1] %>%c(1, ., nlev) %>% res[[1]]$V1[.] %>% as.vector()

#plot function
if(n>20){
  n1<- n-n%%10-10
  n2<-n%%10+10  
  n2.1<-ceiling(n2/2)
  n2.2<-n2-n2.1
  #define 2 figures(y+10) data
  #resleft<-res[c(n1:(length(files)-1))]
  resleft<-res[c((n1+1):length(files))]
  resleft1<-resleft[c(1:n2.1)]
  resleft2<-resleft[c((n2.1+1):length(resleft))]
  #resleft2<-resleft[c(n2.1:length(resleft))]
  resleft1.plotdata<-do.call(rbind,resleft1)
  resleft2.plotdata<-do.call(rbind,resleft2)
  
  #draw figures:x*10 part
  all_res=list()
  qcplot=list()
  
  for(j in 1:(n1/10)){
    all_res[[j]]<- do.call(rbind,res[c((10*j-9):(10*j))])
    qcplot[[j]]<- MultiQC_Plot(all_res[[j]],get_Vname,breaks)
    ggsave(qcplot[[j]],filename=paste("MultiQCplot",j,"_",get_Vname,".pdf",sep=""),width=10, height=6)
    ggsave(qcplot[[j]],filename=paste("MultiQCplotPNG",j,"_",get_Vname,".png",sep=""),width=10, height=6)
  }
  #draw figures:y+10 part
  MultiQC_end1<-MultiQC_Plot(resleft1.plotdata,get_Vname,breaks)
  MultiQC_end2<-MultiQC_Plot(resleft2.plotdata,get_Vname,breaks)
  ggsave(MultiQC_end1,filename=paste("MultiQCEnd01_",get_Vname,".pdf",sep=""), width=10, height=6)
  ggsave(MultiQC_end1,filename=paste("MultiQCEnd01PNG_",get_Vname,".png",sep=""), width=10, height=6)
  ggsave(MultiQC_end2,filename=paste("MultiQCEnd02_",get_Vname,".pdf",sep=""), width=10, height=6)
  ggsave(MultiQC_end2,filename=paste("MultiQCEnd02PNG_",get_Vname,".png",sep = ""), width=10, height=6)
}else if(n>=10 & n<=20){
  n1<- ceiling(n/2)
  n2<- n-n1
  res_Case2<-res[c(1:(length(files)))]
  res_One<-res_Case2[c(1:n1)]
  res_Two<-res_Case2[c((n1+1):length(res_Case2))]
  res_One.plotdata<-do.call(rbind,res_One)
  res_Two.plotdata<-do.call(rbind,res_Two)
  MultiQC_01<-MultiQC_Plot(res_One.plotdata,get_Vname,breaks)
  MultiQC_02<-MultiQC_Plot(res_Two.plotdata,get_Vname,breaks)
  ggsave(MultiQC_01,filename=paste("MultiQC01_",get_Vname,".pdf",sep = ""), width=10, height=6)
  ggsave(MultiQC_01,filename=paste("MultiQC01PNG_",get_Vname,".png",sep = ""), width=10, height=6)
  ggsave(MultiQC_02,filename=paste("MultiQC02_",get_Vname,".pdf",sep = ""), width=10, height=6)
  ggsave(MultiQC_02,filename=paste("MultiQC02PNG_",get_Vname,".png",sep = ""), width=10, height=6)
}else{
  onfigure_Data<-do.call(rbind,res)
  Onefigure_Case <-MultiQC_Plot(onfigure_Data,get_Vname,breaks)   
  ggsave(Onefigure_Case,filename=paste("MultiQC_BaseQ_",get_Vname,".pdf",sep = ""), width=10, height=6)
  ggsave(Onefigure_Case,filename=paste("MultiQC_BaseQ_",get_Vname,".png",sep = ""), width=10, height=6)
}
#END MultiQC_R1_R2_separator
}


#MultiQC_R1 <- MultiQC_R1_R2_separator(R1)
MultiQC_R1_R2_separator(R1)
#MultiQC_R2 <- MultiQC_R1_R2_separator(R2)
MultiQC_R1_R2_separator(R2)



