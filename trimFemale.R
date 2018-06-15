
#setwd("D:////Working/百度项目/SV hotspots stats/1Mbin/")
setwd("D:////Working/百度项目/SV hotspots stats/include repeat 1M bin/")
library(dplyr)

baidufemale<- read.table("baidu_female_list.txt",header = F,check.names = F)

chrY <- read.table("chrYouttest.txt",header = T,check.names = F)

subfemale<-chrY[chrY$Sample %in% baidufemale$V1,]
chrY[chrY$Sample %in% baidufemale$V1,-1] <- 0



subfemale2<-chrY[chrY$Sample %in% baidufemale$V1,]

#chrY[chrY$Sample=="TI1706140199LD01",]


#"TI1706140199LD01"

write.table(chrY,"chrYouttest.txt",quote = FALSE,sep = "\t",row.names = FALSE)

