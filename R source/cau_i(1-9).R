library(readr)
library(magrittr)
suppressMessages(library(dplyr))
suppressMessages(library(here))
setwd(here())
dataFile <- read.csv("owid-covid-data.csv")

##################i.1
i1<-function()
{
  cat("---Cau i.1:\n")
  time <- dataFile %>% select(date)
  mdy <- strptime(time$date,format="%m/%d/%Y")
  year <- format(mdy,"%Y")
  cat("Tap mau du lieu thu duoc vao cac nam: ",unique(year),"\n")
}
i1()
#################i.2
i2<-function()
{
  cat("---Cau i.2:\n")
  isoCode<-dataFile$iso_code
  cnames<-dataFile$location
  conn<-dataFile$continent
  i2.1<-data.frame(isoCode,cnames,conn,stringsAsFactors=FALSE)
  i2.2<-subset(i2.1, i2.1$conn!="")
  a<-unique(i2.2)
  index<-dim(a)[1]
  data1<-a[1:10,c(1,2)]
  colnames(data1)<-c("iso_code:","Country")
  rownames(data1)<-c("1","2","3","4","5","6","7","8","9","10")
  prmatrix(data1, left = TRUE, quote = FALSE)
  cat("Count: ",index,"\n")
}
i2()

###################i.3
i3<-function()
{
  cat("---Cau i.3:\n")
  Con <- dataFile %>% select(continent)
  Con<-unique(Con)
  Con <- Con[Con != ""]
  Con<-sort(Con, decreasing = FALSE)
  Trans<-c("Chau Phi", "Chau A", "Chau Au", "Nam My", "Chau Dai Duong", "Bac My")
  m<-data.frame(unlist(Con),unlist(Trans),stringsAsFactors = FALSE)
  colnames(m)<-c("Continent:","6")
  rownames(m)<-c("1","2","3","4","5","6")
  prmatrix(m, left = TRUE, quote = FALSE)
}
i3()

###############i.4
i4<-function()
{
  cat("---Cau i.4:\n")
  formatedData <- dataFile %>% filter(nchar(as.character(continent))>0)
  table <- formatedData %>% group_by(continent) %>% summarise(observation = length(continent))
  ti4<-sum(table$observation)
  a<-c("Tong:",ti4)
  table<-rbind(table,a)
  suppressWarnings(rownames(table)<-c("1","2","3","4","5","6","7"))
  prmatrix(table, left = TRUE, quote = FALSE)
  return(table)
}
table<-i4()
###############i.5
i5<-function()
{
  cat("---Cau i.5:\n")
  formatedData <- dataFile %>% filter(nchar(as.character(continent))>0)
  table2 <- formatedData %>% group_by(iso_code) %>% summarise(observation = length(iso_code))
  table2.1<-table2[216:225,c(1,2)]
  ti5 <- sum(table2$observation)
  total<-c("Tong:",ti5)
  table2.1<-rbind(table2.1,total)
  suppressWarnings(rownames(table2.1)<-c("1","2","3","4","5","6","7","8","9","10","11"))
  prmatrix(table2.1, left = TRUE, quote = FALSE)
  return(table2<- formatedData %>% group_by(iso_code, location) %>% summarise(observation = length(iso_code)))
}
i5data <-i5()
##################i.6
i6<-function(table)
{
  cat("---Cau i.6:\n")
  mini6<-min(as.numeric(table$observation))
  tmini6 <- table %>% filter(observation == min(as.numeric(observation)))
  cat("Chau luc co luong thu thap du lieu nho nhat la",tmini6$continent,"va gia tri nho nhat do la",tmini6$observation,"\n")
}
i6(table)
###################i.7
i7<-function(table)
{
  cat("---Cau i.7:\n")
  cuttable<-table[1:6,c(1,2)]
  maxi7<-max(as.numeric(cuttable$observation))
  tmaxi7 <- cuttable %>% filter(observation == max(as.numeric(observation)))
  cat("Chau luc co luong thu thap du lieu lon nhat la",tmaxi7$continent,"va gia tri lon nhat do la",tmaxi7$observation,"\n")
}
i7(table)
##################i.8
i8<-function(i5data)
{
  cat("---Cau i.8:\n")
  minData <- min(as.numeric(i5data$observation))
  i8result <- i5data %>% filter (as.numeric(observation)==minData)
  colnames(i8result)<-c("iso_code","Country Name", "Min observation")
  i8result<-i8result[c(2,3)]
  prmatrix(i8result, left = TRUE, quote = FALSE)
}
i8(i5data)
##################i.9
i9<-function(i5data)
{
  cat("---Cau i.9:\n")
  maxData <- max(as.numeric(i5data$observation))
  i9result <- i5data %>% filter (as.numeric(observation)==maxData)
  colnames(i9result)<-c("iso_code","Country Name","Max observation")
  i9result<-i9result[c(2,3)]
  prmatrix(i9result, left = TRUE, quote = FALSE)
}
i9(i5data)

suppressWarnings(detach("package:readr", unload=TRUE))
suppressWarnings(detach("package:magrittr", unload=TRUE))
suppressWarnings(detach("package:dplyr", unload=TRUE))
suppressWarnings(detach("package:here", unload=TRUE))
