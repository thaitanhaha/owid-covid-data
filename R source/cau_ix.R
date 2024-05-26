library("png")
suppressMessages(library(ggplot2))
suppressMessages(library(here))

setwd(here())
dataFile <- read.csv("owid-covid-data.csv")

dir.create("ix", showWarnings = FALSE)
setwd(here("ix"))
#ix
#1
cat("Cau ix.1\n")
sum_cases_VN <- sum(dataFile[which(dataFile$iso_code=="VNM" & dataFile$new_cases!=""),5])
sum_deaths_VN <- sum(dataFile[which(dataFile$iso_code=="VNM" & dataFile$new_deaths!=""),6])
sum_cases_IDN <- sum(dataFile[which(dataFile$iso_code=="IDN" & dataFile$new_cases!=""),5])
sum_deaths_IDN <- sum(dataFile[which(dataFile$iso_code=="IDN" & dataFile$new_deaths!=""),6])
sum_cases_JPN <- sum(dataFile[which(dataFile$iso_code=="JPN" & dataFile$new_cases!=""),5])
sum_deaths_JPN <- sum(dataFile[which(dataFile$iso_code=="JPN" & dataFile$new_deaths!=""),6])

VN <- subset(dataFile, dataFile$iso_code=="VNM")
VN <- as.data.frame(VN, stringsAsFactors = FALSE)
VN <- VN[order(as.Date(VN$date, format="%m/%d/%Y")),]
VN$iso_code <- NULL
VN$continent <- NULL
VN$location <- NULL
VN[is.na(VN)] <- 0
VN$new_cases <- VN$new_cases*100/sum_cases_VN
VN$new_cases <- cumsum(VN$new_cases)
VN$new_deaths <- VN$new_deaths*100/sum_deaths_VN
VN$new_deaths <- cumsum(VN$new_deaths)

IDN <- subset(dataFile, dataFile$iso_code=="IDN")
IDN <- as.data.frame(IDN, stringsAsFactors = FALSE)
IDN <- IDN[order(as.Date(IDN$date, format="%m/%d/%Y")),]
IDN$iso_code <- NULL
IDN$continent <- NULL
IDN$location <- NULL
IDN[is.na(IDN)] <- 0
IDN$new_cases <- IDN$new_cases*100/sum_cases_IDN
IDN$new_cases <- cumsum(IDN$new_cases)
IDN$new_deaths <- IDN$new_deaths*100/sum_deaths_IDN
IDN$new_deaths <- cumsum(IDN$new_deaths)

JPN <- subset(dataFile, dataFile$iso_code=="JPN")
JPN <- as.data.frame(JPN, stringsAsFactors = FALSE)
JPN <- JPN[order(as.Date(JPN$date, format="%m/%d/%Y")),]
JPN$iso_code <- NULL
JPN$continent <- NULL
JPN$location <- NULL
JPN[is.na(JPN)] <- 0
JPN$new_cases <- JPN$new_cases*100/sum_cases_JPN
JPN$new_cases <- cumsum(JPN$new_cases)
JPN$new_deaths <- JPN$new_deaths*100/sum_deaths_JPN
JPN$new_deaths <- cumsum(JPN$new_deaths)

png("Vietnam's accumulation.png")
VN_filler <- c("indianred1", "lawngreen")
VN_label <- c("deaths", "cases")
VN$date <- as.Date(VN$date, "%m/%d/%Y")
plot(VN$new_cases ~ VN$date, xlab = "date", ylab = "percent", type = "l", lwd = 3, xaxt = "n", col = "lawngreen", main = "Vietnam's accumulation")
lines(VN$new_deaths ~ VN$date, type = "l", lwd = 3, xaxt = "n", col = "indianred1")
axis(1, VN$date, format(VN$date, "%d/%m/%y"))
legend("bottomright", VN_label, fill = VN_filler)
dev.off()

png("Indonesia's accumulation.png")
IDN_filler <- c("hotpink1", "deepskyblue2")
IDN_label <- c("deaths", "cases")
IDN$date <- as.Date(IDN$date, "%m/%d/%Y")
plot(IDN$new_cases ~ IDN$date, xlab = "date", ylab = "percent", type = "l", lwd = 3, xaxt = "n", col = "deepskyblue2", main = "Indonesia's accumulation")
lines(IDN$new_deaths ~ IDN$date, type = "l", lwd = 3, xaxt = "n", col = "hotpink1")
axis(1, IDN$date, format(IDN$date, "%d/%m/%y"))
legend("bottomright", IDN_label, fill = IDN_filler)
dev.off()

png("Japan's accumulation.png")
JPN_filler <- c("tomato", "steelblue2")
JPN_label <- c("deaths", "cases")
JPN$date <- as.Date(JPN$date, "%m/%d/%Y")
plot(JPN$new_cases ~ JPN$date, xlab = "date", ylab = "percent", type = "l", lwd = 3, xaxt = "n", col = "steelblue2", main = "Japan's accumulation")
lines(JPN$new_deaths ~ JPN$date, type = "l", lwd = 3, xaxt = "n", col = "tomato")
axis(1, JPN$date, format(JPN$date, "%d/%m/%y"))
legend("bottomright", JPN_label, fill = JPN_filler)
dev.off()

#2
cat("Cau ix.2\n")

VN_2 <- subset(dataFile, dataFile$iso_code=="VNM")
VN_2 <- as.data.frame(VN_2, stringsAsFactors = FALSE)
VN_2 <- VN_2[order(as.Date(VN_2$date, format="%m/%d/%Y")),]
VN_2$date <- as.Date(VN_2$date, "%m/%d/%Y")
VN_2$iso_code <- NULL
VN_2$continent <- NULL
VN_2$location <- NULL
VN_2[is.na(VN_2)] <- 0

IDN_2 <- subset(dataFile, dataFile$iso_code=="IDN")
IDN_2 <- as.data.frame(IDN_2, stringsAsFactors = FALSE)
IDN_2 <- IDN_2[order(as.Date(IDN_2$date, format="%m/%d/%Y")),]
IDN_2$date <- as.Date(IDN_2$date, "%m/%d/%Y")
IDN_2$iso_code <- NULL
IDN_2$continent <- NULL
IDN_2$location <- NULL
IDN_2[is.na(IDN_2)] <- 0

JPN_2 <- subset(dataFile, dataFile$iso_code=="JPN")
JPN_2 <- as.data.frame(JPN_2, stringsAsFactors = FALSE)
JPN_2 <- JPN_2[order(as.Date(JPN_2$date, format="%m/%d/%Y")),]
JPN_2$date <- as.Date(JPN_2$date, "%m/%d/%Y")
JPN_2$iso_code <- NULL
JPN_2$continent <- NULL
JPN_2$location <- NULL
JPN_2[is.na(JPN_2)] <- 0

#01/2020
VN_01_2020 <- subset(VN_2, format(date, "%m-%Y")=="01-2020")
IDN_01_2020 <- subset(IDN_2, format(date, "%m-%Y")=="01-2020")
JPN_01_2020 <- subset(JPN_2, format(date, "%m-%Y")=="01-2020")
#03/2020
VN_03_2020 <- subset(VN_2, format(date, "%m-%Y")=="03-2020")
IDN_03_2020 <- subset(IDN_2, format(date, "%m-%Y")=="03-2020")
JPN_03_2020 <- subset(JPN_2, format(date, "%m-%Y")=="03-2020")
#04/2020
VN_04_2020 <- subset(VN_2, format(date, "%m-%Y")=="04-2020")
IDN_04_2020 <- subset(IDN_2, format(date, "%m-%Y")=="04-2020")
JPN_04_2020 <- subset(JPN_2, format(date, "%m-%Y")=="04-2020")
#05/2020
VN_05_2020 <- subset(VN_2, format(date, "%m-%Y")=="05-2020")
IDN_05_2020 <- subset(IDN_2, format(date, "%m-%Y")=="05-2020")
JPN_05_2020 <- subset(JPN_2, format(date, "%m-%Y")=="05-2020")

#01/2021
VN_01_2021 <- subset(VN_2, format(date, "%m-%Y")=="01-2021")
IDN_01_2021 <- subset(IDN_2, format(date, "%m-%Y")=="01-2021")
JPN_01_2021 <- subset(JPN_2, format(date, "%m-%Y")=="01-2021")
#03/2021
VN_03_2021 <- subset(VN_2, format(date, "%m-%Y")=="03-2021")
IDN_03_2021 <- subset(IDN_2, format(date, "%m-%Y")=="03-2021")
JPN_03_2021 <- subset(JPN_2, format(date, "%m-%Y")=="03-2021")
#04/2021
VN_04_2021 <- subset(VN_2, format(date, "%m-%Y")=="04-2021")
IDN_04_2021 <- subset(IDN_2, format(date, "%m-%Y")=="04-2021")
JPN_04_2021 <- subset(JPN_2, format(date, "%m-%Y")=="04-2021")
#05/2021
VN_05_2021 <- subset(VN_2, format(date, "%m-%Y")=="05-2021")
IDN_05_2021 <- subset(IDN_2, format(date, "%m-%Y")=="05-2021")
JPN_05_2021 <- subset(JPN_2, format(date, "%m-%Y")=="05-2021")

#01/2022
VN_01_2022 <- subset(VN_2, format(date, "%m-%Y")=="01-2022")
IDN_01_2022 <- subset(IDN_2, format(date, "%m-%Y")=="01-2022")
JPN_01_2022 <- subset(JPN_2, format(date, "%m-%Y")=="01-2022")


#VN_01_2020
r_VN_01_2020 <- cor(VN_01_2020$new_cases, VN_01_2020$new_deaths)
plot1 <- ggplot(VN_01_2020, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Vietnam's correlation in 01/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot1
#IDN_01_2020
r_IDN_01_2020 <- cor(IDN_01_2020$new_cases, IDN_01_2020$new_deaths)
plot2 <- ggplot(IDN_01_2020, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Indonesia's correlation in 01/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot2
#JPN_01_2020
r_JPN_01_2020 <- cor(JPN_01_2020$new_cases, JPN_01_2020$new_deaths)
plot3 <- ggplot(JPN_01_2020, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Japan's correlation in 01/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot3
#VN_03_2020
r_VN_03_2020 <- cor(VN_03_2020$new_cases, VN_03_2020$new_deaths)
plot4 <- ggplot(VN_03_2020, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Vietnam's correlation in 03/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot4
#IDN_03_2020
r_IDN_03_2020 <- cor(IDN_03_2020$new_cases, IDN_03_2020$new_deaths)
plot5 <- ggplot(IDN_03_2020, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Indonesia's correlation in 03/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot5
#JPN_03_2020
r_JPN_03_2020 <- cor(JPN_03_2020$new_cases, JPN_03_2020$new_deaths)
plot6 <- ggplot(JPN_03_2020, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Japan's correlation in 03/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot6
#VN_04_2020
r_VN_04_2020 <- cor(VN_04_2020$new_cases, VN_04_2020$new_deaths)
plot7 <- ggplot(VN_04_2020, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Vietnam's correlation in 04/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot7
#IDN_04_2020
r_IDN_04_2020 <- cor(IDN_04_2020$new_cases, IDN_04_2020$new_deaths)
plot8 <- ggplot(IDN_04_2020, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Indonesia's correlation in 04/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot8
#JPN_04_2020
r_JPN_04_2020 <- cor(JPN_04_2020$new_cases, JPN_04_2020$new_deaths)
plot9 <- ggplot(JPN_04_2020, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Japan's correlation in 04/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot9
#VN_05_2020
r_VN_05_2020 <- cor(VN_05_2020$new_cases, VN_05_2020$new_deaths)
plot10 <- ggplot(VN_05_2020, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Vietnam's correlation in 05/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot10
#IDN_05_2020
r_IDN_05_2020 <- cor(IDN_05_2020$new_cases, IDN_05_2020$new_deaths)
plot11 <- ggplot(IDN_05_2020, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Indonesia's correlation in 05/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot11
#JPN_05_2020
r_JPN_05_2020 <- cor(JPN_05_2020$new_cases, JPN_05_2020$new_deaths)
plot12 <- ggplot(JPN_05_2020, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Japan's correlation in 05/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot12
#VN_01_2021
r_VN_01_2021 <- cor(VN_01_2021$new_cases, VN_01_2021$new_deaths)
plot13 <- ggplot(VN_01_2021, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Vietnam's correlation in 01/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot13
#IDN_01_2021
r_IDN_01_2021 <- cor(IDN_01_2021$new_cases, IDN_01_2021$new_deaths)
plot14 <- ggplot(IDN_01_2021, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Indonesia's correlation in 01/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot14
#JPN_01_2021
r_JPN_01_2021 <- cor(JPN_01_2021$new_cases, JPN_01_2021$new_deaths)
plot15 <- ggplot(JPN_01_2021, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Japan's correlation in 01/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot15
#VN_03_2021
r_VN_03_2021 <- cor(VN_03_2021$new_cases, VN_03_2021$new_deaths)
plot16 <- ggplot(VN_03_2021, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Vietnam's correlation in 03/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot16
#IDN_03_2021
r_IDN_03_2021 <- cor(IDN_03_2021$new_cases, IDN_03_2021$new_deaths)
plot17 <- ggplot(IDN_03_2021, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Indonesia's correlation in 03/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot17
#JPN_03_2021
r_JPN_03_2021 <- cor(JPN_03_2021$new_cases, JPN_03_2021$new_deaths)
plot18 <- ggplot(JPN_03_2021, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Japan's correlation in 03/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot18
#VN_04_2021
r_VN_04_2021 <- cor(VN_04_2021$new_cases, VN_04_2021$new_deaths)
plot19 <- ggplot(VN_04_2021, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Vietnam's correlation in 04/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot19
#IDN_04_2021
r_IDN_04_2021 <- cor(IDN_04_2021$new_cases, IDN_04_2021$new_deaths)
plot20 <- ggplot(IDN_04_2021, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Indonesia's correlation in 04/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot20
#JPN_04_2021
r_JPN_04_2021 <- cor(JPN_04_2021$new_cases, JPN_04_2021$new_deaths)
plot21 <- ggplot(JPN_04_2021, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Japan's correlation in 04/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot21
#VN_05_2021
r_VN_05_2021 <- cor(VN_05_2021$new_cases, VN_05_2021$new_deaths)
plot22 <- ggplot(VN_05_2021, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Vietnam's correlation in 05/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot22
#IDN_05_2021
r_IDN_05_2021 <- cor(IDN_05_2021$new_cases, IDN_05_2021$new_deaths)
plot23 <- ggplot(IDN_05_2021, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Indonesia's correlation in 05/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot23
#JPN_05_2021
r_JPN_05_2021 <- cor(JPN_05_2021$new_cases, JPN_05_2021$new_deaths)
plot24 <- ggplot(JPN_05_2021, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Japan's correlation in 05/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot24
#VN_01_2022
r_VN_01_2022 <- cor(VN_01_2022$new_cases, VN_01_2022$new_deaths)
plot25 <- ggplot(VN_01_2022, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Vietnam's correlation in 01/2022")) + theme(plot.title = element_text(hjust = 0.5))
plot25
#IDN_01_2022
r_IDN_01_2022 <- cor(IDN_01_2022$new_cases, IDN_01_2022$new_deaths)
plot26 <- ggplot(IDN_01_2022, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Indonesia's correlation in 01/2022")) + theme(plot.title = element_text(hjust = 0.5))
plot26
#JPN_01_2022
r_JPN_01_2022 <- cor(JPN_01_2022$new_cases, JPN_01_2022$new_deaths)
plot27 <- ggplot(JPN_01_2022, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("Japan's correlation in 01/2022")) + theme(plot.title = element_text(hjust = 0.5))
plot27
pdf("Cau ix_2.pdf")
print(plot1)
print(plot2)
print(plot3)
print(plot4)
print(plot5)
print(plot6)
print(plot7)
print(plot8)
print(plot9)
print(plot10)
print(plot11)
print(plot12)
print(plot13)
print(plot14)
print(plot15)
print(plot16)
print(plot17)
print(plot18)
print(plot19)
print(plot20)
print(plot21)
print(plot22)
print(plot23)
print(plot24)
print(plot25)
print(plot26)
print(plot27)
dev.off()

cat("He so tuong quan cua VN thang 01/2020: ", r_VN_01_2020, "\n")
cat("He so tuong quan cua IDN thang 01/2020: ", r_IDN_01_2020, "\n")
cat("He so tuong quan cua JPN thang 01/2020: ", r_JPN_01_2020, "\n")
cat("He so tuong quan cua VN thang 03/2020: ", r_VN_03_2020, "\n")
cat("He so tuong quan cua IDN thang 03/2020: ", r_IDN_03_2020, "\n")
cat("He so tuong quan cua JPN thang 03/2020: ", r_JPN_03_2020, "\n")
cat("He so tuong quan cua VN thang 04/2020: ", r_VN_04_2020, "\n")
cat("He so tuong quan cua IDN thang 04/2020: ", r_IDN_04_2020, "\n")
cat("He so tuong quan cua JPN thang 04/2020: ", r_JPN_04_2020, "\n")
cat("He so tuong quan cua VN thang 05/2020: ", r_VN_05_2020, "\n")
cat("He so tuong quan cua IDN thang 05/2020: ", r_IDN_05_2020, "\n")
cat("He so tuong quan cua JPN thang 05/2020: ", r_JPN_05_2020, "\n")
cat("He so tuong quan cua VN thang 01/2021: ", r_VN_01_2021, "\n")
cat("He so tuong quan cua IDN thang 01/2021: ", r_IDN_01_2021, "\n")
cat("He so tuong quan cua JPN thang 01/2021: ", r_JPN_01_2021, "\n")
cat("He so tuong quan cua VN thang 03/2021: ", r_VN_03_2021, "\n")
cat("He so tuong quan cua IDN thang 03/2021: ", r_IDN_03_2021, "\n")
cat("He so tuong quan cua JPN thang 03/2021: ", r_JPN_03_2021, "\n")
cat("He so tuong quan cua VN thang 04/2021: ", r_VN_04_2021, "\n")
cat("He so tuong quan cua IDN thang 04/2021: ", r_IDN_04_2021, "\n")
cat("He so tuong quan cua JPN thang 04/2021: ", r_JPN_04_2021, "\n")
cat("He so tuong quan cua VN thang 05/2021: ", r_VN_05_2021, "\n")
cat("He so tuong quan cua IDN thang 05/2021: ", r_IDN_05_2021, "\n")
cat("He so tuong quan cua JPN thang 05/2021: ", r_JPN_05_2021, "\n")
cat("He so tuong quan cua VN thang 01/2022: ", r_VN_01_2022, "\n")
cat("He so tuong quan cua IDN thang 01/2022: ", r_IDN_01_2022, "\n")
cat("He so tuong quan cua JPN thang 01/2022: ", r_JPN_01_2022, "\n")

#3
cat("Cau ix.3\n")
avrg_7_days <- function(data1, data2){
  for (i in 1:length(data1)) {
    count = 1
    for (j in (i-1):(i-6)) {
      if (j>0) {
        count <- count+1
        data2[i] <- data2[i] + data1[j]
      }
    }
    data2[i] <- data2[i]/count
  }
  return(data2)
}

#01_2020
VN_01_2020_avrg <- VN_01_2020
VN_01_2020_avrg <- as.data.frame(VN_01_2020_avrg, stringsAsFactors = FALSE)
VN_01_2020_avrg$new_cases <- avrg_7_days(VN_01_2020$new_cases, VN_01_2020_avrg$new_cases)
VN_01_2020_avrg$new_deaths <- avrg_7_days(VN_01_2020$new_deaths, VN_01_2020_avrg$new_deaths) 

IDN_01_2020_avrg <- IDN_01_2020
#IDN_01_2020_avrg <- as.data.frame(IDN_01_2020_avrg, stringsAsFactors = FALSE)
#IDN_01_2020_avrg$new_cases <- avrg_7_days(IDN_01_2020$new_cases, IDN_01_2020_avrg$new_cases)
#IDN_01_2020_avrg$new_deaths <- avrg_7_days(IDN_01_2020$new_deaths, IDN_01_2020_avrg$new_deaths)

JPN_01_2020_avrg <- JPN_01_2020
JPN_01_2020_avrg <- as.data.frame(JPN_01_2020_avrg, stringsAsFactors = FALSE)
JPN_01_2020_avrg$new_cases <- avrg_7_days(JPN_01_2020$new_cases, JPN_01_2020_avrg$new_cases)
JPN_01_2020_avrg$new_deaths <- avrg_7_days(JPN_01_2020$new_deaths, JPN_01_2020_avrg$new_deaths)

#03_2020
VN_03_2020_avrg <- VN_03_2020
VN_03_2020_avrg <- as.data.frame(VN_03_2020_avrg, stringsAsFactors = FALSE)
VN_03_2020_avrg$new_cases <- avrg_7_days(VN_03_2020$new_cases, VN_03_2020_avrg$new_cases)
VN_03_2020_avrg$new_deaths <- avrg_7_days(VN_03_2020$new_deaths, VN_03_2020_avrg$new_deaths) 

IDN_03_2020_avrg <- IDN_03_2020
IDN_03_2020_avrg <- as.data.frame(IDN_03_2020_avrg, stringsAsFactors = FALSE)
IDN_03_2020_avrg$new_cases <- avrg_7_days(IDN_03_2020$new_cases, IDN_03_2020_avrg$new_cases)
IDN_03_2020_avrg$new_deaths <- avrg_7_days(IDN_03_2020$new_deaths, IDN_03_2020_avrg$new_deaths)

JPN_03_2020_avrg <- JPN_03_2020
JPN_03_2020_avrg <- as.data.frame(JPN_03_2020_avrg, stringsAsFactors = FALSE)
JPN_03_2020_avrg$new_cases <- avrg_7_days(JPN_03_2020$new_cases, JPN_03_2020_avrg$new_cases)
JPN_03_2020_avrg$new_deaths <- avrg_7_days(JPN_03_2020$new_deaths, JPN_03_2020_avrg$new_deaths)

#04_2020
VN_04_2020_avrg <- VN_04_2020
VN_04_2020_avrg <- as.data.frame(VN_04_2020_avrg, stringsAsFactors = FALSE)
VN_04_2020_avrg$new_cases <- avrg_7_days(VN_04_2020$new_cases, VN_04_2020_avrg$new_cases)
VN_04_2020_avrg$new_deaths <- avrg_7_days(VN_04_2020$new_deaths, VN_04_2020_avrg$new_deaths) 

IDN_04_2020_avrg <- IDN_04_2020
IDN_04_2020_avrg <- as.data.frame(IDN_04_2020_avrg, stringsAsFactors = FALSE)
IDN_04_2020_avrg$new_cases <- avrg_7_days(IDN_04_2020$new_cases, IDN_04_2020_avrg$new_cases)
IDN_04_2020_avrg$new_deaths <- avrg_7_days(IDN_04_2020$new_deaths, IDN_04_2020_avrg$new_deaths)

JPN_04_2020_avrg <- JPN_04_2020
JPN_04_2020_avrg <- as.data.frame(JPN_04_2020_avrg, stringsAsFactors = FALSE)
JPN_04_2020_avrg$new_cases <- avrg_7_days(JPN_04_2020$new_cases, JPN_04_2020_avrg$new_cases)
JPN_04_2020_avrg$new_deaths <- avrg_7_days(JPN_04_2020$new_deaths, JPN_04_2020_avrg$new_deaths)

#05_2020
VN_05_2020_avrg <- VN_05_2020
VN_05_2020_avrg <- as.data.frame(VN_05_2020_avrg, stringsAsFactors = FALSE)
VN_05_2020_avrg$new_cases <- avrg_7_days(VN_05_2020$new_cases, VN_05_2020_avrg$new_cases)
VN_05_2020_avrg$new_deaths <- avrg_7_days(VN_05_2020$new_deaths, VN_05_2020_avrg$new_deaths) 

IDN_05_2020_avrg <- IDN_05_2020
IDN_05_2020_avrg <- as.data.frame(IDN_05_2020_avrg, stringsAsFactors = FALSE)
IDN_05_2020_avrg$new_cases <- avrg_7_days(IDN_05_2020$new_cases, IDN_05_2020_avrg$new_cases)
IDN_05_2020_avrg$new_deaths <- avrg_7_days(IDN_05_2020$new_deaths, IDN_05_2020_avrg$new_deaths)

JPN_05_2020_avrg <- JPN_05_2020
JPN_05_2020_avrg <- as.data.frame(JPN_05_2020_avrg, stringsAsFactors = FALSE)
JPN_05_2020_avrg$new_cases <- avrg_7_days(JPN_05_2020$new_cases, JPN_05_2020_avrg$new_cases)
JPN_05_2020_avrg$new_deaths <- avrg_7_days(JPN_05_2020$new_deaths, JPN_05_2020_avrg$new_deaths)

#01_2021
VN_01_2021_avrg <- VN_01_2021
VN_01_2021_avrg <- as.data.frame(VN_01_2021_avrg, stringsAsFactors = FALSE)
VN_01_2021_avrg$new_cases <- avrg_7_days(VN_01_2021$new_cases, VN_01_2021_avrg$new_cases)
VN_01_2021_avrg$new_deaths <- avrg_7_days(VN_01_2021$new_deaths, VN_01_2021_avrg$new_deaths) 

IDN_01_2021_avrg <- IDN_01_2021
IDN_01_2021_avrg <- as.data.frame(IDN_01_2021_avrg, stringsAsFactors = FALSE)
IDN_01_2021_avrg$new_cases <- avrg_7_days(IDN_01_2021$new_cases, IDN_01_2021_avrg$new_cases)
IDN_01_2021_avrg$new_deaths <- avrg_7_days(IDN_01_2021$new_deaths, IDN_01_2021_avrg$new_deaths)

JPN_01_2021_avrg <- JPN_01_2021
JPN_01_2021_avrg <- as.data.frame(JPN_01_2021_avrg, stringsAsFactors = FALSE)
JPN_01_2021_avrg$new_cases <- avrg_7_days(JPN_01_2021$new_cases, JPN_01_2021_avrg$new_cases)
JPN_01_2021_avrg$new_deaths <- avrg_7_days(JPN_01_2021$new_deaths, JPN_01_2021_avrg$new_deaths)

#03_2021
VN_03_2021_avrg <- VN_03_2021
VN_03_2021_avrg <- as.data.frame(VN_03_2021_avrg, stringsAsFactors = FALSE)
VN_03_2021_avrg$new_cases <- avrg_7_days(VN_03_2021$new_cases, VN_03_2021_avrg$new_cases)
VN_03_2021_avrg$new_deaths <- avrg_7_days(VN_03_2021$new_deaths, VN_03_2021_avrg$new_deaths) 

IDN_03_2021_avrg <- IDN_03_2021
IDN_03_2021_avrg <- as.data.frame(IDN_03_2021_avrg, stringsAsFactors = FALSE)
IDN_03_2021_avrg$new_cases <- avrg_7_days(IDN_03_2021$new_cases, IDN_03_2021_avrg$new_cases)
IDN_03_2021_avrg$new_deaths <- avrg_7_days(IDN_03_2021$new_deaths, IDN_03_2021_avrg$new_deaths)

JPN_03_2021_avrg <- JPN_03_2021
JPN_03_2021_avrg <- as.data.frame(JPN_03_2021_avrg, stringsAsFactors = FALSE)
JPN_03_2021_avrg$new_cases <- avrg_7_days(JPN_03_2021$new_cases, JPN_03_2021_avrg$new_cases)
JPN_03_2021_avrg$new_deaths <- avrg_7_days(JPN_03_2021$new_deaths, JPN_03_2021_avrg$new_deaths)

#04_2021
VN_04_2021_avrg <- VN_04_2021
VN_04_2021_avrg <- as.data.frame(VN_04_2021_avrg, stringsAsFactors = FALSE)
VN_04_2021_avrg$new_cases <- avrg_7_days(VN_04_2021$new_cases, VN_04_2021_avrg$new_cases)
VN_04_2021_avrg$new_deaths <- avrg_7_days(VN_04_2021$new_deaths, VN_04_2021_avrg$new_deaths) 

IDN_04_2021_avrg <- IDN_04_2021
IDN_04_2021_avrg <- as.data.frame(IDN_04_2021_avrg, stringsAsFactors = FALSE)
IDN_04_2021_avrg$new_cases <- avrg_7_days(IDN_04_2021$new_cases, IDN_04_2021_avrg$new_cases)
IDN_04_2021_avrg$new_deaths <- avrg_7_days(IDN_04_2021$new_deaths, IDN_04_2021_avrg$new_deaths)

JPN_04_2021_avrg <- JPN_04_2021
JPN_04_2021_avrg <- as.data.frame(JPN_04_2021_avrg, stringsAsFactors = FALSE)
JPN_04_2021_avrg$new_cases <- avrg_7_days(JPN_04_2021$new_cases, JPN_04_2021_avrg$new_cases)
JPN_04_2021_avrg$new_deaths <- avrg_7_days(JPN_04_2021$new_deaths, JPN_04_2021_avrg$new_deaths)

#05_2021
VN_05_2021_avrg <- VN_05_2021
VN_05_2021_avrg <- as.data.frame(VN_05_2021_avrg, stringsAsFactors = FALSE)
VN_05_2021_avrg$new_cases <- avrg_7_days(VN_05_2021$new_cases, VN_05_2021_avrg$new_cases)
VN_05_2021_avrg$new_deaths <- avrg_7_days(VN_05_2021$new_deaths, VN_05_2021_avrg$new_deaths) 

IDN_05_2021_avrg <- IDN_05_2021
IDN_05_2021_avrg <- as.data.frame(IDN_05_2021_avrg, stringsAsFactors = FALSE)
IDN_05_2021_avrg$new_cases <- avrg_7_days(IDN_05_2021$new_cases, IDN_05_2021_avrg$new_cases)
IDN_05_2021_avrg$new_deaths <- avrg_7_days(IDN_05_2021$new_deaths, IDN_05_2021_avrg$new_deaths)

JPN_05_2021_avrg <- JPN_05_2021
JPN_05_2021_avrg <- as.data.frame(JPN_05_2021_avrg, stringsAsFactors = FALSE)
JPN_05_2021_avrg$new_cases <- avrg_7_days(JPN_05_2021$new_cases, JPN_05_2021_avrg$new_cases)
JPN_05_2021_avrg$new_deaths <- avrg_7_days(JPN_05_2021$new_deaths, JPN_05_2021_avrg$new_deaths)

#01_2022
VN_01_2022_avrg <- VN_01_2022
VN_01_2022_avrg <- as.data.frame(VN_01_2022_avrg, stringsAsFactors = FALSE)
VN_01_2022_avrg$new_cases <- avrg_7_days(VN_01_2022$new_cases, VN_01_2022_avrg$new_cases)
VN_01_2022_avrg$new_deaths <- avrg_7_days(VN_01_2022$new_deaths, VN_01_2022_avrg$new_deaths) 

IDN_01_2022_avrg <- IDN_01_2022
IDN_01_2022_avrg <- as.data.frame(IDN_01_2022_avrg, stringsAsFactors = FALSE)
IDN_01_2022_avrg$new_cases <- avrg_7_days(IDN_01_2022$new_cases, IDN_01_2022_avrg$new_cases)
IDN_01_2022_avrg$new_deaths <- avrg_7_days(IDN_01_2022$new_deaths, IDN_01_2022_avrg$new_deaths)

JPN_01_2022_avrg <- JPN_01_2022
JPN_01_2022_avrg <- as.data.frame(JPN_01_2022_avrg, stringsAsFactors = FALSE)
JPN_01_2022_avrg$new_cases <- avrg_7_days(JPN_01_2022$new_cases, JPN_01_2022_avrg$new_cases)
JPN_01_2022_avrg$new_deaths <- avrg_7_days(JPN_01_2022$new_deaths, JPN_01_2022_avrg$new_deaths)

#VN_01_2020
r_VN_01_2020_avrg <- cor(VN_01_2020_avrg$new_cases, VN_01_2020_avrg$new_deaths)
plot1 <- ggplot(VN_01_2020_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Vietnam's correlation in 01/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot1
#IDN_01_2020
r_IDN_01_2020_avrg <- cor(IDN_01_2020_avrg$new_cases, IDN_01_2020_avrg$new_deaths)
plot2 <- ggplot(IDN_01_2020_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Indonesia's correlation in 01/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot2
#JPN_01_2020
r_JPN_01_2020_avrg <- cor(JPN_01_2020_avrg$new_cases, JPN_01_2020_avrg$new_deaths)
plot3 <- ggplot(JPN_01_2020_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Japan's correlation in 01/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot3
#VN_03_2020
r_VN_03_2020_avrg <- cor(VN_03_2020_avrg$new_cases, VN_03_2020_avrg$new_deaths)
plot4 <- ggplot(VN_03_2020_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Vietnam's correlation in 03/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot4
#IDN_03_2020
r_IDN_03_2020_avrg <- cor(IDN_03_2020_avrg$new_cases, IDN_03_2020_avrg$new_deaths)
plot5 <- ggplot(IDN_03_2020_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Indonesia's correlation in 03/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot5
#JPN_03_2020
r_JPN_03_2020_avrg <- cor(JPN_03_2020_avrg$new_cases, JPN_03_2020_avrg$new_deaths)
plot6 <- ggplot(JPN_03_2020_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Japan's correlation in 03/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot6
#VN_04_2020
r_VN_04_2020_avrg <- cor(VN_04_2020_avrg$new_cases, VN_04_2020_avrg$new_deaths)
plot7 <- ggplot(VN_04_2020_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Vietnam's correlation in 04/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot7
#IDN_04_2020
r_IDN_04_2020_avrg <- cor(IDN_04_2020_avrg$new_cases, IDN_04_2020_avrg$new_deaths)
plot8 <- ggplot(IDN_04_2020_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Indonesia's correlation in 04/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot8
#JPN_04_2020
r_JPN_04_2020_avrg <- cor(JPN_04_2020_avrg$new_cases, JPN_04_2020_avrg$new_deaths)
plot9 <- ggplot(JPN_04_2020_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Japan's correlation in 04/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot9
#VN_05_2020
r_VN_05_2020_avrg <- cor(VN_05_2020_avrg$new_cases, VN_05_2020_avrg$new_deaths)
plot10 <- ggplot(VN_05_2020_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Vietnam's correlation in 05/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot10
#IDN_05_2020
r_IDN_05_2020_avrg <- cor(IDN_05_2020_avrg$new_cases, IDN_05_2020_avrg$new_deaths)
plot11 <- ggplot(IDN_05_2020_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Indonesia's correlation in 05/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot11
#JPN_05_2020
r_JPN_05_2020_avrg <- cor(JPN_05_2020_avrg$new_cases, JPN_05_2020_avrg$new_deaths)
plot12 <- ggplot(JPN_05_2020_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Japan's correlation in 05/2020")) + theme(plot.title = element_text(hjust = 0.5))
plot12
#VN_01_2021
r_VN_01_2021_avrg <- cor(VN_01_2021_avrg$new_cases, VN_01_2021_avrg$new_deaths)
plot13 <- ggplot(VN_01_2021_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Vietnam's correlation in 01/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot13
#IDN_01_2021
r_IDN_01_2021_avrg <- cor(IDN_01_2021_avrg$new_cases, IDN_01_2021_avrg$new_deaths)
plot14 <- ggplot(IDN_01_2021_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Indonesia's correlation in 01/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot14
#JPN_01_2021
r_JPN_01_2021_avrg <- cor(JPN_01_2021_avrg$new_cases, JPN_01_2021_avrg$new_deaths)
plot15 <- ggplot(JPN_01_2021_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Japan's correlation in 01/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot15
#VN_03_2021
r_VN_03_2021_avrg <- cor(VN_03_2021_avrg$new_cases, VN_03_2021_avrg$new_deaths)
plot16 <- ggplot(VN_03_2021_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Vietnam's correlation in 03/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot16
#IDN_03_2021
r_IDN_03_2021_avrg <- cor(IDN_03_2021_avrg$new_cases, IDN_03_2021_avrg$new_deaths)
plot17 <- ggplot(IDN_03_2021_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Indonesia's correlation in 03/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot17
#JPN_03_2021
r_JPN_03_2021_avrg <- cor(JPN_03_2021_avrg$new_cases, JPN_03_2021_avrg$new_deaths)
plot18 <- ggplot(JPN_03_2021_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Japan's correlation in 03/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot18
#VN_04_2021
r_VN_04_2021_avrg <- cor(VN_04_2021_avrg$new_cases, VN_04_2021_avrg$new_deaths)
plot19 <- ggplot(VN_04_2021_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Vietnam's correlation in 04/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot19
#IDN_04_2021
r_IDN_04_2021_avrg <- cor(IDN_04_2021_avrg$new_cases, IDN_04_2021_avrg$new_deaths)
plot20 <- ggplot(IDN_04_2021_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Indonesia's correlation in 04/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot20
#JPN_04_2021
r_JPN_04_2021_avrg <- cor(JPN_04_2021_avrg$new_cases, JPN_04_2021_avrg$new_deaths)
plot21 <- ggplot(JPN_04_2021_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Japan's correlation in 04/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot21
#VN_05_2021
r_VN_05_2021_avrg <- cor(VN_05_2021_avrg$new_cases, VN_05_2021_avrg$new_deaths)
plot22 <- ggplot(VN_05_2021_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Vietnam's correlation in 05/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot22
#IDN_05_2021
r_IDN_05_2021_avrg <- cor(IDN_05_2021_avrg$new_cases, IDN_05_2021_avrg$new_deaths)
plot23 <- ggplot(IDN_05_2021_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Indonesia's correlation in 05/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot23
#JPN_05_2021
r_JPN_05_2021_avrg <- cor(JPN_05_2021_avrg$new_cases, JPN_05_2021_avrg$new_deaths)
plot24 <- ggplot(JPN_05_2021_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Japan's correlation in 05/2021")) + theme(plot.title = element_text(hjust = 0.5))
plot24
#VN_01_2022
r_VN_01_2022_avrg <- cor(VN_01_2022_avrg$new_cases, VN_01_2022_avrg$new_deaths)
plot25 <- ggplot(VN_01_2022_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Vietnam's correlation in 01/2022")) + theme(plot.title = element_text(hjust = 0.5))
plot25
#IDN_01_2022
r_IDN_01_2022_avrg <- cor(IDN_01_2022_avrg$new_cases, IDN_01_2022_avrg$new_deaths)
plot26 <- ggplot(IDN_01_2022_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Indonesia's correlation in 01/2022")) + theme(plot.title = element_text(hjust = 0.5))
plot26
#JPN_01_2022
r_JPN_01_2022_avrg <- cor(JPN_01_2022_avrg$new_cases, JPN_01_2022_avrg$new_deaths)
plot27 <- ggplot(JPN_01_2022_avrg, aes(x=new_cases, y=new_deaths)) + geom_point() + theme_bw() + xlab("cases") + ylab("deaths") + ggtitle(paste0("7-day average Japan's correlation in 01/2022")) + theme(plot.title = element_text(hjust = 0.5))
plot27
pdf("Cau ix_3.pdf")
print(plot1)
print(plot2)
print(plot3)
print(plot4)
print(plot5)
print(plot6)
print(plot7)
print(plot8)
print(plot9)
print(plot10)
print(plot11)
print(plot12)
print(plot13)
print(plot14)
print(plot15)
print(plot16)
print(plot17)
print(plot18)
print(plot19)
print(plot20)
print(plot21)
print(plot22)
print(plot23)
print(plot24)
print(plot25)
print(plot26)
print(plot27)
dev.off()

cat("He so tuong quan cua VN thang 01/2020 theo trung binh 7 ngay gan nhat: ", r_VN_01_2020_avrg, "\n")
cat("He so tuong quan cua IDN thang 01/2020 theo trung binh 7 ngay gan nhat: ", NA, "\n")
cat("He so tuong quan cua JPN thang 01/2020 theo trung binh 7 ngay gan nhat: ", r_JPN_01_2020_avrg, "\n")
cat("He so tuong quan cua VN thang 03/2020 theo trung binh 7 ngay gan nhat: ", r_VN_03_2020_avrg, "\n")
cat("He so tuong quan cua IDN thang 03/2020 theo trung binh 7 ngay gan nhat: ", r_IDN_03_2020_avrg, "\n")
cat("He so tuong quan cua JPN thang 03/2020 theo trung binh 7 ngay gan nhat: ", r_JPN_03_2020_avrg, "\n")
cat("He so tuong quan cua VN thang 04/2020 theo trung binh 7 ngay gan nhat: ", r_VN_04_2020_avrg, "\n")
cat("He so tuong quan cua IDN thang 04/2020 theo trung binh 7 ngay gan nhat: ", r_IDN_04_2020_avrg, "\n")
cat("He so tuong quan cua JPN thang 04/2020 theo trung binh 7 ngay gan nhat: ", r_JPN_04_2020_avrg, "\n")
cat("He so tuong quan cua VN thang 05/2020 theo trung binh 7 ngay gan nhat: ", r_VN_05_2020_avrg, "\n")
cat("He so tuong quan cua IDN thang 05/2020 theo trung binh 7 ngay gan nhat: ", r_IDN_05_2020_avrg, "\n")
cat("He so tuong quan cua JPN thang 05/2020 theo trung binh 7 ngay gan nhat: ", r_JPN_05_2020_avrg, "\n")
cat("He so tuong quan cua VN thang 01/2021 theo trung binh 7 ngay gan nhat: ", r_VN_01_2021_avrg, "\n")
cat("He so tuong quan cua IDN thang 01/2021 theo trung binh 7 ngay gan nhat: ", r_IDN_01_2021_avrg, "\n")
cat("He so tuong quan cua JPN thang 01/2021 theo trung binh 7 ngay gan nhat: ", r_JPN_01_2021_avrg, "\n")
cat("He so tuong quan cua VN thang 03/2021 theo trung binh 7 ngay gan nhat: ", r_VN_03_2021_avrg, "\n")
cat("He so tuong quan cua IDN thang 03/2021 theo trung binh 7 ngay gan nhat: ", r_IDN_03_2021_avrg, "\n")
cat("He so tuong quan cua JPN thang 03/2021 theo trung binh 7 ngay gan nhat: ", r_JPN_03_2021_avrg, "\n")
cat("He so tuong quan cua VN thang 04/2021 theo trung binh 7 ngay gan nhat: ", r_VN_04_2021_avrg, "\n")
cat("He so tuong quan cua IDN thang 04/2021 theo trung binh 7 ngay gan nhat: ", r_IDN_04_2021_avrg, "\n")
cat("He so tuong quan cua JPN thang 04/2021 theo trung binh 7 ngay gan nhat: ", r_JPN_04_2021_avrg, "\n")
cat("He so tuong quan cua VN thang 05/2021 theo trung binh 7 ngay gan nhat: ", r_VN_05_2021_avrg, "\n")
cat("He so tuong quan cua IDN thang 05/2021 theo trung binh 7 ngay gan nhat: ", r_IDN_05_2021_avrg, "\n")
cat("He so tuong quan cua JPN thang 05/2021 theo trung binh 7 ngay gan nhat: ", r_JPN_05_2021_avrg, "\n")
cat("He so tuong quan cua VN thang 01/2022 theo trung binh 7 ngay gan nhat: ", r_VN_01_2022_avrg, "\n")
cat("He so tuong quan cua IDN thang 01/2022 theo trung binh 7 ngay gan nhat: ", r_IDN_01_2022_avrg, "\n")
cat("He so tuong quan cua JPN thang 01/2022 theo trung binh 7 ngay gan nhat: ", r_JPN_01_2022_avrg, "\n")

suppressWarnings(detach("package:png", unload=TRUE))
suppressWarnings(detach("package:ggplot2", unload=TRUE))
suppressWarnings(detach("package:here", unload=TRUE))