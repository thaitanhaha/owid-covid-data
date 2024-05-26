library(stringr)
suppressMessages(library(here))
setwd(here())
dataFile <- read.csv("owid-covid-data.csv")

#10
cat("Cau i.10:", "\n")
dte <- table(dataFile$date)
dte <- as.data.frame(dte)
dte_min <- dte$Var1[dte$Freq==min(dte$Freq)]
cat("Cac ngay co luong du lieu thu thap nho nhat la: ", levels(droplevels(dte_min)), "\n")
cat("Luong du lieu thu thap nho nhat la: ", min(dte$Freq), "\n")
#11
cat("Cau i.11:", "\n")
dte_max <- dte$Var1[dte$Freq==max(dte$Freq)]
cat("Cac ngay co luong du lieu thu thap lon nhat la: ", levels(droplevels(dte_max)), "\n")
cat("Luong du lieu thu thap lon nhat la: ", max(dte$Freq), "\n")
#12
cat("Cau i.12:", "\n")
cont_dte <- table(dataFile$continent, dataFile$date)
cont_dte <- as.data.frame(cont_dte, stringsAsFactors = FALSE)
cont_dte$continent <- cont_dte$Var1
cont_dte$date <- cont_dte$Var2
cont_dte$Number_of_Data <- cont_dte$Freq
cont_dte$Var1 <- NULL
cont_dte$Var2 <- NULL
cont_dte$Freq <- NULL
cont_dte = subset(cont_dte, cont_dte$continent != "")
View(cont_dte)
#13
cat("Cau i.13:", "\n")
cat("So luong du lieu thu thap lon nhat theo date va chau luc la: ", max(cont_dte$Number_of_Data), "\n")
#14
cat("Cau i.14:", "\n")
cat("So luong du lieu thu thap nho nhat theo date va chau luc la: ", min(cont_dte$Number_of_Data), "\n")
#15
cat("Cau i.15:", "\n")
k = readline("Nhap k: ")

t = readline("Nhap t: ")

val <- cont_dte$Number_of_Data[(cont_dte$date==k) & (cont_dte$continent==t)]
cat("So luong du lieu thu thap duoc trong ngay",k,"o chau luc",t, "la: ", val, "\n")
#16
cat("Cau i.16:", "\n")
loc <- table(dataFile$iso_code)
loc <- as.data.frame(loc, stringsAsFactors = FALSE)
country1 <- loc[order(loc$Freq),]
country <- subset(country1, duplicated(Freq) | duplicated(Freq, fromLast = TRUE))
colnames(country) <- c("iso_code", "Num_of_Data")
print(country, row.names = FALSE)
#17
cat("Cau i.17", "\n")
i_c <- table(dataFile$iso_code, dataFile$location)
i_c <- as.data.frame(i_c)
i_l <- subset(i_c, i_c$Freq!=0)
i_l <- as.data.frame(i_l, stringsAsFactors = FALSE)
i <- subset(i_l, str_length(i_l$Var1)>3)
i <- as.data.frame(i)
i$Freq <- NULL
colnames(i) <- c("iso_code", "location")
cat("iso_code cua nhung dat nuoc co do dai iso_code lon hon 3: \n")
print(i, row.names = FALSE)

suppressWarnings(detach("package:stringr", unload=TRUE))
suppressWarnings(detach("package:here", unload=TRUE))