suppressMessages(library(readr))
suppressMessages(library(here))
setwd(here())
dir.create("vi", showWarnings = FALSE)
data <- read_csv("owid-covid-data.csv", show_col_types = FALSE)

iso_code <- data$iso_code
continent <- data$continent
location <- data$location
date <- data$date
new_cases <- data$new_cases
new_deaths <- data$new_deaths

datetime <- strptime(date[], format="%m/ %d/ %Y")

# change working directory
setwd(here("vi"))

#du lieu newcases
new.dat1 <- data.frame(location, datetime, new_cases)

#loc theo nuoc
Indonesia_nc <- new.dat1[new.dat1$location == "Indonesia",]
Japan_nc <- new.dat1[new.dat1$location == "Japan",]
Vietnam_nc <- new.dat1[new.dat1$location == "Vietnam",]

#newcases th?ng 1 2020
Indonesia_nc_1_2020 <- na.omit(Indonesia_nc[Indonesia_nc$datetime >= "2020-01-01" & Indonesia_nc$datetime <= "2020-01-31",])


Japan_nc_1_2020 <-  na.omit(Japan_nc[Japan_nc$datetime >= "2020-01-01" & Japan_nc$datetime <= "2020-01-31",])
avg_nc_Jp_1_2020 <- c()
avg_nc_Jp_1_2020[1] <- Japan_nc_1_2020$new_cases[1]
avg_nc_Jp_1_2020[2] <- (Japan_nc_1_2020$new_cases[1] + Japan_nc_1_2020$new_cases[2])/2
avg_nc_Jp_1_2020[3] <- (Japan_nc_1_2020$new_cases[1] + Japan_nc_1_2020$new_cases[2] + Japan_nc_1_2020$new_cases[3])/3
avg_nc_Jp_1_2020[4] <- (Japan_nc_1_2020$new_cases[1] + Japan_nc_1_2020$new_cases[2] + Japan_nc_1_2020$new_cases[3] + Japan_nc_1_2020$new_cases[4])/4
avg_nc_Jp_1_2020[5] <- (Japan_nc_1_2020$new_cases[1] + Japan_nc_1_2020$new_cases[2] + Japan_nc_1_2020$new_cases[3] + Japan_nc_1_2020$new_cases[4] + Japan_nc_1_2020$new_cases[5])/5
avg_nc_Jp_1_2020[6] <- (Japan_nc_1_2020$new_cases[1] + Japan_nc_1_2020$new_cases[2] + Japan_nc_1_2020$new_cases[3] + Japan_nc_1_2020$new_cases[4] + Japan_nc_1_2020$new_cases[5] + Japan_nc_1_2020$new_cases[6])/6
for(i in 7:length(Japan_nc_1_2020$new_cases))
{
  avg_nc_Jp_1_2020[i]=(Japan_nc_1_2020$new_cases[i] + Japan_nc_1_2020$new_cases[i-1] + Japan_nc_1_2020$new_cases[i-2] + Japan_nc_1_2020$new_cases[i-3] + Japan_nc_1_2020$new_cases[i-4] + Japan_nc_1_2020$new_cases[i-5] + Japan_nc_1_2020$new_cases[i-6])/7
}
acml_nc_Jp_1_2020 <- c()
acml_nc_Jp_1_2020[1] <- avg_nc_Jp_1_2020[1]
for(i in 2:length(avg_nc_Jp_1_2020))
{
  acml_nc_Jp_1_2020[i] <- avg_nc_Jp_1_2020[i] + acml_nc_Jp_1_2020[i-1]
}
Japan_nc_1_2020 <- data.frame(Japan_nc_1_2020, avg_nc_Jp_1_2020, acml_nc_Jp_1_2020)



Vietnam_nc_1_2020 <- na.omit(Vietnam_nc[Vietnam_nc$datetime >= "2020-01-01" & Vietnam_nc$datetime <= "2020-01-31",])
avg_nc_Vn_1_2020 <- c()
avg_nc_Vn_1_2020[1] <- Vietnam_nc_1_2020$new_cases[1]
avg_nc_Vn_1_2020[2] <- (Vietnam_nc_1_2020$new_cases[1] + Vietnam_nc_1_2020$new_cases[2])/2
avg_nc_Vn_1_2020[3] <- (Vietnam_nc_1_2020$new_cases[1] + Vietnam_nc_1_2020$new_cases[2] + Vietnam_nc_1_2020$new_cases[3])/3
avg_nc_Vn_1_2020[4] <- (Vietnam_nc_1_2020$new_cases[1] + Vietnam_nc_1_2020$new_cases[2] + Vietnam_nc_1_2020$new_cases[3] + Vietnam_nc_1_2020$new_cases[4])/4
avg_nc_Vn_1_2020[5] <- (Vietnam_nc_1_2020$new_cases[1] + Vietnam_nc_1_2020$new_cases[2] + Vietnam_nc_1_2020$new_cases[3] + Vietnam_nc_1_2020$new_cases[4] + Vietnam_nc_1_2020$new_cases[5])/5
avg_nc_Vn_1_2020[6] <- (Vietnam_nc_1_2020$new_cases[1] + Vietnam_nc_1_2020$new_cases[2] + Vietnam_nc_1_2020$new_cases[3] + Vietnam_nc_1_2020$new_cases[4] + Vietnam_nc_1_2020$new_cases[5] + Vietnam_nc_1_2020$new_cases[6])/6
for(i in 7:length(Vietnam_nc_1_2020$new_cases))
{
  avg_nc_Vn_1_2020[i]=(Vietnam_nc_1_2020$new_cases[i] + Vietnam_nc_1_2020$new_cases[i-1] + Vietnam_nc_1_2020$new_cases[i-2] + Vietnam_nc_1_2020$new_cases[i-3] + Vietnam_nc_1_2020$new_cases[i-4] + Vietnam_nc_1_2020$new_cases[i-5] + Vietnam_nc_1_2020$new_cases[i-6])/7
}
acml_nc_Vn_1_2020 <- c()
acml_nc_Vn_1_2020[1] <- avg_nc_Vn_1_2020[1]
for(i in 2:length(avg_nc_Vn_1_2020))
{
  acml_nc_Vn_1_2020[i] <- avg_nc_Vn_1_2020[i] + acml_nc_Vn_1_2020[i-1]
}
Vietnam_nc_1_2020 <- data.frame(Vietnam_nc_1_2020, avg_nc_Vn_1_2020, acml_nc_Vn_1_2020)




#Newcases th?ng 3 2020
Indonesia_nc_3_2020 <- na.omit(Indonesia_nc[Indonesia_nc$datetime >= "2020-03-01" & Indonesia_nc$datetime <= "2020-03-31",])
avg_nc_Indo_3_2020 <- c()
avg_nc_Indo_3_2020[1] <- Indonesia_nc_3_2020$new_cases[1]
avg_nc_Indo_3_2020[2] <- (Indonesia_nc_3_2020$new_cases[1] + Indonesia_nc_3_2020$new_cases[2])/2
avg_nc_Indo_3_2020[3] <- (Indonesia_nc_3_2020$new_cases[1] + Indonesia_nc_3_2020$new_cases[2] + Indonesia_nc_3_2020$new_cases[3])/3
avg_nc_Indo_3_2020[4] <- (Indonesia_nc_3_2020$new_cases[1] + Indonesia_nc_3_2020$new_cases[2] + Indonesia_nc_3_2020$new_cases[3] + Indonesia_nc_3_2020$new_cases[4])/4
avg_nc_Indo_3_2020[5] <- (Indonesia_nc_3_2020$new_cases[1] + Indonesia_nc_3_2020$new_cases[2] + Indonesia_nc_3_2020$new_cases[3] + Indonesia_nc_3_2020$new_cases[4] + Indonesia_nc_3_2020$new_cases[5])/5
avg_nc_Indo_3_2020[6] <- (Indonesia_nc_3_2020$new_cases[1] + Indonesia_nc_3_2020$new_cases[2] + Indonesia_nc_3_2020$new_cases[3] + Indonesia_nc_3_2020$new_cases[4] + Indonesia_nc_3_2020$new_cases[5] + Indonesia_nc_3_2020$new_cases[6])/6
for(i in 7:length(Indonesia_nc_3_2020$new_cases))
{
  avg_nc_Indo_3_2020[i]=(Indonesia_nc_3_2020$new_cases[i] + Indonesia_nc_3_2020$new_cases[i-1] + Indonesia_nc_3_2020$new_cases[i-2] + Indonesia_nc_3_2020$new_cases[i-3] + Indonesia_nc_3_2020$new_cases[i-4] + Indonesia_nc_3_2020$new_cases[i-5] + Indonesia_nc_3_2020$new_cases[i-6])/7
}
acml_nc_Indo_3_2020 <- c()
acml_nc_Indo_3_2020[1] <- avg_nc_Indo_3_2020[1]
for(i in 2:length(avg_nc_Indo_3_2020))
{
  acml_nc_Indo_3_2020[i] <- avg_nc_Indo_3_2020[i] + acml_nc_Indo_3_2020[i-1]
}
Indonesia_nc_3_2020 <- data.frame(Indonesia_nc_3_2020, avg_nc_Indo_3_2020, acml_nc_Indo_3_2020)



Japan_nc_3_2020 <-  na.omit(Japan_nc[Japan_nc$datetime >= "2020-03-01" & Japan_nc$datetime <= "2020-03-31",])
avg_nc_Jp_3_2020 <- c()
avg_nc_Jp_3_2020[1] <- Japan_nc_3_2020$new_cases[1]
avg_nc_Jp_3_2020[2] <- (Japan_nc_3_2020$new_cases[1] + Japan_nc_3_2020$new_cases[2])/2
avg_nc_Jp_3_2020[3] <- (Japan_nc_3_2020$new_cases[1] + Japan_nc_3_2020$new_cases[2] + Japan_nc_3_2020$new_cases[3])/3
avg_nc_Jp_3_2020[4] <- (Japan_nc_3_2020$new_cases[1] + Japan_nc_3_2020$new_cases[2] + Japan_nc_3_2020$new_cases[3] + Japan_nc_3_2020$new_cases[4])/4
avg_nc_Jp_3_2020[5] <- (Japan_nc_3_2020$new_cases[1] + Japan_nc_3_2020$new_cases[2] + Japan_nc_3_2020$new_cases[3] + Japan_nc_3_2020$new_cases[4] + Japan_nc_3_2020$new_cases[5])/5
avg_nc_Jp_3_2020[6] <- (Japan_nc_3_2020$new_cases[1] + Japan_nc_3_2020$new_cases[2] + Japan_nc_3_2020$new_cases[3] + Japan_nc_3_2020$new_cases[4] + Japan_nc_3_2020$new_cases[5] + Japan_nc_3_2020$new_cases[6])/6
for(i in 7:length(Japan_nc_3_2020$new_cases))
{
  avg_nc_Jp_3_2020[i]=(Japan_nc_3_2020$new_cases[i] + Japan_nc_3_2020$new_cases[i-1] + Japan_nc_3_2020$new_cases[i-2] + Japan_nc_3_2020$new_cases[i-3] + Japan_nc_3_2020$new_cases[i-4] + Japan_nc_3_2020$new_cases[i-5] + Japan_nc_3_2020$new_cases[i-6])/7
}
acml_nc_Jp_3_2020 <- c()
acml_nc_Jp_3_2020[1] <- avg_nc_Jp_3_2020[1]
for(i in 2:length(avg_nc_Jp_3_2020))
{
  acml_nc_Jp_3_2020[i] <- avg_nc_Jp_3_2020[i] + acml_nc_Jp_3_2020[i-1]
}
Japan_nc_3_2020 <- data.frame(Japan_nc_3_2020, avg_nc_Jp_3_2020, acml_nc_Jp_3_2020)



Vietnam_nc_3_2020 <- na.omit(Vietnam_nc[Vietnam_nc$datetime >= "2020-03-01" & Vietnam_nc$datetime <= "2020-03-31",])
avg_nc_Vn_3_2020 <- c()
avg_nc_Vn_3_2020[1] <- Vietnam_nc_3_2020$new_cases[1]
avg_nc_Vn_3_2020[2] <- (Vietnam_nc_3_2020$new_cases[1] + Vietnam_nc_3_2020$new_cases[2])/2
avg_nc_Vn_3_2020[3] <- (Vietnam_nc_3_2020$new_cases[1] + Vietnam_nc_3_2020$new_cases[2] + Vietnam_nc_3_2020$new_cases[3])/3
avg_nc_Vn_3_2020[4] <- (Vietnam_nc_3_2020$new_cases[1] + Vietnam_nc_3_2020$new_cases[2] + Vietnam_nc_3_2020$new_cases[3] + Vietnam_nc_3_2020$new_cases[4])/4
avg_nc_Vn_3_2020[5] <- (Vietnam_nc_3_2020$new_cases[1] + Vietnam_nc_3_2020$new_cases[2] + Vietnam_nc_3_2020$new_cases[3] + Vietnam_nc_3_2020$new_cases[4] + Vietnam_nc_3_2020$new_cases[5])/5
avg_nc_Vn_3_2020[6] <- (Vietnam_nc_3_2020$new_cases[1] + Vietnam_nc_3_2020$new_cases[2] + Vietnam_nc_3_2020$new_cases[3] + Vietnam_nc_3_2020$new_cases[4] + Vietnam_nc_3_2020$new_cases[5] + Vietnam_nc_3_2020$new_cases[6])/6
for(i in 7:length(Vietnam_nc_3_2020$new_cases))
{
  avg_nc_Vn_3_2020[i]=(Vietnam_nc_3_2020$new_cases[i] + Vietnam_nc_3_2020$new_cases[i-1] + Vietnam_nc_3_2020$new_cases[i-2] + Vietnam_nc_3_2020$new_cases[i-3] + Vietnam_nc_3_2020$new_cases[i-4] + Vietnam_nc_3_2020$new_cases[i-5] + Vietnam_nc_3_2020$new_cases[i-6])/7
}
acml_nc_Vn_3_2020 <- c()
acml_nc_Vn_3_2020[1] <- avg_nc_Vn_3_2020[1]
for(i in 2:length(avg_nc_Vn_3_2020))
{
  acml_nc_Vn_3_2020[i] <- avg_nc_Vn_3_2020[i] + acml_nc_Vn_3_2020[i-1]
}
Vietnam_nc_3_2020 <- data.frame(Vietnam_nc_3_2020, avg_nc_Vn_3_2020, acml_nc_Vn_3_2020)




#Newcases thang 4 2020
Indonesia_nc_4_2020 <- na.omit(Indonesia_nc[Indonesia_nc$datetime >= "2020-04-01" & Indonesia_nc$datetime <= "2020-04-30",])
avg_nc_Indo_4_2020 <- c()
avg_nc_Indo_4_2020[1] <- Indonesia_nc_4_2020$new_cases[1]
avg_nc_Indo_4_2020[2] <- (Indonesia_nc_4_2020$new_cases[1] + Indonesia_nc_4_2020$new_cases[2])/2
avg_nc_Indo_4_2020[3] <- (Indonesia_nc_4_2020$new_cases[1] + Indonesia_nc_4_2020$new_cases[2] + Indonesia_nc_4_2020$new_cases[3])/3
avg_nc_Indo_4_2020[4] <- (Indonesia_nc_4_2020$new_cases[1] + Indonesia_nc_4_2020$new_cases[2] + Indonesia_nc_4_2020$new_cases[3] + Indonesia_nc_4_2020$new_cases[4])/4
avg_nc_Indo_4_2020[5] <- (Indonesia_nc_4_2020$new_cases[1] + Indonesia_nc_4_2020$new_cases[2] + Indonesia_nc_4_2020$new_cases[3] + Indonesia_nc_4_2020$new_cases[4] + Indonesia_nc_4_2020$new_cases[5])/5
avg_nc_Indo_4_2020[6] <- (Indonesia_nc_4_2020$new_cases[1] + Indonesia_nc_4_2020$new_cases[2] + Indonesia_nc_4_2020$new_cases[3] + Indonesia_nc_4_2020$new_cases[4] + Indonesia_nc_4_2020$new_cases[5] + Indonesia_nc_4_2020$new_cases[6])/6
for(i in 7:length(Indonesia_nc_4_2020$new_cases))
{
  avg_nc_Indo_4_2020[i]=(Indonesia_nc_4_2020$new_cases[i] + Indonesia_nc_4_2020$new_cases[i-1] + Indonesia_nc_4_2020$new_cases[i-2] + Indonesia_nc_4_2020$new_cases[i-3] + Indonesia_nc_4_2020$new_cases[i-4] + Indonesia_nc_4_2020$new_cases[i-5] + Indonesia_nc_4_2020$new_cases[i-6])/7
}
acml_nc_Indo_4_2020 <- c()
acml_nc_Indo_4_2020[1] <- avg_nc_Indo_4_2020[1]
for(i in 2:length(avg_nc_Indo_4_2020))
{
  acml_nc_Indo_4_2020[i] <- avg_nc_Indo_4_2020[i] + acml_nc_Indo_4_2020[i-1]
}
Indonesia_nc_4_2020 <- data.frame(Indonesia_nc_4_2020, avg_nc_Indo_4_2020, acml_nc_Indo_4_2020)


Japan_nc_4_2020 <-  na.omit(Japan_nc[Japan_nc$datetime >= "2020-04-01" & Japan_nc$datetime <= "2020-04-30",])
avg_nc_Jp_4_2020 <- c()
avg_nc_Jp_4_2020[1] <- Japan_nc_4_2020$new_cases[1]
avg_nc_Jp_4_2020[2] <- (Japan_nc_4_2020$new_cases[1] + Japan_nc_4_2020$new_cases[2])/2
avg_nc_Jp_4_2020[3] <- (Japan_nc_4_2020$new_cases[1] + Japan_nc_4_2020$new_cases[2] + Japan_nc_4_2020$new_cases[3])/3
avg_nc_Jp_4_2020[4] <- (Japan_nc_4_2020$new_cases[1] + Japan_nc_4_2020$new_cases[2] + Japan_nc_4_2020$new_cases[3] + Japan_nc_4_2020$new_cases[4])/4
avg_nc_Jp_4_2020[5] <- (Japan_nc_4_2020$new_cases[1] + Japan_nc_4_2020$new_cases[2] + Japan_nc_4_2020$new_cases[3] + Japan_nc_4_2020$new_cases[4] + Japan_nc_4_2020$new_cases[5])/5
avg_nc_Jp_4_2020[6] <- (Japan_nc_4_2020$new_cases[1] + Japan_nc_4_2020$new_cases[2] + Japan_nc_4_2020$new_cases[3] + Japan_nc_4_2020$new_cases[4] + Japan_nc_4_2020$new_cases[5] + Japan_nc_4_2020$new_cases[6])/6
for(i in 7:length(Japan_nc_4_2020$new_cases))
{
  avg_nc_Jp_4_2020[i]=(Japan_nc_4_2020$new_cases[i] + Japan_nc_4_2020$new_cases[i-1] + Japan_nc_4_2020$new_cases[i-2] + Japan_nc_4_2020$new_cases[i-3] + Japan_nc_4_2020$new_cases[i-4] + Japan_nc_4_2020$new_cases[i-5] + Japan_nc_4_2020$new_cases[i-6])/7
}
acml_nc_Jp_4_2020 <- c()
acml_nc_Jp_4_2020[1] <- avg_nc_Jp_4_2020[1]
for(i in 2:length(avg_nc_Jp_4_2020))
{
  acml_nc_Jp_4_2020[i] <- avg_nc_Jp_4_2020[i] + acml_nc_Jp_4_2020[i-1]
}
Japan_nc_4_2020 <- data.frame(Japan_nc_4_2020, avg_nc_Jp_4_2020, acml_nc_Jp_4_2020)



Vietnam_nc_4_2020 <- na.omit(Vietnam_nc[Vietnam_nc$datetime >= "2020-04-01" & Vietnam_nc$datetime <= "2020-04-30",])
avg_nc_Vn_4_2020 <- c()
avg_nc_Vn_4_2020[1] <- Vietnam_nc_4_2020$new_cases[1]
avg_nc_Vn_4_2020[2] <- (Vietnam_nc_4_2020$new_cases[1] + Vietnam_nc_4_2020$new_cases[2])/2
avg_nc_Vn_4_2020[3] <- (Vietnam_nc_4_2020$new_cases[1] + Vietnam_nc_4_2020$new_cases[2] + Vietnam_nc_4_2020$new_cases[3])/3
avg_nc_Vn_4_2020[4] <- (Vietnam_nc_4_2020$new_cases[1] + Vietnam_nc_4_2020$new_cases[2] + Vietnam_nc_4_2020$new_cases[3] + Vietnam_nc_4_2020$new_cases[4])/4
avg_nc_Vn_4_2020[5] <- (Vietnam_nc_4_2020$new_cases[1] + Vietnam_nc_4_2020$new_cases[2] + Vietnam_nc_4_2020$new_cases[3] + Vietnam_nc_4_2020$new_cases[4] + Vietnam_nc_4_2020$new_cases[5])/5
avg_nc_Vn_4_2020[6] <- (Vietnam_nc_4_2020$new_cases[1] + Vietnam_nc_4_2020$new_cases[2] + Vietnam_nc_4_2020$new_cases[3] + Vietnam_nc_4_2020$new_cases[4] + Vietnam_nc_4_2020$new_cases[5] + Vietnam_nc_4_2020$new_cases[6])/6
for(i in 7:length(Vietnam_nc_4_2020$new_cases))
{
  avg_nc_Vn_4_2020[i]=(Vietnam_nc_4_2020$new_cases[i] + Vietnam_nc_4_2020$new_cases[i-1] + Vietnam_nc_4_2020$new_cases[i-2] + Vietnam_nc_4_2020$new_cases[i-3] + Vietnam_nc_4_2020$new_cases[i-4] + Vietnam_nc_4_2020$new_cases[i-5] + Vietnam_nc_4_2020$new_cases[i-6])/7
}
acml_nc_Vn_4_2020 <- c()
acml_nc_Vn_4_2020[1] <- avg_nc_Vn_4_2020[1]
for(i in 2:length(avg_nc_Vn_4_2020))
{
  acml_nc_Vn_4_2020[i] <- avg_nc_Vn_4_2020[i] + acml_nc_Vn_4_2020[i-1]
}
Vietnam_nc_4_2020 <- data.frame(Vietnam_nc_4_2020, avg_nc_Vn_4_2020, acml_nc_Vn_4_2020)



#Newcases thang 5 2020
Indonesia_nc_5_2020 <- na.omit(Indonesia_nc[Indonesia_nc$datetime >= "2020-05-01" & Indonesia_nc$datetime <= "2020-05-31",])
avg_nc_Indo_5_2020 <- c()
avg_nc_Indo_5_2020[1] <- Indonesia_nc_5_2020$new_cases[1]
avg_nc_Indo_5_2020[2] <- (Indonesia_nc_5_2020$new_cases[1] + Indonesia_nc_5_2020$new_cases[2])/2
avg_nc_Indo_5_2020[3] <- (Indonesia_nc_5_2020$new_cases[1] + Indonesia_nc_5_2020$new_cases[2] + Indonesia_nc_5_2020$new_cases[3])/3
avg_nc_Indo_5_2020[4] <- (Indonesia_nc_5_2020$new_cases[1] + Indonesia_nc_5_2020$new_cases[2] + Indonesia_nc_5_2020$new_cases[3] + Indonesia_nc_5_2020$new_cases[4])/4
avg_nc_Indo_5_2020[5] <- (Indonesia_nc_5_2020$new_cases[1] + Indonesia_nc_5_2020$new_cases[2] + Indonesia_nc_5_2020$new_cases[3] + Indonesia_nc_5_2020$new_cases[4] + Indonesia_nc_5_2020$new_cases[5])/5
avg_nc_Indo_5_2020[6] <- (Indonesia_nc_5_2020$new_cases[1] + Indonesia_nc_5_2020$new_cases[2] + Indonesia_nc_5_2020$new_cases[3] + Indonesia_nc_5_2020$new_cases[4] + Indonesia_nc_5_2020$new_cases[5] + Indonesia_nc_5_2020$new_cases[6])/6
for(i in 7:length(Indonesia_nc_5_2020$new_cases))
{
  avg_nc_Indo_5_2020[i]=(Indonesia_nc_5_2020$new_cases[i] + Indonesia_nc_5_2020$new_cases[i-1] + Indonesia_nc_5_2020$new_cases[i-2] + Indonesia_nc_5_2020$new_cases[i-3] + Indonesia_nc_5_2020$new_cases[i-4] + Indonesia_nc_5_2020$new_cases[i-5] + Indonesia_nc_5_2020$new_cases[i-6])/7
}
acml_nc_Indo_5_2020 <- c()
acml_nc_Indo_5_2020[1] <- avg_nc_Indo_5_2020[1]
for(i in 2:length(avg_nc_Indo_5_2020))
{
  acml_nc_Indo_5_2020[i] <- avg_nc_Indo_5_2020[i] + acml_nc_Indo_5_2020[i-1]
}
Indonesia_nc_5_2020 <- data.frame(Indonesia_nc_5_2020, avg_nc_Indo_5_2020, acml_nc_Indo_5_2020)



Japan_nc_5_2020 <-  na.omit(Japan_nc[Japan_nc$datetime >= "2020-05-01" & Japan_nc$datetime <= "2020-05-31",])
avg_nc_Jp_5_2020 <- c()
avg_nc_Jp_5_2020[1] <- Japan_nc_5_2020$new_cases[1]
avg_nc_Jp_5_2020[2] <- (Japan_nc_5_2020$new_cases[1] + Japan_nc_5_2020$new_cases[2])/2
avg_nc_Jp_5_2020[3] <- (Japan_nc_5_2020$new_cases[1] + Japan_nc_5_2020$new_cases[2] + Japan_nc_5_2020$new_cases[3])/3
avg_nc_Jp_5_2020[4] <- (Japan_nc_5_2020$new_cases[1] + Japan_nc_5_2020$new_cases[2] + Japan_nc_5_2020$new_cases[3] + Japan_nc_5_2020$new_cases[4])/4
avg_nc_Jp_5_2020[5] <- (Japan_nc_5_2020$new_cases[1] + Japan_nc_5_2020$new_cases[2] + Japan_nc_5_2020$new_cases[3] + Japan_nc_5_2020$new_cases[4] + Japan_nc_5_2020$new_cases[5])/5
avg_nc_Jp_5_2020[6] <- (Japan_nc_5_2020$new_cases[1] + Japan_nc_5_2020$new_cases[2] + Japan_nc_5_2020$new_cases[3] + Japan_nc_5_2020$new_cases[4] + Japan_nc_5_2020$new_cases[5] + Japan_nc_5_2020$new_cases[6])/6
for(i in 7:length(Japan_nc_5_2020$new_cases))
{
  avg_nc_Jp_5_2020[i]=(Japan_nc_5_2020$new_cases[i] + Japan_nc_5_2020$new_cases[i-1] + Japan_nc_5_2020$new_cases[i-2] + Japan_nc_5_2020$new_cases[i-3] + Japan_nc_5_2020$new_cases[i-4] + Japan_nc_5_2020$new_cases[i-5] + Japan_nc_5_2020$new_cases[i-6])/7
}
acml_nc_Jp_5_2020 <- c()
acml_nc_Jp_5_2020[1] <- avg_nc_Jp_5_2020[1]
for(i in 2:length(avg_nc_Jp_5_2020))
{
  acml_nc_Jp_5_2020[i] <- avg_nc_Jp_5_2020[i] + acml_nc_Jp_5_2020[i-1]
}
Japan_nc_5_2020 <- data.frame(Japan_nc_5_2020, avg_nc_Jp_5_2020, acml_nc_Jp_5_2020)



Vietnam_nc_5_2020 <- na.omit(Vietnam_nc[Vietnam_nc$datetime >= "2020-05-01" & Vietnam_nc$datetime <= "2020-05-31",])
avg_nc_Vn_5_2020 <- c()
avg_nc_Vn_5_2020[1] <- Vietnam_nc_5_2020$new_cases[1]
avg_nc_Vn_5_2020[2] <- (Vietnam_nc_5_2020$new_cases[1] + Vietnam_nc_5_2020$new_cases[2])/2
avg_nc_Vn_5_2020[3] <- (Vietnam_nc_5_2020$new_cases[1] + Vietnam_nc_5_2020$new_cases[2] + Vietnam_nc_5_2020$new_cases[3])/3
avg_nc_Vn_5_2020[4] <- (Vietnam_nc_5_2020$new_cases[1] + Vietnam_nc_5_2020$new_cases[2] + Vietnam_nc_5_2020$new_cases[3] + Vietnam_nc_5_2020$new_cases[4])/4
avg_nc_Vn_5_2020[5] <- (Vietnam_nc_5_2020$new_cases[1] + Vietnam_nc_5_2020$new_cases[2] + Vietnam_nc_5_2020$new_cases[3] + Vietnam_nc_5_2020$new_cases[4] + Vietnam_nc_5_2020$new_cases[5])/5
avg_nc_Vn_5_2020[6] <- (Vietnam_nc_5_2020$new_cases[1] + Vietnam_nc_5_2020$new_cases[2] + Vietnam_nc_5_2020$new_cases[3] + Vietnam_nc_5_2020$new_cases[4] + Vietnam_nc_5_2020$new_cases[5] + Vietnam_nc_5_2020$new_cases[6])/6
for(i in 7:length(Vietnam_nc_5_2020$new_cases))
{
  avg_nc_Vn_5_2020[i]=(Vietnam_nc_5_2020$new_cases[i] + Vietnam_nc_5_2020$new_cases[i-1] + Vietnam_nc_5_2020$new_cases[i-2] + Vietnam_nc_5_2020$new_cases[i-3] + Vietnam_nc_5_2020$new_cases[i-4] + Vietnam_nc_5_2020$new_cases[i-5] + Vietnam_nc_5_2020$new_cases[i-6])/7
}
acml_nc_Vn_5_2020 <- c()
acml_nc_Vn_5_2020[1] <- avg_nc_Vn_5_2020[1]
for(i in 2:length(avg_nc_Vn_5_2020))
{
  acml_nc_Vn_5_2020[i] <- avg_nc_Vn_5_2020[i] + acml_nc_Vn_5_2020[i-1]
}
Vietnam_nc_5_2020 <- data.frame(Vietnam_nc_5_2020, avg_nc_Vn_5_2020, acml_nc_Vn_5_2020)




#newcases th?ng 1 2021
Indonesia_nc_1_2021 <- na.omit(Indonesia_nc[Indonesia_nc$datetime >= "2021-01-01" & Indonesia_nc$datetime <= "2021-01-31",])
avg_nc_Indo_1_2021 <- c()
avg_nc_Indo_1_2021[1] <- Indonesia_nc_1_2021$new_cases[1]
avg_nc_Indo_1_2021[2] <- (Indonesia_nc_1_2021$new_cases[1] + Indonesia_nc_1_2021$new_cases[2])/2
avg_nc_Indo_1_2021[3] <- (Indonesia_nc_1_2021$new_cases[1] + Indonesia_nc_1_2021$new_cases[2] + Indonesia_nc_1_2021$new_cases[3])/3
avg_nc_Indo_1_2021[4] <- (Indonesia_nc_1_2021$new_cases[1] + Indonesia_nc_1_2021$new_cases[2] + Indonesia_nc_1_2021$new_cases[3] + Indonesia_nc_1_2021$new_cases[4])/4
avg_nc_Indo_1_2021[5] <- (Indonesia_nc_1_2021$new_cases[1] + Indonesia_nc_1_2021$new_cases[2] + Indonesia_nc_1_2021$new_cases[3] + Indonesia_nc_1_2021$new_cases[4] + Indonesia_nc_1_2021$new_cases[5])/5
avg_nc_Indo_1_2021[6] <- (Indonesia_nc_1_2021$new_cases[1] + Indonesia_nc_1_2021$new_cases[2] + Indonesia_nc_1_2021$new_cases[3] + Indonesia_nc_1_2021$new_cases[4] + Indonesia_nc_1_2021$new_cases[5] + Indonesia_nc_1_2021$new_cases[6])/6
for(i in 7:length(Indonesia_nc_1_2021$new_cases))
{
  avg_nc_Indo_1_2021[i]=(Indonesia_nc_1_2021$new_cases[i] + Indonesia_nc_1_2021$new_cases[i-1] + Indonesia_nc_1_2021$new_cases[i-2] + Indonesia_nc_1_2021$new_cases[i-3] + Indonesia_nc_1_2021$new_cases[i-4] + Indonesia_nc_1_2021$new_cases[i-5] + Indonesia_nc_1_2021$new_cases[i-6])/7
}
acml_nc_Indo_1_2021 <- c()
acml_nc_Indo_1_2021[1] <- avg_nc_Indo_1_2021[1]
for(i in 2:length(avg_nc_Indo_1_2021))
{
  acml_nc_Indo_1_2021[i] <- avg_nc_Indo_1_2021[i] + acml_nc_Indo_1_2021[i-1]
}
Indonesia_nc_1_2021 <- data.frame(Indonesia_nc_1_2021, avg_nc_Indo_1_2021, acml_nc_Indo_1_2021)



Japan_nc_1_2021 <-  na.omit(Japan_nc[Japan_nc$datetime >= "2021-01-01" & Japan_nc$datetime <= "2021-01-31",])
avg_nc_Jp_1_2021 <- c()
avg_nc_Jp_1_2021[1] <- Japan_nc_1_2021$new_cases[1]
avg_nc_Jp_1_2021[2] <- (Japan_nc_1_2021$new_cases[1] + Japan_nc_1_2021$new_cases[2])/2
avg_nc_Jp_1_2021[3] <- (Japan_nc_1_2021$new_cases[1] + Japan_nc_1_2021$new_cases[2] + Japan_nc_1_2021$new_cases[3])/3
avg_nc_Jp_1_2021[4] <- (Japan_nc_1_2021$new_cases[1] + Japan_nc_1_2021$new_cases[2] + Japan_nc_1_2021$new_cases[3] + Japan_nc_1_2021$new_cases[4])/4
avg_nc_Jp_1_2021[5] <- (Japan_nc_1_2021$new_cases[1] + Japan_nc_1_2021$new_cases[2] + Japan_nc_1_2021$new_cases[3] + Japan_nc_1_2021$new_cases[4] + Japan_nc_1_2021$new_cases[5])/5
avg_nc_Jp_1_2021[6] <- (Japan_nc_1_2021$new_cases[1] + Japan_nc_1_2021$new_cases[2] + Japan_nc_1_2021$new_cases[3] + Japan_nc_1_2021$new_cases[4] + Japan_nc_1_2021$new_cases[5] + Japan_nc_1_2021$new_cases[6])/6
for(i in 7:length(Japan_nc_1_2021$new_cases))
{
  avg_nc_Jp_1_2021[i]=(Japan_nc_1_2021$new_cases[i] + Japan_nc_1_2021$new_cases[i-1] + Japan_nc_1_2021$new_cases[i-2] + Japan_nc_1_2021$new_cases[i-3] + Japan_nc_1_2021$new_cases[i-4] + Japan_nc_1_2021$new_cases[i-5] + Japan_nc_1_2021$new_cases[i-6])/7
}
acml_nc_Jp_1_2021 <- c()
acml_nc_Jp_1_2021[1] <- avg_nc_Jp_1_2021[1]
for(i in 2:length(avg_nc_Jp_1_2021))
{
  acml_nc_Jp_1_2021[i] <- avg_nc_Jp_1_2021[i] + acml_nc_Jp_1_2021[i-1]
}
Japan_nc_1_2021 <- data.frame(Japan_nc_1_2021, avg_nc_Jp_1_2021, acml_nc_Jp_1_2021)



Vietnam_nc_1_2021 <- na.omit(Vietnam_nc[Vietnam_nc$datetime >= "2021-01-01" & Vietnam_nc$datetime <= "2021-01-31",])
avg_nc_Vn_1_2021 <- c()
avg_nc_Vn_1_2021[1] <- Vietnam_nc_1_2021$new_cases[1]
avg_nc_Vn_1_2021[2] <- (Vietnam_nc_1_2021$new_cases[1] + Vietnam_nc_1_2021$new_cases[2])/2
avg_nc_Vn_1_2021[3] <- (Vietnam_nc_1_2021$new_cases[1] + Vietnam_nc_1_2021$new_cases[2] + Vietnam_nc_1_2021$new_cases[3])/3
avg_nc_Vn_1_2021[4] <- (Vietnam_nc_1_2021$new_cases[1] + Vietnam_nc_1_2021$new_cases[2] + Vietnam_nc_1_2021$new_cases[3] + Vietnam_nc_1_2021$new_cases[4])/4
avg_nc_Vn_1_2021[5] <- (Vietnam_nc_1_2021$new_cases[1] + Vietnam_nc_1_2021$new_cases[2] + Vietnam_nc_1_2021$new_cases[3] + Vietnam_nc_1_2021$new_cases[4] + Vietnam_nc_1_2021$new_cases[5])/5
avg_nc_Vn_1_2021[6] <- (Vietnam_nc_1_2021$new_cases[1] + Vietnam_nc_1_2021$new_cases[2] + Vietnam_nc_1_2021$new_cases[3] + Vietnam_nc_1_2021$new_cases[4] + Vietnam_nc_1_2021$new_cases[5] + Vietnam_nc_1_2021$new_cases[6])/6
for(i in 7:length(Vietnam_nc_1_2021$new_cases))
{
  avg_nc_Vn_1_2021[i]=(Vietnam_nc_1_2021$new_cases[i] + Vietnam_nc_1_2021$new_cases[i-1] + Vietnam_nc_1_2021$new_cases[i-2] + Vietnam_nc_1_2021$new_cases[i-3] + Vietnam_nc_1_2021$new_cases[i-4] + Vietnam_nc_1_2021$new_cases[i-5] + Vietnam_nc_1_2021$new_cases[i-6])/7
}
acml_nc_Vn_1_2021 <- c()
acml_nc_Vn_1_2021[1] <- avg_nc_Vn_1_2021[1]
for(i in 2:length(avg_nc_Vn_1_2021))
{
  acml_nc_Vn_1_2021[i] <- avg_nc_Vn_1_2021[i] + acml_nc_Vn_1_2021[i-1]
}
Vietnam_nc_1_2021 <- data.frame(Vietnam_nc_1_2021, avg_nc_Vn_1_2021, acml_nc_Vn_1_2021)



#Newcases th?ng 3 2021
Indonesia_nc_3_2021 <- na.omit(Indonesia_nc[Indonesia_nc$datetime >= "2021-03-01" & Indonesia_nc$datetime <= "2021-03-31",])
avg_nc_Indo_3_2021 <- c()
avg_nc_Indo_3_2021[1] <- Indonesia_nc_3_2021$new_cases[1]
avg_nc_Indo_3_2021[2] <- (Indonesia_nc_3_2021$new_cases[1] + Indonesia_nc_3_2021$new_cases[2])/2
avg_nc_Indo_3_2021[3] <- (Indonesia_nc_3_2021$new_cases[1] + Indonesia_nc_3_2021$new_cases[2] + Indonesia_nc_3_2021$new_cases[3])/3
avg_nc_Indo_3_2021[4] <- (Indonesia_nc_3_2021$new_cases[1] + Indonesia_nc_3_2021$new_cases[2] + Indonesia_nc_3_2021$new_cases[3] + Indonesia_nc_3_2021$new_cases[4])/4
avg_nc_Indo_3_2021[5] <- (Indonesia_nc_3_2021$new_cases[1] + Indonesia_nc_3_2021$new_cases[2] + Indonesia_nc_3_2021$new_cases[3] + Indonesia_nc_3_2021$new_cases[4] + Indonesia_nc_3_2021$new_cases[5])/5
avg_nc_Indo_3_2021[6] <- (Indonesia_nc_3_2021$new_cases[1] + Indonesia_nc_3_2021$new_cases[2] + Indonesia_nc_3_2021$new_cases[3] + Indonesia_nc_3_2021$new_cases[4] + Indonesia_nc_3_2021$new_cases[5] + Indonesia_nc_3_2021$new_cases[6])/6
for(i in 7:length(Indonesia_nc_3_2021$new_cases))
{
  avg_nc_Indo_3_2021[i]=(Indonesia_nc_3_2021$new_cases[i] + Indonesia_nc_3_2021$new_cases[i-1] + Indonesia_nc_3_2021$new_cases[i-2] + Indonesia_nc_3_2021$new_cases[i-3] + Indonesia_nc_3_2021$new_cases[i-4] + Indonesia_nc_3_2021$new_cases[i-5] + Indonesia_nc_3_2021$new_cases[i-6])/7
}
acml_nc_Indo_3_2021 <- c()
acml_nc_Indo_3_2021[1] <- avg_nc_Indo_3_2021[1]
for(i in 2:length(avg_nc_Indo_3_2021))
{
  acml_nc_Indo_3_2021[i] <- avg_nc_Indo_3_2021[i] + acml_nc_Indo_3_2021[i-1]
}
Indonesia_nc_3_2021 <- data.frame(Indonesia_nc_3_2021, avg_nc_Indo_3_2021, acml_nc_Indo_3_2021)



Japan_nc_3_2021 <-  na.omit(Japan_nc[Japan_nc$datetime >= "2021-03-01" & Japan_nc$datetime <= "2021-03-31",])
avg_nc_Jp_3_2021 <- c()
avg_nc_Jp_3_2021[1] <- Japan_nc_3_2021$new_cases[1]
avg_nc_Jp_3_2021[2] <- (Japan_nc_3_2021$new_cases[1] + Japan_nc_3_2021$new_cases[2])/2
avg_nc_Jp_3_2021[3] <- (Japan_nc_3_2021$new_cases[1] + Japan_nc_3_2021$new_cases[2] + Japan_nc_3_2021$new_cases[3])/3
avg_nc_Jp_3_2021[4] <- (Japan_nc_3_2021$new_cases[1] + Japan_nc_3_2021$new_cases[2] + Japan_nc_3_2021$new_cases[3] + Japan_nc_3_2021$new_cases[4])/4
avg_nc_Jp_3_2021[5] <- (Japan_nc_3_2021$new_cases[1] + Japan_nc_3_2021$new_cases[2] + Japan_nc_3_2021$new_cases[3] + Japan_nc_3_2021$new_cases[4] + Japan_nc_3_2021$new_cases[5])/5
avg_nc_Jp_3_2021[6] <- (Japan_nc_3_2021$new_cases[1] + Japan_nc_3_2021$new_cases[2] + Japan_nc_3_2021$new_cases[3] + Japan_nc_3_2021$new_cases[4] + Japan_nc_3_2021$new_cases[5] + Japan_nc_3_2021$new_cases[6])/6
for(i in 7:length(Japan_nc_3_2021$new_cases))
{
  avg_nc_Jp_3_2021[i]=(Japan_nc_3_2021$new_cases[i] + Japan_nc_3_2021$new_cases[i-1] + Japan_nc_3_2021$new_cases[i-2] + Japan_nc_3_2021$new_cases[i-3] + Japan_nc_3_2021$new_cases[i-4] + Japan_nc_3_2021$new_cases[i-5] + Japan_nc_3_2021$new_cases[i-6])/7
}
acml_nc_Jp_3_2021 <- c()
acml_nc_Jp_3_2021[1] <- avg_nc_Jp_3_2021[1]
for(i in 2:length(avg_nc_Jp_3_2021))
{
  acml_nc_Jp_3_2021[i] <- avg_nc_Jp_3_2021[i] + acml_nc_Jp_3_2021[i-1]
}
Japan_nc_3_2021 <- data.frame(Japan_nc_3_2021, avg_nc_Jp_3_2021, acml_nc_Jp_3_2021)



Vietnam_nc_3_2021 <- na.omit(Vietnam_nc[Vietnam_nc$datetime >= "2021-03-01" & Vietnam_nc$datetime <= "2021-03-31",])
avg_nc_Vn_3_2021 <- c()
avg_nc_Vn_3_2021[1] <- Vietnam_nc_3_2021$new_cases[1]
avg_nc_Vn_3_2021[2] <- (Vietnam_nc_3_2021$new_cases[1] + Vietnam_nc_3_2021$new_cases[2])/2
avg_nc_Vn_3_2021[3] <- (Vietnam_nc_3_2021$new_cases[1] + Vietnam_nc_3_2021$new_cases[2] + Vietnam_nc_3_2021$new_cases[3])/3
avg_nc_Vn_3_2021[4] <- (Vietnam_nc_3_2021$new_cases[1] + Vietnam_nc_3_2021$new_cases[2] + Vietnam_nc_3_2021$new_cases[3] + Vietnam_nc_3_2021$new_cases[4])/4
avg_nc_Vn_3_2021[5] <- (Vietnam_nc_3_2021$new_cases[1] + Vietnam_nc_3_2021$new_cases[2] + Vietnam_nc_3_2021$new_cases[3] + Vietnam_nc_3_2021$new_cases[4] + Vietnam_nc_3_2021$new_cases[5])/5
avg_nc_Vn_3_2021[6] <- (Vietnam_nc_3_2021$new_cases[1] + Vietnam_nc_3_2021$new_cases[2] + Vietnam_nc_3_2021$new_cases[3] + Vietnam_nc_3_2021$new_cases[4] + Vietnam_nc_3_2021$new_cases[5] + Vietnam_nc_3_2021$new_cases[6])/6
for(i in 7:length(Vietnam_nc_3_2021$new_cases))
{
  avg_nc_Vn_3_2021[i]=(Vietnam_nc_3_2021$new_cases[i] + Vietnam_nc_3_2021$new_cases[i-1] + Vietnam_nc_3_2021$new_cases[i-2] + Vietnam_nc_3_2021$new_cases[i-3] + Vietnam_nc_3_2021$new_cases[i-4] + Vietnam_nc_3_2021$new_cases[i-5] + Vietnam_nc_3_2021$new_cases[i-6])/7
}
acml_nc_Vn_3_2021 <- c()
acml_nc_Vn_3_2021[1] <- avg_nc_Vn_3_2021[1]
for(i in 2:length(avg_nc_Vn_3_2021))
{
  acml_nc_Vn_3_2021[i] <- avg_nc_Vn_3_2021[i] + acml_nc_Vn_3_2021[i-1]
}
Vietnam_nc_3_2021 <- data.frame(Vietnam_nc_3_2021, avg_nc_Vn_3_2021, acml_nc_Vn_3_2021)




#Newcases thang 4 2021
Indonesia_nc_4_2021 <- na.omit(Indonesia_nc[Indonesia_nc$datetime >= "2021-04-01" & Indonesia_nc$datetime <= "2021-04-30",])
avg_nc_Indo_4_2021 <- c()
avg_nc_Indo_4_2021[1] <- Indonesia_nc_4_2021$new_cases[1]
avg_nc_Indo_4_2021[2] <- (Indonesia_nc_4_2021$new_cases[1] + Indonesia_nc_4_2021$new_cases[2])/2
avg_nc_Indo_4_2021[3] <- (Indonesia_nc_4_2021$new_cases[1] + Indonesia_nc_4_2021$new_cases[2] + Indonesia_nc_4_2021$new_cases[3])/3
avg_nc_Indo_4_2021[4] <- (Indonesia_nc_4_2021$new_cases[1] + Indonesia_nc_4_2021$new_cases[2] + Indonesia_nc_4_2021$new_cases[3] + Indonesia_nc_4_2021$new_cases[4])/4
avg_nc_Indo_4_2021[5] <- (Indonesia_nc_4_2021$new_cases[1] + Indonesia_nc_4_2021$new_cases[2] + Indonesia_nc_4_2021$new_cases[3] + Indonesia_nc_4_2021$new_cases[4] + Indonesia_nc_4_2021$new_cases[5])/5
avg_nc_Indo_4_2021[6] <- (Indonesia_nc_4_2021$new_cases[1] + Indonesia_nc_4_2021$new_cases[2] + Indonesia_nc_4_2021$new_cases[3] + Indonesia_nc_4_2021$new_cases[4] + Indonesia_nc_4_2021$new_cases[5] + Indonesia_nc_4_2021$new_cases[6])/6
for(i in 7:length(Indonesia_nc_4_2021$new_cases))
{
  avg_nc_Indo_4_2021[i]=(Indonesia_nc_4_2021$new_cases[i] + Indonesia_nc_4_2021$new_cases[i-1] + Indonesia_nc_4_2021$new_cases[i-2] + Indonesia_nc_4_2021$new_cases[i-3] + Indonesia_nc_4_2021$new_cases[i-4] + Indonesia_nc_4_2021$new_cases[i-5] + Indonesia_nc_4_2021$new_cases[i-6])/7
}
acml_nc_Indo_4_2021 <- c()
acml_nc_Indo_4_2021[1] <- avg_nc_Indo_4_2021[1]
for(i in 2:length(avg_nc_Indo_4_2021))
{
  acml_nc_Indo_4_2021[i] <- avg_nc_Indo_4_2021[i] + acml_nc_Indo_4_2021[i-1]
}
Indonesia_nc_4_2021 <- data.frame(Indonesia_nc_4_2021, avg_nc_Indo_4_2021, acml_nc_Indo_4_2021)



Japan_nc_4_2021 <-  na.omit(Japan_nc[Japan_nc$datetime >= "2021-04-01" & Japan_nc$datetime <= "2021-04-30",])
avg_nc_Jp_4_2021 <- c()
avg_nc_Jp_4_2021[1] <- Japan_nc_4_2021$new_cases[1]
avg_nc_Jp_4_2021[2] <- (Japan_nc_4_2021$new_cases[1] + Japan_nc_4_2021$new_cases[2])/2
avg_nc_Jp_4_2021[3] <- (Japan_nc_4_2021$new_cases[1] + Japan_nc_4_2021$new_cases[2] + Japan_nc_4_2021$new_cases[3])/3
avg_nc_Jp_4_2021[4] <- (Japan_nc_4_2021$new_cases[1] + Japan_nc_4_2021$new_cases[2] + Japan_nc_4_2021$new_cases[3] + Japan_nc_4_2021$new_cases[4])/4
avg_nc_Jp_4_2021[5] <- (Japan_nc_4_2021$new_cases[1] + Japan_nc_4_2021$new_cases[2] + Japan_nc_4_2021$new_cases[3] + Japan_nc_4_2021$new_cases[4] + Japan_nc_4_2021$new_cases[5])/5
avg_nc_Jp_4_2021[6] <- (Japan_nc_4_2021$new_cases[1] + Japan_nc_4_2021$new_cases[2] + Japan_nc_4_2021$new_cases[3] + Japan_nc_4_2021$new_cases[4] + Japan_nc_4_2021$new_cases[5] + Japan_nc_4_2021$new_cases[6])/6
for(i in 7:length(Japan_nc_4_2021$new_cases))
{
  avg_nc_Jp_4_2021[i]=(Japan_nc_4_2021$new_cases[i] + Japan_nc_4_2021$new_cases[i-1] + Japan_nc_4_2021$new_cases[i-2] + Japan_nc_4_2021$new_cases[i-3] + Japan_nc_4_2021$new_cases[i-4] + Japan_nc_4_2021$new_cases[i-5] + Japan_nc_4_2021$new_cases[i-6])/7
}
acml_nc_Jp_4_2021 <- c()
acml_nc_Jp_4_2021[1] <- avg_nc_Jp_4_2021[1]
for(i in 2:length(avg_nc_Jp_4_2021))
{
  acml_nc_Jp_4_2021[i] <- avg_nc_Jp_4_2021[i] + acml_nc_Jp_4_2021[i-1]
}
Japan_nc_4_2021 <- data.frame(Japan_nc_4_2021, avg_nc_Jp_4_2021, acml_nc_Jp_4_2021)


Vietnam_nc_4_2021 <- na.omit(Vietnam_nc[Vietnam_nc$datetime >= "2021-04-01" & Vietnam_nc$datetime <= "2021-04-30",])
avg_nc_Vn_4_2021 <- c()
avg_nc_Vn_4_2021[1] <- Vietnam_nc_4_2021$new_cases[1]
avg_nc_Vn_4_2021[2] <- (Vietnam_nc_4_2021$new_cases[1] + Vietnam_nc_4_2021$new_cases[2])/2
avg_nc_Vn_4_2021[3] <- (Vietnam_nc_4_2021$new_cases[1] + Vietnam_nc_4_2021$new_cases[2] + Vietnam_nc_4_2021$new_cases[3])/3
avg_nc_Vn_4_2021[4] <- (Vietnam_nc_4_2021$new_cases[1] + Vietnam_nc_4_2021$new_cases[2] + Vietnam_nc_4_2021$new_cases[3] + Vietnam_nc_4_2021$new_cases[4])/4
avg_nc_Vn_4_2021[5] <- (Vietnam_nc_4_2021$new_cases[1] + Vietnam_nc_4_2021$new_cases[2] + Vietnam_nc_4_2021$new_cases[3] + Vietnam_nc_4_2021$new_cases[4] + Vietnam_nc_4_2021$new_cases[5])/5
avg_nc_Vn_4_2021[6] <- (Vietnam_nc_4_2021$new_cases[1] + Vietnam_nc_4_2021$new_cases[2] + Vietnam_nc_4_2021$new_cases[3] + Vietnam_nc_4_2021$new_cases[4] + Vietnam_nc_4_2021$new_cases[5] + Vietnam_nc_4_2021$new_cases[6])/6
for(i in 7:length(Vietnam_nc_4_2021$new_cases))
{
  avg_nc_Vn_4_2021[i]=(Vietnam_nc_4_2021$new_cases[i] + Vietnam_nc_4_2021$new_cases[i-1] + Vietnam_nc_4_2021$new_cases[i-2] + Vietnam_nc_4_2021$new_cases[i-3] + Vietnam_nc_4_2021$new_cases[i-4] + Vietnam_nc_4_2021$new_cases[i-5] + Vietnam_nc_4_2021$new_cases[i-6])/7
}
acml_nc_Vn_4_2021 <- c()
acml_nc_Vn_4_2021[1] <- avg_nc_Vn_4_2021[1]
for(i in 2:length(avg_nc_Vn_4_2021))
{
  acml_nc_Vn_4_2021[i] <- avg_nc_Vn_4_2021[i] + acml_nc_Vn_4_2021[i-1]
}
Vietnam_nc_4_2021 <- data.frame(Vietnam_nc_4_2021, avg_nc_Vn_4_2021, acml_nc_Vn_4_2021)



#Newcases thang 5 2021
Indonesia_nc_5_2021 <- na.omit(Indonesia_nc[Indonesia_nc$datetime >= "2021-05-01" & Indonesia_nc$datetime <= "2021-05-31",])
avg_nc_Indo_5_2021 <- c()
avg_nc_Indo_5_2021[1] <- Indonesia_nc_5_2021$new_cases[1]
avg_nc_Indo_5_2021[2] <- (Indonesia_nc_5_2021$new_cases[1] + Indonesia_nc_5_2021$new_cases[2])/2
avg_nc_Indo_5_2021[3] <- (Indonesia_nc_5_2021$new_cases[1] + Indonesia_nc_5_2021$new_cases[2] + Indonesia_nc_5_2021$new_cases[3])/3
avg_nc_Indo_5_2021[4] <- (Indonesia_nc_5_2021$new_cases[1] + Indonesia_nc_5_2021$new_cases[2] + Indonesia_nc_5_2021$new_cases[3] + Indonesia_nc_5_2021$new_cases[4])/4
avg_nc_Indo_5_2021[5] <- (Indonesia_nc_5_2021$new_cases[1] + Indonesia_nc_5_2021$new_cases[2] + Indonesia_nc_5_2021$new_cases[3] + Indonesia_nc_5_2021$new_cases[4] + Indonesia_nc_5_2021$new_cases[5])/5
avg_nc_Indo_5_2021[6] <- (Indonesia_nc_5_2021$new_cases[1] + Indonesia_nc_5_2021$new_cases[2] + Indonesia_nc_5_2021$new_cases[3] + Indonesia_nc_5_2021$new_cases[4] + Indonesia_nc_5_2021$new_cases[5] + Indonesia_nc_5_2021$new_cases[6])/6
for(i in 7:length(Indonesia_nc_5_2021$new_cases))
{
  avg_nc_Indo_5_2021[i]=(Indonesia_nc_5_2021$new_cases[i] + Indonesia_nc_5_2021$new_cases[i-1] + Indonesia_nc_5_2021$new_cases[i-2] + Indonesia_nc_5_2021$new_cases[i-3] + Indonesia_nc_5_2021$new_cases[i-4] + Indonesia_nc_5_2021$new_cases[i-5] + Indonesia_nc_5_2021$new_cases[i-6])/7
}
acml_nc_Indo_5_2021 <- c()
acml_nc_Indo_5_2021[1] <- avg_nc_Indo_5_2021[1]
for(i in 2:length(avg_nc_Indo_5_2021))
{
  acml_nc_Indo_5_2021[i] <- avg_nc_Indo_5_2021[i] + acml_nc_Indo_5_2021[i-1]
}
Indonesia_nc_5_2021 <- data.frame(Indonesia_nc_5_2021, avg_nc_Indo_5_2021, acml_nc_Indo_5_2021)



Japan_nc_5_2021 <-  na.omit(Japan_nc[Japan_nc$datetime >= "2021-05-01" & Japan_nc$datetime <= "2021-05-31",])
avg_nc_Jp_5_2021 <- c()
avg_nc_Jp_5_2021[1] <- Japan_nc_5_2021$new_cases[1]
avg_nc_Jp_5_2021[2] <- (Japan_nc_5_2021$new_cases[1] + Japan_nc_5_2021$new_cases[2])/2
avg_nc_Jp_5_2021[3] <- (Japan_nc_5_2021$new_cases[1] + Japan_nc_5_2021$new_cases[2] + Japan_nc_5_2021$new_cases[3])/3
avg_nc_Jp_5_2021[4] <- (Japan_nc_5_2021$new_cases[1] + Japan_nc_5_2021$new_cases[2] + Japan_nc_5_2021$new_cases[3] + Japan_nc_5_2021$new_cases[4])/4
avg_nc_Jp_5_2021[5] <- (Japan_nc_5_2021$new_cases[1] + Japan_nc_5_2021$new_cases[2] + Japan_nc_5_2021$new_cases[3] + Japan_nc_5_2021$new_cases[4] + Japan_nc_5_2021$new_cases[5])/5
avg_nc_Jp_5_2021[6] <- (Japan_nc_5_2021$new_cases[1] + Japan_nc_5_2021$new_cases[2] + Japan_nc_5_2021$new_cases[3] + Japan_nc_5_2021$new_cases[4] + Japan_nc_5_2021$new_cases[5] + Japan_nc_5_2021$new_cases[6])/6
for(i in 7:length(Japan_nc_5_2021$new_cases))
{
  avg_nc_Jp_5_2021[i]=(Japan_nc_5_2021$new_cases[i] + Japan_nc_5_2021$new_cases[i-1] + Japan_nc_5_2021$new_cases[i-2] + Japan_nc_5_2021$new_cases[i-3] + Japan_nc_5_2021$new_cases[i-4] + Japan_nc_5_2021$new_cases[i-5] + Japan_nc_5_2021$new_cases[i-6])/7
}
acml_nc_Jp_5_2021 <- c()
acml_nc_Jp_5_2021[1] <- avg_nc_Jp_5_2021[1]
for(i in 2:length(avg_nc_Jp_5_2021))
{
  acml_nc_Jp_5_2021[i] <- avg_nc_Jp_5_2021[i] + acml_nc_Jp_5_2021[i-1]
}
Japan_nc_5_2021 <- data.frame(Japan_nc_5_2021, avg_nc_Jp_5_2021, acml_nc_Jp_5_2021)



Vietnam_nc_5_2021 <- na.omit(Vietnam_nc[Vietnam_nc$datetime >= "2021-05-01" & Vietnam_nc$datetime <= "2021-05-31",])
avg_nc_Vn_5_2021 <- c()
avg_nc_Vn_5_2021[1] <- Vietnam_nc_5_2021$new_cases[1]
avg_nc_Vn_5_2021[2] <- (Vietnam_nc_5_2021$new_cases[1] + Vietnam_nc_5_2021$new_cases[2])/2
avg_nc_Vn_5_2021[3] <- (Vietnam_nc_5_2021$new_cases[1] + Vietnam_nc_5_2021$new_cases[2] + Vietnam_nc_5_2021$new_cases[3])/3
avg_nc_Vn_5_2021[4] <- (Vietnam_nc_5_2021$new_cases[1] + Vietnam_nc_5_2021$new_cases[2] + Vietnam_nc_5_2021$new_cases[3] + Vietnam_nc_5_2021$new_cases[4])/4
avg_nc_Vn_5_2021[5] <- (Vietnam_nc_5_2021$new_cases[1] + Vietnam_nc_5_2021$new_cases[2] + Vietnam_nc_5_2021$new_cases[3] + Vietnam_nc_5_2021$new_cases[4] + Vietnam_nc_5_2021$new_cases[5])/5
avg_nc_Vn_5_2021[6] <- (Vietnam_nc_5_2021$new_cases[1] + Vietnam_nc_5_2021$new_cases[2] + Vietnam_nc_5_2021$new_cases[3] + Vietnam_nc_5_2021$new_cases[4] + Vietnam_nc_5_2021$new_cases[5] + Vietnam_nc_5_2021$new_cases[6])/6
for(i in 7:length(Vietnam_nc_5_2021$new_cases))
{
  avg_nc_Vn_5_2021[i]=(Vietnam_nc_5_2021$new_cases[i] + Vietnam_nc_5_2021$new_cases[i-1] + Vietnam_nc_5_2021$new_cases[i-2] + Vietnam_nc_5_2021$new_cases[i-3] + Vietnam_nc_5_2021$new_cases[i-4] + Vietnam_nc_5_2021$new_cases[i-5] + Vietnam_nc_5_2021$new_cases[i-6])/7
}
acml_nc_Vn_5_2021 <- c()
acml_nc_Vn_5_2021[1] <- avg_nc_Vn_5_2021[1]
for(i in 2:length(avg_nc_Vn_5_2021))
{
  acml_nc_Vn_5_2021[i] <- avg_nc_Vn_5_2021[i] + acml_nc_Vn_5_2021[i-1]
}
Vietnam_nc_5_2021 <- data.frame(Vietnam_nc_5_2021, avg_nc_Vn_5_2021, acml_nc_Vn_5_2021)



#newcases th?ng 1 2022
Indonesia_nc_1_2022 <- na.omit(Indonesia_nc[Indonesia_nc$datetime >= "2022-01-01" & Indonesia_nc$datetime <= "2022-01-31",])
avg_nc_Indo_1_2022 <- c()
avg_nc_Indo_1_2022[1] <- Indonesia_nc_1_2022$new_cases[1]
avg_nc_Indo_1_2022[2] <- (Indonesia_nc_1_2022$new_cases[1] + Indonesia_nc_1_2022$new_cases[2])/2
avg_nc_Indo_1_2022[3] <- (Indonesia_nc_1_2022$new_cases[1] + Indonesia_nc_1_2022$new_cases[2] + Indonesia_nc_1_2022$new_cases[3])/3
avg_nc_Indo_1_2022[4] <- (Indonesia_nc_1_2022$new_cases[1] + Indonesia_nc_1_2022$new_cases[2] + Indonesia_nc_1_2022$new_cases[3] + Indonesia_nc_1_2022$new_cases[4])/4
avg_nc_Indo_1_2022[5] <- (Indonesia_nc_1_2022$new_cases[1] + Indonesia_nc_1_2022$new_cases[2] + Indonesia_nc_1_2022$new_cases[3] + Indonesia_nc_1_2022$new_cases[4] + Indonesia_nc_1_2022$new_cases[5])/5
avg_nc_Indo_1_2022[6] <- (Indonesia_nc_1_2022$new_cases[1] + Indonesia_nc_1_2022$new_cases[2] + Indonesia_nc_1_2022$new_cases[3] + Indonesia_nc_1_2022$new_cases[4] + Indonesia_nc_1_2022$new_cases[5] + Indonesia_nc_1_2022$new_cases[6])/6
for(i in 7:length(Indonesia_nc_1_2022$new_cases))
{
  avg_nc_Indo_1_2022[i]=(Indonesia_nc_1_2022$new_cases[i] + Indonesia_nc_1_2022$new_cases[i-1] + Indonesia_nc_1_2022$new_cases[i-2] + Indonesia_nc_1_2022$new_cases[i-3] + Indonesia_nc_1_2022$new_cases[i-4] + Indonesia_nc_1_2022$new_cases[i-5] + Indonesia_nc_1_2022$new_cases[i-6])/7
}
acml_nc_Indo_1_2022 <- c()
acml_nc_Indo_1_2022[1] <- avg_nc_Indo_1_2022[1]
for(i in 2:length(avg_nc_Indo_1_2022))
{
  acml_nc_Indo_1_2022[i] <- avg_nc_Indo_1_2022[i] + acml_nc_Indo_1_2022[i-1]
}
Indonesia_nc_1_2022 <- data.frame(Indonesia_nc_1_2022, avg_nc_Indo_1_2022, acml_nc_Indo_1_2022)



Japan_nc_1_2022 <-  na.omit(Japan_nc[Japan_nc$datetime >= "2022-01-01" & Japan_nc$datetime <= "2022-01-31",])
avg_nc_Jp_1_2022 <- c()
avg_nc_Jp_1_2022[1] <- Japan_nc_1_2022$new_cases[1]
avg_nc_Jp_1_2022[2] <- (Japan_nc_1_2022$new_cases[1] + Japan_nc_1_2022$new_cases[2])/2
avg_nc_Jp_1_2022[3] <- (Japan_nc_1_2022$new_cases[1] + Japan_nc_1_2022$new_cases[2] + Japan_nc_1_2022$new_cases[3])/3
avg_nc_Jp_1_2022[4] <- (Japan_nc_1_2022$new_cases[1] + Japan_nc_1_2022$new_cases[2] + Japan_nc_1_2022$new_cases[3] + Japan_nc_1_2022$new_cases[4])/4
avg_nc_Jp_1_2022[5] <- (Japan_nc_1_2022$new_cases[1] + Japan_nc_1_2022$new_cases[2] + Japan_nc_1_2022$new_cases[3] + Japan_nc_1_2022$new_cases[4] + Japan_nc_1_2022$new_cases[5])/5
avg_nc_Jp_1_2022[6] <- (Japan_nc_1_2022$new_cases[1] + Japan_nc_1_2022$new_cases[2] + Japan_nc_1_2022$new_cases[3] + Japan_nc_1_2022$new_cases[4] + Japan_nc_1_2022$new_cases[5] + Japan_nc_1_2022$new_cases[6])/6
for(i in 7:length(Japan_nc_1_2022$new_cases))
{
  avg_nc_Jp_1_2022[i]=(Japan_nc_1_2022$new_cases[i] + Japan_nc_1_2022$new_cases[i-1] + Japan_nc_1_2022$new_cases[i-2] + Japan_nc_1_2022$new_cases[i-3] + Japan_nc_1_2022$new_cases[i-4] + Japan_nc_1_2022$new_cases[i-5] + Japan_nc_1_2022$new_cases[i-6])/7
}
acml_nc_Jp_1_2022 <- c()
acml_nc_Jp_1_2022[1] <- avg_nc_Jp_1_2022[1]
for(i in 2:length(avg_nc_Jp_1_2022))
{
  acml_nc_Jp_1_2022[i] <- avg_nc_Jp_1_2022[i] + acml_nc_Jp_1_2022[i-1]
}
Japan_nc_1_2022 <- data.frame(Japan_nc_1_2022, avg_nc_Jp_1_2022, acml_nc_Jp_1_2022)



Vietnam_nc_1_2022 <- na.omit(Vietnam_nc[Vietnam_nc$datetime >= "2022-01-01" & Vietnam_nc$datetime <= "2022-01-31",])
avg_nc_Vn_1_2022 <- c()
avg_nc_Vn_1_2022[1] <- Vietnam_nc_1_2022$new_cases[1]
avg_nc_Vn_1_2022[2] <- (Vietnam_nc_1_2022$new_cases[1] + Vietnam_nc_1_2022$new_cases[2])/2
avg_nc_Vn_1_2022[3] <- (Vietnam_nc_1_2022$new_cases[1] + Vietnam_nc_1_2022$new_cases[2] + Vietnam_nc_1_2022$new_cases[3])/3
avg_nc_Vn_1_2022[4] <- (Vietnam_nc_1_2022$new_cases[1] + Vietnam_nc_1_2022$new_cases[2] + Vietnam_nc_1_2022$new_cases[3] + Vietnam_nc_1_2022$new_cases[4])/4
avg_nc_Vn_1_2022[5] <- (Vietnam_nc_1_2022$new_cases[1] + Vietnam_nc_1_2022$new_cases[2] + Vietnam_nc_1_2022$new_cases[3] + Vietnam_nc_1_2022$new_cases[4] + Vietnam_nc_1_2022$new_cases[5])/5
avg_nc_Vn_1_2022[6] <- (Vietnam_nc_1_2022$new_cases[1] + Vietnam_nc_1_2022$new_cases[2] + Vietnam_nc_1_2022$new_cases[3] + Vietnam_nc_1_2022$new_cases[4] + Vietnam_nc_1_2022$new_cases[5] + Vietnam_nc_1_2022$new_cases[6])/6
for(i in 7:length(Vietnam_nc_1_2022$new_cases))
{
  avg_nc_Vn_1_2022[i]=(Vietnam_nc_1_2022$new_cases[i] + Vietnam_nc_1_2022$new_cases[i-1] + Vietnam_nc_1_2022$new_cases[i-2] + Vietnam_nc_1_2022$new_cases[i-3] + Vietnam_nc_1_2022$new_cases[i-4] + Vietnam_nc_1_2022$new_cases[i-5] + Vietnam_nc_1_2022$new_cases[i-6])/7
}
acml_nc_Vn_1_2022 <- c()
acml_nc_Vn_1_2022[1] <- avg_nc_Vn_1_2022[1]
for(i in 2:length(avg_nc_Vn_1_2022))
{
  acml_nc_Vn_1_2022[i] <- avg_nc_Vn_1_2022[i] + acml_nc_Vn_1_2022[i-1]
}
Vietnam_nc_1_2022 <- data.frame(Vietnam_nc_1_2022, avg_nc_Vn_1_2022, acml_nc_Vn_1_2022)



#du lieu newdeaths
new.dat2 <- data.frame(location, datetime, new_deaths)

#loc theo nuoc
Indonesia_nd <- new.dat2[new.dat2$location == "Indonesia",]
Japan_nd <- new.dat2[new.dat2$location == "Japan",]
Vietnam_nd <- new.dat2[new.dat2$location == "Vietnam",]

#newdeaths th?ng 1 2020
Indonesia_nd_1_2020 <- na.omit(Indonesia_nd[Indonesia_nd$datetime >= "2020-01-01" & Indonesia_nd$datetime <= "2020-01-31",])


Japan_nd_1_2020 <-  na.omit(Japan_nd[Japan_nd$datetime >= "2020-01-01" & Japan_nd$datetime <= "2020-01-31",])


Vietnam_nd_1_2020 <- na.omit(Vietnam_nd[Vietnam_nd$datetime >= "2020-01-01" & Vietnam_nd$datetime <= "2020-01-31",])




#Newdeaths th?ng 3 2020
Indonesia_nd_3_2020 <- na.omit(Indonesia_nd[Indonesia_nd$datetime >= "2020-03-01" & Indonesia_nd$datetime <= "2020-03-31",])
avg_nd_Indo_3_2020 <- c()
avg_nd_Indo_3_2020[1] <- Indonesia_nd_3_2020$new_deaths[1]
avg_nd_Indo_3_2020[2] <- (Indonesia_nd_3_2020$new_deaths[1] + Indonesia_nd_3_2020$new_deaths[2])/2
avg_nd_Indo_3_2020[3] <- (Indonesia_nd_3_2020$new_deaths[1] + Indonesia_nd_3_2020$new_deaths[2] + Indonesia_nd_3_2020$new_deaths[3])/3
avg_nd_Indo_3_2020[4] <- (Indonesia_nd_3_2020$new_deaths[1] + Indonesia_nd_3_2020$new_deaths[2] + Indonesia_nd_3_2020$new_deaths[3] + Indonesia_nd_3_2020$new_deaths[4])/4
avg_nd_Indo_3_2020[5] <- (Indonesia_nd_3_2020$new_deaths[1] + Indonesia_nd_3_2020$new_deaths[2] + Indonesia_nd_3_2020$new_deaths[3] + Indonesia_nd_3_2020$new_deaths[4] + Indonesia_nd_3_2020$new_deaths[5])/5
avg_nd_Indo_3_2020[6] <- (Indonesia_nd_3_2020$new_deaths[1] + Indonesia_nd_3_2020$new_deaths[2] + Indonesia_nd_3_2020$new_deaths[3] + Indonesia_nd_3_2020$new_deaths[4] + Indonesia_nd_3_2020$new_deaths[5] + Indonesia_nd_3_2020$new_deaths[6])/6
for(i in 7:length(Indonesia_nd_3_2020$new_deaths))
{
  avg_nd_Indo_3_2020[i]=(Indonesia_nd_3_2020$new_deaths[i] + Indonesia_nd_3_2020$new_deaths[i-1] + Indonesia_nd_3_2020$new_deaths[i-2] + Indonesia_nd_3_2020$new_deaths[i-3] + Indonesia_nd_3_2020$new_deaths[i-4] + Indonesia_nd_3_2020$new_deaths[i-5] + Indonesia_nd_3_2020$new_deaths[i-6])/7
}
acml_nd_Indo_3_2020 <- c()
acml_nd_Indo_3_2020[1] <- avg_nd_Indo_3_2020[1]
for(i in 2:length(avg_nd_Indo_3_2020))
{
  acml_nd_Indo_3_2020[i] <- avg_nd_Indo_3_2020[i] + acml_nd_Indo_3_2020[i-1]
}
Indonesia_nd_3_2020 <- data.frame(Indonesia_nd_3_2020, avg_nd_Indo_3_2020, acml_nd_Indo_3_2020)



Japan_nd_3_2020 <-  na.omit(Japan_nd[Japan_nd$datetime >= "2020-03-01" & Japan_nd$datetime <= "2020-03-31",])
avg_nd_Jp_3_2020 <- c()
avg_nd_Jp_3_2020[1] <- Japan_nd_3_2020$new_deaths[1]
avg_nd_Jp_3_2020[2] <- (Japan_nd_3_2020$new_deaths[1] + Japan_nd_3_2020$new_deaths[2])/2
avg_nd_Jp_3_2020[3] <- (Japan_nd_3_2020$new_deaths[1] + Japan_nd_3_2020$new_deaths[2] + Japan_nd_3_2020$new_deaths[3])/3
avg_nd_Jp_3_2020[4] <- (Japan_nd_3_2020$new_deaths[1] + Japan_nd_3_2020$new_deaths[2] + Japan_nd_3_2020$new_deaths[3] + Japan_nd_3_2020$new_deaths[4])/4
avg_nd_Jp_3_2020[5] <- (Japan_nd_3_2020$new_deaths[1] + Japan_nd_3_2020$new_deaths[2] + Japan_nd_3_2020$new_deaths[3] + Japan_nd_3_2020$new_deaths[4] + Japan_nd_3_2020$new_deaths[5])/5
avg_nd_Jp_3_2020[6] <- (Japan_nd_3_2020$new_deaths[1] + Japan_nd_3_2020$new_deaths[2] + Japan_nd_3_2020$new_deaths[3] + Japan_nd_3_2020$new_deaths[4] + Japan_nd_3_2020$new_deaths[5] + Japan_nd_3_2020$new_deaths[6])/6
for(i in 7:length(Japan_nd_3_2020$new_deaths))
{
  avg_nd_Jp_3_2020[i]=(Japan_nd_3_2020$new_deaths[i] + Japan_nd_3_2020$new_deaths[i-1] + Japan_nd_3_2020$new_deaths[i-2] + Japan_nd_3_2020$new_deaths[i-3] + Japan_nd_3_2020$new_deaths[i-4] + Japan_nd_3_2020$new_deaths[i-5] + Japan_nd_3_2020$new_deaths[i-6])/7
}
acml_nd_Jp_3_2020 <- c()
acml_nd_Jp_3_2020[1] <- avg_nd_Jp_3_2020[1]
for(i in 2:length(avg_nd_Jp_3_2020))
{
  acml_nd_Jp_3_2020[i] <- avg_nd_Jp_3_2020[i] + acml_nd_Jp_3_2020[i-1]
}
Japan_nd_3_2020 <- data.frame(Japan_nd_3_2020, avg_nd_Jp_3_2020, acml_nd_Jp_3_2020)



Vietnam_nd_3_2020 <- na.omit(Vietnam_nd[Vietnam_nd$datetime >= "2020-03-01" & Vietnam_nd$datetime <= "2020-03-31",])




#Newdeaths thang 4 2020
Indonesia_nd_4_2020 <- na.omit(Indonesia_nd[Indonesia_nd$datetime >= "2020-04-01" & Indonesia_nd$datetime <= "2020-04-30",])
avg_nd_Indo_4_2020 <- c()
avg_nd_Indo_4_2020[1] <- Indonesia_nd_4_2020$new_deaths[1]
avg_nd_Indo_4_2020[2] <- (Indonesia_nd_4_2020$new_deaths[1] + Indonesia_nd_4_2020$new_deaths[2])/2
avg_nd_Indo_4_2020[3] <- (Indonesia_nd_4_2020$new_deaths[1] + Indonesia_nd_4_2020$new_deaths[2] + Indonesia_nd_4_2020$new_deaths[3])/3
avg_nd_Indo_4_2020[4] <- (Indonesia_nd_4_2020$new_deaths[1] + Indonesia_nd_4_2020$new_deaths[2] + Indonesia_nd_4_2020$new_deaths[3] + Indonesia_nd_4_2020$new_deaths[4])/4
avg_nd_Indo_4_2020[5] <- (Indonesia_nd_4_2020$new_deaths[1] + Indonesia_nd_4_2020$new_deaths[2] + Indonesia_nd_4_2020$new_deaths[3] + Indonesia_nd_4_2020$new_deaths[4] + Indonesia_nd_4_2020$new_deaths[5])/5
avg_nd_Indo_4_2020[6] <- (Indonesia_nd_4_2020$new_deaths[1] + Indonesia_nd_4_2020$new_deaths[2] + Indonesia_nd_4_2020$new_deaths[3] + Indonesia_nd_4_2020$new_deaths[4] + Indonesia_nd_4_2020$new_deaths[5] + Indonesia_nd_4_2020$new_deaths[6])/6
for(i in 7:length(Indonesia_nd_4_2020$new_deaths))
{
  avg_nd_Indo_4_2020[i]=(Indonesia_nd_4_2020$new_deaths[i] + Indonesia_nd_4_2020$new_deaths[i-1] + Indonesia_nd_4_2020$new_deaths[i-2] + Indonesia_nd_4_2020$new_deaths[i-3] + Indonesia_nd_4_2020$new_deaths[i-4] + Indonesia_nd_4_2020$new_deaths[i-5] + Indonesia_nd_4_2020$new_deaths[i-6])/7
}
acml_nd_Indo_4_2020 <- c()
acml_nd_Indo_4_2020[1] <- avg_nd_Indo_4_2020[1]
for(i in 2:length(avg_nd_Indo_4_2020))
{
  acml_nd_Indo_4_2020[i] <- avg_nd_Indo_4_2020[i] + acml_nd_Indo_4_2020[i-1]
}
Indonesia_nd_4_2020 <- data.frame(Indonesia_nd_4_2020, avg_nd_Indo_4_2020, acml_nd_Indo_4_2020)



Japan_nd_4_2020 <-  na.omit(Japan_nd[Japan_nd$datetime >= "2020-04-01" & Japan_nd$datetime <= "2020-04-30",])
avg_nd_Jp_4_2020 <- c()
avg_nd_Jp_4_2020[1] <- Japan_nd_4_2020$new_deaths[1]
avg_nd_Jp_4_2020[2] <- (Japan_nd_4_2020$new_deaths[1] + Japan_nd_4_2020$new_deaths[2])/2
avg_nd_Jp_4_2020[3] <- (Japan_nd_4_2020$new_deaths[1] + Japan_nd_4_2020$new_deaths[2] + Japan_nd_4_2020$new_deaths[3])/3
avg_nd_Jp_4_2020[4] <- (Japan_nd_4_2020$new_deaths[1] + Japan_nd_4_2020$new_deaths[2] + Japan_nd_4_2020$new_deaths[3] + Japan_nd_4_2020$new_deaths[4])/4
avg_nd_Jp_4_2020[5] <- (Japan_nd_4_2020$new_deaths[1] + Japan_nd_4_2020$new_deaths[2] + Japan_nd_4_2020$new_deaths[3] + Japan_nd_4_2020$new_deaths[4] + Japan_nd_4_2020$new_deaths[5])/5
avg_nd_Jp_4_2020[6] <- (Japan_nd_4_2020$new_deaths[1] + Japan_nd_4_2020$new_deaths[2] + Japan_nd_4_2020$new_deaths[3] + Japan_nd_4_2020$new_deaths[4] + Japan_nd_4_2020$new_deaths[5] + Japan_nd_4_2020$new_deaths[6])/6
for(i in 7:length(Japan_nd_4_2020$new_deaths))
{
  avg_nd_Jp_4_2020[i]=(Japan_nd_4_2020$new_deaths[i] + Japan_nd_4_2020$new_deaths[i-1] + Japan_nd_4_2020$new_deaths[i-2] + Japan_nd_4_2020$new_deaths[i-3] + Japan_nd_4_2020$new_deaths[i-4] + Japan_nd_4_2020$new_deaths[i-5] + Japan_nd_4_2020$new_deaths[i-6])/7
}
acml_nd_Jp_4_2020 <- c()
acml_nd_Jp_4_2020[1] <- avg_nd_Jp_4_2020[1]
for(i in 2:length(avg_nd_Jp_4_2020))
{
  acml_nd_Jp_4_2020[i] <- avg_nd_Jp_4_2020[i] + acml_nd_Jp_4_2020[i-1]
}
Japan_nd_4_2020 <- data.frame(Japan_nd_4_2020, avg_nd_Jp_4_2020, acml_nd_Jp_4_2020)



Vietnam_nd_4_2020 <- na.omit(Vietnam_nd[Vietnam_nd$datetime >= "2020-04-01" & Vietnam_nd$datetime <= "2020-04-30",])



#Newdeaths thang 5 2020
Indonesia_nd_5_2020 <- na.omit(Indonesia_nd[Indonesia_nd$datetime >= "2020-05-01" & Indonesia_nd$datetime <= "2020-05-31",])
avg_nd_Indo_5_2020 <- c()
avg_nd_Indo_5_2020[1] <- Indonesia_nd_5_2020$new_deaths[1]
avg_nd_Indo_5_2020[2] <- (Indonesia_nd_5_2020$new_deaths[1] + Indonesia_nd_5_2020$new_deaths[2])/2
avg_nd_Indo_5_2020[3] <- (Indonesia_nd_5_2020$new_deaths[1] + Indonesia_nd_5_2020$new_deaths[2] + Indonesia_nd_5_2020$new_deaths[3])/3
avg_nd_Indo_5_2020[4] <- (Indonesia_nd_5_2020$new_deaths[1] + Indonesia_nd_5_2020$new_deaths[2] + Indonesia_nd_5_2020$new_deaths[3] + Indonesia_nd_5_2020$new_deaths[4])/4
avg_nd_Indo_5_2020[5] <- (Indonesia_nd_5_2020$new_deaths[1] + Indonesia_nd_5_2020$new_deaths[2] + Indonesia_nd_5_2020$new_deaths[3] + Indonesia_nd_5_2020$new_deaths[4] + Indonesia_nd_5_2020$new_deaths[5])/5
avg_nd_Indo_5_2020[6] <- (Indonesia_nd_5_2020$new_deaths[1] + Indonesia_nd_5_2020$new_deaths[2] + Indonesia_nd_5_2020$new_deaths[3] + Indonesia_nd_5_2020$new_deaths[4] + Indonesia_nd_5_2020$new_deaths[5] + Indonesia_nd_5_2020$new_deaths[6])/6
for(i in 7:length(Indonesia_nd_5_2020$new_deaths))
{
  avg_nd_Indo_5_2020[i]=(Indonesia_nd_5_2020$new_deaths[i] + Indonesia_nd_5_2020$new_deaths[i-1] + Indonesia_nd_5_2020$new_deaths[i-2] + Indonesia_nd_5_2020$new_deaths[i-3] + Indonesia_nd_5_2020$new_deaths[i-4] + Indonesia_nd_5_2020$new_deaths[i-5] + Indonesia_nd_5_2020$new_deaths[i-6])/7
}
acml_nd_Indo_5_2020 <- c()
acml_nd_Indo_5_2020[1] <- avg_nd_Indo_5_2020[1]
for(i in 2:length(avg_nd_Indo_5_2020))
{
  acml_nd_Indo_5_2020[i] <- avg_nd_Indo_5_2020[i] + acml_nd_Indo_5_2020[i-1]
}
Indonesia_nd_5_2020 <- data.frame(Indonesia_nd_5_2020, avg_nd_Indo_5_2020, acml_nd_Indo_5_2020)



Japan_nd_5_2020 <-  na.omit(Japan_nd[Japan_nd$datetime >= "2020-05-01" & Japan_nd$datetime <= "2020-05-31",])
avg_nd_Jp_5_2020 <- c()
avg_nd_Jp_5_2020[1] <- Japan_nd_5_2020$new_deaths[1]
avg_nd_Jp_5_2020[2] <- (Japan_nd_5_2020$new_deaths[1] + Japan_nd_5_2020$new_deaths[2])/2
avg_nd_Jp_5_2020[3] <- (Japan_nd_5_2020$new_deaths[1] + Japan_nd_5_2020$new_deaths[2] + Japan_nd_5_2020$new_deaths[3])/3
avg_nd_Jp_5_2020[4] <- (Japan_nd_5_2020$new_deaths[1] + Japan_nd_5_2020$new_deaths[2] + Japan_nd_5_2020$new_deaths[3] + Japan_nd_5_2020$new_deaths[4])/4
avg_nd_Jp_5_2020[5] <- (Japan_nd_5_2020$new_deaths[1] + Japan_nd_5_2020$new_deaths[2] + Japan_nd_5_2020$new_deaths[3] + Japan_nd_5_2020$new_deaths[4] + Japan_nd_5_2020$new_deaths[5])/5
avg_nd_Jp_5_2020[6] <- (Japan_nd_5_2020$new_deaths[1] + Japan_nd_5_2020$new_deaths[2] + Japan_nd_5_2020$new_deaths[3] + Japan_nd_5_2020$new_deaths[4] + Japan_nd_5_2020$new_deaths[5] + Japan_nd_5_2020$new_deaths[6])/6
for(i in 7:length(Japan_nd_5_2020$new_deaths))
{
  avg_nd_Jp_5_2020[i]=(Japan_nd_5_2020$new_deaths[i] + Japan_nd_5_2020$new_deaths[i-1] + Japan_nd_5_2020$new_deaths[i-2] + Japan_nd_5_2020$new_deaths[i-3] + Japan_nd_5_2020$new_deaths[i-4] + Japan_nd_5_2020$new_deaths[i-5] + Japan_nd_5_2020$new_deaths[i-6])/7
}
acml_nd_Jp_5_2020 <- c()
acml_nd_Jp_5_2020[1] <- avg_nd_Jp_5_2020[1]
for(i in 2:length(avg_nd_Jp_5_2020))
{
  acml_nd_Jp_5_2020[i] <- avg_nd_Jp_5_2020[i] + acml_nd_Jp_5_2020[i-1]
}
Japan_nd_5_2020 <- data.frame(Japan_nd_5_2020, avg_nd_Jp_5_2020, acml_nd_Jp_5_2020)



Vietnam_nd_5_2020 <- na.omit(Vietnam_nd[Vietnam_nd$datetime >= "2020-05-01" & Vietnam_nd$datetime <= "2020-05-31",])




#newdeaths th?ng 1 2021
Indonesia_nd_1_2021 <- na.omit(Indonesia_nd[Indonesia_nd$datetime >= "2021-01-01" & Indonesia_nd$datetime <= "2021-01-31",])
avg_nd_Indo_1_2021 <- c()
avg_nd_Indo_1_2021[1] <- Indonesia_nd_1_2021$new_deaths[1]
avg_nd_Indo_1_2021[2] <- (Indonesia_nd_1_2021$new_deaths[1] + Indonesia_nd_1_2021$new_deaths[2])/2
avg_nd_Indo_1_2021[3] <- (Indonesia_nd_1_2021$new_deaths[1] + Indonesia_nd_1_2021$new_deaths[2] + Indonesia_nd_1_2021$new_deaths[3])/3
avg_nd_Indo_1_2021[4] <- (Indonesia_nd_1_2021$new_deaths[1] + Indonesia_nd_1_2021$new_deaths[2] + Indonesia_nd_1_2021$new_deaths[3] + Indonesia_nd_1_2021$new_deaths[4])/4
avg_nd_Indo_1_2021[5] <- (Indonesia_nd_1_2021$new_deaths[1] + Indonesia_nd_1_2021$new_deaths[2] + Indonesia_nd_1_2021$new_deaths[3] + Indonesia_nd_1_2021$new_deaths[4] + Indonesia_nd_1_2021$new_deaths[5])/5
avg_nd_Indo_1_2021[6] <- (Indonesia_nd_1_2021$new_deaths[1] + Indonesia_nd_1_2021$new_deaths[2] + Indonesia_nd_1_2021$new_deaths[3] + Indonesia_nd_1_2021$new_deaths[4] + Indonesia_nd_1_2021$new_deaths[5] + Indonesia_nd_1_2021$new_deaths[6])/6
for(i in 7:length(Indonesia_nd_1_2021$new_deaths))
{
  avg_nd_Indo_1_2021[i]=(Indonesia_nd_1_2021$new_deaths[i] + Indonesia_nd_1_2021$new_deaths[i-1] + Indonesia_nd_1_2021$new_deaths[i-2] + Indonesia_nd_1_2021$new_deaths[i-3] + Indonesia_nd_1_2021$new_deaths[i-4] + Indonesia_nd_1_2021$new_deaths[i-5] + Indonesia_nd_1_2021$new_deaths[i-6])/7
}
acml_nd_Indo_1_2021 <- c()
acml_nd_Indo_1_2021[1] <- avg_nd_Indo_1_2021[1]
for(i in 2:length(avg_nd_Indo_1_2021))
{
  acml_nd_Indo_1_2021[i] <- avg_nd_Indo_1_2021[i] + acml_nd_Indo_1_2021[i-1]
}
Indonesia_nd_1_2021 <- data.frame(Indonesia_nd_1_2021, avg_nd_Indo_1_2021, acml_nd_Indo_1_2021)



Japan_nd_1_2021 <-  na.omit(Japan_nd[Japan_nd$datetime >= "2021-01-01" & Japan_nd$datetime <= "2021-01-31",])
avg_nd_Jp_1_2021 <- c()
avg_nd_Jp_1_2021[1] <- Japan_nd_1_2021$new_deaths[1]
avg_nd_Jp_1_2021[2] <- (Japan_nd_1_2021$new_deaths[1] + Japan_nd_1_2021$new_deaths[2])/2
avg_nd_Jp_1_2021[3] <- (Japan_nd_1_2021$new_deaths[1] + Japan_nd_1_2021$new_deaths[2] + Japan_nd_1_2021$new_deaths[3])/3
avg_nd_Jp_1_2021[4] <- (Japan_nd_1_2021$new_deaths[1] + Japan_nd_1_2021$new_deaths[2] + Japan_nd_1_2021$new_deaths[3] + Japan_nd_1_2021$new_deaths[4])/4
avg_nd_Jp_1_2021[5] <- (Japan_nd_1_2021$new_deaths[1] + Japan_nd_1_2021$new_deaths[2] + Japan_nd_1_2021$new_deaths[3] + Japan_nd_1_2021$new_deaths[4] + Japan_nd_1_2021$new_deaths[5])/5
avg_nd_Jp_1_2021[6] <- (Japan_nd_1_2021$new_deaths[1] + Japan_nd_1_2021$new_deaths[2] + Japan_nd_1_2021$new_deaths[3] + Japan_nd_1_2021$new_deaths[4] + Japan_nd_1_2021$new_deaths[5] + Japan_nd_1_2021$new_deaths[6])/6
for(i in 7:length(Japan_nd_1_2021$new_deaths))
{
  avg_nd_Jp_1_2021[i]=(Japan_nd_1_2021$new_deaths[i] + Japan_nd_1_2021$new_deaths[i-1] + Japan_nd_1_2021$new_deaths[i-2] + Japan_nd_1_2021$new_deaths[i-3] + Japan_nd_1_2021$new_deaths[i-4] + Japan_nd_1_2021$new_deaths[i-5] + Japan_nd_1_2021$new_deaths[i-6])/7
}
acml_nd_Jp_1_2021 <- c()
acml_nd_Jp_1_2021[1] <- avg_nd_Jp_1_2021[1]
for(i in 2:length(avg_nd_Jp_1_2021))
{
  acml_nd_Jp_1_2021[i] <- avg_nd_Jp_1_2021[i] + acml_nd_Jp_1_2021[i-1]
}
Japan_nd_1_2021 <- data.frame(Japan_nd_1_2021, avg_nd_Jp_1_2021, acml_nd_Jp_1_2021)



Vietnam_nd_1_2021 <- na.omit(Vietnam_nd[Vietnam_nd$datetime >= "2021-01-01" & Vietnam_nd$datetime <= "2021-01-31",])
avg_nd_Vn_1_2021 <- c()
avg_nd_Vn_1_2021[1] <- Vietnam_nd_1_2021$new_deaths[1]
avg_nd_Vn_1_2021[2] <- (Vietnam_nd_1_2021$new_deaths[1] + Vietnam_nd_1_2021$new_deaths[2])/2
avg_nd_Vn_1_2021[3] <- (Vietnam_nd_1_2021$new_deaths[1] + Vietnam_nd_1_2021$new_deaths[2] + Vietnam_nd_1_2021$new_deaths[3])/3
avg_nd_Vn_1_2021[4] <- (Vietnam_nd_1_2021$new_deaths[1] + Vietnam_nd_1_2021$new_deaths[2] + Vietnam_nd_1_2021$new_deaths[3] + Vietnam_nd_1_2021$new_deaths[4])/4
avg_nd_Vn_1_2021[5] <- (Vietnam_nd_1_2021$new_deaths[1] + Vietnam_nd_1_2021$new_deaths[2] + Vietnam_nd_1_2021$new_deaths[3] + Vietnam_nd_1_2021$new_deaths[4] + Vietnam_nd_1_2021$new_deaths[5])/5
avg_nd_Vn_1_2021[6] <- (Vietnam_nd_1_2021$new_deaths[1] + Vietnam_nd_1_2021$new_deaths[2] + Vietnam_nd_1_2021$new_deaths[3] + Vietnam_nd_1_2021$new_deaths[4] + Vietnam_nd_1_2021$new_deaths[5] + Vietnam_nd_1_2021$new_deaths[6])/6
for(i in 7:length(Vietnam_nd_1_2021$new_deaths))
{
  avg_nd_Vn_1_2021[i]=(Vietnam_nd_1_2021$new_deaths[i] + Vietnam_nd_1_2021$new_deaths[i-1] + Vietnam_nd_1_2021$new_deaths[i-2] + Vietnam_nd_1_2021$new_deaths[i-3] + Vietnam_nd_1_2021$new_deaths[i-4] + Vietnam_nd_1_2021$new_deaths[i-5] + Vietnam_nd_1_2021$new_deaths[i-6])/7
}
acml_nd_Vn_1_2021 <- c()
acml_nd_Vn_1_2021[1] <- avg_nd_Vn_1_2021[1]
for(i in 2:length(avg_nd_Vn_1_2021))
{
  acml_nd_Vn_1_2021[i] <- avg_nd_Vn_1_2021[i] + acml_nd_Vn_1_2021[i-1]
}
Vietnam_nd_1_2021 <- data.frame(Vietnam_nd_1_2021, avg_nd_Vn_1_2021, acml_nd_Vn_1_2021)




#Newdeaths th?ng 3 2021
Indonesia_nd_3_2021 <- na.omit(Indonesia_nd[Indonesia_nd$datetime >= "2021-03-01" & Indonesia_nd$datetime <= "2021-03-31",])
avg_nd_Indo_3_2021 <- c()
avg_nd_Indo_3_2021[1] <- Indonesia_nd_3_2021$new_deaths[1]
avg_nd_Indo_3_2021[2] <- (Indonesia_nd_3_2021$new_deaths[1] + Indonesia_nd_3_2021$new_deaths[2])/2
avg_nd_Indo_3_2021[3] <- (Indonesia_nd_3_2021$new_deaths[1] + Indonesia_nd_3_2021$new_deaths[2] + Indonesia_nd_3_2021$new_deaths[3])/3
avg_nd_Indo_3_2021[4] <- (Indonesia_nd_3_2021$new_deaths[1] + Indonesia_nd_3_2021$new_deaths[2] + Indonesia_nd_3_2021$new_deaths[3] + Indonesia_nd_3_2021$new_deaths[4])/4
avg_nd_Indo_3_2021[5] <- (Indonesia_nd_3_2021$new_deaths[1] + Indonesia_nd_3_2021$new_deaths[2] + Indonesia_nd_3_2021$new_deaths[3] + Indonesia_nd_3_2021$new_deaths[4] + Indonesia_nd_3_2021$new_deaths[5])/5
avg_nd_Indo_3_2021[6] <- (Indonesia_nd_3_2021$new_deaths[1] + Indonesia_nd_3_2021$new_deaths[2] + Indonesia_nd_3_2021$new_deaths[3] + Indonesia_nd_3_2021$new_deaths[4] + Indonesia_nd_3_2021$new_deaths[5] + Indonesia_nd_3_2021$new_deaths[6])/6
for(i in 7:length(Indonesia_nd_3_2021$new_deaths))
{
  avg_nd_Indo_3_2021[i]=(Indonesia_nd_3_2021$new_deaths[i] + Indonesia_nd_3_2021$new_deaths[i-1] + Indonesia_nd_3_2021$new_deaths[i-2] + Indonesia_nd_3_2021$new_deaths[i-3] + Indonesia_nd_3_2021$new_deaths[i-4] + Indonesia_nd_3_2021$new_deaths[i-5] + Indonesia_nd_3_2021$new_deaths[i-6])/7
}
acml_nd_Indo_3_2021 <- c()
acml_nd_Indo_3_2021[1] <- avg_nd_Indo_3_2021[1]
for(i in 2:length(avg_nd_Indo_3_2021))
{
  acml_nd_Indo_3_2021[i] <- avg_nd_Indo_3_2021[i] + acml_nd_Indo_3_2021[i-1]
}
Indonesia_nd_3_2021 <- data.frame(Indonesia_nd_3_2021, avg_nd_Indo_3_2021, acml_nd_Indo_3_2021)



Japan_nd_3_2021 <-  na.omit(Japan_nd[Japan_nd$datetime >= "2021-03-01" & Japan_nd$datetime <= "2021-03-31",])
avg_nd_Jp_3_2021 <- c()
avg_nd_Jp_3_2021[1] <- Japan_nd_3_2021$new_deaths[1]
avg_nd_Jp_3_2021[2] <- (Japan_nd_3_2021$new_deaths[1] + Japan_nd_3_2021$new_deaths[2])/2
avg_nd_Jp_3_2021[3] <- (Japan_nd_3_2021$new_deaths[1] + Japan_nd_3_2021$new_deaths[2] + Japan_nd_3_2021$new_deaths[3])/3
avg_nd_Jp_3_2021[4] <- (Japan_nd_3_2021$new_deaths[1] + Japan_nd_3_2021$new_deaths[2] + Japan_nd_3_2021$new_deaths[3] + Japan_nd_3_2021$new_deaths[4])/4
avg_nd_Jp_3_2021[5] <- (Japan_nd_3_2021$new_deaths[1] + Japan_nd_3_2021$new_deaths[2] + Japan_nd_3_2021$new_deaths[3] + Japan_nd_3_2021$new_deaths[4] + Japan_nd_3_2021$new_deaths[5])/5
avg_nd_Jp_3_2021[6] <- (Japan_nd_3_2021$new_deaths[1] + Japan_nd_3_2021$new_deaths[2] + Japan_nd_3_2021$new_deaths[3] + Japan_nd_3_2021$new_deaths[4] + Japan_nd_3_2021$new_deaths[5] + Japan_nd_3_2021$new_deaths[6])/6
for(i in 7:length(Japan_nd_3_2021$new_deaths))
{
  avg_nd_Jp_3_2021[i]=(Japan_nd_3_2021$new_deaths[i] + Japan_nd_3_2021$new_deaths[i-1] + Japan_nd_3_2021$new_deaths[i-2] + Japan_nd_3_2021$new_deaths[i-3] + Japan_nd_3_2021$new_deaths[i-4] + Japan_nd_3_2021$new_deaths[i-5] + Japan_nd_3_2021$new_deaths[i-6])/7
}
acml_nd_Jp_3_2021 <- c()
acml_nd_Jp_3_2021[1] <- avg_nd_Jp_3_2021[1]
for(i in 2:length(avg_nd_Jp_3_2021))
{
  acml_nd_Jp_3_2021[i] <- avg_nd_Jp_3_2021[i] + acml_nd_Jp_3_2021[i-1]
}
Japan_nd_3_2021 <- data.frame(Japan_nd_3_2021, avg_nd_Jp_3_2021, acml_nd_Jp_3_2021)



Vietnam_nd_3_2021 <- na.omit(Vietnam_nd[Vietnam_nd$datetime >= "2021-03-01" & Vietnam_nd$datetime <= "2021-03-31",])
avg_nd_Vn_3_2021 <- c()
avg_nd_Vn_3_2021[1] <- Vietnam_nd_3_2021$new_deaths[1]
avg_nd_Vn_3_2021[2] <- (Vietnam_nd_3_2021$new_deaths[1] + Vietnam_nd_3_2021$new_deaths[2])/2
avg_nd_Vn_3_2021[3] <- (Vietnam_nd_3_2021$new_deaths[1] + Vietnam_nd_3_2021$new_deaths[2] + Vietnam_nd_3_2021$new_deaths[3])/3
avg_nd_Vn_3_2021[4] <- (Vietnam_nd_3_2021$new_deaths[1] + Vietnam_nd_3_2021$new_deaths[2] + Vietnam_nd_3_2021$new_deaths[3] + Vietnam_nd_3_2021$new_deaths[4])/4
avg_nd_Vn_3_2021[5] <- (Vietnam_nd_3_2021$new_deaths[1] + Vietnam_nd_3_2021$new_deaths[2] + Vietnam_nd_3_2021$new_deaths[3] + Vietnam_nd_3_2021$new_deaths[4] + Vietnam_nd_3_2021$new_deaths[5])/5
avg_nd_Vn_3_2021[6] <- (Vietnam_nd_3_2021$new_deaths[1] + Vietnam_nd_3_2021$new_deaths[2] + Vietnam_nd_3_2021$new_deaths[3] + Vietnam_nd_3_2021$new_deaths[4] + Vietnam_nd_3_2021$new_deaths[5] + Vietnam_nd_3_2021$new_deaths[6])/6
for(i in 7:length(Vietnam_nd_3_2021$new_deaths))
{
  avg_nd_Vn_3_2021[i]=(Vietnam_nd_3_2021$new_deaths[i] + Vietnam_nd_3_2021$new_deaths[i-1] + Vietnam_nd_3_2021$new_deaths[i-2] + Vietnam_nd_3_2021$new_deaths[i-3] + Vietnam_nd_3_2021$new_deaths[i-4] + Vietnam_nd_3_2021$new_deaths[i-5] + Vietnam_nd_3_2021$new_deaths[i-6])/7
}
acml_nd_Vn_3_2021 <- c()
acml_nd_Vn_3_2021[1] <- avg_nd_Vn_3_2021[1]
for(i in 2:length(avg_nd_Vn_3_2021))
{
  acml_nd_Vn_3_2021[i] <- avg_nd_Vn_3_2021[i] + acml_nd_Vn_3_2021[i-1]
}
Vietnam_nd_3_2021 <- data.frame(Vietnam_nd_3_2021, avg_nd_Vn_3_2021, acml_nd_Vn_3_2021)




#Newdeaths thang 4 2021
Indonesia_nd_4_2021 <- na.omit(Indonesia_nd[Indonesia_nd$datetime >= "2021-04-01" & Indonesia_nd$datetime <= "2021-04-30",])
avg_nd_Indo_4_2021 <- c()
avg_nd_Indo_4_2021[1] <- Indonesia_nd_4_2021$new_deaths[1]
avg_nd_Indo_4_2021[2] <- (Indonesia_nd_4_2021$new_deaths[1] + Indonesia_nd_4_2021$new_deaths[2])/2
avg_nd_Indo_4_2021[3] <- (Indonesia_nd_4_2021$new_deaths[1] + Indonesia_nd_4_2021$new_deaths[2] + Indonesia_nd_4_2021$new_deaths[3])/3
avg_nd_Indo_4_2021[4] <- (Indonesia_nd_4_2021$new_deaths[1] + Indonesia_nd_4_2021$new_deaths[2] + Indonesia_nd_4_2021$new_deaths[3] + Indonesia_nd_4_2021$new_deaths[4])/4
avg_nd_Indo_4_2021[5] <- (Indonesia_nd_4_2021$new_deaths[1] + Indonesia_nd_4_2021$new_deaths[2] + Indonesia_nd_4_2021$new_deaths[3] + Indonesia_nd_4_2021$new_deaths[4] + Indonesia_nd_4_2021$new_deaths[5])/5
avg_nd_Indo_4_2021[6] <- (Indonesia_nd_4_2021$new_deaths[1] + Indonesia_nd_4_2021$new_deaths[2] + Indonesia_nd_4_2021$new_deaths[3] + Indonesia_nd_4_2021$new_deaths[4] + Indonesia_nd_4_2021$new_deaths[5] + Indonesia_nd_4_2021$new_deaths[6])/6
for(i in 7:length(Indonesia_nd_4_2021$new_deaths))
{
  avg_nd_Indo_4_2021[i]=(Indonesia_nd_4_2021$new_deaths[i] + Indonesia_nd_4_2021$new_deaths[i-1] + Indonesia_nd_4_2021$new_deaths[i-2] + Indonesia_nd_4_2021$new_deaths[i-3] + Indonesia_nd_4_2021$new_deaths[i-4] + Indonesia_nd_4_2021$new_deaths[i-5] + Indonesia_nd_4_2021$new_deaths[i-6])/7
}
acml_nd_Indo_4_2021 <- c()
acml_nd_Indo_4_2021[1] <- avg_nd_Indo_4_2021[1]
for(i in 2:length(avg_nd_Indo_4_2021))
{
  acml_nd_Indo_4_2021[i] <- avg_nd_Indo_4_2021[i] + acml_nd_Indo_4_2021[i-1]
}
Indonesia_nd_4_2021 <- data.frame(Indonesia_nd_4_2021, avg_nd_Indo_4_2021, acml_nd_Indo_4_2021)



Japan_nd_4_2021 <-  na.omit(Japan_nd[Japan_nd$datetime >= "2021-04-01" & Japan_nd$datetime <= "2021-04-30",])
avg_nd_Jp_4_2021 <- c()
avg_nd_Jp_4_2021[1] <- Japan_nd_4_2021$new_deaths[1]
avg_nd_Jp_4_2021[2] <- (Japan_nd_4_2021$new_deaths[1] + Japan_nd_4_2021$new_deaths[2])/2
avg_nd_Jp_4_2021[3] <- (Japan_nd_4_2021$new_deaths[1] + Japan_nd_4_2021$new_deaths[2] + Japan_nd_4_2021$new_deaths[3])/3
avg_nd_Jp_4_2021[4] <- (Japan_nd_4_2021$new_deaths[1] + Japan_nd_4_2021$new_deaths[2] + Japan_nd_4_2021$new_deaths[3] + Japan_nd_4_2021$new_deaths[4])/4
avg_nd_Jp_4_2021[5] <- (Japan_nd_4_2021$new_deaths[1] + Japan_nd_4_2021$new_deaths[2] + Japan_nd_4_2021$new_deaths[3] + Japan_nd_4_2021$new_deaths[4] + Japan_nd_4_2021$new_deaths[5])/5
avg_nd_Jp_4_2021[6] <- (Japan_nd_4_2021$new_deaths[1] + Japan_nd_4_2021$new_deaths[2] + Japan_nd_4_2021$new_deaths[3] + Japan_nd_4_2021$new_deaths[4] + Japan_nd_4_2021$new_deaths[5] + Japan_nd_4_2021$new_deaths[6])/6
for(i in 7:length(Japan_nd_4_2021$new_deaths))
{
  avg_nd_Jp_4_2021[i]=(Japan_nd_4_2021$new_deaths[i] + Japan_nd_4_2021$new_deaths[i-1] + Japan_nd_4_2021$new_deaths[i-2] + Japan_nd_4_2021$new_deaths[i-3] + Japan_nd_4_2021$new_deaths[i-4] + Japan_nd_4_2021$new_deaths[i-5] + Japan_nd_4_2021$new_deaths[i-6])/7
}
acml_nd_Jp_4_2021 <- c()
acml_nd_Jp_4_2021[1] <- avg_nd_Jp_4_2021[1]
for(i in 2:length(avg_nd_Jp_4_2021))
{
  acml_nd_Jp_4_2021[i] <- avg_nd_Jp_4_2021[i] + acml_nd_Jp_4_2021[i-1]
}
Japan_nd_4_2021 <- data.frame(Japan_nd_4_2021, avg_nd_Jp_4_2021, acml_nd_Jp_4_2021)



Vietnam_nd_4_2021 <- na.omit(Vietnam_nd[Vietnam_nd$datetime >= "2021-04-01" & Vietnam_nd$datetime <= "2021-04-30",])
avg_nd_Vn_4_2021 <- c()
avg_nd_Vn_4_2021[1] <- Vietnam_nd_4_2021$new_deaths[1]
avg_nd_Vn_4_2021[2] <- (Vietnam_nd_4_2021$new_deaths[1] + Vietnam_nd_4_2021$new_deaths[2])/2
avg_nd_Vn_4_2021[3] <- (Vietnam_nd_4_2021$new_deaths[1] + Vietnam_nd_4_2021$new_deaths[2] + Vietnam_nd_4_2021$new_deaths[3])/3
avg_nd_Vn_4_2021[4] <- (Vietnam_nd_4_2021$new_deaths[1] + Vietnam_nd_4_2021$new_deaths[2] + Vietnam_nd_4_2021$new_deaths[3] + Vietnam_nd_4_2021$new_deaths[4])/4
avg_nd_Vn_4_2021[5] <- (Vietnam_nd_4_2021$new_deaths[1] + Vietnam_nd_4_2021$new_deaths[2] + Vietnam_nd_4_2021$new_deaths[3] + Vietnam_nd_4_2021$new_deaths[4] + Vietnam_nd_4_2021$new_deaths[5])/5
avg_nd_Vn_4_2021[6] <- (Vietnam_nd_4_2021$new_deaths[1] + Vietnam_nd_4_2021$new_deaths[2] + Vietnam_nd_4_2021$new_deaths[3] + Vietnam_nd_4_2021$new_deaths[4] + Vietnam_nd_4_2021$new_deaths[5] + Vietnam_nd_4_2021$new_deaths[6])/6
for(i in 7:length(Vietnam_nd_4_2021$new_deaths))
{
  avg_nd_Vn_4_2021[i]=(Vietnam_nd_4_2021$new_deaths[i] + Vietnam_nd_4_2021$new_deaths[i-1] + Vietnam_nd_4_2021$new_deaths[i-2] + Vietnam_nd_4_2021$new_deaths[i-3] + Vietnam_nd_4_2021$new_deaths[i-4] + Vietnam_nd_4_2021$new_deaths[i-5] + Vietnam_nd_4_2021$new_deaths[i-6])/7
}
acml_nd_Vn_4_2021 <- c()
acml_nd_Vn_4_2021[1] <- avg_nd_Vn_4_2021[1]
for(i in 2:length(avg_nd_Vn_4_2021))
{
  acml_nd_Vn_4_2021[i] <- avg_nd_Vn_4_2021[i] + acml_nd_Vn_4_2021[i-1]
}
Vietnam_nd_4_2021 <- data.frame(Vietnam_nd_4_2021, avg_nd_Vn_4_2021, acml_nd_Vn_4_2021)



#Newdeaths thang 5 2021
Indonesia_nd_5_2021 <- na.omit(Indonesia_nd[Indonesia_nd$datetime >= "2021-05-01" & Indonesia_nd$datetime <= "2021-05-31",])
avg_nd_Indo_5_2021 <- c()
avg_nd_Indo_5_2021[1] <- Indonesia_nd_5_2021$new_deaths[1]
avg_nd_Indo_5_2021[2] <- (Indonesia_nd_5_2021$new_deaths[1] + Indonesia_nd_5_2021$new_deaths[2])/2
avg_nd_Indo_5_2021[3] <- (Indonesia_nd_5_2021$new_deaths[1] + Indonesia_nd_5_2021$new_deaths[2] + Indonesia_nd_5_2021$new_deaths[3])/3
avg_nd_Indo_5_2021[4] <- (Indonesia_nd_5_2021$new_deaths[1] + Indonesia_nd_5_2021$new_deaths[2] + Indonesia_nd_5_2021$new_deaths[3] + Indonesia_nd_5_2021$new_deaths[4])/4
avg_nd_Indo_5_2021[5] <- (Indonesia_nd_5_2021$new_deaths[1] + Indonesia_nd_5_2021$new_deaths[2] + Indonesia_nd_5_2021$new_deaths[3] + Indonesia_nd_5_2021$new_deaths[4] + Indonesia_nd_5_2021$new_deaths[5])/5
avg_nd_Indo_5_2021[6] <- (Indonesia_nd_5_2021$new_deaths[1] + Indonesia_nd_5_2021$new_deaths[2] + Indonesia_nd_5_2021$new_deaths[3] + Indonesia_nd_5_2021$new_deaths[4] + Indonesia_nd_5_2021$new_deaths[5] + Indonesia_nd_5_2021$new_deaths[6])/6
for(i in 7:length(Indonesia_nd_5_2021$new_deaths))
{
  avg_nd_Indo_5_2021[i]=(Indonesia_nd_5_2021$new_deaths[i] + Indonesia_nd_5_2021$new_deaths[i-1] + Indonesia_nd_5_2021$new_deaths[i-2] + Indonesia_nd_5_2021$new_deaths[i-3] + Indonesia_nd_5_2021$new_deaths[i-4] + Indonesia_nd_5_2021$new_deaths[i-5] + Indonesia_nd_5_2021$new_deaths[i-6])/7
}
acml_nd_Indo_5_2021 <- c()
acml_nd_Indo_5_2021[1] <- avg_nd_Indo_5_2021[1]
for(i in 2:length(avg_nd_Indo_5_2021))
{
  acml_nd_Indo_5_2021[i] <- avg_nd_Indo_5_2021[i] + acml_nd_Indo_5_2021[i-1]
}
Indonesia_nd_5_2021 <- data.frame(Indonesia_nd_5_2021, avg_nd_Indo_5_2021, acml_nd_Indo_5_2021)



Japan_nd_5_2021 <-  na.omit(Japan_nd[Japan_nd$datetime >= "2021-05-01" & Japan_nd$datetime <= "2021-05-31",])
avg_nd_Jp_5_2021 <- c()
avg_nd_Jp_5_2021[1] <- Japan_nd_5_2021$new_deaths[1]
avg_nd_Jp_5_2021[2] <- (Japan_nd_5_2021$new_deaths[1] + Japan_nd_5_2021$new_deaths[2])/2
avg_nd_Jp_5_2021[3] <- (Japan_nd_5_2021$new_deaths[1] + Japan_nd_5_2021$new_deaths[2] + Japan_nd_5_2021$new_deaths[3])/3
avg_nd_Jp_5_2021[4] <- (Japan_nd_5_2021$new_deaths[1] + Japan_nd_5_2021$new_deaths[2] + Japan_nd_5_2021$new_deaths[3] + Japan_nd_5_2021$new_deaths[4])/4
avg_nd_Jp_5_2021[5] <- (Japan_nd_5_2021$new_deaths[1] + Japan_nd_5_2021$new_deaths[2] + Japan_nd_5_2021$new_deaths[3] + Japan_nd_5_2021$new_deaths[4] + Japan_nd_5_2021$new_deaths[5])/5
avg_nd_Jp_5_2021[6] <- (Japan_nd_5_2021$new_deaths[1] + Japan_nd_5_2021$new_deaths[2] + Japan_nd_5_2021$new_deaths[3] + Japan_nd_5_2021$new_deaths[4] + Japan_nd_5_2021$new_deaths[5] + Japan_nd_5_2021$new_deaths[6])/6
for(i in 7:length(Japan_nd_5_2021$new_deaths))
{
  avg_nd_Jp_5_2021[i]=(Japan_nd_5_2021$new_deaths[i] + Japan_nd_5_2021$new_deaths[i-1] + Japan_nd_5_2021$new_deaths[i-2] + Japan_nd_5_2021$new_deaths[i-3] + Japan_nd_5_2021$new_deaths[i-4] + Japan_nd_5_2021$new_deaths[i-5] + Japan_nd_5_2021$new_deaths[i-6])/7
}
acml_nd_Jp_5_2021 <- c()
acml_nd_Jp_5_2021[1] <- avg_nd_Jp_5_2021[1]
for(i in 2:length(avg_nd_Jp_5_2021))
{
  acml_nd_Jp_5_2021[i] <- avg_nd_Jp_5_2021[i] + acml_nd_Jp_5_2021[i-1]
}
Japan_nd_5_2021 <- data.frame(Japan_nd_5_2021, avg_nd_Jp_5_2021, acml_nd_Jp_5_2021)



Vietnam_nd_5_2021 <- na.omit(Vietnam_nd[Vietnam_nd$datetime >= "2021-05-01" & Vietnam_nd$datetime <= "2021-05-31",])
avg_nd_Vn_5_2021 <- c()
avg_nd_Vn_5_2021[1] <- Vietnam_nd_5_2021$new_deaths[1]
avg_nd_Vn_5_2021[2] <- (Vietnam_nd_5_2021$new_deaths[1] + Vietnam_nd_5_2021$new_deaths[2])/2
avg_nd_Vn_5_2021[3] <- (Vietnam_nd_5_2021$new_deaths[1] + Vietnam_nd_5_2021$new_deaths[2] + Vietnam_nd_5_2021$new_deaths[3])/3
avg_nd_Vn_5_2021[4] <- (Vietnam_nd_5_2021$new_deaths[1] + Vietnam_nd_5_2021$new_deaths[2] + Vietnam_nd_5_2021$new_deaths[3] + Vietnam_nd_5_2021$new_deaths[4])/4
avg_nd_Vn_5_2021[5] <- (Vietnam_nd_5_2021$new_deaths[1] + Vietnam_nd_5_2021$new_deaths[2] + Vietnam_nd_5_2021$new_deaths[3] + Vietnam_nd_5_2021$new_deaths[4] + Vietnam_nd_5_2021$new_deaths[5])/5
avg_nd_Vn_5_2021[6] <- (Vietnam_nd_5_2021$new_deaths[1] + Vietnam_nd_5_2021$new_deaths[2] + Vietnam_nd_5_2021$new_deaths[3] + Vietnam_nd_5_2021$new_deaths[4] + Vietnam_nd_5_2021$new_deaths[5] + Vietnam_nd_5_2021$new_deaths[6])/6
for(i in 7:length(Vietnam_nd_5_2021$new_deaths))
{
  avg_nd_Vn_5_2021[i]=(Vietnam_nd_5_2021$new_deaths[i] + Vietnam_nd_5_2021$new_deaths[i-1] + Vietnam_nd_5_2021$new_deaths[i-2] + Vietnam_nd_5_2021$new_deaths[i-3] + Vietnam_nd_5_2021$new_deaths[i-4] + Vietnam_nd_5_2021$new_deaths[i-5] + Vietnam_nd_5_2021$new_deaths[i-6])/7
}
acml_nd_Vn_5_2021 <- c()
acml_nd_Vn_5_2021[1] <- avg_nd_Vn_5_2021[1]
for(i in 2:length(avg_nd_Vn_5_2021))
{
  acml_nd_Vn_5_2021[i] <- avg_nd_Vn_5_2021[i] + acml_nd_Vn_5_2021[i-1]
}
Vietnam_nd_5_2021 <- data.frame(Vietnam_nd_5_2021, avg_nd_Vn_5_2021, acml_nd_Vn_5_2021)



#newdeaths th?ng 1 2022
Indonesia_nd_1_2022 <- na.omit(Indonesia_nd[Indonesia_nd$datetime >= "2022-01-01" & Indonesia_nd$datetime <= "2022-01-31",])
avg_nd_Indo_1_2022 <- c()
avg_nd_Indo_1_2022[1] <- Indonesia_nd_1_2022$new_deaths[1]
avg_nd_Indo_1_2022[2] <- (Indonesia_nd_1_2022$new_deaths[1] + Indonesia_nd_1_2022$new_deaths[2])/2
avg_nd_Indo_1_2022[3] <- (Indonesia_nd_1_2022$new_deaths[1] + Indonesia_nd_1_2022$new_deaths[2] + Indonesia_nd_1_2022$new_deaths[3])/3
avg_nd_Indo_1_2022[4] <- (Indonesia_nd_1_2022$new_deaths[1] + Indonesia_nd_1_2022$new_deaths[2] + Indonesia_nd_1_2022$new_deaths[3] + Indonesia_nd_1_2022$new_deaths[4])/4
avg_nd_Indo_1_2022[5] <- (Indonesia_nd_1_2022$new_deaths[1] + Indonesia_nd_1_2022$new_deaths[2] + Indonesia_nd_1_2022$new_deaths[3] + Indonesia_nd_1_2022$new_deaths[4] + Indonesia_nd_1_2022$new_deaths[5])/5
avg_nd_Indo_1_2022[6] <- (Indonesia_nd_1_2022$new_deaths[1] + Indonesia_nd_1_2022$new_deaths[2] + Indonesia_nd_1_2022$new_deaths[3] + Indonesia_nd_1_2022$new_deaths[4] + Indonesia_nd_1_2022$new_deaths[5] + Indonesia_nd_1_2022$new_deaths[6])/6
for(i in 7:length(Indonesia_nd_1_2022$new_deaths))
{
  avg_nd_Indo_1_2022[i]=(Indonesia_nd_1_2022$new_deaths[i] + Indonesia_nd_1_2022$new_deaths[i-1] + Indonesia_nd_1_2022$new_deaths[i-2] + Indonesia_nd_1_2022$new_deaths[i-3] + Indonesia_nd_1_2022$new_deaths[i-4] + Indonesia_nd_1_2022$new_deaths[i-5] + Indonesia_nd_1_2022$new_deaths[i-6])/7
}
acml_nd_Indo_1_2022 <- c()
acml_nd_Indo_1_2022[1] <- avg_nd_Indo_1_2022[1]
for(i in 2:length(avg_nd_Indo_1_2022))
{
  acml_nd_Indo_1_2022[i] <- avg_nd_Indo_1_2022[i] + acml_nd_Indo_1_2022[i-1]
}
Indonesia_nd_1_2022 <- data.frame(Indonesia_nd_1_2022, avg_nd_Indo_1_2022, acml_nd_Indo_1_2022)



Japan_nd_1_2022 <-  na.omit(Japan_nd[Japan_nd$datetime >= "2022-01-01" & Japan_nd$datetime <= "2022-01-31",])
avg_nd_Jp_1_2022 <- c()
avg_nd_Jp_1_2022[1] <- Japan_nd_1_2022$new_deaths[1]
avg_nd_Jp_1_2022[2] <- (Japan_nd_1_2022$new_deaths[1] + Japan_nd_1_2022$new_deaths[2])/2
avg_nd_Jp_1_2022[3] <- (Japan_nd_1_2022$new_deaths[1] + Japan_nd_1_2022$new_deaths[2] + Japan_nd_1_2022$new_deaths[3])/3
avg_nd_Jp_1_2022[4] <- (Japan_nd_1_2022$new_deaths[1] + Japan_nd_1_2022$new_deaths[2] + Japan_nd_1_2022$new_deaths[3] + Japan_nd_1_2022$new_deaths[4])/4
avg_nd_Jp_1_2022[5] <- (Japan_nd_1_2022$new_deaths[1] + Japan_nd_1_2022$new_deaths[2] + Japan_nd_1_2022$new_deaths[3] + Japan_nd_1_2022$new_deaths[4] + Japan_nd_1_2022$new_deaths[5])/5
avg_nd_Jp_1_2022[6] <- (Japan_nd_1_2022$new_deaths[1] + Japan_nd_1_2022$new_deaths[2] + Japan_nd_1_2022$new_deaths[3] + Japan_nd_1_2022$new_deaths[4] + Japan_nd_1_2022$new_deaths[5] + Japan_nd_1_2022$new_deaths[6])/6
for(i in 7:length(Japan_nd_1_2022$new_deaths))
{
  avg_nd_Jp_1_2022[i]=(Japan_nd_1_2022$new_deaths[i] + Japan_nd_1_2022$new_deaths[i-1] + Japan_nd_1_2022$new_deaths[i-2] + Japan_nd_1_2022$new_deaths[i-3] + Japan_nd_1_2022$new_deaths[i-4] + Japan_nd_1_2022$new_deaths[i-5] + Japan_nd_1_2022$new_deaths[i-6])/7
}
acml_nd_Jp_1_2022 <- c()
acml_nd_Jp_1_2022[1] <- avg_nd_Jp_1_2022[1]
for(i in 2:length(avg_nd_Jp_1_2022))
{
  acml_nd_Jp_1_2022[i] <- avg_nd_Jp_1_2022[i] + acml_nd_Jp_1_2022[i-1]
}
Japan_nd_1_2022 <- data.frame(Japan_nd_1_2022, avg_nd_Jp_1_2022, acml_nd_Jp_1_2022)



Vietnam_nd_1_2022 <- na.omit(Vietnam_nd[Vietnam_nd$datetime >= "2022-01-01" & Vietnam_nd$datetime <= "2022-01-31",])
avg_nd_Vn_1_2022 <- c()
avg_nd_Vn_1_2022[1] <- Vietnam_nd_1_2022$new_deaths[1]
avg_nd_Vn_1_2022[2] <- (Vietnam_nd_1_2022$new_deaths[1] + Vietnam_nd_1_2022$new_deaths[2])/2
avg_nd_Vn_1_2022[3] <- (Vietnam_nd_1_2022$new_deaths[1] + Vietnam_nd_1_2022$new_deaths[2] + Vietnam_nd_1_2022$new_deaths[3])/3
avg_nd_Vn_1_2022[4] <- (Vietnam_nd_1_2022$new_deaths[1] + Vietnam_nd_1_2022$new_deaths[2] + Vietnam_nd_1_2022$new_deaths[3] + Vietnam_nd_1_2022$new_deaths[4])/4
avg_nd_Vn_1_2022[5] <- (Vietnam_nd_1_2022$new_deaths[1] + Vietnam_nd_1_2022$new_deaths[2] + Vietnam_nd_1_2022$new_deaths[3] + Vietnam_nd_1_2022$new_deaths[4] + Vietnam_nd_1_2022$new_deaths[5])/5
avg_nd_Vn_1_2022[6] <- (Vietnam_nd_1_2022$new_deaths[1] + Vietnam_nd_1_2022$new_deaths[2] + Vietnam_nd_1_2022$new_deaths[3] + Vietnam_nd_1_2022$new_deaths[4] + Vietnam_nd_1_2022$new_deaths[5] + Vietnam_nd_1_2022$new_deaths[6])/6
for(i in 7:length(Vietnam_nd_1_2022$new_deaths))
{
  avg_nd_Vn_1_2022[i]=(Vietnam_nd_1_2022$new_deaths[i] + Vietnam_nd_1_2022$new_deaths[i-1] + Vietnam_nd_1_2022$new_deaths[i-2] + Vietnam_nd_1_2022$new_deaths[i-3] + Vietnam_nd_1_2022$new_deaths[i-4] + Vietnam_nd_1_2022$new_deaths[i-5] + Vietnam_nd_1_2022$new_deaths[i-6])/7
}
acml_nd_Vn_1_2022 <- c()
acml_nd_Vn_1_2022[1] <- avg_nd_Vn_1_2022[1]
for(i in 2:length(avg_nd_Vn_1_2022))
{
  acml_nd_Vn_1_2022[i] <- avg_nd_Vn_1_2022[i] + acml_nd_Vn_1_2022[i-1]
}
Vietnam_nd_1_2022 <- data.frame(Vietnam_nd_1_2022, avg_nd_Vn_1_2022, acml_nd_Vn_1_2022)






#Ve bieu do
library(ggplot2)


#Bieu do Newcases tung thang
Newcases_1_2020 <- ggplot() + 
  geom_line(data=Japan_nc_1_2020, aes(x=datetime, y=avg_nc_Jp_1_2020, color = 'Japan')) +
  geom_line(data=Vietnam_nc_1_2020, aes(x=datetime, y=avg_nc_Vn_1_2020, color = 'Vietnam')) +
  labs(title = "Newcases 1-2020", x = "date", y = "new cases")
ggsave(file="Newcases_1_2020.png")

Newcases_3_2020 <- ggplot() +
  geom_line(data=Indonesia_nc_3_2020, aes(x=datetime, y=avg_nc_Indo_3_2020, color = 'Indonesia')) +
  geom_line(data=Japan_nc_3_2020, aes(x=datetime, y=avg_nc_Jp_3_2020, color = 'Japan')) +
  geom_line(data=Vietnam_nc_3_2020, aes(x=datetime, y=avg_nc_Vn_3_2020, color = 'Vietnam')) +
  labs(title = "Newcases 3-2020", x = "date", y = "new cases")
ggsave(file="Newcases_3_2020.png")

Newcases_4_2020 <- ggplot() +
  geom_line(data=Indonesia_nc_4_2020, aes(x=datetime, y=avg_nc_Indo_4_2020, color = 'Indonesia')) +
  geom_line(data=Japan_nc_4_2020, aes(x=datetime, y=avg_nc_Jp_4_2020, color = 'Japan')) +
  geom_line(data=Vietnam_nc_4_2020, aes(x=datetime, y=avg_nc_Vn_4_2020, color = 'Vietnam')) +
  labs(title = "Newcases 4-2020", x = "date", y = "new cases")
ggsave(file="Newcases_4_2020.png")

Newcases_5_2020 <- ggplot() +
  geom_line(data=Indonesia_nc_5_2020, aes(x=datetime, y=avg_nc_Indo_5_2020, color = 'Indonesia')) +
  geom_line(data=Japan_nc_5_2020, aes(x=datetime, y=avg_nc_Jp_5_2020, color = 'Japan')) +
  geom_line(data=Vietnam_nc_5_2020, aes(x=datetime, y=avg_nc_Vn_5_2020, color = 'Vietnam')) +
  labs(title = "Newcases 5-2020", x = "date", y = "new cases")
ggsave(file="Newcases_5_2020.png")

Newcases_1_2021 <- ggplot() + 
  geom_line(data=Indonesia_nc_1_2021, aes(x=datetime, y=avg_nc_Indo_1_2021, color = 'Indonesia')) +
  geom_line(data=Japan_nc_1_2021, aes(x=datetime, y=avg_nc_Jp_1_2021, color = 'Japan')) +
  geom_line(data=Vietnam_nc_1_2021, aes(x=datetime, y=avg_nc_Vn_1_2021, color = 'Vietnam')) +
  labs(title = "Newcases 1-2021", x = "date", y = "new cases")
ggsave(file="Newcases_1_2021.png")

Newcases_3_2021 <- ggplot() +
  geom_line(data=Indonesia_nc_3_2021, aes(x=datetime, y=avg_nc_Indo_3_2021, color = 'Indonesia')) +
  geom_line(data=Japan_nc_3_2021, aes(x=datetime, y=avg_nc_Jp_3_2021, color = 'Japan')) +
  geom_line(data=Vietnam_nc_3_2021, aes(x=datetime, y=avg_nc_Vn_3_2021, color = 'Vietnam')) +
  labs(title = "Newcases 3-2021", x = "date", y = "new cases")
ggsave(file="Newcases_3_2021.png")

Newcases_4_2021 <- ggplot() +
  geom_line(data=Indonesia_nc_4_2021, aes(x=datetime, y=avg_nc_Indo_4_2021, color = 'Indonesia')) +
  geom_line(data=Japan_nc_4_2021, aes(x=datetime, y=avg_nc_Jp_4_2021, color = 'Japan')) +
  geom_line(data=Vietnam_nc_4_2021, aes(x=datetime, y=avg_nc_Vn_4_2021, color = 'Vietnam')) +
  labs(title = "Newcases 4-2021", x = "date", y = "new cases")
ggsave(file="Newcases_4_2021.png")

Newcases_5_2021 <- ggplot() +
  geom_line(data=Indonesia_nc_5_2021, aes(x=datetime, y=avg_nc_Indo_5_2021, color = 'Indonesia')) +
  geom_line(data=Japan_nc_5_2021, aes(x=datetime, y=avg_nc_Jp_5_2021, color = 'Japan')) +
  geom_line(data=Vietnam_nc_5_2021, aes(x=datetime, y=avg_nc_Vn_5_2021, color = 'Vietnam')) +
  labs(title = "Newcases 5-2021", x = "date", y = "new cases")
ggsave(file="Newcases_5_2021.png")

Newcases_1_2022 <- ggplot() +
  geom_line(data=Indonesia_nc_1_2022, aes(x=datetime, y=avg_nc_Indo_1_2022, color = 'Indonesia')) +
  geom_line(data=Japan_nc_1_2022, aes(x=datetime, y=avg_nc_Jp_1_2022, color = 'Japan')) +
  geom_line(data=Vietnam_nc_1_2022, aes(x=datetime, y=avg_nc_Vn_1_2022, color = 'Vietnam')) +
  labs(title = "Newcases 1-2022", x = "date", y = "new cases")
ggsave(file="Newcases_1_2022.png")


#Bieu do Newdeaths tung thang

Newdeaths_3_2020 <- ggplot() +
  geom_line(data=Indonesia_nd_3_2020, aes(x=datetime, y=avg_nd_Indo_3_2020, color = 'Indonesia')) +
  geom_line(data=Japan_nd_3_2020, aes(x=datetime, y=avg_nd_Jp_3_2020, color = 'Japan')) +
  labs(title = "Newdeaths 3-2020", x = "date", y = "new deaths")
ggsave(file="Newdeaths_3_2020.png")

Newdeaths_4_2020 <- ggplot() +
  geom_line(data=Indonesia_nd_4_2020, aes(x=datetime, y=avg_nd_Indo_4_2020, color = 'Indonesia')) +
  geom_line(data=Japan_nd_4_2020, aes(x=datetime, y=avg_nd_Jp_4_2020, color = 'Japan')) +
  labs(title = "Newdeaths 4-2020", x = "date", y = "new deaths")
ggsave(file="Newdeaths_4_2020.png")

Newdeaths_5_2020 <- ggplot() +
  geom_line(data=Indonesia_nd_5_2020, aes(x=datetime, y=avg_nd_Indo_5_2020, color = 'Indonesia')) +
  geom_line(data=Japan_nd_5_2020, aes(x=datetime, y=avg_nd_Jp_5_2020, color = 'Japan')) +
  labs(title = "Newdeaths 5-2020", x = "date", y = "new deaths")
ggsave(file="Newdeaths_5_2020.png")

Newdeaths_1_2021 <- ggplot() +
  geom_line(data=Indonesia_nd_1_2021, aes(x=datetime, y=avg_nd_Indo_1_2021, color = 'Indonesia')) +
  geom_line(data=Japan_nd_1_2021, aes(x=datetime, y=avg_nd_Jp_1_2021, color = 'Japan')) +
  geom_line(data=Vietnam_nd_1_2021, aes(x=datetime, y=avg_nd_Vn_1_2021, color = 'Vietnam')) +
  labs(title = "Newdeaths 1-2021", x = "date", y = "new deaths")
ggsave(file="Newdeaths_1_2021.png")

Newdeaths_3_2021 <- ggplot() +
  geom_line(data=Indonesia_nd_3_2021, aes(x=datetime, y=avg_nd_Indo_3_2021, color = 'Indonesia')) +
  geom_line(data=Japan_nd_3_2021, aes(x=datetime, y=avg_nd_Jp_3_2021, color = 'Japan')) +
  geom_line(data=Vietnam_nd_3_2021, aes(x=datetime, y=avg_nd_Vn_3_2021, color = 'Vietnam')) +
  labs(title = "Newdeaths 3-2021", x = "date", y = "new deaths")
ggsave(file="Newdeaths_3_2021.png")

Newdeaths_4_2021 <- ggplot() +
  geom_line(data=Indonesia_nd_4_2021, aes(x=datetime, y=avg_nd_Indo_4_2021, color = 'Indonesia')) +
  geom_line(data=Japan_nd_4_2021, aes(x=datetime, y=avg_nd_Jp_4_2021, color = 'Japan')) +
  geom_line(data=Vietnam_nd_4_2021, aes(x=datetime, y=avg_nd_Vn_4_2021, color = 'Vietnam')) +
  labs(title = "Newdeaths 4-2021", x = "date", y = "new deaths")
ggsave(file="Newdeaths_4_2021.png")

Newdeaths_5_2021 <- ggplot() +
  geom_line(data=Indonesia_nd_5_2021, aes(x=datetime, y=avg_nd_Indo_5_2021, color = 'Indonesia')) +
  geom_line(data=Japan_nd_5_2021, aes(x=datetime, y=avg_nd_Jp_5_2021, color = 'Japan')) +
  geom_line(data=Vietnam_nd_5_2021, aes(x=datetime, y=avg_nd_Vn_5_2021, color = 'Vietnam')) +
  labs(title = "Newdeaths 5-2021", x = "date", y = "new deaths")
ggsave(file="Newdeaths_5_2021.png")

Newdeaths_1_2022 <- ggplot() +
  geom_line(data=Indonesia_nd_1_2022, aes(x=datetime, y=avg_nd_Indo_1_2022, color = 'Indonesia')) +
  geom_line(data=Japan_nd_1_2022, aes(x=datetime, y=avg_nd_Jp_1_2022, color = 'Japan')) +
  geom_line(data=Vietnam_nd_1_2022, aes(x=datetime, y=avg_nd_Vn_1_2022, color = 'Vietnam')) +
  labs(title = "Newdeaths 1-2022", x = "date", y = "new deaths")
ggsave(file="Newdeaths_1_2022.png")


#Bieu do nhiem benh va tu vong tung thang
New_1_2020 <- ggplot() +
  geom_line(data=Japan_nc_1_2020, aes(x=datetime, y=avg_nc_Jp_1_2020, linetype = 'Newcases', color = 'Japan')) +
  geom_line(data=Vietnam_nc_1_2020, aes(x=datetime, y=avg_nc_Vn_1_2020, linetype = 'Newcases', color='red')) +
  labs(title = "Newcases and Newdeaths 1-2020", x = "date", y = "cases")
ggsave(file="Newcases and Newdeaths 1-2020.png")


New_3_2020 <- ggplot() +
  geom_line(data=Indonesia_nc_3_2020, aes(x=datetime, y=avg_nc_Indo_3_2020, linetype = 'Newcases', color='Indonesia')) +
  geom_line(data=Japan_nc_3_2020, aes(x=datetime, y=avg_nc_Jp_3_2020, linetype = 'Newcases', color='Japan')) +
  geom_line(data=Vietnam_nc_3_2020, aes(x=datetime, y=avg_nc_Vn_3_2020, linetype = 'Newcases', color='Vietnam')) +
  geom_line(data=Indonesia_nd_3_2020, aes(x=datetime, y=avg_nd_Indo_3_2020, linetype = 'Newdeaths', color='Indonesia')) +
  geom_line(data=Japan_nd_3_2020, aes(x=datetime, y=avg_nd_Jp_3_2020, linetype = 'Newdeaths', color='Japan')) +
  labs(title = "Newcases and Newdeaths 3-2020", x = "date", y = "cases")
ggsave(file="Newcases and Newdeaths 3-2020.png")


New_4_2020 <- ggplot() +
  geom_line(data=Indonesia_nc_4_2020, aes(x=datetime, y=avg_nc_Indo_4_2020, linetype = 'Newcases', color='Indonesia')) +
  geom_line(data=Japan_nc_4_2020, aes(x=datetime, y=avg_nc_Jp_4_2020, linetype = 'Newcases', color='Japan')) +
  geom_line(data=Vietnam_nc_4_2020, aes(x=datetime, y=avg_nc_Vn_4_2020, linetype = 'Newcases', color='Vietnam')) +
  geom_line(data=Indonesia_nd_4_2020, aes(x=datetime, y=avg_nd_Indo_4_2020, linetype = 'Newdeaths', color='Indonesia')) +
  geom_line(data=Japan_nd_4_2020, aes(x=datetime, y=avg_nd_Jp_4_2020, linetype = 'Newdeaths', color='Japan')) +
  labs(title = "Newcases and Newdeaths 4-2020", x = "date", y = "cases")
ggsave(file="Newcases and Newdeaths 4-2020.png")


New_5_2020 <- ggplot() +
  geom_line(data=Indonesia_nc_5_2020, aes(x=datetime, y=avg_nc_Indo_5_2020, linetype = 'Newcases', color='Indonesia')) +
  geom_line(data=Japan_nc_5_2020, aes(x=datetime, y=avg_nc_Jp_5_2020, linetype = 'Newcases', color='Japan')) +
  geom_line(data=Vietnam_nc_5_2020, aes(x=datetime, y=avg_nc_Vn_5_2020, linetype = 'Newcases', color='Vietnam')) +
  geom_line(data=Indonesia_nd_5_2020, aes(x=datetime, y=avg_nd_Indo_5_2020, linetype = 'Newdeaths', color='Indonesia')) +
  geom_line(data=Japan_nd_5_2020, aes(x=datetime, y=avg_nd_Jp_5_2020, linetype = 'Newdeaths', color='Japan')) +
  labs(title = "Newcases and Newdeaths 5-2020", x = "date", y = "cases")
ggsave(file="Newcases and Newdeaths 5-2020.png")

New_1_2021 <- ggplot() +
  geom_line(data=Indonesia_nc_1_2021, aes(x=datetime, y=avg_nc_Indo_1_2021, linetype = 'Newcases', color='Indonesia')) +
  geom_line(data=Japan_nc_1_2021, aes(x=datetime, y=avg_nc_Jp_1_2021, linetype = 'Newcases', color='Japan')) +
  geom_line(data=Vietnam_nc_1_2021, aes(x=datetime, y=avg_nc_Vn_1_2021, linetype = 'Newcases', color='Vietnam')) +
  geom_line(data=Indonesia_nd_1_2021, aes(x=datetime, y=avg_nd_Indo_1_2021, linetype = 'Newdeaths', color='Indonesia')) +
  geom_line(data=Japan_nd_1_2021, aes(x=datetime, y=avg_nd_Jp_1_2021, linetype = 'Newdeaths', color='Japan')) +
  geom_line(data=Vietnam_nd_1_2021, aes(x=datetime, y=avg_nd_Vn_1_2021, linetype = 'Newdeaths', color='Vietnam')) +
  labs(title = "Newcases and Newdeaths 1-2021", x = "date", y = "cases")
ggsave(file="Newcases and Newdeaths 1-2021.png")

New_3_2021 <- ggplot() +
  geom_line(data=Indonesia_nc_3_2021, aes(x=datetime, y=avg_nc_Indo_3_2021, linetype = 'Newcases', color='Indonesia')) +
  geom_line(data=Japan_nc_3_2021, aes(x=datetime, y=avg_nc_Jp_3_2021, linetype = 'Newcases', color='Japan')) +
  geom_line(data=Vietnam_nc_3_2021, aes(x=datetime, y=avg_nc_Vn_3_2021, linetype = 'Newcases', color='Vietnam')) +
  geom_line(data=Indonesia_nd_3_2021, aes(x=datetime, y=avg_nd_Indo_3_2021, linetype = 'Newdeaths', color='Indonesia')) +
  geom_line(data=Japan_nd_3_2021, aes(x=datetime, y=avg_nd_Jp_3_2021, linetype = 'Newdeaths', color='Japan')) +
  geom_line(data=Vietnam_nd_3_2021, aes(x=datetime, y=avg_nd_Vn_3_2021, linetype = 'Newdeaths', color='Vietnam')) +
  labs(title = "Newcases and Newdeaths 3-2021", x = "date", y = "cases")
ggsave(file="Newcases and Newdeaths 3-2021.png")

New_4_2021 <- ggplot() +
  geom_line(data=Indonesia_nc_4_2021, aes(x=datetime, y=avg_nc_Indo_4_2021, linetype = 'Newcases', color='Indonesia')) +
  geom_line(data=Japan_nc_4_2021, aes(x=datetime, y=avg_nc_Jp_4_2021, linetype = 'Newcases', color='Japan')) +
  geom_line(data=Vietnam_nc_4_2021, aes(x=datetime, y=avg_nc_Vn_4_2021, linetype = 'Newcases', color='Vietnam')) +
  geom_line(data=Indonesia_nd_4_2021, aes(x=datetime, y=avg_nd_Indo_4_2021, linetype = 'Newdeaths', color='Indonesia')) +
  geom_line(data=Japan_nd_4_2021, aes(x=datetime, y=avg_nd_Jp_4_2021, linetype = 'Newdeaths', color='Japan')) +
  geom_line(data=Vietnam_nd_4_2021, aes(x=datetime, y=avg_nd_Vn_4_2021, linetype = 'Newdeaths', color='Vietnam')) +
  labs(title = "Newcases and Newdeaths 4-2021", x = "date", y = "cases")
ggsave(file="Newcases and Newdeaths 4-2021.png")

New_5_2021 <- ggplot() +
  geom_line(data=Indonesia_nc_5_2021, aes(x=datetime, y=avg_nc_Indo_5_2021, linetype = 'Newcases', color='Indonesia')) +
  geom_line(data=Japan_nc_5_2021, aes(x=datetime, y=avg_nc_Jp_5_2021, linetype = 'Newcases', color='Japan')) +
  geom_line(data=Vietnam_nc_5_2021, aes(x=datetime, y=avg_nc_Vn_5_2021, linetype = 'Newcases', color='Vietnam')) +
  geom_line(data=Indonesia_nd_5_2021, aes(x=datetime, y=avg_nd_Indo_5_2021, linetype = 'Newdeaths', color='Indonesia')) +
  geom_line(data=Japan_nd_5_2021, aes(x=datetime, y=avg_nd_Jp_5_2021, linetype = 'Newdeaths', color='Japan')) +
  geom_line(data=Vietnam_nd_5_2021, aes(x=datetime, y=avg_nd_Vn_5_2021, linetype = 'Newdeaths', color='Vietnam')) +
  labs(title = "Newcases and Newdeaths 5-2021", x = "date", y = "cases")
ggsave(file="Newcases and Newdeaths 5-2021.png")

New_1_2022 <- ggplot() +
  geom_line(data=Indonesia_nc_1_2022, aes(x=datetime, y=avg_nc_Indo_1_2022, linetype = 'Newcases', color='Indonesia')) +
  geom_line(data=Japan_nc_1_2022, aes(x=datetime, y=avg_nc_Jp_1_2022, linetype = 'Newcases', color='Japan')) +
  geom_line(data=Vietnam_nc_1_2022, aes(x=datetime, y=avg_nc_Vn_1_2022, linetype = 'Newcases', color='Vietnam')) +
  geom_line(data=Indonesia_nd_1_2022, aes(x=datetime, y=avg_nd_Indo_1_2022, linetype = 'Newdeaths', color='Indonesia')) +
  geom_line(data=Japan_nd_1_2022, aes(x=datetime, y=avg_nd_Jp_1_2022, linetype = 'Newdeaths', color='Japan')) +
  geom_line(data=Vietnam_nd_1_2022, aes(x=datetime, y=avg_nd_Vn_1_2022, linetype = 'Newdeaths', color='Vietnam')) +
  labs(title = "Newcases and Newdeaths 1-2022", x = "date", y = "cases")
ggsave(file="Newcases and Newdeaths 1-2022.png")





# 2 thang cuoi nam
# newcases:
#Indonesia
Indonesia_nc_11_12_2020 <- na.omit(Indonesia_nc[Indonesia_nc$datetime >= "2020-11-01" & Indonesia_nc$datetime <= "2020-12-31",])
avg_nc_Indo_11_12_2020 <- c()
avg_nc_Indo_11_12_2020[1] <- Indonesia_nc_11_12_2020$new_cases[1]
avg_nc_Indo_11_12_2020[2] <- (Indonesia_nc_11_12_2020$new_cases[1] + Indonesia_nc_11_12_2020$new_cases[2])/2
avg_nc_Indo_11_12_2020[3] <- (Indonesia_nc_11_12_2020$new_cases[1] + Indonesia_nc_11_12_2020$new_cases[2] + Indonesia_nc_11_12_2020$new_cases[3])/3
avg_nc_Indo_11_12_2020[4] <- (Indonesia_nc_11_12_2020$new_cases[1] + Indonesia_nc_11_12_2020$new_cases[2] + Indonesia_nc_11_12_2020$new_cases[3] + Indonesia_nc_11_12_2020$new_cases[4])/4
avg_nc_Indo_11_12_2020[5] <- (Indonesia_nc_11_12_2020$new_cases[1] + Indonesia_nc_11_12_2020$new_cases[2] + Indonesia_nc_11_12_2020$new_cases[3] + Indonesia_nc_11_12_2020$new_cases[4] + Indonesia_nc_11_12_2020$new_cases[5])/5
avg_nc_Indo_11_12_2020[6] <- (Indonesia_nc_11_12_2020$new_cases[1] + Indonesia_nc_11_12_2020$new_cases[2] + Indonesia_nc_11_12_2020$new_cases[3] + Indonesia_nc_11_12_2020$new_cases[4] + Indonesia_nc_11_12_2020$new_cases[5] + Indonesia_nc_11_12_2020$new_cases[6])/6
for(i in 7:length(Indonesia_nc_11_12_2020$new_cases))
{
  avg_nc_Indo_11_12_2020[i]=(Indonesia_nc_11_12_2020$new_cases[i] + Indonesia_nc_11_12_2020$new_cases[i-1] + Indonesia_nc_11_12_2020$new_cases[i-2] + Indonesia_nc_11_12_2020$new_cases[i-3] + Indonesia_nc_11_12_2020$new_cases[i-4] + Indonesia_nc_11_12_2020$new_cases[i-5] + Indonesia_nc_11_12_2020$new_cases[i-6])/7
}
acml_nc_Indo_11_12_2020 <- c()
acml_nc_Indo_11_12_2020[1] <- avg_nc_Indo_11_12_2020[1]
for(i in 2:length(avg_nc_Indo_11_12_2020))
{
  acml_nc_Indo_11_12_2020[i] <- avg_nc_Indo_11_12_2020[i] + acml_nc_Indo_11_12_2020[i-1]
}
Indonesia_nc_11_12_2020 <- data.frame(Indonesia_nc_11_12_2020, avg_nc_Indo_11_12_2020, acml_nc_Indo_11_12_2020)



Indonesia_nc_11_12_2021 <- na.omit(Indonesia_nc[Indonesia_nc$datetime >= "2021-11-01" & Indonesia_nc$datetime <= "2021-12-31",])
avg_nc_Indo_11_12_2021 <- c()
avg_nc_Indo_11_12_2021[1] <- Indonesia_nc_11_12_2021$new_cases[1]
avg_nc_Indo_11_12_2021[2] <- (Indonesia_nc_11_12_2021$new_cases[1] + Indonesia_nc_11_12_2021$new_cases[2])/2
avg_nc_Indo_11_12_2021[3] <- (Indonesia_nc_11_12_2021$new_cases[1] + Indonesia_nc_11_12_2021$new_cases[2] + Indonesia_nc_11_12_2021$new_cases[3])/3
avg_nc_Indo_11_12_2021[4] <- (Indonesia_nc_11_12_2021$new_cases[1] + Indonesia_nc_11_12_2021$new_cases[2] + Indonesia_nc_11_12_2021$new_cases[3] + Indonesia_nc_11_12_2021$new_cases[4])/4
avg_nc_Indo_11_12_2021[5] <- (Indonesia_nc_11_12_2021$new_cases[1] + Indonesia_nc_11_12_2021$new_cases[2] + Indonesia_nc_11_12_2021$new_cases[3] + Indonesia_nc_11_12_2021$new_cases[4] + Indonesia_nc_11_12_2021$new_cases[5])/5
avg_nc_Indo_11_12_2021[6] <- (Indonesia_nc_11_12_2021$new_cases[1] + Indonesia_nc_11_12_2021$new_cases[2] + Indonesia_nc_11_12_2021$new_cases[3] + Indonesia_nc_11_12_2021$new_cases[4] + Indonesia_nc_11_12_2021$new_cases[5] + Indonesia_nc_11_12_2021$new_cases[6])/6
for(i in 7:length(Indonesia_nc_11_12_2021$new_cases))
{
  avg_nc_Indo_11_12_2021[i]=(Indonesia_nc_11_12_2021$new_cases[i] + Indonesia_nc_11_12_2021$new_cases[i-1] + Indonesia_nc_11_12_2021$new_cases[i-2] + Indonesia_nc_11_12_2021$new_cases[i-3] + Indonesia_nc_11_12_2021$new_cases[i-4] + Indonesia_nc_11_12_2021$new_cases[i-5] + Indonesia_nc_11_12_2021$new_cases[i-6])/7
}
acml_nc_Indo_11_12_2021 <- c()
acml_nc_Indo_11_12_2021[1] <- avg_nc_Indo_11_12_2021[1]
for(i in 2:length(avg_nc_Indo_11_12_2021))
{
  acml_nc_Indo_11_12_2021[i] <- avg_nc_Indo_11_12_2021[i] + acml_nc_Indo_11_12_2021[i-1]
}
Indonesia_nc_11_12_2021 <- data.frame(Indonesia_nc_11_12_2021, avg_nc_Indo_11_12_2021, acml_nc_Indo_11_12_2021)



#Japan
Japan_nc_11_12_2020 <- na.omit(Japan_nc[Japan_nc$datetime >= "2020-11-01" & Japan_nc$datetime <= "2020-12-31",])
avg_nc_Jp_11_12_2020 <- c()
avg_nc_Jp_11_12_2020[1] <- Japan_nc_11_12_2020$new_cases[1]
avg_nc_Jp_11_12_2020[2] <- (Japan_nc_11_12_2020$new_cases[1] + Japan_nc_11_12_2020$new_cases[2])/2
avg_nc_Jp_11_12_2020[3] <- (Japan_nc_11_12_2020$new_cases[1] + Japan_nc_11_12_2020$new_cases[2] + Japan_nc_11_12_2020$new_cases[3])/3
avg_nc_Jp_11_12_2020[4] <- (Japan_nc_11_12_2020$new_cases[1] + Japan_nc_11_12_2020$new_cases[2] + Japan_nc_11_12_2020$new_cases[3] + Japan_nc_11_12_2020$new_cases[4])/4
avg_nc_Jp_11_12_2020[5] <- (Japan_nc_11_12_2020$new_cases[1] + Japan_nc_11_12_2020$new_cases[2] + Japan_nc_11_12_2020$new_cases[3] + Japan_nc_11_12_2020$new_cases[4] + Japan_nc_11_12_2020$new_cases[5])/5
avg_nc_Jp_11_12_2020[6] <- (Japan_nc_11_12_2020$new_cases[1] + Japan_nc_11_12_2020$new_cases[2] + Japan_nc_11_12_2020$new_cases[3] + Japan_nc_11_12_2020$new_cases[4] + Japan_nc_11_12_2020$new_cases[5] + Japan_nc_11_12_2020$new_cases[6])/6
for(i in 7:length(Japan_nc_11_12_2020$new_cases))
{
  avg_nc_Jp_11_12_2020[i]=(Japan_nc_11_12_2020$new_cases[i] + Japan_nc_11_12_2020$new_cases[i-1] + Japan_nc_11_12_2020$new_cases[i-2] + Japan_nc_11_12_2020$new_cases[i-3] + Japan_nc_11_12_2020$new_cases[i-4] + Japan_nc_11_12_2020$new_cases[i-5] + Japan_nc_11_12_2020$new_cases[i-6])/7
}
acml_nc_Jp_11_12_2020 <- c()
acml_nc_Jp_11_12_2020[1] <- avg_nc_Jp_11_12_2020[1]
for(i in 2:length(avg_nc_Jp_11_12_2020))
{
  acml_nc_Jp_11_12_2020[i] <- avg_nc_Jp_11_12_2020[i] + acml_nc_Jp_11_12_2020[i-1]
}
Japan_nc_11_12_2020 <- data.frame(Japan_nc_11_12_2020, avg_nc_Jp_11_12_2020, acml_nc_Jp_11_12_2020)



Japan_nc_11_12_2021 <- na.omit(Japan_nc[Japan_nc$datetime >= "2021-11-01" & Japan_nc$datetime <= "2021-12-31",])
avg_nc_Jp_11_12_2021 <- c()
avg_nc_Jp_11_12_2021[1] <- Japan_nc_11_12_2021$new_cases[1]
avg_nc_Jp_11_12_2021[2] <- (Japan_nc_11_12_2021$new_cases[1] + Japan_nc_11_12_2021$new_cases[2])/2
avg_nc_Jp_11_12_2021[3] <- (Japan_nc_11_12_2021$new_cases[1] + Japan_nc_11_12_2021$new_cases[2] + Japan_nc_11_12_2021$new_cases[3])/3
avg_nc_Jp_11_12_2021[4] <- (Japan_nc_11_12_2021$new_cases[1] + Japan_nc_11_12_2021$new_cases[2] + Japan_nc_11_12_2021$new_cases[3] + Japan_nc_11_12_2021$new_cases[4])/4
avg_nc_Jp_11_12_2021[5] <- (Japan_nc_11_12_2021$new_cases[1] + Japan_nc_11_12_2021$new_cases[2] + Japan_nc_11_12_2021$new_cases[3] + Japan_nc_11_12_2021$new_cases[4] + Japan_nc_11_12_2021$new_cases[5])/5
avg_nc_Jp_11_12_2021[6] <- (Japan_nc_11_12_2021$new_cases[1] + Japan_nc_11_12_2021$new_cases[2] + Japan_nc_11_12_2021$new_cases[3] + Japan_nc_11_12_2021$new_cases[4] + Japan_nc_11_12_2021$new_cases[5] + Japan_nc_11_12_2021$new_cases[6])/6
for(i in 7:length(Japan_nc_11_12_2021$new_cases))
{
  avg_nc_Jp_11_12_2021[i]=(Japan_nc_11_12_2021$new_cases[i] + Japan_nc_11_12_2021$new_cases[i-1] + Japan_nc_11_12_2021$new_cases[i-2] + Japan_nc_11_12_2021$new_cases[i-3] + Japan_nc_11_12_2021$new_cases[i-4] + Japan_nc_11_12_2021$new_cases[i-5] + Japan_nc_11_12_2021$new_cases[i-6])/7
}
acml_nc_Jp_11_12_2021 <- c()
acml_nc_Jp_11_12_2021[1] <- avg_nc_Jp_11_12_2021[1]
for(i in 2:length(avg_nc_Jp_11_12_2021))
{
  acml_nc_Jp_11_12_2021[i] <- avg_nc_Jp_11_12_2021[i] + acml_nc_Jp_11_12_2021[i-1]
}
Japan_nc_11_12_2021 <- data.frame(Japan_nc_11_12_2021, avg_nc_Jp_11_12_2021, acml_nc_Jp_11_12_2021)



#Vietnam:
Vietnam_nc_11_12_2020 <- na.omit(Vietnam_nc[Vietnam_nc$datetime >= "2020-11-01" & Vietnam_nc$datetime <= "2020-12-31",])
avg_nc_Vn_11_12_2020 <- c()
avg_nc_Vn_11_12_2020[1] <- Vietnam_nc_11_12_2020$new_cases[1]
avg_nc_Vn_11_12_2020[2] <- (Vietnam_nc_11_12_2020$new_cases[1] + Vietnam_nc_11_12_2020$new_cases[2])/2
avg_nc_Vn_11_12_2020[3] <- (Vietnam_nc_11_12_2020$new_cases[1] + Vietnam_nc_11_12_2020$new_cases[2] + Vietnam_nc_11_12_2020$new_cases[3])/3
avg_nc_Vn_11_12_2020[4] <- (Vietnam_nc_11_12_2020$new_cases[1] + Vietnam_nc_11_12_2020$new_cases[2] + Vietnam_nc_11_12_2020$new_cases[3] + Vietnam_nc_11_12_2020$new_cases[4])/4
avg_nc_Vn_11_12_2020[5] <- (Vietnam_nc_11_12_2020$new_cases[1] + Vietnam_nc_11_12_2020$new_cases[2] + Vietnam_nc_11_12_2020$new_cases[3] + Vietnam_nc_11_12_2020$new_cases[4] + Vietnam_nc_11_12_2020$new_cases[5])/5
avg_nc_Vn_11_12_2020[6] <- (Vietnam_nc_11_12_2020$new_cases[1] + Vietnam_nc_11_12_2020$new_cases[2] + Vietnam_nc_11_12_2020$new_cases[3] + Vietnam_nc_11_12_2020$new_cases[4] + Vietnam_nc_11_12_2020$new_cases[5] + Vietnam_nc_11_12_2020$new_cases[6])/6
for(i in 7:length(Vietnam_nc_11_12_2020$new_cases))
{
  avg_nc_Vn_11_12_2020[i]=(Vietnam_nc_11_12_2020$new_cases[i] + Vietnam_nc_11_12_2020$new_cases[i-1] + Vietnam_nc_11_12_2020$new_cases[i-2] + Vietnam_nc_11_12_2020$new_cases[i-3] + Vietnam_nc_11_12_2020$new_cases[i-4] + Vietnam_nc_11_12_2020$new_cases[i-5] + Vietnam_nc_11_12_2020$new_cases[i-6])/7
}
acml_nc_Vn_11_12_2020 <- c()
acml_nc_Vn_11_12_2020[1] <- avg_nc_Vn_11_12_2020[1]
for(i in 2:length(avg_nc_Vn_11_12_2020))
{
  acml_nc_Vn_11_12_2020[i] <- avg_nc_Vn_11_12_2020[i] + acml_nc_Vn_11_12_2020[i-1]
}
Vietnam_nc_11_12_2020 <- data.frame(Vietnam_nc_11_12_2020, avg_nc_Vn_11_12_2020, acml_nc_Vn_11_12_2020)



Vietnam_nc_11_12_2021 <- na.omit(Vietnam_nc[Vietnam_nc$datetime >= "2021-11-01" & Vietnam_nc$datetime <= "2021-12-31",])
avg_nc_Vn_11_12_2021 <- c()
avg_nc_Vn_11_12_2021[1] <- Vietnam_nc_11_12_2021$new_cases[1]
avg_nc_Vn_11_12_2021[2] <- (Vietnam_nc_11_12_2021$new_cases[1] + Vietnam_nc_11_12_2021$new_cases[2])/2
avg_nc_Vn_11_12_2021[3] <- (Vietnam_nc_11_12_2021$new_cases[1] + Vietnam_nc_11_12_2021$new_cases[2] + Vietnam_nc_11_12_2021$new_cases[3])/3
avg_nc_Vn_11_12_2021[4] <- (Vietnam_nc_11_12_2021$new_cases[1] + Vietnam_nc_11_12_2021$new_cases[2] + Vietnam_nc_11_12_2021$new_cases[3] + Vietnam_nc_11_12_2021$new_cases[4])/4
avg_nc_Vn_11_12_2021[5] <- (Vietnam_nc_11_12_2021$new_cases[1] + Vietnam_nc_11_12_2021$new_cases[2] + Vietnam_nc_11_12_2021$new_cases[3] + Vietnam_nc_11_12_2021$new_cases[4] + Vietnam_nc_11_12_2021$new_cases[5])/5
avg_nc_Vn_11_12_2021[6] <- (Vietnam_nc_11_12_2021$new_cases[1] + Vietnam_nc_11_12_2021$new_cases[2] + Vietnam_nc_11_12_2021$new_cases[3] + Vietnam_nc_11_12_2021$new_cases[4] + Vietnam_nc_11_12_2021$new_cases[5] + Vietnam_nc_11_12_2021$new_cases[6])/6
for(i in 7:length(Vietnam_nc_11_12_2021$new_cases))
{
  avg_nc_Vn_11_12_2021[i]=(Vietnam_nc_11_12_2021$new_cases[i] + Vietnam_nc_11_12_2021$new_cases[i-1] + Vietnam_nc_11_12_2021$new_cases[i-2] + Vietnam_nc_11_12_2021$new_cases[i-3] + Vietnam_nc_11_12_2021$new_cases[i-4] + Vietnam_nc_11_12_2021$new_cases[i-5] + Vietnam_nc_11_12_2021$new_cases[i-6])/7
}
acml_nc_Vn_11_12_2021 <- c()
acml_nc_Vn_11_12_2021[1] <- avg_nc_Vn_11_12_2021[1]
for(i in 2:length(avg_nc_Vn_11_12_2021))
{
  acml_nc_Vn_11_12_2021[i] <- avg_nc_Vn_11_12_2021[i] + acml_nc_Vn_11_12_2021[i-1]
}
Vietnam_nc_11_12_2021 <- data.frame(Vietnam_nc_11_12_2021, avg_nc_Vn_11_12_2021, acml_nc_Vn_11_12_2021)





#Newdeaths:
#Indonesia
Indonesia_nd_11_12_2020 <- na.omit(Indonesia_nd[Indonesia_nd$datetime >= "2020-11-01" & Indonesia_nd$datetime <= "2020-12-31",])
avg_nd_Indo_11_12_2020 <- c()
avg_nd_Indo_11_12_2020[1] <- Indonesia_nd_11_12_2020$new_deaths[1]
avg_nd_Indo_11_12_2020[2] <- (Indonesia_nd_11_12_2020$new_deaths[1] + Indonesia_nd_11_12_2020$new_deaths[2])/2
avg_nd_Indo_11_12_2020[3] <- (Indonesia_nd_11_12_2020$new_deaths[1] + Indonesia_nd_11_12_2020$new_deaths[2] + Indonesia_nd_11_12_2020$new_deaths[3])/3
avg_nd_Indo_11_12_2020[4] <- (Indonesia_nd_11_12_2020$new_deaths[1] + Indonesia_nd_11_12_2020$new_deaths[2] + Indonesia_nd_11_12_2020$new_deaths[3] + Indonesia_nd_11_12_2020$new_deaths[4])/4
avg_nd_Indo_11_12_2020[5] <- (Indonesia_nd_11_12_2020$new_deaths[1] + Indonesia_nd_11_12_2020$new_deaths[2] + Indonesia_nd_11_12_2020$new_deaths[3] + Indonesia_nd_11_12_2020$new_deaths[4] + Indonesia_nd_11_12_2020$new_deaths[5])/5
avg_nd_Indo_11_12_2020[6] <- (Indonesia_nd_11_12_2020$new_deaths[1] + Indonesia_nd_11_12_2020$new_deaths[2] + Indonesia_nd_11_12_2020$new_deaths[3] + Indonesia_nd_11_12_2020$new_deaths[4] + Indonesia_nd_11_12_2020$new_deaths[5] + Indonesia_nd_11_12_2020$new_deaths[6])/6
for(i in 7:length(Indonesia_nd_11_12_2020$new_deaths))
{
  avg_nd_Indo_11_12_2020[i]=(Indonesia_nd_11_12_2020$new_deaths[i] + Indonesia_nd_11_12_2020$new_deaths[i-1] + Indonesia_nd_11_12_2020$new_deaths[i-2] + Indonesia_nd_11_12_2020$new_deaths[i-3] + Indonesia_nd_11_12_2020$new_deaths[i-4] + Indonesia_nd_11_12_2020$new_deaths[i-5] + Indonesia_nd_11_12_2020$new_deaths[i-6])/7
}
acml_nd_Indo_11_12_2020 <- c()
acml_nd_Indo_11_12_2020[1] <- avg_nd_Indo_11_12_2020[1]
for(i in 2:length(avg_nd_Indo_11_12_2020))
{
  acml_nd_Indo_11_12_2020[i] <- avg_nd_Indo_11_12_2020[i] + acml_nd_Indo_11_12_2020[i-1]
}
Indonesia_nd_11_12_2020 <- data.frame(Indonesia_nd_11_12_2020, avg_nd_Indo_11_12_2020, acml_nd_Indo_11_12_2020)



Indonesia_nd_11_12_2021 <- na.omit(Indonesia_nd[Indonesia_nd$datetime >= "2021-11-01" & Indonesia_nd$datetime <= "2021-12-31",])
avg_nd_Indo_11_12_2021 <- c()
avg_nd_Indo_11_12_2021[1] <- Indonesia_nd_11_12_2021$new_deaths[1]
avg_nd_Indo_11_12_2021[2] <- (Indonesia_nd_11_12_2021$new_deaths[1] + Indonesia_nd_11_12_2021$new_deaths[2])/2
avg_nd_Indo_11_12_2021[3] <- (Indonesia_nd_11_12_2021$new_deaths[1] + Indonesia_nd_11_12_2021$new_deaths[2] + Indonesia_nd_11_12_2021$new_deaths[3])/3
avg_nd_Indo_11_12_2021[4] <- (Indonesia_nd_11_12_2021$new_deaths[1] + Indonesia_nd_11_12_2021$new_deaths[2] + Indonesia_nd_11_12_2021$new_deaths[3] + Indonesia_nd_11_12_2021$new_deaths[4])/4
avg_nd_Indo_11_12_2021[5] <- (Indonesia_nd_11_12_2021$new_deaths[1] + Indonesia_nd_11_12_2021$new_deaths[2] + Indonesia_nd_11_12_2021$new_deaths[3] + Indonesia_nd_11_12_2021$new_deaths[4] + Indonesia_nd_11_12_2021$new_deaths[5])/5
avg_nd_Indo_11_12_2021[6] <- (Indonesia_nd_11_12_2021$new_deaths[1] + Indonesia_nd_11_12_2021$new_deaths[2] + Indonesia_nd_11_12_2021$new_deaths[3] + Indonesia_nd_11_12_2021$new_deaths[4] + Indonesia_nd_11_12_2021$new_deaths[5] + Indonesia_nd_11_12_2021$new_deaths[6])/6
for(i in 7:length(Indonesia_nd_11_12_2021$new_deaths))
{
  avg_nd_Indo_11_12_2021[i]=(Indonesia_nd_11_12_2021$new_deaths[i] + Indonesia_nd_11_12_2021$new_deaths[i-1] + Indonesia_nd_11_12_2021$new_deaths[i-2] + Indonesia_nd_11_12_2021$new_deaths[i-3] + Indonesia_nd_11_12_2021$new_deaths[i-4] + Indonesia_nd_11_12_2021$new_deaths[i-5] + Indonesia_nd_11_12_2021$new_deaths[i-6])/7
}
acml_nd_Indo_11_12_2021 <- c()
acml_nd_Indo_11_12_2021[1] <- avg_nd_Indo_11_12_2021[1]
for(i in 2:length(avg_nd_Indo_11_12_2021))
{
  acml_nd_Indo_11_12_2021[i] <- avg_nd_Indo_11_12_2021[i] + acml_nd_Indo_11_12_2021[i-1]
}
Indonesia_nd_11_12_2021 <- data.frame(Indonesia_nd_11_12_2021, avg_nd_Indo_11_12_2021, acml_nd_Indo_11_12_2021)




#Japan
Japan_nd_11_12_2020 <- na.omit(Japan_nd[Japan_nd$datetime >= "2020-11-01" & Japan_nd$datetime <= "2020-12-31",])
avg_nd_Jp_11_12_2020 <- c()
avg_nd_Jp_11_12_2020[1] <- Japan_nd_11_12_2020$new_deaths[1]
avg_nd_Jp_11_12_2020[2] <- (Japan_nd_11_12_2020$new_deaths[1] + Japan_nd_11_12_2020$new_deaths[2])/2
avg_nd_Jp_11_12_2020[3] <- (Japan_nd_11_12_2020$new_deaths[1] + Japan_nd_11_12_2020$new_deaths[2] + Japan_nd_11_12_2020$new_deaths[3])/3
avg_nd_Jp_11_12_2020[4] <- (Japan_nd_11_12_2020$new_deaths[1] + Japan_nd_11_12_2020$new_deaths[2] + Japan_nd_11_12_2020$new_deaths[3] + Japan_nd_11_12_2020$new_deaths[4])/4
avg_nd_Jp_11_12_2020[5] <- (Japan_nd_11_12_2020$new_deaths[1] + Japan_nd_11_12_2020$new_deaths[2] + Japan_nd_11_12_2020$new_deaths[3] + Japan_nd_11_12_2020$new_deaths[4] + Japan_nd_11_12_2020$new_deaths[5])/5
avg_nd_Jp_11_12_2020[6] <- (Japan_nd_11_12_2020$new_deaths[1] + Japan_nd_11_12_2020$new_deaths[2] + Japan_nd_11_12_2020$new_deaths[3] + Japan_nd_11_12_2020$new_deaths[4] + Japan_nd_11_12_2020$new_deaths[5] + Japan_nd_11_12_2020$new_deaths[6])/6
for(i in 7:length(Japan_nd_11_12_2020$new_deaths))
{
  avg_nd_Jp_11_12_2020[i]=(Japan_nd_11_12_2020$new_deaths[i] + Japan_nd_11_12_2020$new_deaths[i-1] + Japan_nd_11_12_2020$new_deaths[i-2] + Japan_nd_11_12_2020$new_deaths[i-3] + Japan_nd_11_12_2020$new_deaths[i-4] + Japan_nd_11_12_2020$new_deaths[i-5] + Japan_nd_11_12_2020$new_deaths[i-6])/7
}
acml_nd_Jp_11_12_2020 <- c()
acml_nd_Jp_11_12_2020[1] <- avg_nd_Jp_11_12_2020[1]
for(i in 2:length(avg_nd_Jp_11_12_2020))
{
  acml_nd_Jp_11_12_2020[i] <- avg_nd_Jp_11_12_2020[i] + acml_nd_Jp_11_12_2020[i-1]
}
Japan_nd_11_12_2020 <- data.frame(Japan_nd_11_12_2020, avg_nd_Jp_11_12_2020, acml_nd_Jp_11_12_2020)




Japan_nd_11_12_2021 <- na.omit(Japan_nd[Japan_nd$datetime >= "2021-11-01" & Japan_nd$datetime <= "2021-12-31",])
avg_nd_Jp_11_12_2021 <- c()
avg_nd_Jp_11_12_2021[1] <- Japan_nd_11_12_2021$new_deaths[1]
avg_nd_Jp_11_12_2021[2] <- (Japan_nd_11_12_2021$new_deaths[1] + Japan_nd_11_12_2021$new_deaths[2])/2
avg_nd_Jp_11_12_2021[3] <- (Japan_nd_11_12_2021$new_deaths[1] + Japan_nd_11_12_2021$new_deaths[2] + Japan_nd_11_12_2021$new_deaths[3])/3
avg_nd_Jp_11_12_2021[4] <- (Japan_nd_11_12_2021$new_deaths[1] + Japan_nd_11_12_2021$new_deaths[2] + Japan_nd_11_12_2021$new_deaths[3] + Japan_nd_11_12_2021$new_deaths[4])/4
avg_nd_Jp_11_12_2021[5] <- (Japan_nd_11_12_2021$new_deaths[1] + Japan_nd_11_12_2021$new_deaths[2] + Japan_nd_11_12_2021$new_deaths[3] + Japan_nd_11_12_2021$new_deaths[4] + Japan_nd_11_12_2021$new_deaths[5])/5
avg_nd_Jp_11_12_2021[6] <- (Japan_nd_11_12_2021$new_deaths[1] + Japan_nd_11_12_2021$new_deaths[2] + Japan_nd_11_12_2021$new_deaths[3] + Japan_nd_11_12_2021$new_deaths[4] + Japan_nd_11_12_2021$new_deaths[5] + Japan_nd_11_12_2021$new_deaths[6])/6
for(i in 7:length(Japan_nd_11_12_2021$new_deaths))
{
  avg_nd_Jp_11_12_2021[i]=(Japan_nd_11_12_2021$new_deaths[i] + Japan_nd_11_12_2021$new_deaths[i-1] + Japan_nd_11_12_2021$new_deaths[i-2] + Japan_nd_11_12_2021$new_deaths[i-3] + Japan_nd_11_12_2021$new_deaths[i-4] + Japan_nd_11_12_2021$new_deaths[i-5] + Japan_nd_11_12_2021$new_deaths[i-6])/7
}
acml_nd_Jp_11_12_2021 <- c()
acml_nd_Jp_11_12_2021[1] <- avg_nd_Jp_11_12_2021[1]
for(i in 2:length(avg_nd_Jp_11_12_2021))
{
  acml_nd_Jp_11_12_2021[i] <- avg_nd_Jp_11_12_2021[i] + acml_nd_Jp_11_12_2021[i-1]
}
Japan_nd_11_12_2021 <- data.frame(Japan_nd_11_12_2021, avg_nd_Jp_11_12_2021, acml_nd_Jp_11_12_2021)






#Vietnam:
Vietnam_nd_11_12_2020 <- na.omit(Vietnam_nd[Vietnam_nd$datetime >= "2020-11-01" & Vietnam_nd$datetime <= "2020-12-31",])
avg_nd_Vn_11_12_2020 <- c()
avg_nd_Vn_11_12_2020[1] <- Vietnam_nd_11_12_2020$new_deaths[1]
avg_nd_Vn_11_12_2020[2] <- (Vietnam_nd_11_12_2020$new_deaths[1] + Vietnam_nd_11_12_2020$new_deaths[2])/2
avg_nd_Vn_11_12_2020[3] <- (Vietnam_nd_11_12_2020$new_deaths[1] + Vietnam_nd_11_12_2020$new_deaths[2] + Vietnam_nd_11_12_2020$new_deaths[3])/3
avg_nd_Vn_11_12_2020[4] <- (Vietnam_nd_11_12_2020$new_deaths[1] + Vietnam_nd_11_12_2020$new_deaths[2] + Vietnam_nd_11_12_2020$new_deaths[3] + Vietnam_nd_11_12_2020$new_deaths[4])/4
avg_nd_Vn_11_12_2020[5] <- (Vietnam_nd_11_12_2020$new_deaths[1] + Vietnam_nd_11_12_2020$new_deaths[2] + Vietnam_nd_11_12_2020$new_deaths[3] + Vietnam_nd_11_12_2020$new_deaths[4] + Vietnam_nd_11_12_2020$new_deaths[5])/5
avg_nd_Vn_11_12_2020[6] <- (Vietnam_nd_11_12_2020$new_deaths[1] + Vietnam_nd_11_12_2020$new_deaths[2] + Vietnam_nd_11_12_2020$new_deaths[3] + Vietnam_nd_11_12_2020$new_deaths[4] + Vietnam_nd_11_12_2020$new_deaths[5] + Vietnam_nd_11_12_2020$new_deaths[6])/6
for(i in 7:length(Vietnam_nd_11_12_2020$new_deaths))
{
  avg_nd_Vn_11_12_2020[i]=(Vietnam_nd_11_12_2020$new_deaths[i] + Vietnam_nd_11_12_2020$new_deaths[i-1] + Vietnam_nd_11_12_2020$new_deaths[i-2] + Vietnam_nd_11_12_2020$new_deaths[i-3] + Vietnam_nd_11_12_2020$new_deaths[i-4] + Vietnam_nd_11_12_2020$new_deaths[i-5] + Vietnam_nd_11_12_2020$new_deaths[i-6])/7
}
acml_nd_Vn_11_12_2020 <- c()
acml_nd_Vn_11_12_2020[1] <- avg_nd_Vn_11_12_2020[1]
for(i in 2:length(avg_nd_Vn_11_12_2020))
{
  acml_nd_Vn_11_12_2020[i] <- avg_nd_Vn_11_12_2020[i] + acml_nd_Vn_11_12_2020[i-1]
}
Vietnam_nd_11_12_2020 <- data.frame(Vietnam_nd_11_12_2020, avg_nd_Vn_11_12_2020, acml_nd_Vn_11_12_2020)




Vietnam_nd_11_12_2021 <- na.omit(Vietnam_nd[Vietnam_nd$datetime >= "2021-11-01" & Vietnam_nd$datetime <= "2021-12-31",])
avg_nd_Vn_11_12_2021 <- c()
avg_nd_Vn_11_12_2021[1] <- Vietnam_nd_11_12_2021$new_deaths[1]
avg_nd_Vn_11_12_2021[2] <- (Vietnam_nd_11_12_2021$new_deaths[1] + Vietnam_nd_11_12_2021$new_deaths[2])/2
avg_nd_Vn_11_12_2021[3] <- (Vietnam_nd_11_12_2021$new_deaths[1] + Vietnam_nd_11_12_2021$new_deaths[2] + Vietnam_nd_11_12_2021$new_deaths[3])/3
avg_nd_Vn_11_12_2021[4] <- (Vietnam_nd_11_12_2021$new_deaths[1] + Vietnam_nd_11_12_2021$new_deaths[2] + Vietnam_nd_11_12_2021$new_deaths[3] + Vietnam_nd_11_12_2021$new_deaths[4])/4
avg_nd_Vn_11_12_2021[5] <- (Vietnam_nd_11_12_2021$new_deaths[1] + Vietnam_nd_11_12_2021$new_deaths[2] + Vietnam_nd_11_12_2021$new_deaths[3] + Vietnam_nd_11_12_2021$new_deaths[4] + Vietnam_nd_11_12_2021$new_deaths[5])/5
avg_nd_Vn_11_12_2021[6] <- (Vietnam_nd_11_12_2021$new_deaths[1] + Vietnam_nd_11_12_2021$new_deaths[2] + Vietnam_nd_11_12_2021$new_deaths[3] + Vietnam_nd_11_12_2021$new_deaths[4] + Vietnam_nd_11_12_2021$new_deaths[5] + Vietnam_nd_11_12_2021$new_deaths[6])/6
for(i in 7:length(Vietnam_nd_11_12_2021$new_deaths))
{
  avg_nd_Vn_11_12_2021[i]=(Vietnam_nd_11_12_2021$new_deaths[i] + Vietnam_nd_11_12_2021$new_deaths[i-1] + Vietnam_nd_11_12_2021$new_deaths[i-2] + Vietnam_nd_11_12_2021$new_deaths[i-3] + Vietnam_nd_11_12_2021$new_deaths[i-4] + Vietnam_nd_11_12_2021$new_deaths[i-5] + Vietnam_nd_11_12_2021$new_deaths[i-6])/7
}
acml_nd_Vn_11_12_2021 <- c()
acml_nd_Vn_11_12_2021[1] <- avg_nd_Vn_11_12_2021[1]
for(i in 2:length(avg_nd_Vn_11_12_2021))
{
  acml_nd_Vn_11_12_2021[i] <- avg_nd_Vn_11_12_2021[i] + acml_nd_Vn_11_12_2021[i-1]
}
Vietnam_nd_11_12_2021 <- data.frame(Vietnam_nd_11_12_2021, avg_nd_Vn_11_12_2021, acml_nd_Vn_11_12_2021)



library(ggplot2)
#Bieu do thu thap nhiem benh cho 2 thang cuoi nam

Newcases_11_12_2020 <- ggplot() +
  geom_line(data=Indonesia_nc_11_12_2020, aes(x=datetime, y=avg_nc_Indo_11_12_2020, color = 'Indonesia')) +
  geom_line(data=Japan_nc_11_12_2020, aes(x=datetime, y=avg_nc_Jp_11_12_2020, color = 'Japan')) +
  geom_line(data=Vietnam_nc_11_12_2020, aes(x=datetime, y=avg_nc_Vn_11_12_2020, color = 'Vietnam')) +
  labs(title = "Newcases in the last 2 months of 2020", x = "date", y = "new cases")
ggsave(file="Newcases in the last 2 months of 2020.png")


Newcases_11_12_2021 <- ggplot() +
  geom_line(data=Indonesia_nc_11_12_2021, aes(x=datetime, y=avg_nc_Indo_11_12_2021, color = 'Indonesia')) +
  geom_line(data=Japan_nc_11_12_2021, aes(x=datetime, y=avg_nc_Jp_11_12_2021, color = 'Japan')) +
  geom_line(data=Vietnam_nc_11_12_2021, aes(x=datetime, y=avg_nc_Vn_11_12_2021, color = 'Vietnam')) +
  labs(title = "Newcases in the last 2 months of 2021", x = "date", y = "new cases")
ggsave(file="Newcases in the last 2 months of 2021.png")

#Bieu do thu thap tu vong cho 2 thang cuoi nam

Newdeaths_11_12_2020 <- ggplot() +
  geom_line(data=Indonesia_nd_11_12_2020, aes(x=datetime, y=avg_nd_Indo_11_12_2020, color = 'Indonesia')) +
  geom_line(data=Japan_nd_11_12_2020, aes(x=datetime, y=avg_nd_Jp_11_12_2020, color = 'Japan')) +
  geom_line(data=Vietnam_nd_11_12_2020, aes(x=datetime, y=avg_nd_Vn_11_12_2020, color = 'Vietnam')) +
  labs(title = "Newdeaths in the last 2 months of 2020", x = "date", y = "new deaths")
ggsave(file="Newdeaths in the last 2 months of 2020.png")


Newdeaths_11_12_2021 <- ggplot() +
  geom_line(data=Indonesia_nd_11_12_2021, aes(x=datetime, y=avg_nd_Indo_11_12_2021, color = 'Indonesia')) +
  geom_line(data=Japan_nd_11_12_2021, aes(x=datetime, y=avg_nd_Jp_11_12_2021, color = 'Japan')) +
  geom_line(data=Vietnam_nd_11_12_2021, aes(x=datetime, y=avg_nd_Vn_11_12_2021, color = 'Vietnam')) +
  labs(title = "Newdeaths in the last 2 months of 2021", x = "date", y = "new deaths")
ggsave(file="Newdeaths in the last 2 months of 2021.png")

#Bieu do thu thap nhiem benh va tu vong 2 thang cuoi nam

New_11_12_2020 <- ggplot() +
  geom_line(data=Indonesia_nc_11_12_2020, aes(x=datetime, y=avg_nc_Indo_11_12_2020, linetype = 'Newcases', color='Indonesia')) +
  geom_line(data=Japan_nc_11_12_2020, aes(x=datetime, y=avg_nc_Jp_11_12_2020, linetype = 'Newcases', color='Japan')) +
  geom_line(data=Vietnam_nc_11_12_2020, aes(x=datetime, y=avg_nc_Vn_11_12_2020, linetype = 'Newcases', color='Vietnam')) +
  geom_line(data=Indonesia_nd_11_12_2020, aes(x=datetime, y=avg_nd_Indo_11_12_2020, linetype = 'Newdeaths', color='Indonesia')) +
  geom_line(data=Japan_nd_11_12_2020, aes(x=datetime, y=avg_nd_Jp_11_12_2020, linetype = 'Newdeaths', color='Japan')) +
  geom_line(data=Vietnam_nd_11_12_2020, aes(x=datetime, y=avg_nd_Vn_11_12_2020, linetype = 'Newdeaths', color='Vietnam')) +
  labs(title = "Newcases and Newdeaths in the last 2 months of 2020", x = "date", y = "cases")
ggsave(file="Newcases and Newdeaths in the last 2 months of 2020.png")

New_11_12_2021 <- ggplot() +
  geom_line(data=Indonesia_nc_11_12_2021, aes(x=datetime, y=avg_nc_Indo_11_12_2021, linetype = 'Newcases', color='Indonesia')) +
  geom_line(data=Japan_nc_11_12_2021, aes(x=datetime, y=avg_nc_Jp_11_12_2021, linetype = 'Newcases', color='Japan')) +
  geom_line(data=Vietnam_nc_11_12_2021, aes(x=datetime, y=avg_nc_Vn_11_12_2021, linetype = 'Newcases', color='Vietnam')) +
  geom_line(data=Indonesia_nd_11_12_2021, aes(x=datetime, y=avg_nd_Indo_11_12_2021, linetype = 'Newdeaths', color='Indonesia')) +
  geom_line(data=Japan_nd_11_12_2021, aes(x=datetime, y=avg_nd_Jp_11_12_2021, linetype = 'Newdeaths', color='Japan')) +
  geom_line(data=Vietnam_nd_11_12_2021, aes(x=datetime, y=avg_nd_Vn_11_12_2021, linetype = 'Newdeaths', color='Vietnam')) +
  labs(title = "Newcases and Newdeaths in the last 2 months of 2021", x = "date", y = "cases")
ggsave(file="Newcases and Newdeaths in the last 2 months of 2021.png")


#Bieu do nhiem benh tich luy tung thang
Acml_Newcases_1_2020 <- ggplot() + 
  geom_line(data=Japan_nc_1_2020, aes(x=datetime, y=acml_nc_Jp_1_2020, color = 'Japan')) +
  geom_line(data=Vietnam_nc_1_2020, aes(x=datetime, y=acml_nc_Vn_1_2020, color = 'Vietnam')) +
  labs(title = "Cumulative Newcases 1-2020", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newcases 1-2020.png")

Acml_Newcases_3_2020 <- ggplot() +
  geom_line(data=Indonesia_nc_3_2020, aes(x=datetime, y=acml_nc_Indo_3_2020, color = 'Indonesia')) +
  geom_line(data=Japan_nc_3_2020, aes(x=datetime, y=acml_nc_Jp_3_2020, color = 'Japan')) +
  geom_line(data=Vietnam_nc_3_2020, aes(x=datetime, y=acml_nc_Vn_3_2020, color = 'Vietnam')) +
  labs(title = "Cumulative Newcases 3-2020", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newcases 3-2020.png")

Acml_Newcases_4_2020 <- ggplot() +
  geom_line(data=Indonesia_nc_4_2020, aes(x=datetime, y=acml_nc_Indo_4_2020, color = 'Indonesia')) +
  geom_line(data=Japan_nc_4_2020, aes(x=datetime, y=acml_nc_Jp_4_2020, color = 'Japan')) +
  geom_line(data=Vietnam_nc_4_2020, aes(x=datetime, y=acml_nc_Vn_4_2020, color = 'Vietnam')) +
  labs(title = "Cumulative Newcases 4-2020", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newcases 4-2020.png")

Acml_Newcases_5_2020 <- ggplot() +
  geom_line(data=Indonesia_nc_5_2020, aes(x=datetime, y=acml_nc_Indo_5_2020, color = 'Indonesia')) +
  geom_line(data=Japan_nc_5_2020, aes(x=datetime, y=acml_nc_Jp_5_2020, color = 'Japan')) +
  geom_line(data=Vietnam_nc_5_2020, aes(x=datetime, y=acml_nc_Vn_5_2020, color = 'Vietnam')) +
  labs(title = "Cumulative Newcases 5-2020", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newcases 5-2020.png")

Acml_Newcases_1_2021 <- ggplot() + 
  geom_line(data=Indonesia_nc_1_2021, aes(x=datetime, y=acml_nc_Indo_1_2021, color = 'Indonesia')) +
  geom_line(data=Japan_nc_1_2021, aes(x=datetime, y=acml_nc_Jp_1_2021, color = 'Japan')) +
  geom_line(data=Vietnam_nc_1_2021, aes(x=datetime, y=acml_nc_Vn_1_2021, color = 'Vietnam')) +
  labs(title = "Cumulative Newcases 1-2021", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newcases 1-2021.png")

Acml_Newcases_3_2021 <- ggplot() +
  geom_line(data=Indonesia_nc_3_2021, aes(x=datetime, y=acml_nc_Indo_3_2021, color = 'Indonesia')) +
  geom_line(data=Japan_nc_3_2021, aes(x=datetime, y=acml_nc_Jp_3_2021, color = 'Japan')) +
  geom_line(data=Vietnam_nc_3_2021, aes(x=datetime, y=acml_nc_Vn_3_2021, color = 'Vietnam')) +
  labs(title = "Cumulative Newcases 3-2021", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newcases 3-2021.png")

Acml_Newcases_4_2021 <- ggplot() +
  geom_line(data=Indonesia_nc_4_2021, aes(x=datetime, y=acml_nc_Indo_4_2021, color = 'Indonesia')) +
  geom_line(data=Japan_nc_4_2021, aes(x=datetime, y=acml_nc_Jp_4_2021, color = 'Japan')) +
  geom_line(data=Vietnam_nc_4_2021, aes(x=datetime, y=acml_nc_Vn_4_2021, color = 'Vietnam')) +
  labs(title = "Cumulative Newcases 4-2021", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newcases 4-2021.png")

Acml_Newcases_5_2021 <- ggplot() +
  geom_line(data=Indonesia_nc_5_2021, aes(x=datetime, y=acml_nc_Indo_5_2021, color = 'Indonesia')) +
  geom_line(data=Japan_nc_5_2021, aes(x=datetime, y=acml_nc_Jp_5_2021, color = 'Japan')) +
  geom_line(data=Vietnam_nc_5_2021, aes(x=datetime, y=acml_nc_Vn_5_2021, color = 'Vietnam')) +
  labs(title = "Cumulative Newcases 5-2021", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newcases 5-2021.png")

Acml_Newcases_1_2022 <- ggplot() +
  geom_line(data=Indonesia_nc_1_2022, aes(x=datetime, y=acml_nc_Indo_1_2022, color = 'Indonesia')) +
  geom_line(data=Japan_nc_1_2022, aes(x=datetime, y=acml_nc_Jp_1_2022, color = 'Japan')) +
  geom_line(data=Vietnam_nc_1_2022, aes(x=datetime, y=acml_nc_Vn_1_2022, color = 'Vietnam')) +
  labs(title = "Cumulative Newcases 1-2022", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newcases 1-2022.png")




#Bieu do tu vong tich luy hang thang

Acml_Newdeaths_3_2020 <- ggplot() +
  geom_line(data=Indonesia_nd_3_2020, aes(x=datetime, y=acml_nd_Indo_3_2020, color = 'Indonesia')) +
  geom_line(data=Japan_nd_3_2020, aes(x=datetime, y=acml_nd_Jp_3_2020, color = 'Japan')) +
  labs(title = "Cumulative Newdeaths 3-2020", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newdeaths 3-2020.png")

Acml_Newdeaths_4_2020 <- ggplot() +
  geom_line(data=Indonesia_nd_4_2020, aes(x=datetime, y=acml_nd_Indo_4_2020, color = 'Indonesia')) +
  geom_line(data=Japan_nd_4_2020, aes(x=datetime, y=acml_nd_Jp_4_2020, color = 'Japan')) +
  labs(title = "Cumulative Newdeaths 4-2020", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newdeaths 4-2020.png")

Acml_Newdeaths_5_2020 <- ggplot() +
  geom_line(data=Indonesia_nd_5_2020, aes(x=datetime, y=acml_nd_Indo_5_2020, color = 'Indonesia')) +
  geom_line(data=Japan_nd_5_2020, aes(x=datetime, y=acml_nd_Jp_5_2020, color = 'Japan')) +
  labs(title = "Cumulative Newdeaths 5-2020", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newdeaths 5-2020.png")

Acml_Newdeaths_1_2021 <- ggplot() +
  geom_line(data=Indonesia_nd_1_2021, aes(x=datetime, y=acml_nd_Indo_1_2021, color = 'Indonesia')) +
  geom_line(data=Japan_nd_1_2021, aes(x=datetime, y=acml_nd_Jp_1_2021, color = 'Japan')) +
  geom_line(data=Vietnam_nd_1_2021, aes(x=datetime, y=acml_nd_Vn_1_2021, color = 'Vietnam')) +
  labs(title = "Cumulative Newdeaths 1-2021", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newdeaths 1-2021.png")

Acml_Newdeaths_3_2021 <- ggplot() +
  geom_line(data=Indonesia_nd_3_2021, aes(x=datetime, y=acml_nd_Indo_3_2021, color = 'Indonesia')) +
  geom_line(data=Japan_nd_3_2021, aes(x=datetime, y=acml_nd_Jp_3_2021, color = 'Japan')) +
  geom_line(data=Vietnam_nd_3_2021, aes(x=datetime, y=acml_nd_Vn_3_2021, color = 'Vietnam')) +
  labs(title = "Cumulative Newdeaths 3-2021", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newdeaths 3-2021.png")

Acml_Newdeaths_4_2021 <- ggplot() +
  geom_line(data=Indonesia_nd_4_2021, aes(x=datetime, y=acml_nd_Indo_4_2021, color = 'Indonesia')) +
  geom_line(data=Japan_nd_4_2021, aes(x=datetime, y=acml_nd_Jp_4_2021, color = 'Japan')) +
  geom_line(data=Vietnam_nd_4_2021, aes(x=datetime, y=acml_nd_Vn_4_2021, color = 'Vietnam')) +
  labs(title = "Cumulative Newdeaths 4-2021", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newdeaths 4-2021.png")

Acml_Newdeaths_5_2021 <- ggplot() +
  geom_line(data=Indonesia_nd_5_2021, aes(x=datetime, y=acml_nd_Indo_5_2021, color = 'Indonesia')) +
  geom_line(data=Japan_nd_5_2021, aes(x=datetime, y=acml_nd_Jp_5_2021, color = 'Japan')) +
  geom_line(data=Vietnam_nd_5_2021, aes(x=datetime, y=acml_nd_Vn_5_2021, color = 'Vietnam')) +
  labs(title = "Cumulative Newdeaths 5-2021", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newdeaths 5-2021.png")

Acml_Newdeaths_1_2022 <- ggplot() +
  geom_line(data=Indonesia_nd_1_2022, aes(x=datetime, y=acml_nd_Indo_1_2022, color = 'Indonesia')) +
  geom_line(data=Japan_nd_1_2022, aes(x=datetime, y=acml_nd_Jp_1_2022, color = 'Japan')) +
  geom_line(data=Vietnam_nd_1_2022, aes(x=datetime, y=acml_nd_Vn_1_2022, color = 'Vietnam')) +
  labs(title = "Cumulative Newdeaths 1-2022", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newdeaths 1-2022.png")

suppressWarnings(detach("package:readr", unload=TRUE))
suppressWarnings(detach("package:here", unload=TRUE))
