library(readr)
suppressMessages(library(here))
setwd(here())
dir.create("x", showWarnings = FALSE)
data <- read_csv("owid-covid-data.csv", show_col_types = FALSE)

iso_code <- data$iso_code
continent <- data$continent
location <- data$location
date <- data$date
new_cases <- data$new_cases
new_deaths <- data$new_deaths

datetime <- strptime(date[], format="%m/ %d/ %Y")

# change working directory
setwd(here("x"))

#du lieu newcases
new.dat1 <- data.frame(location, datetime, new_cases)

#loc theo nuoc
Indonesia_nc <- new.dat1[new.dat1$location == "Indonesia",]
Japan_nc <- new.dat1[new.dat1$location == "Japan",]
Vietnam_nc <- new.dat1[new.dat1$location == "Vietnam",]



#Newcases 3-2020
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

#Cau 1 bai x
Indo_nc_last_7d <- na.omit(Indonesia_nc[Indonesia_nc$datetime >= "2022-02-13" & Indonesia_nc$datetime <= "2022-02-19",])
Jp_nc_last_7d <- na.omit(Japan_nc[Japan_nc$datetime >= "2022-02-13" & Japan_nc$datetime <= "2022-02-19",])
Vn_nc_last_7d <- na.omit(Vietnam_nc[Vietnam_nc$datetime >= "2022-02-13" & Vietnam_nc$datetime <= "2022-02-19",])

library(ggplot2)

Newcases_last_7d <- ggplot() +
  geom_line(data=Indo_nc_last_7d, aes(x=datetime, y=new_cases, color = 'Indonesia')) + geom_point(data=Indo_nc_last_7d, aes(x=datetime, y=new_cases, color = 'Indonesia')) +
  geom_line(data=Jp_nc_last_7d, aes(x=datetime, y=new_cases ,color = 'Japan')) + geom_point(data=Jp_nc_last_7d, aes(x=datetime, y=new_cases ,color = 'Japan')) +
  geom_line(data=Vn_nc_last_7d, aes(x=datetime, y=new_cases ,color = 'Vietnam')) + geom_point(data=Vn_nc_last_7d, aes(x=datetime, y=new_cases ,color = 'Vietnam')) +
  labs(title = "Newcases in the last 7 days", x = "date", y = "new cases")
ggsave(file="Newcases in the last 7 days.png")

#Cau 3 bai x
Acml_3_2020 <- ggplot() +
  geom_line(data=Indonesia_nc_3_2020, aes(x=datetime, y=acml_nc_Indo_3_2020, color = 'Indonesia', linetype = 'Cumulative Newcases')) +
  geom_line(data=Japan_nc_3_2020, aes(x=datetime, y=acml_nc_Jp_3_2020, color = 'Japan', linetype = 'Cumulative Newcases')) +
  geom_line(data=Vietnam_nc_3_2020, aes(x=datetime, y=acml_nc_Vn_3_2020, color = 'Vietnam', linetype = 'Cumulative Newcases')) +
  geom_line(data=Indonesia_nd_3_2020, aes(x=datetime, y=acml_nd_Indo_3_2020, color = 'Indonesia', linetype = 'Cumulative Newdeaths')) +
  geom_line(data=Japan_nd_3_2020, aes(x=datetime, y=acml_nd_Jp_3_2020, color = 'Japan', linetype = 'Cumulative Newdeaths')) +
  labs(title = "Cumulative Newcases and Newdeaths 3-2020", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newcases and Newdeaths 3-2020.png")

Acml_4_2020 <- ggplot() +
  geom_line(data=Indonesia_nc_4_2020, aes(x=datetime, y=acml_nc_Indo_4_2020, color = 'Indonesia', linetype = 'Cumulative Newcases')) +
  geom_line(data=Japan_nc_4_2020, aes(x=datetime, y=acml_nc_Jp_4_2020, color = 'Japan', linetype = 'Cumulative Newcases')) +
  geom_line(data=Vietnam_nc_4_2020, aes(x=datetime, y=acml_nc_Vn_4_2020, color = 'Vietnam', linetype = 'Cumulative Newcases')) +
  geom_line(data=Indonesia_nd_4_2020, aes(x=datetime, y=acml_nd_Indo_4_2020, color = 'Indonesia', linetype = 'Cumulative Newdeaths')) +
  geom_line(data=Japan_nd_4_2020, aes(x=datetime, y=acml_nd_Jp_4_2020, color = 'Japan', linetype = 'Cumulative Newdeaths')) +
  labs(title = "Cumulative Newcases and Newdeaths 4-2020", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newcases and Newdeaths 4-2020.png")

Acml_5_2020 <- ggplot() +
  geom_line(data=Indonesia_nc_5_2020, aes(x=datetime, y=acml_nc_Indo_5_2020, color = 'Indonesia', linetype = 'Cumulative Newcases')) +
  geom_line(data=Japan_nc_5_2020, aes(x=datetime, y=acml_nc_Jp_5_2020, color = 'Japan', linetype = 'Cumulative Newcases')) +
  geom_line(data=Vietnam_nc_5_2020, aes(x=datetime, y=acml_nc_Vn_5_2020, color = 'Vietnam', linetype = 'Cumulative Newcases')) +
  geom_line(data=Indonesia_nd_5_2020, aes(x=datetime, y=acml_nd_Indo_5_2020, color = 'Indonesia', linetype = 'Cumulative Newdeaths')) +
  geom_line(data=Japan_nd_5_2020, aes(x=datetime, y=acml_nd_Jp_5_2020, color = 'Japan', linetype = 'Cumulative Newdeaths')) +
  labs(title = "Cumulative Newcases and Newdeaths 5-2020", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newcases and Newdeaths 5-2020.png")

Acml_1_2021 <- ggplot() +
  geom_line(data=Indonesia_nc_1_2021, aes(x=datetime, y=acml_nc_Indo_1_2021, color = 'Indonesia', linetype = 'Cumulative Newcases')) +
  geom_line(data=Japan_nc_1_2021, aes(x=datetime, y=acml_nc_Jp_1_2021, color = 'Japan', linetype = 'Cumulative Newcases')) +
  geom_line(data=Vietnam_nc_1_2021, aes(x=datetime, y=acml_nc_Vn_1_2021, color = 'Vietnam', linetype = 'Cumulative Newcases')) +
  geom_line(data=Indonesia_nd_1_2021, aes(x=datetime, y=acml_nd_Indo_1_2021, color = 'Indonesia', linetype = 'Cumulative Newdeaths')) +
  geom_line(data=Japan_nd_1_2021, aes(x=datetime, y=acml_nd_Jp_1_2021, color = 'Japan', linetype = 'Cumulative Newdeaths')) +
  geom_line(data=Vietnam_nd_1_2021, aes(x=datetime, y=acml_nd_Vn_1_2021, color = 'Vietnam', linetype = 'Cumulative Newdeaths')) +
  labs(title = "Cumulative Newcases and Newdeaths 1-2021", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newcases and Newdeaths 1-2021.png")

Acml_3_2021 <- ggplot() +
  geom_line(data=Indonesia_nc_3_2021, aes(x=datetime, y=acml_nc_Indo_3_2021, color = 'Indonesia', linetype = 'Cumulative Newcases')) +
  geom_line(data=Japan_nc_3_2021, aes(x=datetime, y=acml_nc_Jp_3_2021, color = 'Japan', linetype = 'Cumulative Newcases')) +
  geom_line(data=Vietnam_nc_3_2021, aes(x=datetime, y=acml_nc_Vn_3_2021, color = 'Vietnam', linetype = 'Cumulative Newcases')) +
  geom_line(data=Indonesia_nd_3_2021, aes(x=datetime, y=acml_nd_Indo_3_2021, color = 'Indonesia', linetype = 'Cumulative Newdeaths')) +
  geom_line(data=Japan_nd_3_2021, aes(x=datetime, y=acml_nd_Jp_3_2021, color = 'Japan', linetype = 'Cumulative Newdeaths')) +
  geom_line(data=Vietnam_nd_3_2021, aes(x=datetime, y=acml_nd_Vn_3_2021, color = 'Vietnam', linetype = 'Cumulative Newdeaths')) +
  labs(title = "Cumulative Newcases and Newdeaths 3-2021", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newcases and Newdeaths 3-2021.png")

Acml_4_2021 <- ggplot() +
  geom_line(data=Indonesia_nc_4_2021, aes(x=datetime, y=acml_nc_Indo_4_2021, color = 'Indonesia', linetype = 'Cumulative Newcases')) +
  geom_line(data=Japan_nc_4_2021, aes(x=datetime, y=acml_nc_Jp_4_2021, color = 'Japan', linetype = 'Cumulative Newcases')) +
  geom_line(data=Vietnam_nc_4_2021, aes(x=datetime, y=acml_nc_Vn_4_2021, color = 'Vietnam', linetype = 'Cumulative Newcases')) +
  geom_line(data=Indonesia_nd_4_2021, aes(x=datetime, y=acml_nd_Indo_4_2021, color = 'Indonesia', linetype = 'Cumulative Newdeaths')) +
  geom_line(data=Japan_nd_4_2021, aes(x=datetime, y=acml_nd_Jp_4_2021, color = 'Japan', linetype = 'Cumulative Newdeaths')) +
  geom_line(data=Vietnam_nd_4_2021, aes(x=datetime, y=acml_nd_Vn_4_2021, color = 'Vietnam', linetype = 'Cumulative Newdeaths')) +
  labs(title = "Cumulative Newcases and Newdeaths 4-2021", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newcases and Newdeaths 4-2021.png")

Acml_5_2021 <- ggplot() +
  geom_line(data=Indonesia_nc_5_2021, aes(x=datetime, y=acml_nc_Indo_5_2021, color = 'Indonesia', linetype = 'Cumulative Newcases')) +
  geom_line(data=Japan_nc_5_2021, aes(x=datetime, y=acml_nc_Jp_5_2021, color = 'Japan', linetype = 'Cumulative Newcases')) +
  geom_line(data=Vietnam_nc_5_2021, aes(x=datetime, y=acml_nc_Vn_5_2021, color = 'Vietnam', linetype = 'Cumulative Newcases')) +
  geom_line(data=Indonesia_nd_5_2021, aes(x=datetime, y=acml_nd_Indo_5_2021, color = 'Indonesia', linetype = 'Cumulative Newdeaths')) +
  geom_line(data=Japan_nd_5_2021, aes(x=datetime, y=acml_nd_Jp_5_2021, color = 'Japan', linetype = 'Cumulative Newdeaths')) +
  geom_line(data=Vietnam_nd_5_2021, aes(x=datetime, y=acml_nd_Vn_5_2021, color = 'Vietnam', linetype = 'Cumulative Newdeaths')) +
  labs(title = "Cumulative Newcases and Newdeaths 5-2021", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newcases and Newdeaths 5-2021.png")

Acml_1_2022 <- ggplot() +
  geom_line(data=Indonesia_nc_1_2022, aes(x=datetime, y=acml_nc_Indo_1_2022, color = 'Indonesia', linetype = 'Cumulative Newcases')) +
  geom_line(data=Japan_nc_1_2022, aes(x=datetime, y=acml_nc_Jp_1_2022, color = 'Japan', linetype = 'Cumulative Newcases')) +
  geom_line(data=Vietnam_nc_1_2022, aes(x=datetime, y=acml_nc_Vn_1_2022, color = 'Vietnam', linetype = 'Cumulative Newcases')) +
  geom_line(data=Indonesia_nd_1_2022, aes(x=datetime, y=acml_nd_Indo_1_2022, color = 'Indonesia', linetype = 'Cumulative Newdeaths')) +
  geom_line(data=Japan_nd_1_2022, aes(x=datetime, y=acml_nd_Jp_1_2022, color = 'Japan', linetype = 'Cumulative Newdeaths')) +
  geom_line(data=Vietnam_nd_1_2022, aes(x=datetime, y=acml_nd_Vn_1_2022, color = 'Vietnam', linetype = 'Cumulative Newdeaths')) +
  labs(title = "Cumulative Newcases and Newdeaths 1-2022", x = "date", y = "number of accumulation")
ggsave(file="Cumulative Newcases and Newdeaths 1-2022.png")

suppressWarnings(detach("package:readr", unload=TRUE))
suppressWarnings(detach("package:here", unload=TRUE))