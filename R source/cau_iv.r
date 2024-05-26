suppressMessages(library(readr))
suppressMessages(library(magrittr))
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(lubridate))
suppressMessages(library(ggplot2))
here::here("data", "df.rda")
setwd(here::here())
dir.create("iv", showWarnings = FALSE)
dataFile <- read.csv("owid-covid-data.csv")
dataFile <- dataFile %>% filter(nchar(as.character(continent))>0)

# change working directory
setwd(here::here("iv"))

########4.1 && 4.2
Countries <- dataFile %>% select(location)
Con <- dataFile %>% select(continent)
temp<- cbind(Countries,Con)
temp <- distinct(temp)
Countries <- temp %>% group_by(continent) %>% summarise(numCountry = length(continent))
probability <- prop.table(Countries[,2])
cumulative <- cumsum(Countries[,2])
Countries <- cbind(Countries, probability,cumulative)
colnames(Countries)<-c("continent","numCountry","probability","cumulative")

########4.1

graph1 <- ggplot(data = Countries, aes(x=continent, y=cumulative)) +
geom_bar(stat = "identity", position = "dodge", fill = "steelblue") +
labs(title = "",x="Continent", y="Cumulative frequence") 

graph1
ggsave("iv.1) Cumulative frequence.png", plot = graph1)
########4.2

graph2 <- ggplot(data = Countries, aes(x=continent, y=probability)) +
  geom_bar(stat = "identity", position = "dodge", fill = "steelblue") +
  labs(title = "",x="Continent", y="Relavtive frequence")

graph2
ggsave("iv.2) Relavtive frequence.png", plot = graph2)


#########4.3&&4.4
dataFile$new_cases <- abs(dataFile$new_cases)
dataFile$new_deaths <- abs(dataFile$new_deaths)
InJaVi <- dataFile %>% filter(location == "Indonesia" | location == "Japan" | location == "Vietnam")
InJaVi <- InJaVi %>% select(location | date | new_cases | new_deaths)

tmp <- InJaVi
formatedDate <- as.Date(tmp$date, format = "%m/%d/%Y")
tmp[,"date"] <-formatedDate

thelastday <- max(formatedDate)
thelastsevendays <- tmp %>% group_by(location) %>% filter(date > thelastday - 7)

##########4.3
graph3 <- ggplot(data = thelastsevendays, aes(x = date, y = new_cases, fill = factor(location))) +
  theme_bw() +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "New cases for the last 7 days", x = "Date", y = "New Cases") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_fill_manual("Location", values = c("Indonesia" = "black", "Japan" = "red", "Vietnam" = "blue"))

graph3
suppressWarnings(ggsave("iv.3) New cases for the last 7 days.png", plot = graph3))

##########4.4
graph4 <- ggplot(data = thelastsevendays, aes(x = date, y = new_deaths,fill = factor(location))) +
  theme_bw() + geom_bar(stat = "identity", position = "dodge") +
  labs(title = "New deaths for the last 7 days", x = "Date", y = "New Deaths") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_fill_manual("Location", values = c("Indonesia" = "black", "Japan" = "red", "Vietnam" = "blue"))

graph4
ggsave("iv.4) New deaths for the last 7 days.png", plot = graph4)

#########4.5&&4.6

######This is the start of ii.5
indoFile <- subset(dataFile, dataFile$location == "Indonesia")
japanFile <- subset(dataFile, dataFile$location == "Japan")
vietnamFile <- subset(dataFile, dataFile$location == "Vietnam")

ii_File <- list(indoFile, japanFile, vietnamFile)
ii_string <- cbind("Indonesia", "Japan", "Vietnam")

#QUANTILE
cases_Q1 <- vector(length = 3)
cases_Q2 <- vector(length = 3)
cases_Q3 <- vector(length = 3)

deaths_Q1 <- vector(length = 3)
deaths_Q2 <- vector(length = 3)
deaths_Q3 <- vector(length = 3)

for (i in 1:3) {
  cases_Q1[i] = unname(quantile(na.omit(data.frame(ii_File[i])$new_cases))[2])
  cases_Q2[i] = unname(quantile(na.omit(data.frame(ii_File[i])$new_cases))[3])
  cases_Q3[i] = unname(quantile(na.omit(data.frame(ii_File[i])$new_cases))[4])
  
  deaths_Q1[i] = unname(quantile(na.omit(data.frame(ii_File[i])$new_deaths))[2])
  deaths_Q2[i] = unname(quantile(na.omit(data.frame(ii_File[i])$new_deaths))[3])
  deaths_Q3[i] = unname(quantile(na.omit(data.frame(ii_File[i])$new_deaths))[4])
}

#OUTLIERS
cases_outlier <- vector(length = 3)
deaths_outlier <- vector(length = 3)

for (i in 1:3) {
  cases_IQR = cases_Q3[i] - cases_Q1[1] 
  cases_outlier[i] = nrow(subset(data.frame(ii_File[i]), new_cases < cases_Q1[i] - 1.5*cases_IQR |
                                   new_cases > cases_Q3[i] + 1.5*cases_IQR))
  
  deaths_IQR = deaths_Q3[i] - deaths_Q1[1]
  deaths_outlier[i] = nrow(subset(data.frame(ii_File[i]), new_deaths < deaths_Q1[i] - 1.5*deaths_IQR |
                                    new_deaths > deaths_Q3[i] + 1.5*deaths_IQR))
  
}

################this is the end of ii.5


###this is what I have to do when got the ii.5-data
datafromii.5 <- cbind(c("Indonesia","Japan","Vietnam"),as.data.frame(cases_outlier),as.data.frame(deaths_outlier))
colnames(datafromii.5) <- c("Country","casesOutliers","deathsOutliers")

##########4.5
graph5 <- ggplot(data = datafromii.5, aes(x=Country, y=casesOutliers, fill = factor(Country))) + 
  geom_bar(stat="identity") +
  theme_bw() +
  labs(title = "Number of infection outliers of each country") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_fill_manual("Country", values = c("Indonesia" = "black", "Japan" = "red", "Vietnam" = "blue"))

graph5
ggsave("iv.5) caseOutPlot.png", plot = graph5)

##########4.6
graph6 <- ggplot(data = datafromii.5, aes(x=Country, y=deathsOutliers, fill = factor(Country))) + 
  geom_bar(stat="identity") +
  theme_bw() +
  labs(title = "Number of deaths outliers of each country") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_fill_manual("Country", values = c("Indonesia" = "black", "Japan" = "red", "Vietnam" = "blue"))

graph6
ggsave("iv.6) deathOutPlot.png", plot = graph6)

suppressWarnings(detach("package:readr", unload=TRUE))
suppressWarnings(detach("package:magrittr", unload=TRUE))
suppressWarnings(detach("package:plyr", unload=TRUE))
suppressWarnings(detach("package:dplyr", unload=TRUE))
suppressWarnings(detach("package:lubridate", unload=TRUE))
suppressWarnings(detach("package:ggplot2", unload=TRUE))