suppressMessages(library(ggplot2))
suppressMessages(library(lubridate))
suppressMessages(library(dplyr))
suppressMessages(library(zoo))
suppressMessages(library(here))
setwd(here())
dir.create("viii", showWarnings = FALSE)

dataFile <- read.csv("owid-covid-data.csv")

dataFile$new_cases = abs(dataFile$new_cases)
dataFile$new_deaths = abs(dataFile$new_deaths)

setwd(here("viii"))

dataFile$date <- as.Date(dataFile$date, format = "%m/%d/%Y")

#Cau 1
data_viii <- function(year) {
  subset(dataFile, year(dataFile$date) == year & 
    (month(dataFile$date) == 1 | month(dataFile$date) == 3 | 
    month(dataFile$date) == 4 | month(dataFile$date) == 5 | 
    month(dataFile$date) == 11 | month(dataFile$date) == 12))
}

data_viii_2020 <- data_viii(2020)
data_viii_2021 <- data_viii(2021)
data_viii_2022 <- data_viii(2022)

sum_cases <- function(data) {
  aggregate(x = data$new_cases, by = list(data$date), 
    FUN = sum, na.rm = TRUE)
}

sum_cases_2020 <- sum_cases(data_viii_2020)
sum_cases_2021 <- sum_cases(data_viii_2021)
sum_cases_2022 <- sum_cases(data_viii_2022)
names(sum_cases_2020)[1] = 'Date'
names(sum_cases_2021)[1] = 'Date'
names(sum_cases_2022)[1] = 'Date'

avg_7d <- function(data){
  data %>% group_by(format.Date(Date, "%Y/%m")) %>% 
    mutate(avg_7 = rollapply(x, width=7,
    FUN=function(x) mean(na.omit(x)), 
    fill=NA, by=1, partial=TRUE, align="right"))
}

sum_cases_2020 <- avg_7d(sum_cases_2020)
sum_cases_2021 <- avg_7d(sum_cases_2021)
sum_cases_2022 <- avg_7d(sum_cases_2022)

p <- function(data.fr, mth, str){
  geom_line(data = subset(data.fr,month(data.fr$Date) == mth), 
    mapping = aes(x=day(Date), y=avg_7, color=str), size = 1)
}

p_cases_2020 <- ggplot() + 
  p(sum_cases_2020, 1 ,'January') + 
  p(sum_cases_2020, 3 ,'March') + 
  p(sum_cases_2020, 4 ,'April') + 
  p(sum_cases_2020, 5,'May') + 
  labs(title="7-day average of new cases in January, March, April, May in 2020", x = "Day", y = "Cases") + 
  scale_color_discrete(name="Month")

p_cases_2021 <- ggplot() + 
  p(sum_cases_2021, 1, 'January') + 
  p(sum_cases_2021, 3, 'March') + 
  p(sum_cases_2021, 4, 'April') + 
  p(sum_cases_2021, 5, 'May') + 
  labs(title="7-day average of new cases in January, March, April, May in 2021", x = "Day", y = "Cases") + 
  scale_color_discrete(name="Month")

p_cases_2022 <- ggplot() + p(sum_cases_2022, 1, 'January') + 
  labs(title="7-day average of new cases in January, March, April, May in 2022", x = "Day", y = "Cases") + 
  scale_color_discrete(name="Month")

p_cases_2020
p_cases_2021
p_cases_2022




#Cau 2
sum_deaths <- function(data) {
  aggregate(x = data$new_deaths, by = list(data$date), FUN = sum, na.rm = TRUE)
}

sum_deaths_2020 <- sum_deaths(data_viii_2020)
sum_deaths_2021 <- sum_deaths(data_viii_2021)
sum_deaths_2022 <- sum_deaths(data_viii_2022)
names(sum_deaths_2020)[1] = 'Date'
names(sum_deaths_2021)[1] = 'Date'
names(sum_deaths_2022)[1] = 'Date'


sum_deaths_2020 <- avg_7d(sum_deaths_2020)
sum_deaths_2021 <- avg_7d(sum_deaths_2021)
sum_deaths_2022 <- avg_7d(sum_deaths_2022)

p_deaths_2020 <- ggplot() + 
  p(sum_deaths_2020, 1, 'January') + 
  p(sum_deaths_2020, 3, 'March') + p(sum_deaths_2020, 4, 'April') + 
  p(sum_deaths_2020, 5, 'May') + 
  labs(title="7-day average of new deaths in January, March, April, May in 2020", x = "Day", y = "Cases") + 
  scale_color_discrete(name="Month")

p_deaths_2021 <- ggplot() + 
  p(sum_deaths_2021, 1, 'January') + 
  p(sum_deaths_2021, 3, 'March') + p(sum_deaths_2021, 4, 'April') + 
  p(sum_deaths_2021, 5, 'May') + 
  labs(title="7-day average of new deaths in January, March, April, May in 2021", x = "Day", y = "Cases") + 
  scale_color_discrete(name="Month")

p_deaths_2022 <- ggplot() + 
  p(sum_deaths_2022, 1, 'January') + 
  labs(title="7-day average of new deaths in January, March, April, May in 2022", x = "Day", y = "Cases") + 
  scale_color_discrete(name="Month")

p_deaths_2020
p_deaths_2021
p_deaths_2022




#Cau 3
p_2last_cases_2020 <- ggplot() + 
  p(sum_cases_2020, 11, 'November') + 
  p(sum_cases_2020, 12, 'December') + 
  labs(title="7-day average of new cases in the last 2 months in 2020", x = "Day", y = "Cases") + 
  scale_color_discrete(name="Month")

p_2last_cases_2021 <- ggplot() + 
  p(sum_cases_2021, 11, 'November') + 
  p(sum_cases_2021, 12, 'December') + 
  labs(title="7-day average of new cases in the last 2 months in 2021", x = "Day", y = "Cases") + 
  scale_color_discrete(name="Month")

p_2last_cases_2020
p_2last_cases_2021






#Cau 4
p_2last_deaths_2020 <- ggplot() + 
  p(sum_deaths_2020, 11, 'November') + 
  p(sum_deaths_2020, 12, 'December') + 
  labs(title="7-day average of new deaths in the last 2 months in 2020", x = "Day", y = "Cases")  + 
  scale_color_discrete(name="Month")

p_2last_deaths_2021 <- ggplot() + 
  p(sum_deaths_2021, 11, 'November') + 
  p(sum_deaths_2021, 12, 'December') + 
  labs(title="7-day average of new deaths in the last 2 months in 2021", x = "Day", y = "Cases")  + 
  scale_color_discrete(name="Month")

p_2last_deaths_2020
p_2last_deaths_2021




#Cau 5
sum_cases_2020 <- sum_cases_2020 %>% mutate(cummulative = cumsum(avg_7))
sum_cases_2021 <- sum_cases_2021 %>% mutate(cummulative = cumsum(avg_7))

p_cum <- function(data.fr, mth, str){
  geom_line(data = subset(data.fr, month(data.fr$Date) == mth), mapping = aes(x=day(Date), y=cummulative, color = str), size = 1)
}

p_cum_cases_2020 <- ggplot() + 
  p_cum(sum_cases_2020, 11, 'November') + 
  p_cum(sum_cases_2020, 12, 'December') + 
  labs(title="Cummulative sum of 7-day of new cases in the last 2 months in 2020", x = "Day", y = "Cases") + 
  scale_color_discrete(name="Month")

p_cum_cases_2021 <- ggplot() + 
  p_cum(sum_cases_2021, 11, 'November') + 
  p_cum(sum_cases_2021, 12, 'December') + 
  labs(title="Cummulative sum of 7-day of new cases in the last 2 months in 2021", x = "Day", y = "Cases") + 
  scale_color_discrete(name="Month")

p_cum_cases_2020
p_cum_cases_2021





#Cau 6
sum_deaths_2020 <- sum_deaths_2020 %>% mutate(cummulative = cumsum(avg_7))
sum_deaths_2021 <- sum_deaths_2021 %>% mutate(cummulative = cumsum(avg_7))

p_cum_deaths_2020 <- ggplot() + p_cum(sum_deaths_2020, 11, 'November') + p_cum(sum_deaths_2020, 12, 'December') + 
  labs(title="Cummulative sum of 7-day of new deaths in the last 2 months in 2020", x = "Day", y = "Cases") + 
  scale_color_discrete(name="Month")
p_cum_deaths_2021 <- ggplot() + p_cum(sum_deaths_2021, 11, 'November') + p_cum(sum_deaths_2021, 12, 'December') + 
  labs(title="Cummulative sum of 7-day of new deaths in the last 2 months in 2021", x = "Day", y = "Cases") + 
  scale_color_discrete(name="Month")

p_cum_deaths_2020
p_cum_deaths_2021



ggsave("p_cases_2020.png", plot = p_cases_2020)
ggsave("p_cases_2021.png", plot = p_cases_2021)
ggsave("p_cases_2022.png", plot = p_cases_2022)

ggsave("p_deaths_2020.png", plot = p_deaths_2020)
ggsave("p_deaths_2021.png", plot = p_deaths_2021)
ggsave("p_deaths_2022.png", plot = p_deaths_2022)

ggsave("p_2last_cases_2020.png", plot = p_2last_cases_2020)
ggsave("p_2last_cases_2021.png", plot = p_2last_cases_2021)

ggsave("p_2last_deaths_2020.png", plot = p_2last_deaths_2020)
ggsave("p_2last_deaths_2021.png", plot = p_2last_deaths_2021)

ggsave("p_cum_cases_2020.png", plot = p_cum_cases_2020)
ggsave("p_cum_cases_2021.png", plot = p_cum_cases_2021)

ggsave("p_cum_deaths_2020.png", plot = p_cum_deaths_2020)
ggsave("p_cum_deaths_2021.png", plot = p_cum_deaths_2021)

suppressWarnings(detach("package:ggplot2", unload=TRUE))
suppressWarnings(detach("package:lubridate", unload=TRUE))
suppressWarnings(detach("package:dplyr", unload=TRUE))
suppressWarnings(detach("package:zoo", unload=TRUE))
suppressWarnings(detach("package:here", unload=TRUE))