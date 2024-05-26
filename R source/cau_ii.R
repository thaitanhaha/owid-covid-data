suppressMessages(library(here))
setwd(here())
dir.create("ii", showWarnings = FALSE)

dataFile <- read.csv("owid-covid-data.csv")

dataFile$new_cases = abs(dataFile$new_cases)
dataFile$new_deaths = abs(dataFile$new_deaths)

# change working directory
setwd(here("ii"))


indoFile <- subset(dataFile, dataFile$location == "Indonesia")
japanFile <- subset(dataFile, dataFile$location == "Japan")
vietnamFile <- subset(dataFile, dataFile$location == "Vietnam")

ii_File <- list(indoFile, japanFile, vietnamFile)
ii_string <- cbind("Indonesia", "Japan", "Vietnam")



#MIN MAX
cases_min <- vector(length = 3)
cases_max <- vector(length = 3)
deaths_min <- vector(length = 3)
deaths_max <- vector(length = 3)

for (i in 1:3) {
  cases_min[i] = min(na.omit(data.frame(ii_File[i])$new_cases))
  cases_max[i] = max(na.omit(data.frame(ii_File[i])$new_cases))
  deaths_min[i] = min(na.omit(data.frame(ii_File[i])$new_deaths))
  deaths_max[i] = max(na.omit(data.frame(ii_File[i])$new_deaths))
  cat(ii_string[i], "min new cases =", cases_min[i], "\n")
  cat(ii_string[i], "max new cases =", cases_max[i], "\n")
  cat(ii_string[i], "min new deaths =", deaths_min[i], "\n")
  cat(ii_string[i], "max new deaths =", deaths_max[i], "\n", "\n")
}


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
  
  cat(ii_string[i], "Q1 new cases =", cases_Q1[i], "\n")
  cat(ii_string[i], "Q3 new cases =", cases_Q3[i], "\n")
  cat(ii_string[i], "Q1 new deaths =", deaths_Q1[i], "\n")
  cat(ii_string[i], "Q3 new deaths =", deaths_Q3[i], "\n", "\n")
}



#AVG
cases_avg <- vector(length = 3)
deaths_avg <- vector(length = 3)

for (i in 1:3) {
  cases_avg[i] = mean(na.omit(data.frame(ii_File[i])$new_cases))
  deaths_avg[i] = mean(na.omit(data.frame(ii_File[i])$new_deaths))
  cat(ii_string[i], "average new cases =", cases_avg[i], "\n")
  cat(ii_string[i], "average new deaths =", deaths_avg[i], "\n", "\n")
}


#STD
cases_std <- vector(length = 3)
deaths_std <- vector(length = 3)

for (i in 1:3) {
  cases_std[i] = sd(na.omit(data.frame(ii_File[i])$new_cases))
  deaths_std[i] = sd(na.omit(data.frame(ii_File[i])$new_deaths))
  cat(ii_string[i], "standard deviation new cases =", cases_std[i], "\n")
  cat(ii_string[i], "standard deviation new deaths =", deaths_std[i], "\n", "\n")
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
  
  cat(ii_string[i], "outliers new cases =", cases_outlier[i], "\n")
  cat(ii_string[i], "outliers new deaths =", deaths_outlier[i], "\n", "\n")
}




#TABLE
cases_table <- vector()
for (i in 1:3) {
  cases_table = rbind(cases_table, cbind("Countries" = ii_string[i], "Min"=cases_min[i], "Q1"=cases_Q1[i], 
                            "Q2"=cases_Q2[i], "Q3"=cases_Q3[i], "Max"=cases_max[i], 
                            "Avg"=cases_avg[i], "Std"=cases_std[i], "Outlier"=cases_outlier[i]))
}

View(cases_table)

deaths_table <- vector()
for (i in 1:3) {
  deaths_table = rbind(deaths_table, cbind("Countries" = ii_string[i], "Min"=deaths_min[i], "Q1"=deaths_Q1[i], 
                                         "Q2"=deaths_Q2[i], "Q3"=deaths_Q3[i], "Max"=deaths_max[i], 
                                         "Avg"=deaths_avg[i], "Std"=deaths_std[i], "Outlier"=deaths_outlier[i]))
}

View(deaths_table)


#BOXPLOT
for (i in 1:3) {
  png(file = paste("boxplot_cases_",ii_string[i],".png"))
  boxplot(data.frame(ii_File[i])$new_cases, main=paste(ii_string[i], "new_cases boxplot"))
  dev.off()
  
  png(file = paste("boxplot_deaths_",ii_string[i],".png"))
  boxplot(data.frame(ii_File[i])$new_deaths, main=paste(ii_string[i], "new_deaths boxplot"))
  dev.off()
}

suppressWarnings(detach("package:here", unload=TRUE))

