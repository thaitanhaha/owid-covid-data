# assignment: discrete mathematics
# question iii

suppressMessages(library(readr))
suppressMessages(library(here))
suppressMessages(library(crayon))
setwd(here())
# startTime <- Sys.time()

assignment_iii <- function(country_code) {
  # browser() # debugging
  dataFile <- read_csv("owid-covid-data.csv", show_col_types = FALSE)
  
  # convert negative entries to positive
  dataFile$new_cases <- abs(dataFile$new_cases)
  dataFile$new_deaths <- abs(dataFile$new_deaths)
  
  # separate country from database
  dataFile_ISO <- subset(dataFile, iso_code==country_code)
  
  # separate new cases & find maximum & minimum value(s)
  dataFile_cases <- subset(dataFile_ISO, dataFile_ISO$new_cases>0)
  cases_min <- min(dataFile_cases$new_cases)
  cases_max <- max(dataFile_cases$new_cases)
  
  # separate new deaths & find maximum & minimum value(s)
  dataFile_deaths <- subset(dataFile_ISO, dataFile_ISO$new_deaths>0)
  deaths_min <- min(dataFile_deaths$new_deaths)
  deaths_max <- max(dataFile_deaths$new_deaths)
  
  # iii.1
  invalid_cases <- subset(dataFile_ISO, !(new_cases %in% dataFile_cases$new_cases) | is.na(new_cases), select = c(location, new_cases, new_deaths))
  invalid_deaths <- subset(dataFile_ISO, !(new_deaths %in% dataFile_deaths$new_deaths) | is.na(new_deaths), select = c(location, new_cases, new_deaths))
  
  # iii.2,3
  cases_min_Freq <- table(dataFile_ISO$new_cases==cases_min)
  cases_max_Freq <- table(dataFile_ISO$new_cases==cases_max)
  deaths_min_Freq <- table(dataFile_ISO$new_deaths==deaths_min)
  deaths_max_Freq <- table(dataFile_ISO$new_deaths==deaths_max)
  
  # iii.4
  colnames(invalid_cases) <- c("Countries", "Infections", "Deaths")
  colnames(invalid_deaths) <- c("Countries", "Infections", "Deaths")
  min_max_cases <- subset(dataFile_ISO, new_cases==cases_min | new_cases==cases_max, select = c(location, new_cases, new_deaths))
  min_max_deaths <- subset(dataFile_ISO, new_deaths==deaths_min | new_deaths==deaths_max, select = c(location, new_cases, new_deaths))
  colnames(min_max_cases) <- c("Countries", "Infections", "Deaths")
  colnames(min_max_deaths) <- c("Countries", "Infections", "Deaths")
  
  # iii.5,6
  condition_NA <- function(x) is.na(x)
  dataFile_cases_NA <- rle(condition_NA(dataFile_ISO$new_cases))
  suppressWarnings(cases_NA_maxFreq <- max(dataFile_cases_NA$lengths[dataFile_cases_NA$values == TRUE], na.rm = TRUE))
  suppressWarnings(cases_NA_minFreq <- min(dataFile_cases_NA$lengths[dataFile_cases_NA$values == TRUE], na.rm = TRUE))
  # checks if NA, Inf/-Inf or NaN -> resets to 0
  cases_NA_maxFreq[!is.finite(cases_NA_maxFreq)] <- 0
  cases_NA_minFreq[!is.finite(cases_NA_minFreq)] <- 0
  dataFile_deaths_NA <- rle(condition_NA(dataFile_ISO$new_deaths))
  deaths_NA_maxFreq <- max(dataFile_deaths_NA$lengths[dataFile_deaths_NA$values == TRUE], na.rm = TRUE)
  deaths_NA_minFreq <- min(dataFile_deaths_NA$lengths[dataFile_deaths_NA$values == TRUE], na.rm = TRUE)
  # checks if NA, Inf/-Inf or NaN -> resets to 0
  deaths_NA_maxFreq[!is.finite(deaths_NA_maxFreq)] <- 0
  deaths_NA_maxFreq[!is.finite(deaths_NA_maxFreq)] <- 0
  
  # iii.7,8
  condition_no_new_cases <- function(x) x==0
  dataFile_cases_zero <- rle(condition_no_new_cases(dataFile_ISO$new_cases))
  cases_zero_maxFreq <- max(dataFile_cases_zero$lengths[dataFile_cases_zero$values == TRUE], na.rm = TRUE)
  cases_zero_minFreq <- min(dataFile_cases_zero$lengths[dataFile_cases_zero$values == TRUE], na.rm = TRUE)
  # checks if NA, Inf/-Inf or NaN -> resets to 0
  cases_zero_maxFreq[!is.finite(cases_zero_maxFreq)] <- 0
  cases_zero_minFreq[!is.finite(cases_zero_minFreq)] <- 0
  # dataFile_deaths_zero <- rle(condition_no_new_cases(dataFile_ISO$new_deaths))
  # deaths_zero_maxFreq <- max(dataFile_deaths_zero$lengths[dataFile_deaths_zero$values == TRUE])
  # deaths_zero_minFreq <- min(dataFile_deaths_zero$lengths[dataFile_deaths_zero$values == TRUE])
  
  # output
  cat(inverse(unique(dataFile$location[dataFile$iso_code==country_code]), "\t\n"))
  cat("---Ca nhiem--- \n")
  cat("1. So ngay du lieu khong duoc bao cao moi:", nrow(invalid_cases), "ngay \n")                                                                              
  cat("2. So ngay co so ca nhiem thap nhat:", unname(cases_min_Freq["TRUE"]), "ngay \n")
  cat("3. So ngay co so ca nhiem cao nhat:", unname(cases_max_Freq["TRUE"]), "ngay \n")
  cat("4. \nKhong duoc bao cao moi: \n") 
  print(invalid_cases)
  cat("\nBao cao moi: \n")
  print(min_max_cases)
  cat("\n")
  cat("5. So ngay ngan nhat lien tiep khong co du lieu duoc bao cao:", cases_NA_minFreq,"ngay \n")
  cat("6. So ngay dai nhat lien tiep khong co du lieu duoc bao cao:", cases_NA_maxFreq,"ngay \n")
  cat("7. So ngay ngan nhat lien tiep khong co nguoi nhiem benh moi:", cases_zero_minFreq,"ngay \n")
  cat("8. So ngay dai nhat lien tiep khong co nguoi nhiem benh moi:", cases_zero_maxFreq,"ngay \n")
  cat("\n")
  cat("---Ca tu vong--- \n")
  cat("1. So ngay du lieu khong duoc bao cao moi:", nrow(invalid_deaths), "ngay \n")
  cat("2. So ngay co so ca tu vong thap nhat:", unname(deaths_min_Freq["TRUE"]), "ngay \n")
  cat("3. So ngay co so ca tu vong cao nhat:", unname(deaths_max_Freq["TRUE"]), "ngay \n")
  cat("4. \nKhong duoc bao cao moi:") 
  print(invalid_deaths)
  cat("\nBao cao moi: \n")
  print(min_max_deaths)
  cat("\n")
  cat("5. So ngay ngan nhat lien tiep khong co du lieu duoc bao cao:", deaths_NA_minFreq, "ngay \n")
  cat("6. So ngay dai nhat lien tiep khong co du lieu duoc bao cao:", deaths_NA_maxFreq, "ngay \n")
  cat("\n")
}

assignment_iii("IDN")
assignment_iii("JPN")
assignment_iii("VNM")

suppressWarnings(detach("package:readr", unload=TRUE))
suppressWarnings(detach("package:here", unload=TRUE))
suppressWarnings(detach("package:crayon", unload=TRUE))

# endTime <- Sys.time()
# print(endTime - startTime)