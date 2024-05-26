suppressMessages(library(here))
setwd(here())
dataFile <- read.csv("owid-covid-data.csv")

#### x.4
x4<-function(){
  data_x_4 <- aggregate(x = dataFile$new_cases, by = list(dataFile$date), FUN = sum, na.rm = TRUE)
  data_x_4 <- data_x_4[order(as.Date(data_x_4$Group.1, format="%m/%d/%Y")),]
  i=1
  k=10000
  while (i < nrow(data_x_4)) {
    if (i == 1) cat ('Ngay bat dau   ', 'Ngay ket thuc', "\n")
    temp = c()
    if (data_x_4[i,2] >= k) {
      temp = cbind(temp, toString(data_x_4[i,1]))
      while (data_x_4[i,2] >= k) {
        i = i + 1
        if (i > nrow(data_x_4)) break
      }
      temp = cbind(temp, "    ", toString(data_x_4[i-1,1]))
      cat(temp, "\n")
    }
    else i = i + 1
  }
}

#### x.5
x5<-function(){
  data_x_5 <- aggregate(x = dataFile$new_deaths, by = list(dataFile$date), FUN = sum, na.rm = TRUE)
  data_x_5 <- data_x_5[order(as.Date(data_x_5$Group.1, format="%m/%d/%Y")),]
  i=1
  k=10000
  while (i < nrow(data_x_5)) {
    if (i == 1) cat ('Ngay bat dau  ', 'Ngay ket thuc', "\n")
    temp = c()
    if (data_x_5[i,2] >= k) {
      temp = cbind(temp, toString(data_x_5[i,1]))
      while (data_x_5[i,2] >= k) {
        i = i + 1
        if (i > nrow(data_x_5)) break
      }
      temp = cbind(temp, "    ", toString(data_x_5[i-1,1]))
      cat(temp, "\n")
    }
    else i = i + 1
  }
}
x5()
suppressWarnings(detach("package:here", unload=TRUE))