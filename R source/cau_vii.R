# 4315
# IDN // Indonesia
# JPN // Japan
# VNM // Vietnam

suppressMessages(library(here))
setwd(here())
dir.create("vii", showWarnings = FALSE)

dataFile <- read.csv("owid-covid-data.csv")
dataFile$date <- strptime(dataFile$date, format="%m/%d/%Y")

# change working directory
setwd(here("vii"))

# dataFile


data_newcases <- c(rep(0,times=12))
data_newcases[1] <- sum(dataFile[which(dataFile$new_cases>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2020"&
									format(dataFile$date,"%m")=="01"),5])
data_newcases[2] <- sum(dataFile[which(dataFile$new_cases>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2020"&
									format(dataFile$date,"%m")=="03"),5])
data_newcases[3] <- sum(dataFile[which(dataFile$new_cases>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2020"&
									format(dataFile$date,"%m")=="04"),5])
data_newcases[4] <- sum(dataFile[which(dataFile$new_cases>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2020"&
									format(dataFile$date,"%m")=="05"),5])
data_newcases[5] <- sum(dataFile[which(dataFile$new_cases>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2021"&
									format(dataFile$date,"%m")=="01"),5])
data_newcases[6] <- sum(dataFile[which(dataFile$new_cases>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2021"&
									format(dataFile$date,"%m")=="03"),5])
data_newcases[7] <- sum(dataFile[which(dataFile$new_cases>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2021"&
									format(dataFile$date,"%m")=="04"),5])
data_newcases[8] <- sum(dataFile[which(dataFile$new_cases>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2021"&
									format(dataFile$date,"%m")=="05"),5])
data_newcases[9] <- sum(dataFile[which(dataFile$new_cases>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2022"&
									format(dataFile$date,"%m")=="01"),5])
data_newcases[10] <- sum(dataFile[which(dataFile$new_cases>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2022"&
									format(dataFile$date,"%m")=="03"),5])
data_newcases[11] <- sum(dataFile[which(dataFile$new_cases>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2022"&
									format(dataFile$date,"%m")=="04"),5])
data_newcases[12] <- sum(dataFile[which(dataFile$new_cases>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2022"&
									format(dataFile$date,"%m")=="05"),5])

png(file = "newcase.png",width=1000)
barplot(data_newcases,
	main="NEW CASES ALL OVER THE WORLD",
	# beside=TRUE,
	col="tomato",
	names.arg=c("01/2020","03/2020","04/2020","05/2020","01/2021","03/2021","04/2021",
				"05/2021","01/2022","03/2022","04/2022","05/2022"),
	ylab="Cases",
	xlab="Year",
	)
dev.off()


data_newdeath <- c(rep(0,times=12))
data_newdeath[1] <- sum(dataFile[which(dataFile$new_deaths>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2020"&
									format(dataFile$date,"%m")=="01"),5])
data_newdeath[2] <- sum(dataFile[which(dataFile$new_deaths>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2020"&
									format(dataFile$date,"%m")=="03"),5])
data_newdeath[3] <- sum(dataFile[which(dataFile$new_deaths>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2020"&
									format(dataFile$date,"%m")=="04"),5])
data_newdeath[4] <- sum(dataFile[which(dataFile$new_deaths>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2020"&
									format(dataFile$date,"%m")=="05"),5])
data_newdeath[5] <- sum(dataFile[which(dataFile$new_deaths>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2021"&
									format(dataFile$date,"%m")=="01"),5])
data_newdeath[6] <- sum(dataFile[which(dataFile$new_deaths>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2021"&
									format(dataFile$date,"%m")=="03"),5])
data_newdeath[7] <- sum(dataFile[which(dataFile$new_deaths>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2021"&
									format(dataFile$date,"%m")=="04"),5])
data_newdeath[8] <- sum(dataFile[which(dataFile$new_deaths>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2021"&
									format(dataFile$date,"%m")=="05"),5])
data_newdeath[9] <- sum(dataFile[which(dataFile$new_deaths>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2022"&
									format(dataFile$date,"%m")=="01"),5])
data_newdeath[10] <- sum(dataFile[which(dataFile$new_deaths>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2022"&
									format(dataFile$date,"%m")=="03"),5])
data_newdeath[11] <- sum(dataFile[which(dataFile$new_deaths>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2022"&
									format(dataFile$date,"%m")=="04"),5])
data_newdeath[12] <- sum(dataFile[which(dataFile$new_deaths>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2022"&
									format(dataFile$date,"%m")=="05"),5])

png(file = "deathcase.png",width=1000)
barplot(data_newdeath,
	main="NEW DEATHS CASES ALL OVER THE WORLD",
	# beside=TRUE,
	col="tomato",
	names.arg=c("01/2020","03/2020","04/2020","05/2020","01/2021","03/2021","04/2021",
				"05/2021","01/2022","03/2022","04/2022","05/2022"),
	ylab="Case",
	xlab="Year",
	)
dev.off()



data_newcases_2months_2020 <- sum(dataFile[which(dataFile$new_cases>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2020"&
									(format(dataFile$date,"%m")=="12"|
									format(dataFile$date,"%m")=="11")),5]) 
data_newcases_2months_2021 <- sum(dataFile[which(dataFile$new_cases>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2021"&
									(format(dataFile$date,"%m")=="12"|
									format(dataFile$date,"%m")=="11")),5])
data_newcases_2months_2022 <- sum(dataFile[which(dataFile$new_cases>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2022"&
									(format(dataFile$date,"%m")=="12"|
									format(dataFile$date,"%m")=="11")),5])
data_newcases_2months <- c(data_newcases_2months_2020,data_newcases_2months_2021,data_newcases_2months_2022)
png(file="data_newcases_2months.png")
barplot(data_newcases_2months,
	main="NEW CASES ALL OVER THE WORLD IN LAST 2 MONTHS",
	col="tomato",
	ylab="Cases",
	xlab="Year",
	names.arg=c("2020","2021","2022"),
	)
dev.off()


data_deathcases_2months_2020 <- sum(dataFile[which(dataFile$new_deaths>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2020"&
									(format(dataFile$date,"%m")=="12"|
									format(dataFile$date,"%m")=="11")),6]) 
data_deathcases_2months_2021 <- sum(dataFile[which(dataFile$new_deaths>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2021"&
									(format(dataFile$date,"%m")=="12"|
									format(dataFile$date,"%m")=="11")),6])
data_deathcases_2months_2022 <- sum(dataFile[which(dataFile$new_deaths>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2022"&
									(format(dataFile$date,"%m")=="12"|
									format(dataFile$date,"%m")=="11")),6])
data_deathcases_2months <- c(data_deathcases_2months_2020,data_deathcases_2months_2021,data_deathcases_2months_2022)
png(file="data_deathcases_2months.png")
barplot(data_deathcases_2months,
	main="NEW DEATH CASES ALL OVER THE WORLD IN LAST 2 MONTHS",
	col="tomato",
	ylab="Cases",
	xlab="Year",
	names.arg=c("2020","2021","2022"),
	)
dev.off()


data_newcases_20 <- c(0,0)
data_newcases_20[1] <- sum(dataFile[which(dataFile$new_cases>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2020"&
									format(dataFile$date,"%m")=="11"),5]) 
data_newcases_20[2] <- sum(dataFile[which(dataFile$new_cases>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2020"&
									(format(dataFile$date,"%m")=="12"|
									format(dataFile$date,"%m")=="11")),5]) 
data_newcases_21 <- c(0,0)
data_newcases_21[1] <- sum(dataFile[which(dataFile$new_cases>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2021"&
									format(dataFile$date,"%m")=="11"),5]) 
data_newcases_21[2] <- sum(dataFile[which(dataFile$new_cases>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2021"&
									(format(dataFile$date,"%m")=="12"|
									format(dataFile$date,"%m")=="11")),5]) 

data_newcases_22 <- c(0,0)
data_newcases_22[1] <- sum(dataFile[which(dataFile$new_cases>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2022"&
									format(dataFile$date,"%m")=="11"),5]) 
data_newcases_22[2] <- sum(dataFile[which(dataFile$new_cases>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2022"&
									(format(dataFile$date,"%m")=="12"|
									format(dataFile$date,"%m")=="11")),5])  

data_newcase_multi <- data.frame(data_newcases_20,data_newcases_21,data_newcases_22)

png(file="data_multi.png")
barplot(as.matrix(data_newcase_multi),
		main="DATA NEW CASE MULTI",
		ylab="Cases",
		xlab="Year",
		beside=TRUE,
		col=c("tomato","steelblue2"),
		legend.text=c("November","December"),
		args.legend=list(x="topright"),
		names.arg=c("2020","2021","2022"),)
dev.off()


data_deathscases_20 <- c(0,0)
data_deathscases_20[1] <- sum(dataFile[which(dataFile$new_deaths>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2020"&
									format(dataFile$date,"%m")=="11"),6]) 
data_deathscases_20[2] <- sum(dataFile[which(dataFile$new_deaths>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2020"&
									(format(dataFile$date,"%m")=="12"|
									format(dataFile$date,"%m")=="11")),6]) 
data_deathscases_21 <- c(0,0)
data_deathscases_21[1] <- sum(dataFile[which(dataFile$new_deaths>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2021"&
									format(dataFile$date,"%m")=="11"),6]) 
data_deathscases_21[2] <- sum(dataFile[which(dataFile$new_deaths>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2021"&
									(format(dataFile$date,"%m")=="12"|
									format(dataFile$date,"%m")=="11")),6]) 

data_deathscases_22 <- c(0,0)
data_deathscases_22[1] <- sum(dataFile[which(dataFile$new_deaths>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2022"&
									format(dataFile$date,"%m")=="11"),6]) 
data_deathscases_22[2] <- sum(dataFile[which(dataFile$new_deaths>0&
									dataFile$iso_code!="OWID_WRL"&
									format(dataFile$date,"%Y")=="2022"&
									(format(dataFile$date,"%m")=="12"|
									format(dataFile$date,"%m")=="11")),6])  

data_deathscase_multi <- data.frame(data_deathscases_20,data_deathscases_21,data_deathscases_22)

png(file="data_multi_deaths.png")
barplot(as.matrix(data_deathscase_multi),
		main="DATA DEATHS CASE MULTI",
		ylab="Cases",
		xlab="Year",
		beside=TRUE,
		col=c("tomato","steelblue2"),
		legend.text=c("November","December"),
		args.legend=list(x="topright"),
		names.arg=c("2020","2021","2022"),)
dev.off()
suppressWarnings(detach("package:here", unload=TRUE))
