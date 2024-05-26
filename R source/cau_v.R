# assignment: discrete mathematics
# question iii
# 4 3 1 5 

suppressMessages(library(readr))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(here))
suppressMessages(library(scales))

# startTime <- Sys.time()
country_code="IDN"
assignment_v <- function(country_code) {
  setwd(here())
  dir.create("v", showWarnings = FALSE)
  
  dataFile <- read_csv("owid-covid-data.csv", show_col_types = FALSE)
  country_name <- unique(dataFile$location[dataFile$iso_code==country_code])
  
  # change working directory
  setwd(here("v"))
  
  # convert negative entries to positive
  dataFile$new_cases <- abs(dataFile$new_cases)
  dataFile$new_deaths <- abs(dataFile$new_deaths)
  
  # convert date to machine-readable format
  dataFile$date <- as.Date(dataFile$date, format="%m/%d/%Y")
  
  # separate country from database
  dataFile_ISO <- subset(dataFile, iso_code==country_code)
  
  #### New cases ####
  # 2020 
  dataFile_cases <- subset(dataFile_ISO, new_cases>0 &
                    ((format(date,"%m")=="01") | (format(date,"%m")=="03") |
                    (format(date,"%m")=="04") | (format(date,"%m")=="05")) & 
                    format(date,"%Y")=="2020",
                    select = c(date, new_cases))
  dataFile_cases <- dataFile_cases %>% mutate(month = as.numeric(format(dataFile_cases$date,"%m"))) %>%
                                       mutate(month = month.name[month])
  dataFile_cases$month = factor(dataFile_cases$month, levels = c("January", "March", "April", "May"))
  dataFile_cases_plot <- ggplot(data = dataFile_cases, mapping = aes(x = date, y = new_cases, label = new_cases)) +
                        geom_line() + geom_point() + 
                        facet_grid(~ dataFile_cases$month, scales = "free_x", drop = FALSE) +
                        labs(x = "",
                             y = "Number of new cases",
                             title = paste("New COVID-19 cases in",country_name),
                             subtitle = "Number of newly reported COVID-19 cases by date in January, March, April and May of 2020") +
                        theme_bw() + theme(text = element_text(size = 14)) +
                        theme(plot.title = element_text(face = "bold")) +
                        theme(plot.subtitle = element_text(face = "italic")) +
                        theme(axis.text.x = element_text(angle = 0, size = 9)) +
                        theme(plot.margin = margin(1,1.2,0.5,1, "cm")) +
                        theme(panel.spacing.x = unit(4, "mm")) +
                        scale_y_continuous(labels = label_number())
  ggsave(dataFile_cases_plot, filename = paste(country_code,"2020_new_cases.pdf",sep="_"), width = 12, height = 6)
  
  # 2021
  dataFile_cases <- subset(dataFile_ISO, new_cases>0 &
                    ((format(date,"%m")=="01") | (format(date,"%m")=="03") |
                    (format(date,"%m")=="04") | (format(date,"%m")=="05")) & 
                    format(date,"%Y")=="2021",
                    select = c(date, new_cases))
  dataFile_cases <- dataFile_cases %>% mutate(month = as.numeric(format(dataFile_cases$date,"%m"))) %>%
                                       mutate(month = month.name[month])
  dataFile_cases$month = factor(dataFile_cases$month, levels = c("January", "March", "April", "May"))
  dataFile_cases_plot <- ggplot(data = dataFile_cases, mapping = aes(x = date, y = new_cases, label = new_cases)) +
                        geom_line() + geom_point() + 
                        facet_grid(~ dataFile_cases$month, scales = "free_x", drop = FALSE) +
                        labs(x = "",
                             y = "Number of new cases",
                             title = paste("New COVID-19 cases in",country_name),
                             subtitle = "Number of newly reported COVID-19 cases by date in January, March, April and May of 2021") +
                        theme_bw() + theme(text = element_text(size = 14)) +
                        theme(plot.title = element_text(face = "bold")) +
                        theme(plot.subtitle = element_text(face = "italic")) +
                        theme(axis.text.x = element_text(angle = 0, size = 9)) +
                        theme(plot.margin = margin(1,1.2,0.5,1, "cm")) +
                        theme(panel.spacing.x = unit(4, "mm")) +
                        scale_y_continuous(labels = label_number())
  ggsave(dataFile_cases_plot, filename = paste(country_code,"2021_new_cases.pdf",sep="_"), width = 12, height = 6)
  
  # 2022
  dataFile_cases <- subset(dataFile_ISO, new_cases>0 &
                    format(date,"%m")=="01" &
                    format(date,"%Y")=="2022",
                    select = c(date, new_cases))
  dataFile_cases_plot <- ggplot(data = dataFile_cases, mapping = aes(x = date, y = new_cases, label = new_cases)) +
                        geom_line() + geom_point() + 
                        labs(x = "",
                             y = "Number of new cases",
                             title = paste("New COVID-19 cases in",country_name),
                             subtitle = "Number of newly reported COVID-19 cases by date in January of 2022") +
                        theme_bw() + theme(text = element_text(size = 14)) +
                        theme(plot.title = element_text(face = "bold")) +
                        theme(plot.subtitle = element_text(face = "italic")) +
                        theme(axis.text.x = element_text(angle = 0, size = 9)) +
                        theme(plot.margin = margin(1,1.2,0.5,1, "cm")) +
                        theme(panel.spacing.x = unit(4, "mm")) +
                        scale_y_continuous(labels = label_number())
  ggsave(dataFile_cases_plot, filename = paste(country_code,"2022_new_cases.pdf",sep="_"), width = 12, height = 6)
  #### end ####
  
  #### New deaths ####
  # 2020 
  dataFile_deaths <- subset(dataFile_ISO, new_deaths>0 &
                    ((format(date,"%m")=="01") | (format(date,"%m")=="03") |
                    (format(date,"%m")=="04") | (format(date,"%m")=="05")) & 
                    format(date,"%Y")=="2020",
                    select = c(date, new_deaths))
  dataFile_deaths <- dataFile_deaths %>% mutate(month = as.numeric(format(dataFile_deaths$date,"%m"))) %>%
                                       mutate(month = month.name[month])
  dataFile_deaths$month = factor(dataFile_deaths$month, levels = c("January", "March", "April", "May"))
  dataFile_deaths_plot <- ggplot(data = dataFile_deaths, mapping = aes(x = date, y = new_deaths, label = new_deaths)) +
                        geom_line() + geom_point() + 
                        facet_grid(~ dataFile_deaths$month, scales = "free_x", drop = FALSE) +
                        labs(x = "",
                             y = "Number of new deaths",
                             title = paste("New COVID-19 deaths in",country_name),
                             subtitle = "Number of newly reported COVID-19 deaths by date in January, March, April and May of 2020") +
                        theme_bw() + theme(text = element_text(size = 14)) +
                        theme(plot.title = element_text(face = "bold")) +
                        theme(plot.subtitle = element_text(face = "italic")) +
                        theme(axis.text.x = element_text(angle = 0, size = 9)) +
                        theme(plot.margin = margin(1,1.2,0.5,1, "cm")) +
                        theme(panel.spacing.x = unit(4, "mm")) +
                        scale_y_continuous(labels = label_number())
  ggsave(dataFile_deaths_plot, filename = paste(country_code,"2020_new_deaths.pdf",sep="_"), width = 12, height = 6)
  
  # 2021
  dataFile_deaths <- subset(dataFile_ISO, new_deaths>0 &
                    ((format(date,"%m")=="01") | (format(date,"%m")=="03") |
                    (format(date,"%m")=="04") | (format(date,"%m")=="05")) & 
                    format(date,"%Y")=="2021",
                    select = c(date, new_deaths))
  dataFile_deaths <- dataFile_deaths %>% mutate(month = as.numeric(format(dataFile_deaths$date,"%m"))) %>%
                                       mutate(month = month.name[month])
  dataFile_deaths$month = factor(dataFile_deaths$month, levels = c("January", "March", "April", "May"))
  dataFile_deaths_plot <- ggplot(data = dataFile_deaths, mapping = aes(x = date, y = new_deaths, label = new_deaths)) +
                        geom_line() + geom_point() + 
                        facet_grid(~ dataFile_deaths$month, scales = "free_x", drop = FALSE) +
                        labs(x = "",
                             y = "Number of new deaths",
                             title = paste("New COVID-19 deaths in",country_name),
                             subtitle = "Number of newly reported COVID-19 deaths by date in January, March, April and May of 2021") +
                        theme_bw() + theme(text = element_text(size = 14)) +
                        theme(plot.title = element_text(face = "bold")) +
                        theme(plot.subtitle = element_text(face = "italic")) +
                        theme(axis.text.x = element_text(angle = 0, size = 9)) +
                        theme(plot.margin = margin(1,1.2,0.5,1, "cm")) +
                        theme(panel.spacing.x = unit(4, "mm")) +
                        scale_y_continuous(labels = label_number())
  ggsave(dataFile_deaths_plot, filename = paste(country_code,"2021_new_deaths.pdf",sep="_"), width = 12, height = 6)
  
  # 2022
  dataFile_deaths <- subset(dataFile_ISO, new_deaths>0 &
                    format(date,"%m")=="01" &
                    format(date,"%Y")=="2022",
                    select = c(date, new_deaths))
  dataFile_deaths_plot <- ggplot(data = dataFile_deaths, mapping = aes(x = date, y = new_deaths, label = new_deaths)) +
                        geom_line() + geom_point() + 
                        labs(x = "",
                             y = "Number of new deaths",
                             title = paste("New COVID-19 deaths in",country_name),
                             subtitle = "Number of newly reported COVID-19 deaths by date in January of 2022") +
                        theme_bw() + theme(text = element_text(size = 14)) +
                        theme(plot.title = element_text(face = "bold")) +
                        theme(plot.subtitle = element_text(face = "italic")) +
                        theme(axis.text.x = element_text(angle = 0, size = 9)) +
                        theme(plot.margin = margin(1,1.2,0.5,1, "cm")) +
                        theme(panel.spacing.x = unit(4, "mm")) +
                        scale_y_continuous(labels = label_number())
  ggsave(dataFile_deaths_plot, filename = paste(country_code,"2022_new_deaths.pdf",sep="_"), width = 12, height = 6)
  #### end ####
  
  #### New cases&deaths ####
  colors <- c("Cases" = "steelblue", "Deaths" = "darkred")
  
  # 2020 
  dataFile_cases <- subset(dataFile_ISO, new_cases>0 &
                    ((format(date,"%m")=="01") | (format(date,"%m")=="03") |
                    (format(date,"%m")=="04") | (format(date,"%m")=="05")) & 
                    format(date,"%Y")=="2020",
                    select = c(date, new_cases, new_deaths))
  dataFile_cases <- dataFile_cases %>% mutate(month = as.numeric(format(dataFile_cases$date,"%m"))) %>%
                                       mutate(month = month.name[month])
  dataFile_cases$month = factor(dataFile_cases$month, levels = c("January", "March", "April", "May"))
  dataFile_cases_plot <- ggplot(data = dataFile_cases, mapping = aes(x = date)) +
                        geom_line(aes(y = new_cases, color = "Cases")) + 
                        geom_line(aes(y = new_deaths, color = "Deaths")) + 
                        geom_point(aes(y = new_cases, color = "Cases")) +
                        geom_point(aes(y = new_deaths, color = "Deaths")) +
                        facet_grid(~ dataFile_cases$month, scales = "free_x", drop = FALSE) +
                        labs(x = "",
                             y = "Number of new cases",
                             title = paste("New COVID-19 cases (and deaths) in",country_name),
                             subtitle = "Number of newly reported COVID-19 cases (and deaths) by date in January, March, April and May of 2020") +
                        scale_color_manual(values = colors) +
                        theme_bw() + theme(text = element_text(size = 14)) +
                        theme(plot.title = element_text(face = "bold")) +
                        theme(plot.subtitle = element_text(face = "italic")) +
                        theme(axis.text.x = element_text(angle = 0, size = 9)) +
                        theme(plot.margin = margin(1,1.2,0.5,1, "cm")) +
                        theme(panel.spacing.x = unit(4, "mm")) +
                        theme(legend.position = "bottom") +
                        theme(legend.title = element_blank()) +
                        scale_y_continuous(labels = label_number())
  ggsave(dataFile_cases_plot, filename = paste(country_code,"2020_new_cases_deaths.pdf",sep="_"), width = 12, height = 6)
  
  # 2021
  dataFile_cases <- subset(dataFile_ISO, new_cases>0 &
                    ((format(date,"%m")=="01") | (format(date,"%m")=="03") |
                    (format(date,"%m")=="04") | (format(date,"%m")=="05")) & 
                    format(date,"%Y")=="2021",
                    select = c(date, new_cases, new_deaths))
  dataFile_cases <- dataFile_cases %>% mutate(month = as.numeric(format(dataFile_cases$date,"%m"))) %>%
                                       mutate(month = month.name[month])
  dataFile_cases$month = factor(dataFile_cases$month, levels = c("January", "March", "April", "May"))
  dataFile_cases_plot <- ggplot(data = dataFile_cases, mapping = aes(x = date)) +
                        geom_line(aes(y = new_cases, color = "Cases")) + 
                        geom_line(aes(y = new_deaths, color = "Deaths")) + 
                        geom_point(aes(y = new_cases, color = "Cases")) +
                        geom_point(aes(y = new_deaths, color = "Deaths")) +
                        facet_grid(~ dataFile_cases$month, scales = "free_x", drop = FALSE) +
                        labs(x = "",
                             y = "Number of new cases",
                             title = paste("New COVID-19 cases (and deaths) in",country_name),
                             subtitle = "Number of newly reported COVID-19 cases (and deaths) by date in January, March, April and May of 2021") +
                        scale_color_manual(values = colors) +
                        theme_bw() + theme(text = element_text(size = 14)) +
                        theme(plot.title = element_text(face = "bold")) +
                        theme(plot.subtitle = element_text(face = "italic")) +
                        theme(axis.text.x = element_text(angle = 0, size = 9)) +
                        theme(plot.margin = margin(1,1.2,0.5,1, "cm")) +
                        theme(panel.spacing.x = unit(4, "mm")) +
                        theme(legend.position = "bottom") +
                        theme(legend.title = element_blank()) +
                        scale_y_continuous(labels = label_number())
  ggsave(dataFile_cases_plot, filename = paste(country_code,"2021_new_cases_deaths.pdf",sep="_"), width = 12, height = 6)
  
  # 2022
  dataFile_cases <- subset(dataFile_ISO, new_cases>0 &
                    format(date,"%m")=="01" & 
                    format(date,"%Y")=="2022",
                    select = c(date, new_cases, new_deaths))
  dataFile_cases_plot <- ggplot(data = dataFile_cases, mapping = aes(x = date)) +
                        geom_line(aes(y = new_cases, color = "Cases")) + 
                        geom_line(aes(y = new_deaths, color = "Deaths")) + 
                        geom_point(aes(y = new_cases, color = "Cases")) +
                        geom_point(aes(y = new_deaths, color = "Deaths")) +
                        labs(x = "",
                             y = "Number of new cases",
                             title = paste("New COVID-19 cases (and deaths) in",country_name),
                             subtitle = "Number of newly reported COVID-19 cases (and deaths) by date in January of 2022") +
                        scale_color_manual(values = colors) +
                        theme_bw() + theme(text = element_text(size = 14)) +
                        theme(plot.title = element_text(face = "bold")) +
                        theme(plot.subtitle = element_text(face = "italic")) +
                        theme(axis.text.x = element_text(angle = 0, size = 9)) +
                        theme(plot.margin = margin(1,1.2,0.5,1, "cm")) +
                        theme(panel.spacing.x = unit(4, "mm")) +
                        theme(legend.position = "bottom") +
                        theme(legend.title = element_blank()) +
                        scale_y_continuous(labels = label_number())
  ggsave(dataFile_cases_plot, filename = paste(country_code,"2022_new_cases_deaths.pdf",sep="_"), width = 12, height = 6)
  
  #### end ####
  
  #### New cases (last 2 months) ####
  # 2020 
  dataFile_cases <- subset(dataFile_ISO, new_cases>0 &
                    ((format(date,"%m")=="11") | (format(date,"%m")=="12")) & 
                    format(date,"%Y")=="2020",
                    select = c(date, new_cases))
  dataFile_cases_plot <- ggplot(data = dataFile_cases, mapping = aes(x = date, y = new_cases, label = new_cases)) +
                        geom_line() + geom_point() + 
                        labs(x = "",
                             y = "Number of new cases",
                             title = paste("New COVID-19 cases in",country_name),
                             subtitle = "Number of newly reported COVID-19 cases by date in November and December of 2020") +
                        theme_bw() + theme(text = element_text(size = 14)) +
                        theme(plot.title = element_text(face = "bold")) +
                        theme(plot.subtitle = element_text(face = "italic")) +
                        theme(axis.text.x = element_text(angle = 0, size = 9)) +
                        theme(plot.margin = margin(1,1.2,0.5,1, "cm")) +
                        theme(panel.spacing.x = unit(4, "mm")) +
                        scale_y_continuous(labels = label_number())
  ggsave(dataFile_cases_plot, filename = paste(country_code,"2020_new_cases_nov+dec.pdf",sep="_"), width = 12, height = 6)
  
  # 2021
  dataFile_cases <- subset(dataFile_ISO, new_cases>0 &
                    ((format(date,"%m")=="11") | (format(date,"%m")=="12")) & 
                    format(date,"%Y")=="2021",
                    select = c(date, new_cases))
  dataFile_cases_plot <- ggplot(data = dataFile_cases, mapping = aes(x = date, y = new_cases, label = new_cases)) +
                        geom_line() + geom_point() + 
                        labs(x = "",
                             y = "Number of new cases",
                             title = paste("New COVID-19 cases in",country_name),
                             subtitle = "Number of newly reported COVID-19 cases by date in November and December of 2021") +
                        theme_bw() + theme(text = element_text(size = 14)) +
                        theme(plot.title = element_text(face = "bold")) +
                        theme(plot.subtitle = element_text(face = "italic")) +
                        theme(axis.text.x = element_text(angle = 0, size = 9)) +
                        theme(plot.margin = margin(1,1.2,0.5,1, "cm")) +
                        theme(panel.spacing.x = unit(4, "mm")) +
                        scale_y_continuous(labels = label_number())
  ggsave(dataFile_cases_plot, filename = paste(country_code,"2021_new_cases_nov+dec.pdf",sep="_"), width = 12, height = 6)
  #### end ####
  
  #### New deaths (last 2 months) ####
  # 2020 
  dataFile_deaths <- subset(dataFile_ISO, new_deaths>0 &
                    ((format(date,"%m")=="11") | (format(date,"%m")=="12")) & 
                    format(date,"%Y")=="2020",
                    select = c(date, new_deaths))
  dataFile_deaths_plot <- ggplot(data = dataFile_deaths, mapping = aes(x = date, y = new_deaths, label = new_deaths)) +
                        geom_line() + geom_point() + 
                        labs(x = "",
                             y = "Number of new deaths",
                             title = paste("New COVID-19 deaths in",country_name),
                             subtitle = "Number of newly reported COVID-19 deaths by date in November and December of 2020") +
                        theme_bw() + theme(text = element_text(size = 14)) +
                        theme(plot.title = element_text(face = "bold")) +
                        theme(plot.subtitle = element_text(face = "italic")) +
                        theme(axis.text.x = element_text(angle = 0, size = 9)) +
                        theme(plot.margin = margin(1,1.2,0.5,1, "cm")) +
                        theme(panel.spacing.x = unit(4, "mm")) +
                        scale_y_continuous(labels = label_number())
  ggsave(dataFile_deaths_plot, filename = paste(country_code,"2020_new_deaths_nov+dec.pdf",sep="_"), width = 12, height = 6)
  
  # 2021
  dataFile_deaths <- subset(dataFile_ISO, new_deaths>0 &
                    ((format(date,"%m")=="11") | (format(date,"%m")=="12")) & 
                    format(date,"%Y")=="2021",
                    select = c(date, new_deaths))
  dataFile_deaths_plot <- ggplot(data = dataFile_deaths, mapping = aes(x = date, y = new_deaths, label = new_deaths)) +
                        geom_line() + geom_point() + 
                        labs(x = "",
                             y = "Number of new deaths",
                             title = paste("New COVID-19 deaths in",country_name),
                             subtitle = "Number of newly reported COVID-19 deaths by date in November and December of 2021") +
                        theme_bw() + theme(text = element_text(size = 14)) +
                        theme(plot.title = element_text(face = "bold")) +
                        theme(plot.subtitle = element_text(face = "italic")) +
                        theme(axis.text.x = element_text(angle = 0, size = 9)) +
                        theme(plot.margin = margin(1,1.2,0.5,1, "cm")) +
                        theme(panel.spacing.x = unit(4, "mm")) +
                        scale_y_continuous(labels = label_number())
  ggsave(dataFile_deaths_plot, filename = paste(country_code,"2021_new_deaths_nov+dec.pdf",sep="_"), width = 12, height = 6)
  #### end ####
  
  #### New cases&deaths (last 2 months) ####
  colors <- c("Cases" = "steelblue", "Deaths" = "darkred")
  
  # 2020 
  dataFile_cases <- subset(dataFile_ISO, new_cases>0 &
                    ((format(date,"%m")=="11") | (format(date,"%m")=="12")) & 
                    format(date,"%Y")=="2020",
                    select = c(date, new_cases, new_deaths))
  dataFile_cases_plot <- ggplot(data = dataFile_cases, mapping = aes(x = date)) +
                        geom_line(aes(y = new_cases, color = "Cases")) + 
                        geom_line(aes(y = new_deaths, color = "Deaths")) + 
                        geom_point(aes(y = new_cases, color = "Cases")) +
                        geom_point(aes(y = new_deaths, color = "Deaths")) +
                        labs(x = "",
                             y = "Number of new cases",
                             title = paste("New COVID-19 cases (and deaths) in",country_name),
                             subtitle = "Number of newly reported COVID-19 cases (and deaths) by date in November and December of 2020") +
                        scale_color_manual(values = colors) +
                        theme_bw() + theme(text = element_text(size = 14)) +
                        theme(plot.title = element_text(face = "bold")) +
                        theme(plot.subtitle = element_text(face = "italic")) +
                        theme(axis.text.x = element_text(angle = 0, size = 9)) +
                        theme(plot.margin = margin(1,1.2,0.5,1, "cm")) +
                        theme(panel.spacing.x = unit(4, "mm")) +
                        theme(legend.position = "bottom") +
                        theme(legend.title = element_blank()) +
                        scale_y_continuous(labels = label_number())
  ggsave(dataFile_cases_plot, filename = paste(country_code,"2020_new_cases_deaths_nov+dec.pdf",sep="_"), width = 12, height = 6)
  
  # 2021
  dataFile_cases <- subset(dataFile_ISO, new_cases>0 &
                    ((format(date,"%m")=="11") | (format(date,"%m")=="12")) & 
                    format(date,"%Y")=="2021",
                    select = c(date, new_cases, new_deaths))
  dataFile_cases_plot <- ggplot(data = dataFile_cases, mapping = aes(x = date)) +
                        geom_line(aes(y = new_cases, color = "Cases")) + 
                        geom_line(aes(y = new_deaths, color = "Deaths")) + 
                        geom_point(aes(y = new_cases, color = "Cases")) +
                        geom_point(aes(y = new_deaths, color = "Deaths")) +
                        labs(x = "",
                             y = "Number of new cases",
                             title = paste("New COVID-19 cases (and deaths) in",country_name),
                             subtitle = "Number of newly reported COVID-19 cases (and deaths) by date in November and December of 2021") +
                        scale_color_manual(values = colors) +
                        theme_bw() + theme(text = element_text(size = 14)) +
                        theme(plot.title = element_text(face = "bold")) +
                        theme(plot.subtitle = element_text(face = "italic")) +
                        theme(axis.text.x = element_text(angle = 0, size = 9)) +
                        theme(plot.margin = margin(1,1.2,0.5,1, "cm")) +
                        theme(panel.spacing.x = unit(4, "mm")) +
                        theme(legend.position = "bottom") +
                        theme(legend.title = element_blank()) +
                        scale_y_continuous(labels = label_number())
  ggsave(dataFile_cases_plot, filename = paste(country_code,"2021_new_cases_deaths_nov+dec.pdf",sep="_"), width = 12, height = 6)
  #### end ####
  
  #### Cumulative cases ####
  # 2020 
  dataFile_cases <- subset(dataFile_ISO, new_cases>0 &
                    ((format(date,"%m")=="01") | (format(date,"%m")=="03") |
                    (format(date,"%m")=="04") | (format(date,"%m")=="05")) & 
                    format(date,"%Y")=="2020",
                    select = c(date, new_cases))
  dataFile_cases$new_cases <- cumsum(dataFile_cases$new_cases)
  dataFile_cases <- dataFile_cases %>% mutate(month = as.numeric(format(dataFile_cases$date,"%m"))) %>%
                                       mutate(month = month.name[month])
  dataFile_cases$month = factor(dataFile_cases$month, levels = c("January", "March", "April", "May"))
  dataFile_cases_plot <- ggplot(data = dataFile_cases, mapping = aes(x = date, y = new_cases, label = new_cases)) +
                        geom_line() + geom_point() + 
                        facet_grid(~ dataFile_cases$month, scales = "free_x", drop = FALSE) +
                        labs(x = "",
                             y = "Number of cases",
                             title = paste("COVID-19 cases in",country_name),
                             subtitle = "Number of cumulative COVID-19 cases by date in January, March, April and May of 2020") +
                        theme_bw() + theme(text = element_text(size = 14)) +
                        theme(plot.title = element_text(face = "bold")) +
                        theme(plot.subtitle = element_text(face = "italic")) +
                        theme(axis.text.x = element_text(angle = 0, size = 9)) +
                        theme(plot.margin = margin(1,1.2,0.5,1, "cm")) +
                        theme(panel.spacing.x = unit(4, "mm")) +
                        scale_y_continuous(labels = label_number())
  ggsave(dataFile_cases_plot, filename = paste(country_code,"2020_cumulative_cases.pdf",sep="_"), width = 12, height = 6)
  
  # 2021
  dataFile_cases <- subset(dataFile_ISO, new_cases>0 &
                    ((format(date,"%m")=="01") | (format(date,"%m")=="03") |
                    (format(date,"%m")=="04") | (format(date,"%m")=="05")) & 
                    format(date,"%Y")=="2021",
                    select = c(date, new_cases))
  dataFile_cases$new_cases <- cumsum(dataFile_cases$new_cases)
  dataFile_cases <- dataFile_cases %>% mutate(month = as.numeric(format(dataFile_cases$date,"%m"))) %>%
                                       mutate(month = month.name[month])
  dataFile_cases$month = factor(dataFile_cases$month, levels = c("January", "March", "April", "May"))
  dataFile_cases_plot <- ggplot(data = dataFile_cases, mapping = aes(x = date, y = new_cases, label = new_cases)) +
                        geom_line() + geom_point() + 
                        facet_grid(~ dataFile_cases$month, scales = "free_x", drop = FALSE) +
                        labs(x = "",
                             y = "Number of cases",
                             title = paste("COVID-19 cases in",country_name),
                             subtitle = "Number of cumulative COVID-19 cases by date in January, March, April and May of 2021") +
                        theme_bw() + theme(text = element_text(size = 14)) +
                        theme(plot.title = element_text(face = "bold")) +
                        theme(plot.subtitle = element_text(face = "italic")) +
                        theme(axis.text.x = element_text(angle = 0, size = 9)) +
                        theme(plot.margin = margin(1,1.2,0.5,1, "cm")) +
                        theme(panel.spacing.x = unit(4, "mm")) +
                        scale_y_continuous(labels = label_number())
  ggsave(dataFile_cases_plot, filename = paste(country_code,"2021_cumulative_cases.pdf",sep="_"), width = 12, height = 6)
  
  # 2022
  dataFile_cases <- subset(dataFile_ISO, new_cases>0 &
                    format(date,"%m")=="01" &
                    format(date,"%Y")=="2022",
                    select = c(date, new_cases))
  dataFile_cases$new_cases <- cumsum(dataFile_cases$new_cases)
  dataFile_cases_plot <- ggplot(data = dataFile_cases, mapping = aes(x = date, y = new_cases, label = new_cases)) +
                        geom_line() + geom_point() + 
                        labs(x = "",
                             y = "Number of cases",
                             title = paste("COVID-19 cases in",country_name),
                             subtitle = "Number of cumulative COVID-19 cases by date in January of 2022") +
                        theme_bw() + theme(text = element_text(size = 14)) +
                        theme(plot.title = element_text(face = "bold")) +
                        theme(plot.subtitle = element_text(face = "italic")) +
                        theme(axis.text.x = element_text(angle = 0, size = 9)) +
                        theme(plot.margin = margin(1,1.2,0.5,1, "cm")) +
                        theme(panel.spacing.x = unit(4, "mm")) +
                        scale_y_continuous(labels = label_number())
  ggsave(dataFile_cases_plot, filename = paste(country_code,"2022_cumulative_cases.pdf",sep="_"), width = 12, height = 6)
  #### end ####
  
  #### Cumulative deaths ####
  # 2020 
  dataFile_deaths <- subset(dataFile_ISO, new_deaths>0 &
                    ((format(date,"%m")=="01") | (format(date,"%m")=="03") |
                    (format(date,"%m")=="04") | (format(date,"%m")=="05")) & 
                    format(date,"%Y")=="2020",
                    select = c(date, new_deaths))
  dataFile_deaths$new_deaths <- cumsum(dataFile_deaths$new_deaths)
  dataFile_deaths <- dataFile_deaths %>% mutate(month = as.numeric(format(dataFile_deaths$date,"%m"))) %>%
                                       mutate(month = month.name[month])
  dataFile_deaths$month = factor(dataFile_deaths$month, levels = c("January", "March", "April", "May"))
  dataFile_deaths_plot <- ggplot(data = dataFile_deaths, mapping = aes(x = date, y = new_deaths, label = new_deaths)) +
                        geom_line() + geom_point() + 
                        facet_grid(~ dataFile_deaths$month, scales = "free_x", drop = FALSE) +
                        labs(x = "",
                             y = "Number of deaths",
                             title = paste("COVID-19 deaths in",country_name),
                             subtitle = "Number of cumulative COVID-19 deaths by date in January, March, April and May of 2020") +
                        theme_bw() + theme(text = element_text(size = 14)) +
                        theme(plot.title = element_text(face = "bold")) +
                        theme(plot.subtitle = element_text(face = "italic")) +
                        theme(axis.text.x = element_text(angle = 0, size = 9)) +
                        theme(plot.margin = margin(1,1.2,0.5,1, "cm")) +
                        theme(panel.spacing.x = unit(4, "mm")) +
                        scale_y_continuous(labels = label_number())
  ggsave(dataFile_deaths_plot, filename = paste(country_code,"2020_cumulative_deaths.pdf",sep="_"), width = 12, height = 6)
  
  # 2021
  dataFile_deaths <- subset(dataFile_ISO, new_deaths>0 &
                    ((format(date,"%m")=="01") | (format(date,"%m")=="03") |
                    (format(date,"%m")=="04") | (format(date,"%m")=="05")) & 
                    format(date,"%Y")=="2021",
                    select = c(date, new_deaths))
  dataFile_deaths$new_deaths <- cumsum(dataFile_deaths$new_deaths)
  dataFile_deaths <- dataFile_deaths %>% mutate(month = as.numeric(format(dataFile_deaths$date,"%m"))) %>%
                                       mutate(month = month.name[month])
  dataFile_deaths$month = factor(dataFile_deaths$month, levels = c("January", "March", "April", "May"))
  dataFile_deaths_plot <- ggplot(data = dataFile_deaths, mapping = aes(x = date, y = new_deaths, label = new_deaths)) +
                        geom_line() + geom_point() + 
                        facet_grid(~ dataFile_deaths$month, scales = "free_x", drop = FALSE) +
                        labs(x = "",
                             y = "Number of deaths",
                             title = paste("COVID-19 deaths in",country_name),
                             subtitle = "Number of cumulative COVID-19 deaths by date in January, March, April and May of 2021") +
                        theme_bw() + theme(text = element_text(size = 14)) +
                        theme(plot.title = element_text(face = "bold")) +
                        theme(plot.subtitle = element_text(face = "italic")) +
                        theme(axis.text.x = element_text(angle = 0, size = 9)) +
                        theme(plot.margin = margin(1,1.2,0.5,1, "cm")) +
                        theme(panel.spacing.x = unit(4, "mm")) +
                        scale_y_continuous(labels = label_number())
  ggsave(dataFile_deaths_plot, filename = paste(country_code,"2021_cumulative_deaths.pdf",sep="_"), width = 12, height = 6)
  
  # 2022
  dataFile_deaths <- subset(dataFile_ISO, new_deaths>0 &
                    format(date,"%m")=="01" &
                    format(date,"%Y")=="2022",
                    select = c(date, new_deaths))
  dataFile_deaths$new_deaths <- cumsum(dataFile_deaths$new_deaths)
  dataFile_deaths_plot <- ggplot(data = dataFile_deaths, mapping = aes(x = date, y = new_deaths, label = new_deaths)) +
                        geom_line() + geom_point() + 
                        # scale_x_continuous(breaks = unique(dataFile_deaths$date)) +
                        # scale_x_date(date_labels = "%b %d") +
                        labs(x = "",
                             y = "Number of deaths",
                             title = paste("COVID-19 deaths in",country_name),
                             subtitle = "Number of cumulative COVID-19 deaths by date in January of 2022") +
                        theme_bw() + theme(text = element_text(size = 14)) +
                        theme(plot.title = element_text(face = "bold")) +
                        theme(plot.subtitle = element_text(face = "italic")) +
                        theme(axis.text.x = element_text(angle = 0, size = 9)) +
                        theme(plot.margin = margin(1,1.2,0.5,1, "cm")) +
                        theme(panel.spacing.x = unit(4, "mm")) +
                        scale_y_continuous(labels = label_number())
  ggsave(dataFile_deaths_plot, filename = paste(country_code,"2022_cumulative_deaths.pdf",sep="_"), width = 12, height = 6)
  #### end ####
}

assignment_v("IDN")
assignment_v("JPN")
assignment_v("VNM")

suppressWarnings(detach("package:readr", unload=TRUE))
suppressWarnings(detach("package:dplyr", unload=TRUE))
suppressWarnings(detach("package:ggplot2", unload=TRUE))
suppressWarnings(detach("package:here", unload=TRUE))
suppressWarnings(detach("package:scales", unload=TRUE))

# endTime <- Sys.time()
# print(endTime - startTime)