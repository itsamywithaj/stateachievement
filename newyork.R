rm(list = ls())
library(knitr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
install.packages("readxl") # CRAN version
library(readxl)
getwd()
setwd("/Users/amyjiravisitcul/Downloads/States/")
# NEW YORK CITY
nyc_enroll <- read_excel("raw_data/NYC_enrollment_2014-2019.xlsx", sheet = "District") # Read NYC's demographic snapshot
names(nyc_enroll) <- tolower(names(nyc_enroll))
names(nyc_enroll)[1] <- "admin_dist"
nyc_enroll$admin_dist <- as.numeric(nyc_enroll$admin_dist)
str(nyc_enroll)
nyc_enroll <- nyc_enroll %>% 
  filter(year == "2018-19",
         (admin_dist > 1 & admin_dist < 10)|
           (admin_dist > 12 & admin_dist < 18)|
           (admin_dist > 20 & admin_dist < 33 & admin_dist != 23 &
              admin_dist != 25 & admin_dist != 26 & admin_dist != 28 & admin_dist != 31))
# NYC districts include members in 2-9, 13-17, 21, 22, 24, 27, 29, 30, 32
View(nyc_enroll)
names(nyc_enroll)[19:38] <- c("perc_f", "count_m", "perc_m","count_asian","perc_asian","count_black","perc_black",
                              "count_hisp","perc_hisp","count_mult","perc_mult","count_white","perc_white",
                              "count_swd","perc_swd","count_ell","perc_ell", "count_poverty","perc_poverty","econ_need")
nyc_enroll_dist <- nyc_enroll %>% 
  select(names(nyc_enroll)[c(1:3,18:38)])
nyc_enroll <- read_excel("raw_data/NYC_enrollment_2014-2019.xlsx", sheet = "School")
names(nyc_enroll) <- tolower(names(nyc_enroll))
nyc_enroll_sch <- nyc_enroll %>% 
  filter(year == "2018-19") %>% 
  separate(dbn, c("dist", "other"),
                  sep = 2)
nyc_enroll_sch %>% 
  filter(dist == "24") %>% 
  View() # confirming that the 1st two characters of "dbn" are, in fact, administrative districts
names(nyc_enroll_sch)[20:40] <- c("count_f","perc_f", "count_m", "perc_m","count_asian","perc_asian",
                                  "count_black","perc_black","count_hisp","perc_hisp","count_mult",
                                  "perc_mult","count_white","perc_white", "count_swd","perc_swd",
                                  "count_ell","perc_ell", "count_poverty","perc_poverty","econ_need")
summary(as.factor(nyc_enroll_sch$dist))
nyc_enroll_sch$dist <- as.numeric(nyc_enroll_sch$dist)
nyc_enroll_summary <- nyc_enroll_sch %>%
  group_by(dist) %>% 
  summarize(n = n(),
            avg_f = mean(perc_f),
          sd_f = sd(perc_f),
          avg_asian = mean(perc_asian),
          sd_asian = sd(perc_asian),
          avg_black = mean(perc_black),
          sd_black = sd(perc_black),
          avg_hisp = mean(perc_hisp),
          sd_hisp = sd(perc_hisp),
          avg_white = mean(perc_white),
          sd_white = sd(perc_white),
          avg_mult = mean(perc_mult),
          sd_mult = sd(perc_mult),
          avg_swd = mean(perc_swd),
          sd_swd = sd(perc_swd),
          avg_ell = mean(perc_ell),
          sd_ell = sd(perc_ell),
          avg_econ = mean(econ_need),
          sd_econ = sd(econ_need)) %>% 
  filter((dist > 1 & dist < 10)|
  (dist > 12 & dist < 18)|
  (dist > 20 & dist < 33 & dist != 23 &
     dist != 25 & dist != 26 & dist != 28 & dist != 31))
# NYC districts include members in 2-9, 13-17, 21, 22, 24, 27, 29, 30, 32
write.csv(nyc_enroll_summary, file = file.path('nyc_comp_enroll.csv'),row.names = FALSE)
