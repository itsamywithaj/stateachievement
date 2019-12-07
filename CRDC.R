rm(list = ls()) # clear working environment
getwd() # check working directory
setwd("~/Downloads/2015-16-crdc-data/Data Files and Layouts/")
install.packages("tidyverse") # install tidyverse
library(tidyverse)
#--------
# Starting from scratch with raw data
data <- read.csv('CRDC 2015-16 School Data.csv')
View(data)
memb_states <- data %>% 
  filter(LEA_STATE == "MO"|
           LEA_STATE == "NY"|
           LEA_STATE == "GA" |
           LEA_STATE == "MD"|
           LEA_STATE == "MA"|
           LEA_STATE == "RI"|
           LEA_STATE=="NY"|
           LEA_STATE == "MI"|
           LEA_STATE == "DC"|
           LEA_STATE == "NJ"|
           LEA_STATE == "CA"|
           LEA_STATE == "CO"|
           LEA_STATE == "LA"|
           LEA_STATE == "SC"|
           LEA_STATE == "NC" |
           LEA_STATE == "TN" |
           LEA_STATE == "OK" |
           LEA_STATE == "TX")
write.csv(memb_states, file = file.path('memb_states_crdc1516.csv'), row.names = FALSE)
#------- 
# pick up from here
memb_states <- read.csv('memb_states_crdc1516.csv')
View(memb_states)
names(memb_states)
#------
# All the states where we have members: their enrollment data on race, gender, sub populations
memb_states_racegender <- memb_states %>% 
  select(LEA_STATE, LEA_NAME, SCH_NAME, SCH_STATUS_CHARTER,
         SCH_ENR_HI_M, SCH_ENR_HI_F,
         SCH_ENR_AM_M, SCH_ENR_AM_F,
         SCH_ENR_AS_M, SCH_ENR_AS_F,
         SCH_ENR_HP_M, SCH_ENR_HP_F,
         SCH_ENR_BL_M, SCH_ENR_BL_F,
         SCH_ENR_WH_M, SCH_ENR_WH_F,
         SCH_ENR_TR_M, SCH_ENR_TR_F,
         TOT_ENR_M,TOT_ENR_F)
names(memb_states_racegender) <- tolower(names(memb_states_racegender))
names(memb_states_racegender) <- c("state","district","school_name","charter",
                                   "hispanic_male","hispanic_female","amind_male","amind_female",
                                   "asian_male","asian_female","hawpi_male","hawpi_female",
                                   "black_male","black_female","white_male","white_female",
                                   "multiple_male","multiple_female","total_male","total_female")
memb_states_racegender$total = memb_states_racegender$total_female + memb_states_racegender$total_male
names(memb_states_racegender)
memb_states_racegender <- memb_states_racegender %>% 
  mutate(hisp = hispanic_male + hispanic_female, # add up the genders to get totals and percents by race
         perc_hisp = hisp / total,
         amind = amind_male + amind_female,
         perc_amind = amind / total,
         asian = asian_male + asian_female,
         perc_asian = asian / total,
         hawpi = hawpi_female + hawpi_male,
         perc_hawpi = hawpi / total,
         black = black_male + black_female,
         perc_black = black / total,
         white = white_male + white_female,
         perc_white = white / total,
         multiple = multiple_male + multiple_female,
         perc_multiple = multiple / total) %>% 
  arrange(state, district, school_name)
View(memb_states_racegender)
subpops_memb_states <- memb_states %>% # LEP and SWD data among states where we have members
  select(LEA_STATE, LEA_NAME, SCH_NAME, SCH_STATUS_CHARTER,
         TOT_IDEAENR_M, TOT_IDEAENR_F,TOT_504ENR_F, TOT_504ENR_M, # Students served with either IEP or 504
         TOT_LEPENR_F, TOT_LEPENR_M, # students identified as LEP
         TOT_ENR_M, TOT_ENR_F) # total enrollment
names(subpops_memb_states) <- tolower(names(subpops_memb_states))
names(subpops_memb_states) <- c("state","district","school_name","charter","iep_m","iep_f","504_f","504_m","lep_f","lep_m","total_m","total_f")
subpops_memb_states <- subpops_memb_states %>% 
  mutate(total_el = lep_f + lep_m,
         total_enrl = total_m + total_f,
         perc_lep = total_el / total_enrl,
         total_swd = iep_m + iep_f + `504_f` + `504_m`,
         perc_swd = total_swd / total_enrl) %>% 
  arrange(district, school_name)
  
# --------
# SWD and LEP for California as a state
cali_subpops <- subpops_memb_states %>% 
  filter(state == "CA")
cali_memb_subpops <- cali_subpops[c(1367:1369,1371,3267, 7778:7784, 8667:8672, 9238:9239,9601:9602),] # pull member rows
View(cali_memb_subpops)
a <- cali_subpops %>% 
  group_by(district) %>% 
  summarize(meanlep_dist = mean(perc_lep),
            sdlep_dist = sd(perc_lep),
            meanswd_dist = mean(perc_swd),
            sdswd_dist = sd(perc_swd),
            n = n())
View(a)
rownames(a) <- a$district
a <- a[c("Los Angeles Unified","San Diego Unified"),] # comparison districts
# ---- 
# LEP California members
# 

cali_memb_subpops$sys_lep = cali_memb_subpops$total_el
names(cali_memb_subpops)
rownames(cali_memb_subpops) <- 1:22
summary(cali_memb_subpops)
cali_memb_subpops[c(1:5,19:22),2] <- "Los Angeles Unified"
cali_memb_subpops <- cali_memb_subpops %>% 
  arrange(district, school_name)
cali_memb_subpops$sys_lep <- as.numeric(c(a[1,2],a[1,2],a[1,2],a[1,2],a[1,2],a[1,2],a[1,2], a[1,2],a[1,2], # LA Unified
                                          a[2,2],a[2,2],a[2,2],a[2,2],a[2,2],a[2,2],a[2,2],
                                          a[2,2],a[2,2],a[2,2],a[2,2],a[2,2],a[2,2])) # San Diego Unified
cali_memb_subpops$std_lep1 <- as.numeric(c(a[1,3],a[1,3],a[1,3],a[1,3],a[1,3],a[1,3],a[1,3],a[1,3],a[1,3], # LA Unified
                                          a[2,3],a[2,3],a[2,3],a[2,3],a[2,3],a[2,3],a[2,3],
                                          a[2,3],a[2,3],a[2,3],a[2,3],a[2,3],a[2,3])) # San Diego Unified)
cali_memb_subpops <- cali_memb_subpops %>% 
  mutate(diff_lep = perc_lep - sys_lep,
         std_lep = diff_lep / std_lep1)
cali_lep <- cali_memb_subpops %>% 
  select(district, school_name, perc_lep, diff_lep, std_lep)
View(cali_lep)
View(cali_memb_subpops)

#-----
# SWD California members
cali_memb_subpops$sys_swd <- as.numeric(c(a[1,4],a[1,4],a[1,4],a[1,4],a[1,4],a[1,4],a[1,4], a[1,4],a[1,4], # LA Unified
                                          a[2,4],a[2,4],a[2,4],a[2,4],a[2,4],a[2,4],a[2,4],
                                          a[2,4],a[2,4],a[2,4],a[2,4],a[2,4],a[2,4])) # San Diego Unified
cali_memb_subpops$std_swd1 <- as.numeric(c(a[1,5],a[1,5],a[1,5],a[1,5],a[1,5],a[1,5],a[1,5],a[1,5],a[1,5], # LA Unified
                                           a[2,5],a[2,5],a[2,5],a[2,5],a[2,5],a[2,5],a[2,5],
                                           a[2,5],a[2,5],a[2,5],a[2,5],a[2,5],a[2,5])) # San Diego Unified)
cali_memb_subpops <- cali_memb_subpops %>% 
  mutate(diff_swd = perc_swd - sys_swd,
         std_swd = diff_swd / std_swd1)
cali_swd <- cali_memb_subpops %>% 
  select(district, school_name, perc_swd, sys_swd, diff_swd, std_swd)
View(cali_swd)

# ----
# New York members race gender
ny_subpops <- subpops_memb_states %>% 
  filter(state == "NY") 
View(ny_subpops)
ny_racegender <- memb_states_racegender %>% 
  filter(state == "NY") 
View(ny_racegender)
ny_dist_racegender <- ny_racegender %>% 
  group_by(district) %>% 
  summarize(mean_amind = mean(perc_amind),
            sd_amind = sd(perc_amind),
            mean_asian = mean(perc_asian),
            sd_asian = sd(perc_asian),
            mean_hawpi = mean(perc_hawpi),
            sd_hawpi = sd(perc_hawpi),
            mean_black = mean(perc_black),
            sd_black = sd(perc_black),
            mean_hisp = mean(perc_hisp),
            sd_hisp = sd(perc_hisp),
            mean_white = mean(perc_white),
            sd_white = sd(perc_white),
            mean_multiple = mean(perc_multiple),
            sd_multiple = sd(perc_multiple),
            n = n()) %>% 
  arrange(district)
rownames(ny_dist_racegender) <- ny_dist_racegender$district
NYC_racegender <- ny_dist_racegender[c("NEW YORK CITY PUBLIC SCHOOLS"),]
View(NYC_racegender)
View(ny_dist_racegender)
ny_memb_racegender <- ny_racegender[c(3,320,479:480,613,1224),] # Academy of City, Prospect, CQA, Roots, Hellenic
View(ny_memb_racegender)
str(ny_memb_racegender)
ny_memb_racegender <- ny_memb_racegender %>% 
  arrange(school_name) %>% 
  select(school_name, total, perc_amind, perc_asian, perc_hawpi, perc_black, perc_hisp, perc_white, perc_multiple)
ny_memb_racegender$sys_amind = as.numeric(c(NYC_racegender[1,2],NYC_racegender[1,2],NYC_racegender[1,2],
                                            NYC_racegender[1,2],NYC_racegender[1,2],NYC_racegender[1,2]))
ny_memb_racegender$sys_asian = as.numeric(c(NYC_racegender[1,4],NYC_racegender[1,4],NYC_racegender[1,4],
                                            NYC_racegender[1,4],NYC_racegender[1,4],NYC_racegender[1,4]))
ny_memb_racegender$sys_hawpi = as.numeric(c(NYC_racegender[1,6],NYC_racegender[1,6],NYC_racegender[1,6],
                                            NYC_racegender[1,6],NYC_racegender[1,6],NYC_racegender[1,6]))
ny_memb_racegender$sys_black = as.numeric(c(NYC_racegender[1,8],NYC_racegender[1,8],NYC_racegender[1,8],
                                            NYC_racegender[1,8],NYC_racegender[1,8],NYC_racegender[1,8]))
ny_memb_racegender$sys_hisp = as.numeric(c(NYC_racegender[1,10],NYC_racegender[1,10],NYC_racegender[1,10],
                                            NYC_racegender[1,10],NYC_racegender[1,10],NYC_racegender[1,10]))
ny_memb_racegender$sys_white = as.numeric(c(NYC_racegender[1,12],NYC_racegender[1,12],NYC_racegender[1,12],
                                            NYC_racegender[1,12],NYC_racegender[1,12],NYC_racegender[1,12]))
ny_memb_racegender$sys_multiple = as.numeric(c(NYC_racegender[1,14],NYC_racegender[1,14],NYC_racegender[1,14],
                                            NYC_racegender[1,14],NYC_racegender[1,14],NYC_racegender[1,14]))

ny_memb_racegender <- ny_memb_racegender %>% 
  mutate(diff_amind = perc_amind - sys_amind,
         diff_asian = perc_asian - sys_asian,
         diff_hawpi = perc_hawpi - sys_hawpi,
         diff_black = perc_black - sys_black,
         diff_hisp = perc_hisp - sys_hisp,
         diff_white = perc_white - sys_white,
         diff_multiple = perc_multiple - sys_multiple)
names(NYC_racegender)
ny_memb_racegender$std_amind = ny_memb_racegender$diff_amind / as.numeric(c(NYC_racegender[1,3],
                                                                            NYC_racegender[1,3],
                                                                 NYC_racegender[1,3],
                                                                 NYC_racegender[1,3],
                                                                 NYC_racegender[1,3],
                                                                 NYC_racegender[1,3]))
ny_memb_racegender$std_asian = ny_memb_racegender$diff_asian / as.numeric(c(NYC_racegender[1,5],
                                                                            NYC_racegender[1,5],
                                                                            NYC_racegender[1,5],
                                                                            NYC_racegender[1,5],
                                                                            NYC_racegender[1,5],
                                                                            NYC_racegender[1,5]))
ny_memb_racegender$std_hawpi = ny_memb_racegender$diff_hawpi / as.numeric(c(NYC_racegender[1,7],
                                                                            NYC_racegender[1,7],
                                                                            NYC_racegender[1,7],
                                                                            NYC_racegender[1,7],
                                                                            NYC_racegender[1,7],
                                                                            NYC_racegender[1,7]))
ny_memb_racegender$std_black = ny_memb_racegender$diff_black / as.numeric(c(NYC_racegender[1,9],
                                                                            NYC_racegender[1,9],
                                                                            NYC_racegender[1,9],
                                                                            NYC_racegender[1,9],
                                                                            NYC_racegender[1,9],
                                                                            NYC_racegender[1,9]))
ny_memb_racegender$std_hisp = ny_memb_racegender$diff_hisp / as.numeric(c(NYC_racegender[1,11],
                                                                            NYC_racegender[1,11],
                                                                            NYC_racegender[1,11],
                                                                            NYC_racegender[1,11],
                                                                            NYC_racegender[1,11],
                                                                            NYC_racegender[1,11]))
ny_memb_racegender$std_white = ny_memb_racegender$diff_white / as.numeric(c(NYC_racegender[1,13],
                                                                            NYC_racegender[1,13],
                                                                            NYC_racegender[1,13],
                                                                            NYC_racegender[1,13],
                                                                            NYC_racegender[1,13],
                                                                            NYC_racegender[1,13]))
ny_memb_racegender$std_multiple = ny_memb_racegender$diff_multiple / as.numeric(c(NYC_racegender[1,15],
                                                                            NYC_racegender[1,15],
                                                                            NYC_racegender[1,15],
                                                                            NYC_racegender[1,15],
                                                                            NYC_racegender[1,15],
                                                                            NYC_racegender[1,15]))
ny_memb_racegender <- ny_memb_racegender %>% 
  select(school_name, total, perc_amind, sys_amind,std_amind,
         perc_asian, sys_asian, std_asian,
         perc_black, sys_black, std_black,
         perc_hisp, sys_hisp, std_hisp,
         perc_white, sys_white, std_white,
         perc_multiple, sys_multiple, std_multiple)
write.csv(ny_memb_racegender, file = file.path('ny_schooloftheyear.csv'))

#---------
# NY members SWD and LEP
ny_memb_subpops <- ny_subpops[c(3,320,479:480,613,1224),]
View(ny_memb_subpops)
NYC_subpops <- ny_subpops %>% 
  group_by(district) %>% 
  summarize(mean_swd = mean(perc_swd),
            sd_swd = mean(perc_lep),
            mean_lep = mean(perc_lep),
            sd_lep = sd(perc_lep))
rownames(NYC_subpops) <- NYC_subpops$district
NYC_subpops <- NYC_subpops[c("NEW YORK CITY PUBLIC SCHOOLS"),]
ny_memb_subpops$sys_swd = as.numeric(c(NYC_subpops[1,2],NYC_subpops[1,2],NYC_subpops[1,2],
                                       NYC_subpops[1,2],NYC_subpops[1,2],NYC_subpops[1,2]))
dc_memb_subpops$sys_lep = as.numeric(c(NYC_subpops[1,4],NYC_subpops[1,4],NYC_subpops[1,4],
                                       NYC_subpops[1,4],NYC_subpops[1,4],NYC_subpops[1,4]))
ny_memb_subpops <- ny_memb_subpops %>% 
  select(school_name, perc_swd,perc_lep,sys_swd,sys_lep) %>% 
  mutate(diff_swd = perc_swd - sys_swd,
         diff_lep = perc_lep - sys_lep)
ny_memb_subpops$std_swd = ny_memb_subpops$diff_swd / as.numeric(c(NYC_subpops[1,3],
                                                                  NYC_subpops[1,3],
                                                                  NYC_subpops[1,3],
                                                                  NYC_subpops[1,3],
                                                                  NYC_subpops[1,3],
                                                                  NYC_subpops[1,3]))
ny_memb_subpops$std_lep = ny_memb_subpops$diff_lep / as.numeric(c(NYC_subpops[1,5],
                                                                  NYC_subpops[1,5],
                                                                  NYC_subpops[1,5],
                                                                  NYC_subpops[1,5],
                                                                  NYC_subpops[1,5],
                                                                  NYC_subpops[1,5]))
ny_memb_subpops <- ny_memb_subpops %>% 
  select(school_name, perc_lep, sys_lep, std_lep, perc_swd, sys_swd, std_swd)
write.csv(ny_memb_subpops, file = file.path('ny_subpops_schooloftheyear.csv'))

#--------
# DC race and gender
dc_subpops <- subpops_memb_states %>% 
  filter(state == "DC") 
View(dc_subpops)
dc_racegender <- memb_states_racegender %>% 
  filter(state == "DC") %>% 
  arrange(district, school_name)
dc_dist_racegender <- dc_racegender %>% # comparative stats for 200+ DC schools
  summarize(mean_amind = mean(perc_amind),
            sd_amind = sd(perc_amind),
            mean_asian = mean(perc_asian),
            sd_asian = sd(perc_asian),
            mean_hawpi = mean(perc_hawpi),
            sd_hawpi = sd(perc_hawpi),
            mean_black = mean(perc_black),
            sd_black = sd(perc_black),
            mean_hisp = mean(perc_hisp),
            sd_hisp = sd(perc_hisp),
            mean_white = mean(perc_white),
            sd_white = sd(perc_white),
            mean_multiple = mean(perc_multiple),
            sd_multiple = sd(perc_multiple),
            n = n())
dc_memb_racegender <- dc_racegender[c(149:151,173),] # pull out rows for members
dc_memb_racegender <- dc_memb_racegender %>% 
  arrange(school_name) %>% 
  select(school_name, total, perc_amind, perc_asian, perc_hawpi, perc_black, perc_hisp, perc_white, perc_multiple)
names(dc_dist_racegender)
dc_memb_racegender$sys_amind = as.numeric(c(dc_dist_racegender[1,1],dc_dist_racegender[1,1],
                                            dc_dist_racegender[1,1],dc_dist_racegender[1,1]))
dc_memb_racegender$sys_asian = as.numeric(c(dc_dist_racegender[1,3],dc_dist_racegender[1,3],
                                            dc_dist_racegender[1,3],dc_dist_racegender[1,3]))
dc_memb_racegender$sys_hawpi = as.numeric(c(dc_dist_racegender[1,5],dc_dist_racegender[1,5],
                                            dc_dist_racegender[1,5],dc_dist_racegender[1,5]))
dc_memb_racegender$sys_black = as.numeric(c(dc_dist_racegender[1,7],dc_dist_racegender[1,7],
                                            dc_dist_racegender[1,7],dc_dist_racegender[1,7]))
dc_memb_racegender$sys_hisp = as.numeric(c(dc_dist_racegender[1,9],dc_dist_racegender[1,9],
                                           dc_dist_racegender[1,9],dc_dist_racegender[1,9]))
dc_memb_racegender$sys_white = as.numeric(c(dc_dist_racegender[1,11],dc_dist_racegender[1,11],
                                            dc_dist_racegender[1,11],dc_dist_racegender[1,11]))
dc_memb_racegender$sys_multiple = as.numeric(c(dc_dist_racegender[1,13],dc_dist_racegender[1,13],
                                               dc_dist_racegender[1,13],dc_dist_racegender[1,13]))
dc_memb_racegender <- dc_memb_racegender %>% 
  mutate(diff_amind = perc_amind - sys_amind,
         diff_asian = perc_asian - sys_asian,
         diff_hawpi = perc_hawpi - sys_hawpi,
         diff_black = perc_black - sys_black,
         diff_hisp = perc_hisp - sys_hisp,
         diff_white = perc_white - sys_white,
         diff_multiple = perc_multiple - sys_multiple)
names(dc_dist_racegender)
dc_memb_racegender$std_amind = dc_memb_racegender$diff_amind / as.numeric(c(dc_dist_racegender[1,2],
                                                                            dc_dist_racegender[1,2],
                                                                            dc_dist_racegender[1,2],
                                                                            dc_dist_racegender[1,2]))
dc_memb_racegender$std_asian = dc_memb_racegender$diff_asian / as.numeric(c(dc_dist_racegender[1,4],
                                                                            dc_dist_racegender[1,4],
                                                                            dc_dist_racegender[1,4],
                                                                            dc_dist_racegender[1,4]))
dc_memb_racegender$std_hawpi = dc_memb_racegender$diff_hawpi / as.numeric(c(dc_dist_racegender[1,6],
                                                                            dc_dist_racegender[1,6],
                                                                            dc_dist_racegender[1,6],
                                                                            dc_dist_racegender[1,6]))
dc_memb_racegender$std_black = dc_memb_racegender$diff_black / as.numeric(c(dc_dist_racegender[1,8],
                                                                            dc_dist_racegender[1,8],
                                                                            dc_dist_racegender[1,8],
                                                                            dc_dist_racegender[1,8]))
dc_memb_racegender$std_hisp = dc_memb_racegender$diff_hisp / as.numeric(c(dc_dist_racegender[1,10],
                                                                          dc_dist_racegender[1,10],
                                                                          dc_dist_racegender[1,10],
                                                                          dc_dist_racegender[1,10]))
dc_memb_racegender$std_white = dc_memb_racegender$diff_white / as.numeric(c(dc_dist_racegender[1,12],
                                                                            dc_dist_racegender[1,12],
                                                                            dc_dist_racegender[1,12],
                                                                            dc_dist_racegender[1,12]))
dc_memb_racegender$std_multiple = dc_memb_racegender$diff_multiple / as.numeric(c(dc_dist_racegender[1,14],
                                                                                  dc_dist_racegender[1,14],
                                                                                  dc_dist_racegender[1,14],
                                                                                  dc_dist_racegender[1,14]))
dc_memb_racegender <- dc_memb_racegender %>% 
  select(school_name, total, perc_amind, sys_amind,std_amind,
         perc_asian, sys_asian, std_asian,
         perc_black, sys_black, std_black,
         perc_hisp, sys_hisp, std_hisp,
         perc_white, sys_white, std_white,
         perc_multiple, sys_multiple, std_multiple)
write.csv(dc_memb_racegender, file = file.path('dc_schooloftheyear.csv'))

#--------
# Colorado race and gender
co_subpops <- subpops_memb_states %>% 
  filter(state == "CO") 
View(co_subpops)
co_racegender <- memb_states_racegender %>% 
  filter(state == "CO") %>% 
  arrange(district, school_name)
co_dist_racegender <- co_racegender %>% # comparative stats for Colorado districts
  group_by(district) %>% 
  summarize(mean_amind = mean(perc_amind),
            sd_amind = sd(perc_amind),
            mean_asian = mean(perc_asian),
            sd_asian = sd(perc_asian),
            mean_hawpi = mean(perc_hawpi),
            sd_hawpi = sd(perc_hawpi),
            mean_black = mean(perc_black),
            sd_black = sd(perc_black),
            mean_hisp = mean(perc_hisp),
            sd_hisp = sd(perc_hisp),
            mean_white = mean(perc_white),
            sd_white = sd(perc_white),
            mean_multiple = mean(perc_multiple),
            sd_multiple = sd(perc_multiple),
            n = n())
rownames(co_dist_racegender) = co_dist_racegender$district
co_dist_racegender <- co_dist_racegender[c("School District No. 1 in the county of Denver and State of C"),]
co_memb_racegender <- co_racegender[c(1371:1380),] # pull out rows for members
co_memb_racegender <- co_memb_racegender %>% 
  arrange(school_name) %>% 
  select(school_name, total, perc_amind, perc_asian, perc_hawpi, perc_black, perc_hisp, perc_white, perc_multiple)
names(co_dist_racegender)
co_memb_racegender$sys_amind = as.numeric(c(co_dist_racegender[1,2],co_dist_racegender[1,2],
                                            co_dist_racegender[1,2],co_dist_racegender[1,2],
                                            co_dist_racegender[1,2],co_dist_racegender[1,2],
                                            co_dist_racegender[1,2],co_dist_racegender[1,2],
                                            co_dist_racegender[1,2],co_dist_racegender[1,2]))
co_memb_racegender$sys_asian = as.numeric(c(co_dist_racegender[1,4],co_dist_racegender[1,4],
                                            co_dist_racegender[1,4],co_dist_racegender[1,4],
                                            co_dist_racegender[1,4],co_dist_racegender[1,4],
                                            co_dist_racegender[1,4],co_dist_racegender[1,4],
                                            co_dist_racegender[1,4],co_dist_racegender[1,4]))
co_memb_racegender$sys_hawpi = as.numeric(c(co_dist_racegender[1,6],co_dist_racegender[1,6],
                                            co_dist_racegender[1,6],co_dist_racegender[1,6],
                                            co_dist_racegender[1,6],co_dist_racegender[1,6],
                                            co_dist_racegender[1,6],co_dist_racegender[1,6],
                                            co_dist_racegender[1,6],co_dist_racegender[1,6]))
co_memb_racegender$sys_black = as.numeric(c(co_dist_racegender[1,8],co_dist_racegender[1,8],
                                            co_dist_racegender[1,8],co_dist_racegender[1,8],
                                            co_dist_racegender[1,8],co_dist_racegender[1,8],
                                            co_dist_racegender[1,8],co_dist_racegender[1,8],
                                            co_dist_racegender[1,8],co_dist_racegender[1,8]))
co_memb_racegender$sys_hisp = as.numeric(c(co_dist_racegender[1,10],co_dist_racegender[1,10],
                                           co_dist_racegender[1,10],co_dist_racegender[1,10],
                                           co_dist_racegender[1,10],co_dist_racegender[1,10],
                                           co_dist_racegender[1,10],co_dist_racegender[1,10],
                                           co_dist_racegender[1,10],co_dist_racegender[1,10]))
co_memb_racegender$sys_white = as.numeric(c(co_dist_racegender[1,12],co_dist_racegender[1,12],
                                            co_dist_racegender[1,12],co_dist_racegender[1,12],
                                            co_dist_racegender[1,12],co_dist_racegender[1,12],
                                            co_dist_racegender[1,12],co_dist_racegender[1,12],
                                            co_dist_racegender[1,12],co_dist_racegender[1,12]))
co_memb_racegender$sys_multiple = as.numeric(c(co_dist_racegender[1,14],co_dist_racegender[1,14],
                                               co_dist_racegender[1,14],co_dist_racegender[1,14],
                                               co_dist_racegender[1,14],co_dist_racegender[1,14],
                                               co_dist_racegender[1,14],co_dist_racegender[1,14],
                                               co_dist_racegender[1,14],co_dist_racegender[1,14]))
co_memb_racegender <- co_memb_racegender %>% 
  mutate(diff_amind = perc_amind - sys_amind,
         diff_asian = perc_asian - sys_asian,
         diff_hawpi = perc_hawpi - sys_hawpi,
         diff_black = perc_black - sys_black,
         diff_hisp = perc_hisp - sys_hisp,
         diff_white = perc_white - sys_white,
         diff_multiple = perc_multiple - sys_multiple)
names(co_dist_racegender)
co_memb_racegender$std_amind = co_memb_racegender$diff_amind / as.numeric(c(co_dist_racegender[1,3],co_dist_racegender[1,3],
                                                                            co_dist_racegender[1,3],co_dist_racegender[1,3],
                                                                            co_dist_racegender[1,3],co_dist_racegender[1,3],
                                                                            co_dist_racegender[1,3],co_dist_racegender[1,3],
                                                                            co_dist_racegender[1,3],co_dist_racegender[1,3]))
co_memb_racegender$std_asian = co_memb_racegender$diff_asian / as.numeric(c(co_dist_racegender[1,5],co_dist_racegender[1,5],
                                                                            co_dist_racegender[1,5],co_dist_racegender[1,5],
                                                                            co_dist_racegender[1,5],co_dist_racegender[1,5],
                                                                            co_dist_racegender[1,5],co_dist_racegender[1,5],
                                                                            co_dist_racegender[1,5],co_dist_racegender[1,5]))
co_memb_racegender$std_hawpi = co_memb_racegender$diff_hawpi / as.numeric(c(co_dist_racegender[1,7],co_dist_racegender[1,7],
                                                                            co_dist_racegender[1,7],co_dist_racegender[1,7],
                                                                            co_dist_racegender[1,7],co_dist_racegender[1,7],
                                                                            co_dist_racegender[1,7],co_dist_racegender[1,7],
                                                                            co_dist_racegender[1,7],co_dist_racegender[1,7]))
co_memb_racegender$std_black = co_memb_racegender$diff_black / as.numeric(c(co_dist_racegender[1,9],co_dist_racegender[1,9],
                                                                            co_dist_racegender[1,9],co_dist_racegender[1,9],
                                                                            co_dist_racegender[1,9],co_dist_racegender[1,9],
                                                                            co_dist_racegender[1,9],co_dist_racegender[1,9],
                                                                            co_dist_racegender[1,9],co_dist_racegender[1,9]))
co_memb_racegender$std_hisp = co_memb_racegender$diff_hisp / as.numeric(c(co_dist_racegender[1,11],co_dist_racegender[1,11],
                                                                          co_dist_racegender[1,11],co_dist_racegender[1,11],
                                                                          co_dist_racegender[1,11],co_dist_racegender[1,11],
                                                                          co_dist_racegender[1,11],co_dist_racegender[1,11],
                                                                          co_dist_racegender[1,11],co_dist_racegender[1,11]))
co_memb_racegender$std_white = co_memb_racegender$diff_white / as.numeric(c(co_dist_racegender[1,13],co_dist_racegender[1,13],
                                                                            co_dist_racegender[1,13],co_dist_racegender[1,13],
                                                                            co_dist_racegender[1,13],co_dist_racegender[1,13],
                                                                            co_dist_racegender[1,13],co_dist_racegender[1,13],
                                                                            co_dist_racegender[1,13],co_dist_racegender[1,13]))
co_memb_racegender$std_multiple = co_memb_racegender$diff_multiple / as.numeric(c(co_dist_racegender[1,15],co_dist_racegender[1,15],
                                                                                  co_dist_racegender[1,15],co_dist_racegender[1,15],
                                                                                  co_dist_racegender[1,15],co_dist_racegender[1,15],
                                                                                  co_dist_racegender[1,15],co_dist_racegender[1,15],
                                                                                  co_dist_racegender[1,15],co_dist_racegender[1,15]))
co_memb_racegender <- co_memb_racegender %>% 
  select(school_name, total, perc_amind, sys_amind,std_amind,
         perc_asian, sys_asian, std_asian,
         perc_black, sys_black, std_black,
         perc_hisp, sys_hisp, std_hisp,
         perc_white, sys_white, std_white,
         perc_multiple, sys_multiple, std_multiple)
write.csv(co_memb_racegender, file = file.path('co_schooloftheyear.csv'))

#------
# Colorado SWD and LEP
co_memb_subpops <- co_subpops[c(1371:1380),]
View(co_memb_subpops)
denver_subpops <- co_subpops %>% 
  group_by(district) %>% 
  summarize(mean_swd = mean(perc_swd),
            sd_swd = mean(perc_lep),
            mean_lep = mean(perc_lep),
            sd_lep = sd(perc_lep))
rownames(denver_subpops) <- denver_subpops$district
denver_subpops <- denver_subpops[c("School District No. 1 in the county of Denver and State of C"),]
names(denver_subpops)
co_memb_subpops$sys_swd = as.numeric(c(denver_subpops[1,2],denver_subpops[1,2],denver_subpops[1,2],
                                       denver_subpops[1,2],denver_subpops[1,2],denver_subpops[1,2],
                                       denver_subpops[1,2],denver_subpops[1,2],denver_subpops[1,2],
                                       denver_subpops[1,2]))
co_memb_subpops$sys_lep = as.numeric(c(denver_subpops[1,4],denver_subpops[1,4],denver_subpops[1,4],
                                       denver_subpops[1,4],denver_subpops[1,4],denver_subpops[1,4],
                                       denver_subpops[1,4],denver_subpops[1,4],denver_subpops[1,4],
                                       denver_subpops[1,4]))
co_memb_subpops <- co_memb_subpops %>% 
  select(school_name, perc_swd,perc_lep,sys_swd,sys_lep) %>% 
  mutate(diff_swd = perc_swd - sys_swd,
         diff_lep = perc_lep - sys_lep)
co_memb_subpops$std_swd = co_memb_subpops$diff_swd / as.numeric(c(denver_subpops[1,3],denver_subpops[1,3],
                                                                  denver_subpops[1,3],denver_subpops[1,3],
                                                                  denver_subpops[1,3], denver_subpops[1,3],
                                                                  denver_subpops[1,3], denver_subpops[1,3],
                                                                  denver_subpops[1,3], denver_subpops[1,3]))
co_memb_subpops$std_lep = co_memb_subpops$diff_lep / as.numeric(c(denver_subpops[1,5],denver_subpops[1,5],
                                                                   denver_subpops[1,5],denver_subpops[1,5],
                                                                   denver_subpops[1,5], denver_subpops[1,5],
                                                                   denver_subpops[1,5], denver_subpops[1,5],
                                                                   denver_subpops[1,5], denver_subpops[1,5]))
co_memb_subpops <- co_memb_subpops %>% 
  select(school_name, perc_lep, sys_lep, std_lep, perc_swd, sys_swd, std_swd)
write.csv(co_memb_subpops, file = file.path('co_subpops_schooloftheyear.csv'))
#------
# Georgia race and gender
ga_subpops <- subpops_memb_states %>% 
  filter(state == "GA") 
View(ga_subpops)
ga_racegender <- memb_states_racegender %>% 
  filter(state == "GA") %>% 
  arrange(district, school_name)
ga_dist_racegender <- ga_racegender %>% # comparative stats for Colorado districts
  group_by(district) %>% 
  summarize(mean_amind = mean(perc_amind),
            sd_amind = sd(perc_amind),
            mean_asian = mean(perc_asian),
            sd_asian = sd(perc_asian),
            mean_hawpi = mean(perc_hawpi),
            sd_hawpi = sd(perc_hawpi),
            mean_black = mean(perc_black),
            sd_black = sd(perc_black),
            mean_hisp = mean(perc_hisp),
            sd_hisp = sd(perc_hisp),
            mean_white = mean(perc_white),
            sd_white = sd(perc_white),
            mean_multiple = mean(perc_multiple),
            sd_multiple = sd(perc_multiple),
            n = n())
rownames(ga_dist_racegender) = ga_dist_racegender$district
ga_dist_racegender <- ga_dist_racegender[c("Atlanta Public Schools"),]
ga_memb_racegender <- ga_racegender[c(14:15),] # pull out rows for members
ga_memb_racegender <- ga_memb_racegender %>% 
  arrange(school_name) %>% 
  select(school_name, total, perc_amind, perc_asian, perc_hawpi, perc_black, perc_hisp, perc_white, perc_multiple)
names(ga_dist_racegender)
ga_memb_racegender$sys_amind = as.numeric(c(ga_dist_racegender[1,2],ga_dist_racegender[1,2]))
ga_memb_racegender$sys_asian = as.numeric(c(ga_dist_racegender[1,4],ga_dist_racegender[1,4]))
ga_memb_racegender$sys_hawpi = as.numeric(c(ga_dist_racegender[1,6],ga_dist_racegender[1,6]))
ga_memb_racegender$sys_black = as.numeric(c(ga_dist_racegender[1,8],ga_dist_racegender[1,8]))
ga_memb_racegender$sys_hisp = as.numeric(c(ga_dist_racegender[1,10],ga_dist_racegender[1,10]))
ga_memb_racegender$sys_white = as.numeric(c(ga_dist_racegender[1,12],ga_dist_racegender[1,12]))
ga_memb_racegender$sys_multiple = as.numeric(c(ga_dist_racegender[1,14],ga_dist_racegender[1,14]))
ga_memb_racegender <- ga_memb_racegender %>% 
  mutate(diff_amind = perc_amind - sys_amind,
         diff_asian = perc_asian - sys_asian,
         diff_hawpi = perc_hawpi - sys_hawpi,
         diff_black = perc_black - sys_black,
         diff_hisp = perc_hisp - sys_hisp,
         diff_white = perc_white - sys_white,
         diff_multiple = perc_multiple - sys_multiple)
names(ga_dist_racegender)
ga_memb_racegender$std_amind = ga_memb_racegender$diff_amind / as.numeric(c(ga_dist_racegender[1,3],ga_dist_racegender[1,3]))
ga_memb_racegender$std_asian = ga_memb_racegender$diff_asian / as.numeric(c(ga_dist_racegender[1,5],ga_dist_racegender[1,5]))
ga_memb_racegender$std_hawpi = ga_memb_racegender$diff_hawpi / as.numeric(c(ga_dist_racegender[1,7],ga_dist_racegender[1,7]))
ga_memb_racegender$std_black = ga_memb_racegender$diff_black / as.numeric(c(ga_dist_racegender[1,9],ga_dist_racegender[1,9]))
ga_memb_racegender$std_hisp = ga_memb_racegender$diff_hisp / as.numeric(c(ga_dist_racegender[1,11],ga_dist_racegender[1,11]))
ga_memb_racegender$std_white = ga_memb_racegender$diff_white / as.numeric(c(ga_dist_racegender[1,13],ga_dist_racegender[1,13]))
ga_memb_racegender$std_multiple = ga_memb_racegender$diff_multiple / as.numeric(c(ga_dist_racegender[1,15],ga_dist_racegender[1,15]))
ga_memb_racegender <- ga_memb_racegender %>% 
  select(school_name, total, perc_amind, sys_amind,std_amind,
         perc_asian, sys_asian, std_asian,
         perc_black, sys_black, std_black,
         perc_hisp, sys_hisp, std_hisp,
         perc_white, sys_white, std_white,
         perc_multiple, sys_multiple, std_multiple)
write.csv(ga_memb_racegender, file = file.path('ga_schooloftheyear.csv'))

#------
# Georgia LEP, SWD
ga_memb_subpops <- ga_subpops[c(14:15),]
View(ga_memb_subpops)
atl_subpops <- ga_subpops %>% 
  group_by(district) %>% 
  summarize(mean_swd = mean(perc_swd),
            sd_swd = mean(perc_lep),
            mean_lep = mean(perc_lep),
            sd_lep = sd(perc_lep))
atl_subpops <- atl_subpops[c("Atlanta Public Schools"),]
names(atl_subpops)
ga_memb_subpops$sys_swd = as.numeric(c(atl_subpops[1,2],atl_subpops[1,2]))
ga_memb_subpops$sys_lep = as.numeric(c(atl_subpops[1,4],atl_subpops[1,4]))
ga_memb_subpops <- ga_memb_subpops %>% 
  select(school_name, perc_swd,perc_lep,sys_swd,sys_lep) %>% 
  mutate(diff_swd = perc_swd - sys_swd,
         diff_lep = perc_lep - sys_lep)
ga_memb_subpops$std_swd = ga_memb_subpops$diff_swd / as.numeric(c(atl_subpops[1,3],atl_subpops[1,3]))
ga_memb_subpops$std_lep = ga_memb_subpops$diff_lep / as.numeric(c(atl_subpops[1,5],atl_subpops[1,5]))
ga_memb_subpops <- ga_memb_subpops %>% 
  select(school_name, perc_lep, sys_lep, std_lep, perc_swd, sys_swd, std_swd)
write.csv(ga_memb_subpops, file = file.path('ga_subpops_schooloftheyear.csv'))

#-------
# Massachusetts gender and race
ma_subpops <- subpops_memb_states %>% 
  filter(state == "MA") 
View(ma_subpops)
ma_racegender <- memb_states_racegender %>% 
  filter(state == "MA") %>% 
  arrange(district, school_name)
ma_dist_racegender <- ma_racegender %>% # comparative stats for Massachusetts districts
  group_by(district) %>% 
  summarize(mean_amind = mean(perc_amind),
            sd_amind = sd(perc_amind),
            mean_asian = mean(perc_asian),
            sd_asian = sd(perc_asian),
            mean_hawpi = mean(perc_hawpi),
            sd_hawpi = sd(perc_hawpi),
            mean_black = mean(perc_black),
            sd_black = sd(perc_black),
            mean_hisp = mean(perc_hisp),
            sd_hisp = sd(perc_hisp),
            mean_white = mean(perc_white),
            sd_white = sd(perc_white),
            mean_multiple = mean(perc_multiple),
            sd_multiple = sd(perc_multiple),
            n = n())
rownames(ma_dist_racegender) = ma_dist_racegender$district
ma_dist_racegender <- ma_dist_racegender[c("Boston"),]
ma_memb_racegender <- ma_racegender[c(292),] # pull out rows for members
ma_memb_racegender <- ma_memb_racegender %>% 
  arrange(school_name) %>% 
  select(school_name, total, perc_amind, perc_asian, perc_hawpi, perc_black, perc_hisp, perc_white, perc_multiple)
names(ma_dist_racegender)
ma_memb_racegender$sys_amind = as.numeric(c(ma_dist_racegender[1,2]))
ma_memb_racegender$sys_asian = as.numeric(c(ma_dist_racegender[1,4]))
ma_memb_racegender$sys_hawpi = as.numeric(c(ma_dist_racegender[1,6]))
ma_memb_racegender$sys_black = as.numeric(c(ma_dist_racegender[1,8]))
ma_memb_racegender$sys_hisp = as.numeric(c(ma_dist_racegender[1,10]))
ma_memb_racegender$sys_white = as.numeric(c(ma_dist_racegender[1,12]))
ma_memb_racegender$sys_multiple = as.numeric(c(ma_dist_racegender[1,14]))
ma_memb_racegender <- ma_memb_racegender %>% 
  mutate(diff_amind = perc_amind - sys_amind,
         diff_asian = perc_asian - sys_asian,
         diff_hawpi = perc_hawpi - sys_hawpi,
         diff_black = perc_black - sys_black,
         diff_hisp = perc_hisp - sys_hisp,
         diff_white = perc_white - sys_white,
         diff_multiple = perc_multiple - sys_multiple)
names(ma_dist_racegender)
ma_memb_racegender$std_amind = ma_memb_racegender$diff_amind / as.numeric(c(ma_dist_racegender[1,3]))
ma_memb_racegender$std_asian = ma_memb_racegender$diff_asian / as.numeric(c(ma_dist_racegender[1,5]))
ma_memb_racegender$std_hawpi = ma_memb_racegender$diff_hawpi / as.numeric(c(ma_dist_racegender[1,7]))
ma_memb_racegender$std_black = ma_memb_racegender$diff_black / as.numeric(c(ma_dist_racegender[1,9]))
ma_memb_racegender$std_hisp = ma_memb_racegender$diff_hisp / as.numeric(c(ma_dist_racegender[1,11]))
ma_memb_racegender$std_white = ma_memb_racegender$diff_white / as.numeric(c(ma_dist_racegender[1,13]))
ma_memb_racegender$std_multiple = ma_memb_racegender$diff_multiple / as.numeric(c(ma_dist_racegender[1,15]))
ma_memb_racegender <- ma_memb_racegender %>% 
  select(school_name, total, perc_amind, sys_amind,std_amind,
         perc_asian, sys_asian, std_asian,
         perc_black, sys_black, std_black,
         perc_hisp, sys_hisp, std_hisp,
         perc_white, sys_white, std_white,
         perc_multiple, sys_multiple, std_multiple)
write.csv(ma_memb_racegender, file = file.path('ma_schooloftheyear.csv'))

#-------
# Massachusetts SWD, LEP
ma_memb_subpops <- ma_subpops[c(292),]
View(ma_memb_subpops)
bos_subpops <- ma_subpops %>% 
  group_by(district) %>% 
  summarize(mean_swd = mean(perc_swd),
            sd_swd = mean(perc_lep),
            mean_lep = mean(perc_lep),
            sd_lep = sd(perc_lep))
bos_subpops <- bos_subpops[c(42),]
names(bos_subpops)
ma_memb_subpops$sys_swd = as.numeric(c(bos_subpops[1,2]))
ma_memb_subpops$sys_lep = as.numeric(c(bos_subpops[1,4]))
ma_memb_subpops <- ma_memb_subpops %>% 
  select(school_name, perc_swd,perc_lep,sys_swd,sys_lep) %>% 
  mutate(diff_swd = perc_swd - sys_swd,
         diff_lep = perc_lep - sys_lep)
ma_memb_subpops$std_swd = ma_memb_subpops$diff_swd / as.numeric(c(bos_subpops[1,3]))
ma_memb_subpops$std_lep = ma_memb_subpops$diff_lep / as.numeric(c(bos_subpops[1,5]))
ma_memb_subpops <- ma_memb_subpops %>% 
  select(school_name, perc_lep, sys_lep, std_lep, perc_swd, sys_swd, std_swd)
write.csv(ma_memb_subpops, file = file.path('ma_subpops_schooloftheyear.csv'))
summary(ma_racegender)
z <- ma_racegender %>% 
  filter(district == "Boston") %>% 
  select(state, amind, asian, black, hisp, white, multiple,total)
install.packages("data.table")
library(data.table)
DT <- data.table(z)
z <- DT[, lapply(.SD, sum),by=list(state)]
View(z)
z %>% 
  mutate(perc_amind = amind / total,
         perc_asian = asian / total,
         perc_black = black / total,
         perc_hisp = hisp / total,
         perc_white = white / total,
         perc_mult = multiple / total
         
         )

#------------
# # Maryland gender and race
md_subpops <- subpops_memb_states %>% 
  filter(state == "MD") 
View(md_subpops)
md_racegender <- memb_states_racegender %>% 
  filter(state == "MD") %>% 
  arrange(district, school_name)
md_dist_racegender <- md_racegender %>% # comparative stats for Maryland districts
  group_by(district) %>% 
  summarize(mean_amind = mean(perc_amind),
            sd_amind = sd(perc_amind),
            mean_asian = mean(perc_asian),
            sd_asian = sd(perc_asian),
            mean_hawpi = mean(perc_hawpi),
            sd_hawpi = sd(perc_hawpi),
            mean_black = mean(perc_black),
            sd_black = sd(perc_black),
            mean_hisp = mean(perc_hisp),
            sd_hisp = sd(perc_hisp),
            mean_white = mean(perc_white),
            sd_white = sd(perc_white),
            mean_multiple = mean(perc_multiple),
            sd_multiple = sd(perc_multiple),
            n = n())
rownames(md_dist_racegender) = md_dist_racegender$district
md_dist_racegender <- md_dist_racegender[c("Baltimore City Public Schools"),]
md_memb_racegender <- md_racegender[c(182:184),] # pull out rows for members
md_memb_racegender <- md_memb_racegender %>% 
  arrange(school_name) %>% 
  select(school_name, total, perc_amind, perc_asian, perc_hawpi, perc_black, perc_hisp, perc_white, perc_multiple)
names(md_dist_racegender)
md_memb_racegender$sys_amind = as.numeric(c(md_dist_racegender[1,2],
                                            md_dist_racegender[1,2],
                                            md_dist_racegender[1,2]))
md_memb_racegender$sys_asian = as.numeric(c(md_dist_racegender[1,4],
                                            md_dist_racegender[1,4],
                                            md_dist_racegender[1,4]))
md_memb_racegender$sys_hawpi = as.numeric(c(md_dist_racegender[1,6],
                                            md_dist_racegender[1,6],
                                            md_dist_racegender[1,6]))
md_memb_racegender$sys_black = as.numeric(c(md_dist_racegender[1,8],
                                            md_dist_racegender[1,8],
                                            md_dist_racegender[1,8]))
md_memb_racegender$sys_hisp = as.numeric(c(md_dist_racegender[1,10],
                                           md_dist_racegender[1,10],
                                           md_dist_racegender[1,10]))
md_memb_racegender$sys_white = as.numeric(c(md_dist_racegender[1,12],
                                            md_dist_racegender[1,12],
                                            md_dist_racegender[1,12]))
md_memb_racegender$sys_multiple = as.numeric(c(md_dist_racegender[1,14],
                                               md_dist_racegender[1,14],
                                               md_dist_racegender[1,14]))
md_memb_racegender <- md_memb_racegender %>% 
  mutate(diff_amind = perc_amind - sys_amind,
         diff_asian = perc_asian - sys_asian,
         diff_hawpi = perc_hawpi - sys_hawpi,
         diff_black = perc_black - sys_black,
         diff_hisp = perc_hisp - sys_hisp,
         diff_white = perc_white - sys_white,
         diff_multiple = perc_multiple - sys_multiple)
names(md_dist_racegender)
md_memb_racegender$std_amind = md_memb_racegender$diff_amind / as.numeric(c(md_dist_racegender[1,3],
                                                                            md_dist_racegender[1,3],
                                                                            md_dist_racegender[1,3]))
md_memb_racegender$std_asian = md_memb_racegender$diff_asian / as.numeric(c(md_dist_racegender[1,5],
                                                                            md_dist_racegender[1,5],
                                                                            md_dist_racegender[1,5]))
md_memb_racegender$std_hawpi = md_memb_racegender$diff_hawpi / as.numeric(c(md_dist_racegender[1,7],
                                                                            md_dist_racegender[1,7],
                                                                            md_dist_racegender[1,7]))
md_memb_racegender$std_black = md_memb_racegender$diff_black / as.numeric(c(md_dist_racegender[1,9],
                                                                            md_dist_racegender[1,9],
                                                                            md_dist_racegender[1,9]))
md_memb_racegender$std_hisp = md_memb_racegender$diff_hisp / as.numeric(c(md_dist_racegender[1,11],
                                                                          md_dist_racegender[1,11],
                                                                          md_dist_racegender[1,11]))
md_memb_racegender$std_white = md_memb_racegender$diff_white / as.numeric(c(md_dist_racegender[1,13],
                                                                            md_dist_racegender[1,13],
                                                                            md_dist_racegender[1,13]))
md_memb_racegender$std_multiple = md_memb_racegender$diff_multiple / as.numeric(c(md_dist_racegender[1,15],
                                                                                  md_dist_racegender[1,15],
                                                                                  md_dist_racegender[1,15]))
md_memb_racegender <- md_memb_racegender %>% 
  select(school_name, total, perc_amind, sys_amind,std_amind,
         perc_asian, sys_asian, std_asian,
         perc_black, sys_black, std_black,
         perc_hisp, sys_hisp, std_hisp,
         perc_white, sys_white, std_white,
         perc_multiple, sys_multiple, std_multiple)
write.csv(md_memb_racegender, file = file.path('md_schooloftheyear.csv'))

#-------
# Maryland SWD, LEP
md_memb_subpops <- md_subpops[c(182:184),]
View(md_memb_subpops)
balt_subpops <- md_subpops %>% 
  group_by(district) %>% 
  summarize(mean_swd = mean(perc_swd),
            sd_swd = mean(perc_lep),
            mean_lep = mean(perc_lep),
            sd_lep = sd(perc_lep))
balt_subpops <- balt_subpops[c(3),]
names(balt_subpops)
md_memb_subpops$sys_swd = as.numeric(c(balt_subpops[1,2],
                                       balt_subpops[1,2],
                                       balt_subpops[1,2]))
md_memb_subpops$sys_lep = as.numeric(c(balt_subpops[1,4],
                                       balt_subpops[1,4],
                                       balt_subpops[1,4]))
md_memb_subpops <- md_memb_subpops %>% 
  select(school_name, perc_swd,perc_lep,sys_swd,sys_lep) %>% 
  mutate(diff_swd = perc_swd - sys_swd,
         diff_lep = perc_lep - sys_lep)
md_memb_subpops$std_swd = md_memb_subpops$diff_swd / as.numeric(c(balt_subpops[1,3],
                                                                  balt_subpops[1,3],
                                                                  balt_subpops[1,3]))
md_memb_subpops$std_lep = md_memb_subpops$diff_lep / as.numeric(c(balt_subpops[1,5],
                                                                  balt_subpops[1,5],
                                                                  balt_subpops[1,5]))
md_memb_subpops <- md_memb_subpops %>% 
  select(school_name, perc_lep, sys_lep, std_lep, perc_swd, sys_swd, std_swd)
write.csv(md_memb_subpops, file = file.path('md_subpops_schooloftheyear.csv'))

#---------
# Missouri race gender data
mo_subpops <- subpops_memb_states %>% 
  filter(state == "MO") 
View(mo_subpops)
mo_racegender <- memb_states_racegender %>% 
  filter(state == "MO") %>% 
  arrange(district, school_name)
mo_dist_racegender <- mo_racegender %>% # comparative stats for Missouri districts
  group_by(district) %>% 
  summarize(mean_amind = mean(perc_amind),
            sd_amind = sd(perc_amind),
            mean_asian = mean(perc_asian),
            sd_asian = sd(perc_asian),
            mean_hawpi = mean(perc_hawpi),
            sd_hawpi = sd(perc_hawpi),
            mean_black = mean(perc_black),
            sd_black = sd(perc_black),
            mean_hisp = mean(perc_hisp),
            sd_hisp = sd(perc_hisp),
            mean_white = mean(perc_white),
            sd_white = sd(perc_white),
            mean_multiple = mean(perc_multiple),
            sd_multiple = sd(perc_multiple),
            n = n())
rownames(mo_dist_racegender) = mo_dist_racegender$district
mo_dist_racegender <- mo_dist_racegender[c("ST. LOUIS CITY"),]
mo_memb_racegender <- mo_racegender[c(270),] # pull out rows for members
mo_memb_racegender <- mo_memb_racegender %>% 
  arrange(school_name) %>% 
  select(school_name, total, perc_amind, perc_asian, perc_hawpi, perc_black, perc_hisp, perc_white, perc_multiple)
names(mo_dist_racegender)
mo_memb_racegender$sys_amind = as.numeric(c(mo_dist_racegender[1,2]))
mo_memb_racegender$sys_asian = as.numeric(c(mo_dist_racegender[1,4]))
mo_memb_racegender$sys_hawpi = as.numeric(c(mo_dist_racegender[1,6]))
mo_memb_racegender$sys_black = as.numeric(c(mo_dist_racegender[1,8]))
mo_memb_racegender$sys_hisp = as.numeric(c(mo_dist_racegender[1,10]))
mo_memb_racegender$sys_white = as.numeric(c(mo_dist_racegender[1,12]))
mo_memb_racegender$sys_multiple = as.numeric(c(mo_dist_racegender[1,14]))
mo_memb_racegender <- mo_memb_racegender %>% 
  mutate(diff_amind = perc_amind - sys_amind,
         diff_asian = perc_asian - sys_asian,
         diff_hawpi = perc_hawpi - sys_hawpi,
         diff_black = perc_black - sys_black,
         diff_hisp = perc_hisp - sys_hisp,
         diff_white = perc_white - sys_white,
         diff_multiple = perc_multiple - sys_multiple)
names(mo_dist_racegender)
mo_memb_racegender$std_amind = mo_memb_racegender$diff_amind / as.numeric(c(mo_dist_racegender[1,3]))
mo_memb_racegender$std_asian = mo_memb_racegender$diff_asian / as.numeric(c(mo_dist_racegender[1,5]))
mo_memb_racegender$std_hawpi = mo_memb_racegender$diff_hawpi / as.numeric(c(mo_dist_racegender[1,7]))
mo_memb_racegender$std_black = mo_memb_racegender$diff_black / as.numeric(c(mo_dist_racegender[1,9]))
mo_memb_racegender$std_hisp = mo_memb_racegender$diff_hisp / as.numeric(c(mo_dist_racegender[1,11]))
mo_memb_racegender$std_white = mo_memb_racegender$diff_white / as.numeric(c(mo_dist_racegender[1,13]))
mo_memb_racegender$std_multiple = mo_memb_racegender$diff_multiple / as.numeric(c(mo_dist_racegender[1,15]))
mo_memb_racegender <- mo_memb_racegender %>% 
  select(school_name, total, perc_amind, sys_amind,std_amind,
         perc_asian, sys_asian, std_asian,
         perc_black, sys_black, std_black,
         perc_hisp, sys_hisp, std_hisp,
         perc_white, sys_white, std_white,
         perc_multiple, sys_multiple, std_multiple)
write.csv(mo_memb_racegender, file = file.path('mo_schooloftheyear.csv'))


#----------
# Missouri LEP, SWD
mo_memb_subpops <- mo_subpops[c(270),]
View(mo_memb_subpops)
stl_subpops <- mo_subpops %>% 
  group_by(district) %>% 
  summarize(mean_swd = mean(perc_swd),
            sd_swd = mean(perc_lep),
            mean_lep = mean(perc_lep),
            sd_lep = sd(perc_lep))
stl_subpops <- stl_subpops[c(492),]
names(stl_subpops)
mo_memb_subpops$sys_swd = as.numeric(c(stl_subpops[1,2]))
mo_memb_subpops$sys_lep = as.numeric(c(stl_subpops[1,4]))
mo_memb_subpops <- mo_memb_subpops %>% 
  select(school_name, perc_swd,perc_lep,sys_swd,sys_lep) %>% 
  mutate(diff_swd = perc_swd - sys_swd,
         diff_lep = perc_lep - sys_lep)
mo_memb_subpops$std_swd = mo_memb_subpops$diff_swd / as.numeric(c(stl_subpops[1,3]))
mo_memb_subpops$std_lep = mo_memb_subpops$diff_lep / as.numeric(c(stl_subpops[1,5]))
mo_memb_subpops <- mo_memb_subpops %>% 
  select(school_name, perc_lep, sys_lep, std_lep, perc_swd, sys_swd, std_swd)
write.csv(mo_memb_subpops, file = file.path('mo_subpops_schooloftheyear.csv'))
