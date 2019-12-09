rm(list = ls()) # clear working environment
getwd() # check working directory
setwd("~/Downloads/States/")
install.packages("tidyverse") # install tidyverse
library(tidyverse)
install.packages("readxl") # CRAN version
library(readxl)
# ------------------------------------------------------------------------------------------------------------------------
# LOUISIANA
louisiana_sys <- read_excel("LA_enrl_2019-10-01.xlsx", sheet = "Total by School System") # Read Louisiana's data
louisiana_sys <- louisiana_sys[-c(1,2,3,4),]
colnames(louisiana_sys) = louisiana_sys[1, ]
louisiana_sys = louisiana_sys[-1, ]
View(louisiana_sys)
louisiana_school <- read_excel("LA_enrl_2019-10.xlsx", sheet = "Total by Site") # Separate by school tab
View(louisiana_school)
louisiana_school <- louisiana_school[-c(1,2,3,4),]
colnames(louisiana_school) = louisiana_school[1, ]
louisiana_school = louisiana_school[-1,]
louisiana_school <- select(louisiana_school, "School System Name",
                           "SiteName", "Total Students", "%Female","%Male",
                           "AmInd","Asian","Black","Hispanic","HawPI","White","Multiple",
                           "%LEP","ED%","PreK","Kindergarten","Grade1","Grade2","Grade3","Grade4","Grade5",
                           "Grade6","Grade7","Grade8","Grade9","GradeT9","Grade10","Grade11","Grade12")
names(louisiana_school)[1] <- "sys_name"
names(louisiana_school)[14] <- "%ED"
names(louisiana_school)[3] <- "total_stud"
names(louisiana_school) <- tolower(names(louisiana_school))
names(louisiana_school)
str(louisiana_school)
louisiana_school[,3:29] <- sapply(louisiana_school[,3:29],as.numeric)
louisiana_school <- louisiana_school %>% 
  mutate(perc_white = white / total_stud,
         perc_amind = amind / total_stud,
         perc_asian = asian / total_stud,
         perc_black = black / total_stud,
         perc_hispanic = hispanic / total_stud,
         perc_hawpi = hawpi / total_stud,
         perc_multiple = multiple / total_stud) %>% 
  distinct(sitename, .keep_all = TRUE)
View(louisiana_school)
row.names(louisiana_school) <- louisiana_school$sitename # name each row based on school name
louisiana_members <- louisiana_school[c("International High School of New Orleans",
                                        "International School of Louisiana",
                                        "Kenner Discovery Health Sciences Academy",
                                        "Lycee Francais de la Nouvelle-Orleans",
                                        "Morris Jeff Community School"),]
x <- louisiana_school %>% 
  group_by(sys_name) %>% 
  summarize(mean_stud = mean(total_stud),
            sd_stud = sd(total_stud),
            mean_amind = mean(perc_amind),
            sd_amind = sd(perc_amind),
            mean_asian = mean(perc_asian),
            sd_asian = sd(perc_asian),
            mean_black = mean(perc_black),
            sd_black = sd(perc_black),
            mean_hispanic = mean(perc_hispanic),
            sd_hispanic = sd(perc_hispanic),
            mean_hawpi = mean(perc_hawpi),
            sd_hapwi = sd(perc_hawpi),
            mean_white = mean(perc_white),
            sd_white = sd(perc_white),
            mean_multiple = mean(perc_multiple),
            sd_multiple = sd(perc_multiple),
            mean_ed = mean(`%ed`),
            sd_ed = sd(`%ed`),
            mean_lep = mean(`%lep`),
            sd_lep = sd(`%lep`),
            n = n()) %>% 
  distinct(sys_name, .keep_all = TRUE)
View(x)
x[82,1] <- "All of Louisiana"
x[82,"n"] <- sum(x[,"n"])-1
row.names(x) <- x$sys_name # name each row based on school system
louisiana_memsys <- x[c("Type 2 Charters","Jefferson Parish","Orleans Parish","All of Louisiana"),]
louisiana_members <- louisiana_members %>% select(sys_name,sitename,total_stud,
                             perc_amind,perc_asian,perc_black,perc_hispanic,
                             perc_hawpi,perc_white,perc_multiple,`%lep`,`%ed`) %>% 
  arrange(sys_name)
View(louisiana_memsys)
View(louisiana_members)
louisiana_memsys <- louisiana_memsys %>% 
  arrange(sys_name)
louisiana_members$state_total <- c(louisiana_memsys[1, 2],
                                   louisiana_memsys[1, 2],
                                   louisiana_memsys[1, 2],
                                   louisiana_memsys[1, 2],
                                   louisiana_memsys[1, 2])
louisiana_members$state_amind <- c(louisiana_memsys[1, 3],
                                   louisiana_memsys[1, 3],
                                   louisiana_memsys[1, 3],
                                   louisiana_memsys[1, 3],
                                   louisiana_memsys[1, 3])
louisiana_members$state_asian <- c(louisiana_memsys[1, 4],
                                   louisiana_memsys[1, 4],
                                   louisiana_memsys[1, 4],
                                   louisiana_memsys[1, 4],
                                   louisiana_memsys[1, 4])
louisiana_members$state_black <- c(louisiana_memsys[1, 5],
                                   louisiana_memsys[1, 5],
                                   louisiana_memsys[1, 5],
                                   louisiana_memsys[1, 5],
                                   louisiana_memsys[1, 5])
louisiana_members$state_hispanic <- c(louisiana_memsys[1, 6],
                                   louisiana_memsys[1, 6],
                                   louisiana_memsys[1, 6],
                                   louisiana_memsys[1, 6],
                                   louisiana_memsys[1, 6])
louisiana_members$state_hawpi <- c(louisiana_memsys[1, 7],
                                      louisiana_memsys[1, 7],
                                      louisiana_memsys[1, 7],
                                      louisiana_memsys[1, 7],
                                      louisiana_memsys[1, 7])
louisiana_members$state_white <- c(louisiana_memsys[1, 8],
                                   louisiana_memsys[1, 8],
                                   louisiana_memsys[1, 8],
                                   louisiana_memsys[1, 8],
                                   louisiana_memsys[1, 8])
louisiana_members$state_multiple <- c(louisiana_memsys[1, 9],
                                   louisiana_memsys[1, 9],
                                   louisiana_memsys[1, 9],
                                   louisiana_memsys[1, 9],
                                   louisiana_memsys[1, 9])
louisiana_members$state_ed <- c(louisiana_memsys[1, 10],
                                      louisiana_memsys[1, 10],
                                      louisiana_memsys[1, 10],
                                      louisiana_memsys[1, 10],
                                      louisiana_memsys[1, 10])
louisiana_members$state_lep <- c(louisiana_memsys[1, 11],
                                      louisiana_memsys[1, 11],
                                      louisiana_memsys[1, 11],
                                      louisiana_memsys[1, 11],
                                      louisiana_memsys[1, 11])
View(louisiana_memsys)
View(louisiana_members)
louisiana_members <- louisiana_members %>% select(sitename, sys_name, total_stud, state_total, 
                             perc_amind, state_amind, 
                             perc_asian, state_asian, 
                             perc_black, state_black, 
                             perc_hawpi, state_hawpi, 
                             perc_hispanic, state_hispanic, 
                             perc_white, state_white, 
                             perc_multiple, state_multiple, 
                             `%ed`, state_ed, `%lep`, state_lep)
louisiana_members$sys_avgenrolled <- c(louisiana_memsys[2, 2],
                                   louisiana_memsys[3, 2],
                                   louisiana_memsys[4, 2],
                                   louisiana_memsys[4, 2],
                                   louisiana_memsys[4, 2])
louisiana_members$sys_amind <- c(louisiana_memsys[2, 3],
                                   louisiana_memsys[3, 3],
                                   louisiana_memsys[4, 3],
                                   louisiana_memsys[4, 3],
                                   louisiana_memsys[4, 3])
louisiana_members$sys_asian <- c(louisiana_memsys[2, 4],
                                   louisiana_memsys[3, 4],
                                   louisiana_memsys[4, 4],
                                   louisiana_memsys[4, 4],
                                   louisiana_memsys[4, 4])
louisiana_members$sys_black <- c(louisiana_memsys[2, 5],
                                   louisiana_memsys[3, 5],
                                   louisiana_memsys[4, 5],
                                   louisiana_memsys[4, 5],
                                   louisiana_memsys[4, 5])
louisiana_members$sys_hispanic <- c(louisiana_memsys[2, 6],
                                      louisiana_memsys[3, 6],
                                      louisiana_memsys[4, 6],
                                      louisiana_memsys[4, 6],
                                      louisiana_memsys[4, 6])
louisiana_members$sys_hawpi <- c(louisiana_memsys[2, 7],
                                   louisiana_memsys[3, 7],
                                   louisiana_memsys[4, 7],
                                   louisiana_memsys[4, 7],
                                   louisiana_memsys[4, 7])
louisiana_members$sys_white <- c(louisiana_memsys[2, 8],
                                   louisiana_memsys[3, 8],
                                   louisiana_memsys[4, 8],
                                   louisiana_memsys[4, 8],
                                   louisiana_memsys[4, 8])
louisiana_members$sys_multiple <- c(louisiana_memsys[2, 9],
                                      louisiana_memsys[3, 9],
                                      louisiana_memsys[4, 9],
                                      louisiana_memsys[4, 9],
                                      louisiana_memsys[4, 9])
louisiana_members$sys_ed <- c(louisiana_memsys[2, 10],
                                louisiana_memsys[3, 10],
                                louisiana_memsys[4, 10],
                                louisiana_memsys[4, 10],
                                louisiana_memsys[4, 10])
louisiana_members$sys_lep <- c(louisiana_memsys[2, 11],
                                 louisiana_memsys[3, 11],
                                 louisiana_memsys[4, 11],
                                 louisiana_memsys[4, 11],
                                 louisiana_memsys[4, 11])
louisiana_members <- louisiana_members %>% 
  select(sitename, sys_name, total_stud, sys_avgenrolled, state_total,
         perc_amind, sys_amind, state_amind, 
         perc_asian, sys_asian, state_asian, 
         perc_black, sys_black, state_black, 
         perc_hawpi, sys_hawpi, state_hawpi, 
         perc_hispanic, sys_hispanic, state_hispanic, 
         perc_white, sys_white, state_white, 
         perc_multiple, sys_multiple, state_multiple,
         `%ed`, sys_ed, state_ed, `%lep`, sys_lep, state_lep)
View(louisiana_members)
louisiana_members[,3:32] <- sapply(louisiana_members[,3:32],as.numeric)
louisiana_members <- louisiana_members %>% 
  mutate(diff_total = total_stud - sys_avgenrolled,
         diff_amind = perc_amind - sys_amind,
         diff_asian = perc_asian - sys_asian,
         diff_black = perc_black - sys_black,
         diff_hawpi = perc_hawpi - sys_hawpi,
         diff_hispanic = perc_hispanic - sys_hispanic,
         diff_white = perc_white - sys_white,
         diff_multiple = perc_multiple - sys_multiple,
         diff_ed = `%ed` - sys_ed,
         diff_lep = `%lep` - sys_lep)
View(louisiana_memsys)
View(louisiana_members)
names(louisiana_members)
names(louisiana_memsys)
louisiana_members$std_total = c(louisiana_members[1,33] / louisiana_memsys[2, 3], # Kenner
                                louisiana_members[2,33] / louisiana_memsys[3, 3], # Morris Jeff
                                louisiana_members[3,33] / louisiana_memsys[4, 3], # IHS NOLA
                                louisiana_members[4,33] / louisiana_memsys[4, 3], # Int'l Louis
                                louisiana_members[5,33] / louisiana_memsys[4, 3]) # Lycee Francais
louisiana_members$std_amind = c(louisiana_members[1,34] / louisiana_memsys[2, 5], # Kenner
                                louisiana_members[2,34] / louisiana_memsys[3, 5], # Morris Jeff
                                louisiana_members[3,34] / louisiana_memsys[4, 5], # IHS NOLA
                                louisiana_members[4,34] / louisiana_memsys[4, 5], # Int'l Louis
                                louisiana_members[5,34] / louisiana_memsys[4, 5]) # Lycee Francais
louisiana_members$std_asian = c(louisiana_members[1,35] / louisiana_memsys[2, 7], # Kenner
                                louisiana_members[2,35] / louisiana_memsys[3, 7], # Morris Jeff
                                louisiana_members[3,35] / louisiana_memsys[4, 7], # IHS NOLA
                                louisiana_members[4,35] / louisiana_memsys[4, 7], # Int'l Louis
                                louisiana_members[5,35] / louisiana_memsys[4, 7]) # Lycee Francais
louisiana_members$std_black = c(louisiana_members[1,36] / louisiana_memsys[2, 9], # Kenner
                                louisiana_members[2,36] / louisiana_memsys[3, 9], # Morris Jeff
                                louisiana_members[3,36] / louisiana_memsys[4, 9], # IHS NOLA
                                louisiana_members[4,36] / louisiana_memsys[4, 9], # Int'l Louis
                                louisiana_members[5,36] / louisiana_memsys[4, 9]) # Lycee Francais
louisiana_members$std_hawpi = c(louisiana_members[1,37] / louisiana_memsys[2, 13], # Kenner
                                louisiana_members[2,37] / louisiana_memsys[3, 13], # Morris Jeff
                                louisiana_members[3,37] / louisiana_memsys[4, 13], # IHS NOLA
                                louisiana_members[4,37] / louisiana_memsys[4, 13], # Int'l Louis
                                louisiana_members[5,37] / louisiana_memsys[4, 13]) # Lycee Francais
louisiana_members$std_hispanic = c(louisiana_members[1,38] / louisiana_memsys[2, 11], # Kenner
                                louisiana_members[2,38] / louisiana_memsys[3, 11], # Morris Jeff
                                louisiana_members[3,38] / louisiana_memsys[4, 11], # IHS NOLA
                                louisiana_members[4,38] / louisiana_memsys[4, 11], # Int'l Louis
                                louisiana_members[5,38] / louisiana_memsys[4, 11]) # Lycee Francais
louisiana_members$std_white = c(louisiana_members[1,39] / louisiana_memsys[2, 15], # Kenner
                                louisiana_members[2,39] / louisiana_memsys[3, 15], # Morris Jeff
                                louisiana_members[3,39] / louisiana_memsys[4, 15], # IHS NOLA
                                louisiana_members[4,39] / louisiana_memsys[4, 15], # Int'l Louis
                                louisiana_members[5,39] / louisiana_memsys[4, 15]) # Lycee Francais
louisiana_members$std_multiple = c(louisiana_members[1,40] / louisiana_memsys[2, 17], # Kenner
                                louisiana_members[2,40] / louisiana_memsys[3, 17], # Morris Jeff
                                louisiana_members[3,40] / louisiana_memsys[4, 17], # IHS NOLA
                                louisiana_members[4,40] / louisiana_memsys[4, 17], # Int'l Louis
                                louisiana_members[5,40] / louisiana_memsys[4, 17]) # Lycee Francais
louisiana_members$std_ed = c(louisiana_members[1,41] / louisiana_memsys[2, 19], # Kenner
                             louisiana_members[2,41] / louisiana_memsys[3, 19], # Morris Jeff
                             louisiana_members[3,41] / louisiana_memsys[4, 19], # IHS NOLA
                             louisiana_members[4,41] / louisiana_memsys[4, 19], # Int'l Louis
                             louisiana_members[5,41] / louisiana_memsys[4, 19]) # Lycee Francais
louisiana_members$std_lep = c(louisiana_members[1,42] / louisiana_memsys[2, 21], # Kenner
                             louisiana_members[2,42] / louisiana_memsys[3, 21], # Morris Jeff
                             louisiana_members[3,42] / louisiana_memsys[4, 21], # IHS NOLA
                             louisiana_members[4,42] / louisiana_memsys[4, 21], # Int'l Louis
                             louisiana_members[5,42] / louisiana_memsys[4, 21]) # Lycee Francais
y <- louisiana_members %>% 
  select(sitename,sys_name,
         total_stud,sys_avgenrolled,std_total,
         perc_amind, sys_amind, std_amind,
         perc_asian, sys_asian, std_asian,
         perc_black, sys_black, std_black,
         perc_hawpi, sys_hawpi, std_hawpi,
         perc_hispanic, sys_hispanic, std_hispanic,
         perc_white, sys_white, std_white,
         perc_multiple, sys_multiple, std_multiple, 
         `%ed`, sys_ed, std_ed,
         `%lep`, sys_lep, std_lep)
View(y)
str(y)
y[,3:32] <- sapply(y[,3:32],as.numeric)
write.csv(y, file = file.path('standardized_LA_mem.csv'), row.names = FALSE)
# ------------------------------------------------------------------------------------------------------------------------
# CALIFORNIA Enrollment by Race
california <- read.csv("CA_enrl_2019-03-28.csv")
View(california)
names(california) <- tolower(names(california))
names(california)
california <- california %>% 
  select(school, county, district, gender, ethnic, enr_total)
View(california)
str(california)
names(california)
california$ethnic <- as.factor(california$ethnic)
levels(california$ethnic) <- c("unreported","amind","asian","hawpi","filipino","hispanic","black","white","multiple")
head(california)
levels(california$ethnic)[5] <- "asian"
california <- california %>% 
  distinct(school, district, gender, ethnic, .keep_all = TRUE) %>% 
  spread(key = ethnic,
         value = enr_total,
         fill = 0) %>% 
  mutate(total = unreported + amind + asian + hawpi + hispanic + black + white + multiple)
levels(california$gender) <- c("F","F") # combine genders to sum it up
california <- california %>% 
  select(school, district, unreported, amind, asian, hawpi, hispanic, black, white, multiple, total)
install.packages("data.table")
library(data.table)
DT <- data.table(california)
california <- DT[, lapply(.SD, sum), by=list(school, district)]

View(california)
california <- california %>% 
  mutate(perc_white = white / total,
         perc_amind = amind / total,
         perc_asian = asian / total,
         perc_black = black / total,
         perc_hispanic = hispanic / total,
         perc_hawpi = hawpi / total,
         perc_multiple = multiple / total,
         perc_unreported = unreported / total)
y <- california %>% 
  group_by(district) %>% # California's mean and sd of proportional subgroups within schools by district
  summarize(mean_stud = mean(total),
            sd_stud = sd(total),
            mean_amind = mean(perc_amind),
            sd_amind = sd(perc_amind),
            mean_asian = mean(perc_asian),
            sd_asian = sd(perc_asian),
            mean_black = mean(perc_black),
            sd_black = sd(perc_black),
            mean_hispanic = mean(perc_hispanic),
            sd_hispanic = sd(perc_hispanic),
            mean_hawpi = mean(perc_hawpi),
            sd_hapwi = sd(perc_hawpi),
            mean_white = mean(perc_white),
            sd_white = sd(perc_white),
            mean_multiple = mean(perc_multiple),
            sd_multiple = sd(perc_multiple),
            mean_unreported = mean(perc_unreported),
            sd_unreported = sd(perc_unreported),
            n = n()) %>% 
  distinct(district, .keep_all = TRUE)
View(y)
california_members <- california[c(1666:1668,1683,3922:3937,9342,9692,9694),]
names(california_members)
View(california_members)
california_members <- california_members %>% 
  arrange(district, school)
y <- y %>% 
  arrange(district)
rownames(y) <- y$district
y_mem <- y[c("Los Angeles Unified","San Diego Unified"),]  # districts of member schools
View(y_mem)  
california_members$sys_avgenrolled <- c(y_mem[1, 2], # 9 members in LA Unified
                                        y_mem[1, 2],
                                        y_mem[1, 2],
                                        y_mem[1, 2],
                                        y_mem[1, 2],
                                        y_mem[1, 2],
                                        y_mem[1, 2],
                                        y_mem[1, 2],
                                        y_mem[1, 2],
                                        y_mem[2, 2], # 14 members in San Diego
                                        y_mem[2, 2],
                                        y_mem[2, 2],
                                        y_mem[2, 2],
                                        y_mem[2, 2],
                                        y_mem[2, 2],
                                        y_mem[2, 2],
                                        y_mem[2, 2],
                                        y_mem[2, 2],
                                        y_mem[2, 2],
                                        y_mem[2, 2],
                                        y_mem[2, 2],
                                        y_mem[2, 2],
                                        y_mem[2, 2])
california_members$sys_unreported <- c(y_mem[1, 18], # 9 members in LA Unified
                                        y_mem[1, 18],
                                        y_mem[1, 18],
                                        y_mem[1, 18],
                                        y_mem[1, 18],
                                        y_mem[1, 18],
                                        y_mem[1, 18],
                                        y_mem[1, 18],
                                        y_mem[1, 18],
                                        y_mem[2, 18], # 14 members in San Diego
                                        y_mem[2, 18],
                                        y_mem[2, 18],
                                        y_mem[2, 18],
                                        y_mem[2, 18],
                                        y_mem[2, 18],
                                        y_mem[2, 18],
                                        y_mem[2, 18],
                                        y_mem[2, 18],
                                        y_mem[2, 18],
                                        y_mem[2, 18],
                                        y_mem[2, 18],
                                        y_mem[2, 18],
                                        y_mem[2, 18])
california_members$sys_amind <- c(y_mem[1, 4], # 9 members in LA Unified
                                        y_mem[1, 4],
                                        y_mem[1, 4],
                                        y_mem[1, 4],
                                        y_mem[1, 4],
                                        y_mem[1, 4],
                                        y_mem[1, 4],
                                        y_mem[1, 4],
                                        y_mem[1, 4],
                                        y_mem[2, 4], # 14 members in San Diego
                                        y_mem[2, 4],
                                        y_mem[2, 4],
                                        y_mem[2, 4],
                                        y_mem[2, 4],
                                        y_mem[2, 4],
                                        y_mem[2, 4],
                                        y_mem[2, 4],
                                        y_mem[2, 4],
                                        y_mem[2, 4],
                                        y_mem[2, 4],
                                        y_mem[2, 4],
                                        y_mem[2, 4],
                                        y_mem[2, 4])
california_members$sys_asian <- c(y_mem[1, 6], # 9 members in LA Unified
                                  y_mem[1, 6],
                                  y_mem[1, 6],
                                  y_mem[1, 6],
                                  y_mem[1, 6],
                                  y_mem[1, 6],
                                  y_mem[1, 6],
                                  y_mem[1, 6],
                                  y_mem[1, 6],
                                  y_mem[2, 6], # 14 members in San Diego
                                  y_mem[2, 6],
                                  y_mem[2, 6],
                                  y_mem[2, 6],
                                  y_mem[2, 6],
                                  y_mem[2, 6],
                                  y_mem[2, 6],
                                  y_mem[2, 6],
                                  y_mem[2, 6],
                                  y_mem[2, 6],
                                  y_mem[2, 6],
                                  y_mem[2, 6],
                                  y_mem[2, 6],
                                  y_mem[2, 6])
california_members$sys_black <- c(y_mem[1, 8], # 9 members in LA Unified
                                  y_mem[1, 8],
                                  y_mem[1, 8],
                                  y_mem[1, 8],
                                  y_mem[1, 8],
                                  y_mem[1, 8],
                                  y_mem[1, 8],
                                  y_mem[1, 8],
                                  y_mem[1, 8],
                                  y_mem[2, 8], # 14 members in San Diego
                                  y_mem[2, 8],
                                  y_mem[2, 8],
                                  y_mem[2, 8],
                                  y_mem[2, 8],
                                  y_mem[2, 8],
                                  y_mem[2, 8],
                                  y_mem[2, 8],
                                  y_mem[2, 8],
                                  y_mem[2, 8],
                                  y_mem[2, 8],
                                  y_mem[2, 8],
                                  y_mem[2, 8],
                                  y_mem[2, 8])
california_members$sys_hispanic <- c(y_mem[1, 10], # 9 members in LA Unified
                                  y_mem[1, 10],
                                  y_mem[1, 10],
                                  y_mem[1, 10],
                                  y_mem[1, 10],
                                  y_mem[1, 10],
                                  y_mem[1, 10],
                                  y_mem[1, 10],
                                  y_mem[1, 10],
                                  y_mem[2, 10], # 14 members in San Diego
                                  y_mem[2, 10],
                                  y_mem[2, 10],
                                  y_mem[2, 10],
                                  y_mem[2, 10],
                                  y_mem[2, 10],
                                  y_mem[2, 10],
                                  y_mem[2, 10],
                                  y_mem[2, 10],
                                  y_mem[2, 10],
                                  y_mem[2, 10],
                                  y_mem[2, 10],
                                  y_mem[2, 10],
                                  y_mem[2, 10])
california_members$sys_hawpi <- c(y_mem[1, 12], # 9 members in LA Unified
                                     y_mem[1, 12],
                                     y_mem[1, 12],
                                     y_mem[1, 12],
                                     y_mem[1, 12],
                                     y_mem[1, 12],
                                     y_mem[1, 12],
                                     y_mem[1, 12],
                                     y_mem[1, 12],
                                     y_mem[2, 12], # 14 members in San Diego
                                     y_mem[2, 12],
                                     y_mem[2, 12],
                                     y_mem[2, 12],
                                     y_mem[2, 12],
                                     y_mem[2, 12],
                                     y_mem[2, 12],
                                     y_mem[2, 12],
                                     y_mem[2, 12],
                                     y_mem[2, 12],
                                     y_mem[2, 12],
                                     y_mem[2, 12],
                                     y_mem[2, 12],
                                     y_mem[2, 12])
california_members$sys_white <- c(y_mem[1, 14], # 9 members in LA Unified
                                  y_mem[1, 14],
                                  y_mem[1, 14],
                                  y_mem[1, 14],
                                  y_mem[1, 14],
                                  y_mem[1, 14],
                                  y_mem[1, 14],
                                  y_mem[1, 14],
                                  y_mem[1, 14],
                                  y_mem[2, 14], # 14 members in San Diego
                                  y_mem[2, 14],
                                  y_mem[2, 14],
                                  y_mem[2, 14],
                                  y_mem[2, 14],
                                  y_mem[2, 14],
                                  y_mem[2, 14],
                                  y_mem[2, 14],
                                  y_mem[2, 14],
                                  y_mem[2, 14],
                                  y_mem[2, 14],
                                  y_mem[2, 14],
                                  y_mem[2, 14],
                                  y_mem[2, 14])
california_members$sys_multiple <- c(y_mem[1, 16], # 9 members in LA Unified
                                  y_mem[1, 16],
                                  y_mem[1, 16],
                                  y_mem[1, 16],
                                  y_mem[1, 16],
                                  y_mem[1, 16],
                                  y_mem[1, 16],
                                  y_mem[1, 16],
                                  y_mem[1, 16],
                                  y_mem[2, 16], # 16 members in San Diego
                                  y_mem[2, 16],
                                  y_mem[2, 16],
                                  y_mem[2, 16],
                                  y_mem[2, 16],
                                  y_mem[2, 16],
                                  y_mem[2, 16],
                                  y_mem[2, 16],
                                  y_mem[2, 16],
                                  y_mem[2, 16],
                                  y_mem[2, 16],
                                  y_mem[2, 16],
                                  y_mem[2, 16],
                                  y_mem[2, 16])

names(y_mem)
View(california_members)
california_members <- data.frame(california_members)
california_members[,3:28] <- sapply(california_members[,3:28],as.numeric)
california_members <- california_members %>% 
  mutate(diff_total = total - sys_avgenrolled,
         diff_amind = perc_amind - sys_amind,
         diff_asian = perc_asian - sys_asian,
         diff_black = perc_black - sys_black,
         diff_hawpi = perc_hawpi - sys_hawpi,
         diff_hispanic = perc_hispanic - sys_hispanic,
         diff_white = perc_white - sys_white,
         diff_multiple = perc_multiple - sys_multiple,
         diff_unreported = perc_unreported - sys_unreported)
View(california_members)
names(california_members) # diff total / standard deviation
california_members$std_total = c(california_members[1,29] / y_mem[1, 3], # LA Unified Schools:
                                california_members[2,29] / y_mem[1, 3], # COTW Hollywood, Mar Vista
                                california_members[3,29] / y_mem[1, 3], # Silver Lake
                                california_members[4,29] / y_mem[1, 3], # CLIC
                                california_members[5,29] / y_mem[1, 3], # HTH LA
                                california_members[6,29] / y_mem[1, 3], # HTH LA Middle
                                california_members[7,29] / y_mem[1, 3], # The City
                                california_members[8,29] / y_mem[1, 3], # Valley ES
                                california_members[9,29] / y_mem[1, 3], # Valley MS
                                california_members[10,29] / y_mem[2, 3], # San Diego Schools: HTH Elem
                                california_members[11,29] / y_mem[2, 3], # HTH Elem Explorer
                                california_members[12,29] / y_mem[2, 3], # HTH
                                california_members[13,29] / y_mem[2, 3], # HTH International
                                california_members[14,29] / y_mem[2, 3], # HTH Media Arts
                                california_members[15,29] / y_mem[2, 3], # HT Middle
                                california_members[16,29] / y_mem[2, 3], # HT Middle Media Arts
                                california_members[17,29] / y_mem[2, 3], # HT Elem Chula Vista
                                california_members[18,29] / y_mem[2, 3], # HT Elem North County
                                california_members[19,29] / y_mem[2, 3], # HTH Chula Vista 
                                california_members[20,29] / y_mem[2, 3], # HTH Mesa
                                california_members[21,29] / y_mem[2, 3], # HTH North County
                                california_members[22,29] / y_mem[2, 3], # HT Middle Chula Vista
                                california_members[23,29] / y_mem[2, 3]) # HT Middle North County
california_members$std_amind = c(california_members[1,30] / y_mem[1, 5], # LA Unified Schools:
                                 california_members[2,30] / y_mem[1, 5], # COTW Hollywood, Mar Vista
                                 california_members[3,30] / y_mem[1, 5], # Silver Lake
                                 california_members[4,30] / y_mem[1, 5], # CLIC
                                 california_members[5,30] / y_mem[1, 5], # HTH LA
                                 california_members[6,30] / y_mem[1, 5], # HTH LA Middle
                                 california_members[7,30] / y_mem[1, 5], # The City
                                 california_members[8,30] / y_mem[1, 5], # Valley ES
                                 california_members[9,30] / y_mem[1, 5], # Valley MS
                                 california_members[10,30] / y_mem[2, 5], # San Diego Schools: HTH Elem
                                 california_members[11,30] / y_mem[2, 5], # HTH Elem Explorer
                                 california_members[12,30] / y_mem[2, 5], # HTH
                                 california_members[13,30] / y_mem[2, 5], # HTH International
                                 california_members[14,30] / y_mem[2, 5], # HTH Media Arts
                                 california_members[15,30] / y_mem[2, 5], # HT Middle
                                 california_members[16,30] / y_mem[2, 5], # HT Middle Media Arts
                                 california_members[17,30] / y_mem[2, 5], # HT Elem Chula Vista
                                 california_members[18,30] / y_mem[2, 5], # HT Elem North County
                                 california_members[19,30] / y_mem[2, 5], # HTH Chula Vista 
                                 california_members[20,30] / y_mem[2, 5], # HTH Mesa
                                 california_members[22,30] / y_mem[2, 5], # HTH North County
                                 california_members[22,30] / y_mem[2, 5], # HT Middle Chula Vista
                                 california_members[23,30] / y_mem[2, 5]) # HT Middle North County
california_members$std_asian = c(california_members[1,31] / y_mem[1, 7], # LA Unified Schools:
                                 california_members[2,31] / y_mem[1, 7], # COTW Hollywood, Mar Vista
                                 california_members[3,31] / y_mem[1, 7], # Silver Lake
                                 california_members[4,31] / y_mem[1, 7], # CLIC
                                 california_members[5,31] / y_mem[1, 7], # HTH LA
                                 california_members[6,31] / y_mem[1, 7], # HTH LA Middle
                                 california_members[7,31] / y_mem[1, 7], # The City
                                 california_members[8,31] / y_mem[1, 7], # Valley ES
                                 california_members[9,31] / y_mem[1, 7], # Valley MS
                                 california_members[10,31] / y_mem[2, 7], # San Diego Schools: HTH Elem
                                 california_members[11,31] / y_mem[2, 7], # HTH Elem Explorer
                                 california_members[12,31] / y_mem[2, 7], # HTH
                                 california_members[13,31] / y_mem[2, 7], # HTH International
                                 california_members[14,31] / y_mem[2, 7], # HTH Media Arts
                                 california_members[15,31] / y_mem[2, 7], # HT Middle
                                 california_members[16,31] / y_mem[2, 7], # HT Middle Media Arts
                                 california_members[17,31] / y_mem[2, 7], # HT Elem Chula Vista
                                 california_members[18,31] / y_mem[2, 7], # HT Elem North County
                                 california_members[19,31] / y_mem[2, 7], # HTH Chula Vista 
                                 california_members[20,31] / y_mem[2, 7], # HTH Mesa
                                 california_members[22,31] / y_mem[2, 7], # HTH North County
                                 california_members[22,31] / y_mem[2, 7], # HT Middle Chula Vista
                                 california_members[23,31] / y_mem[2, 7]) # HT Middle North County
california_members$std_black = c(california_members[1,32] / y_mem[1, 9], # LA Unified Schools:
                                 california_members[2,32] / y_mem[1, 9], # COTW Hollywood, Mar Vista
                                 california_members[3,32] / y_mem[1, 9], # Silver Lake
                                 california_members[4,32] / y_mem[1, 9], # CLIC
                                 california_members[5,32] / y_mem[1, 9], # HTH LA
                                 california_members[6,32] / y_mem[1, 9], # HTH LA Middle
                                 california_members[7,32] / y_mem[1, 9], # The City
                                 california_members[8,32] / y_mem[1, 9], # Valley ES
                                 california_members[9,32] / y_mem[1, 9], # Valley MS
                                 california_members[10,32] / y_mem[2, 9], # San Diego Schools: HTH Elem
                                 california_members[11,32] / y_mem[2, 9], # HTH Elem Explorer
                                 california_members[12,32] / y_mem[2, 9], # HTH
                                 california_members[13,32] / y_mem[2, 9], # HTH International
                                 california_members[14,32] / y_mem[2, 9], # HTH Media Arts
                                 california_members[15,32] / y_mem[2, 9], # HT Middle
                                 california_members[16,32] / y_mem[2, 9], # HT Middle Media Arts
                                 california_members[17,32] / y_mem[2, 9], # HT Elem Chula Vista
                                 california_members[18,32] / y_mem[2, 9], # HT Elem North County
                                 california_members[19,32] / y_mem[2, 9], # HTH Chula Vista 
                                 california_members[20,32] / y_mem[2, 9], # HTH Mesa
                                 california_members[22,32] / y_mem[2, 9], # HTH North County
                                 california_members[22,32] / y_mem[2, 9], # HT Middle Chula Vista
                                 california_members[23,32] / y_mem[2, 9]) # HT Middle North County
california_members$std_hawpi = c(california_members[1,33] / y_mem[1, 13], # LA Unified Schools:
                                 california_members[2,33] / y_mem[1, 13], # COTW Hollywood, Mar Vista
                                 california_members[3,33] / y_mem[1, 13], # Silver Lake
                                 california_members[4,33] / y_mem[1, 13], # CLIC
                                 california_members[5,33] / y_mem[1, 13], # HTH LA
                                 california_members[6,33] / y_mem[1, 13], # HTH LA Middle
                                 california_members[7,33] / y_mem[1, 13], # The City
                                 california_members[8,33] / y_mem[1, 13], # Valley ES
                                 california_members[9,33] / y_mem[1, 13], # Valley MS
                                 california_members[10,33] / y_mem[2, 13], # San Diego Schools: HTH Elem
                                 california_members[11,33] / y_mem[2, 13], # HTH Elem Explorer
                                 california_members[12,33] / y_mem[2, 13], # HTH
                                 california_members[13,33] / y_mem[2, 13], # HTH International
                                 california_members[14,33] / y_mem[2, 13], # HTH Media Arts
                                 california_members[15,33] / y_mem[2, 13], # HT Middle
                                 california_members[16,33] / y_mem[2, 13], # HT Middle Media Arts
                                 california_members[17,33] / y_mem[2, 13], # HT Elem Chula Vista
                                 california_members[18,33] / y_mem[2, 13], # HT Elem North County
                                 california_members[19,33] / y_mem[2, 13], # HTH Chula Vista 
                                 california_members[20,33] / y_mem[2, 13], # HTH Mesa
                                 california_members[22,33] / y_mem[2, 13], # HTH North County
                                 california_members[22,33] / y_mem[2, 13], # HT Middle Chula Vista
                                 california_members[23,33] / y_mem[2, 13]) # HT Middle North County
california_members$std_hispanic = c(california_members[1,34] / y_mem[1, 11], # LA Unified Schools:
                                 california_members[2,34] / y_mem[1, 11], # COTW Hollywood, Mar Vista
                                 california_members[3,34] / y_mem[1, 11], # Silver Lake
                                 california_members[4,34] / y_mem[1, 11], # CLIC
                                 california_members[5,34] / y_mem[1, 11], # HTH LA
                                 california_members[6,34] / y_mem[1, 11], # HTH LA Middle
                                 california_members[7,34] / y_mem[1, 11], # The City
                                 california_members[8,34] / y_mem[1, 11], # Valley ES
                                 california_members[9,34] / y_mem[1, 11], # Valley MS
                                 california_members[10,34] / y_mem[2, 11], # San Diego Schools: HTH Elem
                                 california_members[11,34] / y_mem[2, 11], # HTH Elem Explorer
                                 california_members[12,34] / y_mem[2, 11], # HTH
                                 california_members[13,34] / y_mem[2, 11], # HTH International
                                 california_members[14,34] / y_mem[2, 11], # HTH Media Arts
                                 california_members[15,34] / y_mem[2, 11], # HT Middle
                                 california_members[16,34] / y_mem[2, 11], # HT Middle Media Arts
                                 california_members[17,34] / y_mem[2, 11], # HT Elem Chula Vista
                                 california_members[18,34] / y_mem[2, 11], # HT Elem North County
                                 california_members[19,34] / y_mem[2, 11], # HTH Chula Vista 
                                 california_members[20,34] / y_mem[2, 11], # HTH Mesa
                                 california_members[22,34] / y_mem[2, 11], # HTH North County
                                 california_members[22,34] / y_mem[2, 11], # HT Middle Chula Vista
                                 california_members[23,34] / y_mem[2, 11]) # HT Middle North County
california_members$std_white = c(california_members[1,35] / y_mem[1, 15], # LA Unified Schools:
                                    california_members[2,35] / y_mem[1, 15], # COTW Hollywood, Mar Vista
                                    california_members[3,35] / y_mem[1, 15], # Silver Lake
                                    california_members[4,35] / y_mem[1, 15], # CLIC
                                    california_members[5,35] / y_mem[1, 15], # HTH LA
                                    california_members[6,35] / y_mem[1, 15], # HTH LA Middle
                                    california_members[7,35] / y_mem[1, 15], # The City
                                    california_members[8,35] / y_mem[1, 15], # Valley ES
                                    california_members[9,35] / y_mem[1, 15], # Valley MS
                                    california_members[10,35] / y_mem[2, 15], # San Diego Schools: HTH Elem
                                    california_members[11,35] / y_mem[2, 15], # HTH Elem Explorer
                                    california_members[12,35] / y_mem[2, 15], # HTH
                                    california_members[13,35] / y_mem[2, 15], # HTH International
                                    california_members[14,35] / y_mem[2, 15], # HTH Media Arts
                                    california_members[15,35] / y_mem[2, 15], # HT Middle
                                    california_members[16,35] / y_mem[2, 15], # HT Middle Media Arts
                                    california_members[17,35] / y_mem[2, 15], # HT Elem Chula Vista
                                    california_members[18,35] / y_mem[2, 15], # HT Elem North County
                                    california_members[19,35] / y_mem[2, 15], # HTH Chula Vista 
                                    california_members[20,35] / y_mem[2, 15], # HTH Mesa
                                    california_members[22,35] / y_mem[2, 15], # HTH North County
                                    california_members[22,35] / y_mem[2, 15], # HT Middle Chula Vista
                                    california_members[23,35] / y_mem[2, 15]) # HT Middle North County
california_members$std_multiple = c(california_members[1,36] / y_mem[1, 17], # LA Unified Schools:
                                 california_members[2,36] / y_mem[1, 17], # COTW Hollywood, Mar Vista
                                 california_members[3,36] / y_mem[1, 17], # Silver Lake
                                 california_members[4,36] / y_mem[1, 17], # CLIC
                                 california_members[5,36] / y_mem[1, 17], # HTH LA
                                 california_members[6,36] / y_mem[1, 17], # HTH LA Middle
                                 california_members[7,36] / y_mem[1, 17], # The City
                                 california_members[8,36] / y_mem[1, 17], # Valley ES
                                 california_members[9,36] / y_mem[1, 17], # Valley MS
                                 california_members[10,36] / y_mem[2, 17], # San Diego Schools: HTH Elem
                                 california_members[11,36] / y_mem[2, 17], # HTH Elem Explorer
                                 california_members[12,36] / y_mem[2, 17], # HTH
                                 california_members[13,36] / y_mem[2, 17], # HTH International
                                 california_members[14,36] / y_mem[2, 17], # HTH Media Arts
                                 california_members[15,36] / y_mem[2, 17], # HT Middle
                                 california_members[16,36] / y_mem[2, 17], # HT Middle Media Arts
                                 california_members[17,36] / y_mem[2, 17], # HT Elem Chula Vista
                                 california_members[18,36] / y_mem[2, 17], # HT Elem North County
                                 california_members[19,36] / y_mem[2, 17], # HTH Chula Vista 
                                 california_members[20,36] / y_mem[2, 17], # HTH Mesa
                                 california_members[22,36] / y_mem[2, 17], # HTH North County
                                 california_members[22,36] / y_mem[2, 17], # HT Middle Chula Vista
                                 california_members[23,36] / y_mem[2, 17]) # HT Middle North County
california_members$std_unreported = c(california_members[1,37] / y_mem[1, 19], # LA Unified Schools:
                                    california_members[2,37] / y_mem[1, 19], # COTW Hollywood, Mar Vista
                                    california_members[3,37] / y_mem[1, 19], # Silver Lake
                                    california_members[4,37] / y_mem[1, 19], # CLIC
                                    california_members[5,37] / y_mem[1, 19], # HTH LA
                                    california_members[6,37] / y_mem[1, 19], # HTH LA Middle
                                    california_members[7,37] / y_mem[1, 19], # The City
                                    california_members[8,37] / y_mem[1, 19], # Valley ES
                                    california_members[9,37] / y_mem[1, 19], # Valley MS
                                    california_members[10,37] / y_mem[2, 19], # San Diego Schools: HTH Elem
                                    california_members[11,37] / y_mem[2, 19], # HTH Elem Explorer
                                    california_members[12,37] / y_mem[2, 19], # HTH
                                    california_members[13,37] / y_mem[2, 19], # HTH International
                                    california_members[14,37] / y_mem[2, 19], # HTH Media Arts
                                    california_members[15,37] / y_mem[2, 19], # HT Middle
                                    california_members[16,37] / y_mem[2, 19], # HT Middle Media Arts
                                    california_members[17,37] / y_mem[2, 19], # HT Elem Chula Vista
                                    california_members[18,37] / y_mem[2, 19], # HT Elem North County
                                    california_members[19,37] / y_mem[2, 19], # HTH Chula Vista 
                                    california_members[20,37] / y_mem[2, 19], # HTH Mesa
                                    california_members[22,37] / y_mem[2, 19], # HTH North County
                                    california_members[22,37] / y_mem[2, 19], # HT Middle Chula Vista
                                    california_members[23,37] / y_mem[2, 19]) # HT Middle North County
View(california_members)
names(california_members)
names(y_mem)
z <- california_members %>% 
  select(school,district,
         total, sys_avgenrolled, std_total,
         perc_amind, sys_amind, std_amind,
         perc_asian, sys_asian, std_asian,
         perc_black, sys_black, std_black,
         perc_hawpi, sys_hawpi, std_hawpi,
         perc_hispanic, sys_hispanic, std_hispanic,
         perc_white, sys_white, std_white,
         perc_multiple, sys_multiple, std_multiple,
         perc_unreported, sys_unreported, std_unreported)
View(z)
str(z)
z[,3:29] <- sapply(z[,3:29],as.numeric)
write.csv(z, file = file.path('standardized_CA_mem.csv'), row.names = FALSE)

# ------------------------------------------------------------------------------------------------------------------------
# California FRPM
library(readxl)
cali_frpm <- read_excel("CA_ frpm1819.xlsx", sheet = "FRPM School-Level Data ") # Read FRPM data
colnames(cali_frpm) <- cali_frpm[1,]
cali_frpm <- cali_frpm[-1,]
names(cali_frpm) <- tolower(names(cali_frpm))
names(cali_frpm)[7] <- "school_name"
names(cali_frpm)[22] <- "perc_elig"
names(cali_frpm)[6] <- "district"
cali_frpm <- cali_frpm %>% 
  select(school_name, district, perc_elig) %>% 
  arrange(district, school_name)
View(cali_frpm)
str(cali_frpm)
cali_frpm$perc_elig <- as.numeric(cali_frpm$perc_elig)
cali_frpm$district <- as.factor(cali_frpm$district)
a <- cali_frpm %>% 
  group_by(district) %>% 
  summarize(mean_dist = mean(perc_elig),
          sd_dist = sd(perc_elig))
rownames(a) <- a$district
cali_frpm_mem_dist <- a[c("Los Angeles Unified",
                          "San Diego Unified"),]
View(cali_frpm_mem_dist)
cali_frpm_members <- cali_frpm[c(4413:4416, 4623:4624, 5082, 5114,5115,8078:8084,9016:9022),]
cali_frpm_members$sys_frpm <- as.numeric(c(cali_frpm_mem_dist[1,2],cali_frpm_mem_dist[1,2],cali_frpm_mem_dist[1,2],
                                cali_frpm_mem_dist[1,2],cali_frpm_mem_dist[1,2],cali_frpm_mem_dist[1,2],
                                cali_frpm_mem_dist[1,2],cali_frpm_mem_dist[1,2],cali_frpm_mem_dist[1,2],
                                cali_frpm_mem_dist[2,2],cali_frpm_mem_dist[2,2],
                                cali_frpm_mem_dist[2,2],cali_frpm_mem_dist[2,2],
                                cali_frpm_mem_dist[2,2],cali_frpm_mem_dist[2,2],
                                cali_frpm_mem_dist[2,2],cali_frpm_mem_dist[2,2],
                                cali_frpm_mem_dist[2,2],cali_frpm_mem_dist[2,2],
                                cali_frpm_mem_dist[2,2],cali_frpm_mem_dist[2,2],
                                cali_frpm_mem_dist[2,2],cali_frpm_mem_dist[2,2]))

cali_frpm_members <- cali_frpm_members %>% 
  mutate(diff = perc_elig - sys_frpm)
View(cali_frpm_members)
names(cali_frpm_members)
cali_frpm_members[1:9,6] <- cali_frpm_members[1:9,5] / as.numeric(c(cali_frpm_mem_dist[1,3],cali_frpm_mem_dist[1,3],
                                                                   cali_frpm_mem_dist[1,3],cali_frpm_mem_dist[1,3],
                                                                   cali_frpm_mem_dist[1,3],cali_frpm_mem_dist[1,3],
                                                                   cali_frpm_mem_dist[1,3],cali_frpm_mem_dist[1,3],
                                                                   cali_frpm_mem_dist[1,3]))
cali_frpm_members[10:23,6] <- cali_frpm_members[10:23,5] / as.numeric(c(cali_frpm_mem_dist[2,3],cali_frpm_mem_dist[2,3],
                                                                    cali_frpm_mem_dist[2,3],cali_frpm_mem_dist[2,3],
                                                                    cali_frpm_mem_dist[2,3],cali_frpm_mem_dist[2,3],
                                                                    cali_frpm_mem_dist[2,3],cali_frpm_mem_dist[2,3],
                                                                    cali_frpm_mem_dist[2,3]))

# ------------------------------------------------------------------------------------------------------------------------
# California LEP
cali_lep <- read.csv('CA_lep1819.csv') # Read the English learner data
cali_frpm <- read.csv('CA_enrl_2019-03-28.csv')
View(cali_lep)
names(cali_lep) <- tolower(names(cali_lep))
names(cali_lep)
levels(cali_lep$language)[1:67] <- "a" # code languages as repeats to sum it up
cali_lep <- cali_lep %>% 
  select(district, school, total_el) %>% 
  arrange(district, school)
str(cali_lep)
cali_lep$total_el <- as.numeric(cali_lep$total_el)
install.packages("data.table")
library(data.table)
DT <- data.table(cali_lep)
DT$total_el <- as.numeric(DT$total_el)
cali_lep <- DT[, lapply(.SD, sum), by=list(district, school)]
cali_lep <- cali_lep %>% 
  arrange(district, school)
View(cali_lep)
View(a)
a <- cali_lep %>% 
  group_by(district) %>% 
  summarize(mean_dist = mean(total_el),
            sd_dist = sd(total_el),
            n = n())
rownames(a) <- a$district
cali_sys_lep <- a[c("Los Angeles Unified",
                          "San Diego Unified"),]
View(cali_sys_lep)
cali_lep_members <- cali_lep[c(4138:4141,4348:4349,4806, 4838:4839,7651:7657,8567:8573)]
View(cali_lep_members)
names(cali_lep_members)
names(california_members)
cali_lep_members$perc_lep <- cali_lep_members[,3] / california_members[,11]

# ------------------------------------------------------------------------------------------------------------------------
# NEW YORK CITY
nyc_math_sys <- read_excel("NYC_math_2013-2019.xlsx", sheet = "All") # Read NYC's math
# sheets: All, SWD, Ethnicity, Gender, Econ Status, ELL
View(nyc_math_sys)
names(nyc_math_sys) <- tolower(names(nyc_math_sys))
names(nyc_math_sys)[3] <- "school"
names(nyc_math_sys)[7:18] <- c("n_math","mean_score","count_lvl1","perc_lvl1","count_lvl2","perc_lvl2",
                               "count_lvl3","perc_lvl3","count_lvl4","perc_lvl4","count_lvl3and4","perc_lvl3and4")
names(nyc_math_sys)
nyc_math_sys <- nyc_math_sys %>% 
  filter(grade == "All Grades", year == "2019") %>% 
  select(school, n_math, mean_score, count_lvl3and4, perc_lvl3and4) %>% 
  arrange(school)
str(nyc_math_sys)
nyc_math_sys$mean_score <- as.numeric(nyc_math_sys$mean_score)
nyc_math_sys$count_lvl3and4 <- as.numeric(nyc_math_sys$count_lvl3and4)
nyc_math_sys$perc_lvl3and4 <- as.numeric(nyc_math_sys$perc_lvl3and4)
nyc_public_summary <- nyc_math_sys %>% 
  summarize(avg_schoolmean = mean(mean_score),
          sd_schoolmean = sd(mean_score),
          avg_perc_lvl34 = mean(perc_lvl3and4),
          sd_perc_lvl34 = sd(perc_lvl3and4),
          n = n())
nyc_public_summary
nyc_math_charters <- read_excel("NYC_charter_2013-2019.xlsx", sheet = "Math") # load charter data. Sheet names: Math, ELA
names(nyc_math_charters) <- tolower(names(nyc_math_charters))
names(nyc_math_charters)[3] <- "school"
names(nyc_math_charters)[7:18] <- c("n_math","mean_score","count_lvl1","perc_lvl1","count_lvl2","perc_lvl2",
                                "count_lvl3","perc_lvl3","count_lvl4","perc_lvl4","count_lvl3and4","perc_lvl3and4")
nyc_math_charters <- nyc_math_charters %>% 
  filter(grade == "All Grades", year == "2019") %>% 
  select(school, category, n_math, mean_score, count_lvl3and4, perc_lvl3and4)
nyc_math_memb <- nyc_math_charters %>% 
  filter(school == "ACADEMY OF THE CITY CHARTER SCHOOL" |
           school == "BROOKLYN PROSPECT CHARTER SCHOOL" |
           school == "BROOKLYN PROSPECT CHARTER SCHOOL DOWNTOWN" |
           school == "CENTRAL QUEENS ACADEMY CHARTER SCHOOL"|
           school == "COMMUNITY ROOTS CHARTER SCHOOL" |
           school == "HELLENIC CLASSICAL CHARTER SCHOOL")
nyc_math_charters$mean_score <- as.numeric(nyc_math_charters$mean_score)
nyc_math_charters$count_lvl3and4 <- as.numeric(nyc_math_charters$count_lvl3and4)
nyc_math_charters$perc_lvl3and4 <- as.numeric(nyc_math_charters$perc_lvl3and4)
nyc_charters_summary <- nyc_math_charters %>% 
  summarize(avg_schoolmean = mean(mean_score),
            sd_schoolmean = sd(mean_score),
            avg_perc_lvl34 = mean(perc_lvl3and4),
            sd_perc_lvl34 = sd(perc_lvl3and4),
            n = n())
nyc_charters_summary
nyc_public_summary
nyc_math_summary <- rbind(nyc_charters_summary, nyc_public_summary)
nyc_math_summary$comparison <- c("NYC charters","NYC public")
nyc_math_summary <- nyc_math_summary %>% # City's comparison groups
  select(comparison,n,avg_schoolmean,sd_schoolmean,avg_perc_lvl34,sd_perc_lvl34)
str(nyc_math_summary)
nyc_math_memb$mean_score <- as.numeric(nyc_math_memb$mean_score)
nyc_math_memb$count_lvl3and4 <- as.numeric(nyc_math_memb$count_lvl3and4)
nyc_math_memb$perc_lvl3and4 <- as.numeric(nyc_math_memb$perc_lvl3and4)
nyc_math_memb$diffscore_charter = as.numeric(c(nyc_math_memb[1,4] - nyc_math_summary[1,3],
                                          nyc_math_memb[2,4] - nyc_math_summary[1,3],
                                          nyc_math_memb[3,4] - nyc_math_summary[1,3],
                                          nyc_math_memb[4,4] - nyc_math_summary[1,3],
                                          nyc_math_memb[5,4] - nyc_math_summary[1,3],
                                          nyc_math_memb[6,4] - nyc_math_summary[1,3]))
nyc_math_memb$diffscore_public = as.numeric(c(nyc_math_memb[1,4] - nyc_math_summary[2,3],
                                          nyc_math_memb[2,4] - nyc_math_summary[2,3],
                                          nyc_math_memb[3,4] - nyc_math_summary[2,3],
                                          nyc_math_memb[4,4] - nyc_math_summary[2,3],
                                          nyc_math_memb[5,4] - nyc_math_summary[2,3],
                                          nyc_math_memb[6,4] - nyc_math_summary[2,3]))
str(nyc_math_memb)
sd <- nyc_math_summary[1,4]
sd2 <- nyc_math_summary[2,4]
nyc_math_memb <- nyc_math_memb %>% 
  mutate(stdscore_charter = diffscore_charter / as.numeric(c(sd,sd,sd,sd,sd,sd)),
         stdscore_public = diffscore_public / as.numeric(c(sd2,sd2,sd2,sd2,sd2,sd2)))
nyc_math_memb$difflvl_charter = as.numeric(c(nyc_math_memb[1,6] - nyc_math_summary[1,5],
                                             nyc_math_memb[2,6] - nyc_math_summary[1,5],
                                             nyc_math_memb[3,6] - nyc_math_summary[1,5],
                                             nyc_math_memb[4,6] - nyc_math_summary[1,5],
                                             nyc_math_memb[5,6] - nyc_math_summary[1,5],
                                             nyc_math_memb[6,6] - nyc_math_summary[1,5]))
nyc_math_memb$difflvl_public = as.numeric(c(nyc_math_memb[1,6] - nyc_math_summary[2,5],
                                             nyc_math_memb[2,6] - nyc_math_summary[2,5],
                                             nyc_math_memb[3,6] - nyc_math_summary[2,5],
                                             nyc_math_memb[4,6] - nyc_math_summary[2,5],
                                             nyc_math_memb[5,6] - nyc_math_summary[2,5],
                                             nyc_math_memb[6,6] - nyc_math_summary[2,5]))
nyc_math_memb <- nyc_math_memb %>% 
  mutate(stdlvl_charter = difflvl_charter / as.numeric(c(nyc_math_summary[1,6],
                                                         nyc_math_summary[1,6],
                                                         nyc_math_summary[1,6],
                                                         nyc_math_summary[1,6],
                                                         nyc_math_summary[1,6],
                                                         nyc_math_summary[1,6])),
         stdlvl_public = difflvl_public / as.numeric(c(nyc_math_summary[2,6],
                                                       nyc_math_summary[2,6],
                                                       nyc_math_summary[2,6],
                                                       nyc_math_summary[2,6],
                                                       nyc_math_summary[2,6],
                                                       nyc_math_summary[2,6]))) %>% 
  select(school, n_math, 
         mean_score, diffscore_charter, stdscore_charter, diffscore_public, stdscore_public,
         perc_lvl3and4, difflvl_charter, stdlvl_charter, difflvl_public, stdlvl_public) %>% 
  arrange(school)
nyc_achievement <- nyc_math_memb
names(nyc_achievement)[3:12] <- c("mean_math","diffmath_charter","stdmath_charter",
                                "diffmath_public", "stdmath_public",
                                "mathperc_lvl34", "diffmathlvl_charter","stdmathlvl_charter",
                                "diffmathlvl_public","stdmath_public")
write.csv(nyc_achievement, file = file.path('standardized_NYC_mem.csv'), row.names = FALSE)

nyc_ela_sys <- read_excel("NYC_ELA_2013-2019.xlsx", sheet = "All") # Read NYC's ELA
# sheets: All, SWD, Ethnicity, Gender, Econ Status, ELL
View(nyc_ela_sys)
names(nyc_ela_sys) <- tolower(names(nyc_ela_sys))
names(nyc_ela_sys)[3] <- "school"
names(nyc_ela_sys)[7:18] <- c("n_ela","mean_score","count_lvl1","perc_lvl1","count_lvl2","perc_lvl2",
                               "count_lvl3","perc_lvl3","count_lvl4","perc_lvl4","count_lvl3and4","perc_lvl3and4")
names(nyc_ela_sys)
nyc_ela_sys <- nyc_ela_sys %>% 
  filter(grade == "All Grades", year == "2019") %>% 
  select(school, n_ela, mean_score, count_lvl3and4, perc_lvl3and4) %>% 
  arrange(school)
str(nyc_ela_sys)
nyc_ela_sys$mean_score <- as.numeric(nyc_ela_sys$mean_score)
nyc_ela_sys$count_lvl3and4 <- as.numeric(nyc_ela_sys$count_lvl3and4)
nyc_ela_sys$perc_lvl3and4 <- as.numeric(nyc_ela_sys$perc_lvl3and4)
nyc_public_summary <- nyc_ela_sys %>% 
  summarize(avg_schoolmean = mean(mean_score),
            sd_schoolmean = sd(mean_score),
            avg_perc_lvl34 = mean(perc_lvl3and4),
            sd_perc_lvl34 = sd(perc_lvl3and4),
            n = n())
nyc_public_summary
nyc_ela_charters <- read_excel("NYC_charter_2013-2019.xlsx", sheet = "ELA") # load charter data. Sheet names: Math, ELA
names(nyc_ela_charters) <- tolower(names(nyc_ela_charters))
names(nyc_ela_charters)[3] <- "school"
names(nyc_ela_charters)[7:18] <- c("n_ela","mean_score","count_lvl1","perc_lvl1","count_lvl2","perc_lvl2",
                                    "count_lvl3","perc_lvl3","count_lvl4","perc_lvl4","count_lvl3and4","perc_lvl3and4")
nyc_ela_charters <- nyc_ela_charters %>% 
  filter(grade == "All Grades", year == "2019") %>% 
  select(school, category, n_ela, mean_score, count_lvl3and4, perc_lvl3and4)
nyc_ela_memb <- nyc_ela_charters %>% 
  filter(school == "ACADEMY OF THE CITY CHARTER SCHOOL" |
           school == "BROOKLYN PROSPECT CHARTER SCHOOL" |
           school == "BROOKLYN PROSPECT CHARTER SCHOOL DOWNTOWN" |
           school == "CENTRAL QUEENS ACADEMY CHARTER SCHOOL"|
           school == "COMMUNITY ROOTS CHARTER SCHOOL" |
           school == "HELLENIC CLASSICAL CHARTER SCHOOL")
nyc_ela_charters$mean_score <- as.numeric(nyc_ela_charters$mean_score)
nyc_ela_charters$count_lvl3and4 <- as.numeric(nyc_ela_charters$count_lvl3and4)
nyc_ela_charters$perc_lvl3and4 <- as.numeric(nyc_ela_charters$perc_lvl3and4)
nyc_charters_summary <- nyc_ela_charters %>% 
  summarize(avg_schoolmean = mean(mean_score),
            sd_schoolmean = sd(mean_score),
            avg_perc_lvl34 = mean(perc_lvl3and4),
            sd_perc_lvl34 = sd(perc_lvl3and4),
            n = n())
nyc_charters_summary
nyc_public_summary
nyc_ela_summary <- rbind(nyc_charters_summary, nyc_public_summary)
nyc_ela_summary$comparison <- c("NYC charters","NYC public")
nyc_ela_summary <- nyc_ela_summary %>% # City's comparison groups
  select(comparison,n,avg_schoolmean,sd_schoolmean,avg_perc_lvl34,sd_perc_lvl34)
str(nyc_ela_summary)
nyc_ela_memb$mean_score <- as.numeric(nyc_ela_memb$mean_score)
nyc_ela_memb$count_lvl3and4 <- as.numeric(nyc_ela_memb$count_lvl3and4)
nyc_ela_memb$perc_lvl3and4 <- as.numeric(nyc_ela_memb$perc_lvl3and4)
nyc_ela_memb$diffscore_charter = as.numeric(c(nyc_ela_memb[1,4] - nyc_ela_summary[1,3],
                                               nyc_ela_memb[2,4] - nyc_ela_summary[1,3],
                                               nyc_ela_memb[3,4] - nyc_ela_summary[1,3],
                                               nyc_ela_memb[4,4] - nyc_ela_summary[1,3],
                                               nyc_ela_memb[5,4] - nyc_ela_summary[1,3],
                                               nyc_ela_memb[6,4] - nyc_ela_summary[1,3]))
nyc_ela_memb$diffscore_public = as.numeric(c(nyc_ela_memb[1,4] - nyc_ela_summary[2,3],
                                              nyc_ela_memb[2,4] - nyc_ela_summary[2,3],
                                              nyc_ela_memb[3,4] - nyc_ela_summary[2,3],
                                              nyc_ela_memb[4,4] - nyc_ela_summary[2,3],
                                              nyc_ela_memb[5,4] - nyc_ela_summary[2,3],
                                              nyc_ela_memb[6,4] - nyc_ela_summary[2,3]))
str(nyc_ela_memb)
sd <- nyc_ela_summary[1,4]
sd2 <- nyc_ela_summary[2,4]
nyc_ela_memb <- nyc_ela_memb %>% 
  mutate(stdscore_charter = diffscore_charter / as.numeric(c(sd,sd,sd,sd,sd,sd)),
         stdscore_public = diffscore_public / as.numeric(c(sd2,sd2,sd2,sd2,sd2,sd2)))
nyc_ela_memb$difflvl_charter = as.numeric(c(nyc_ela_memb[1,6] - nyc_ela_summary[1,5],
                                             nyc_ela_memb[2,6] - nyc_ela_summary[1,5],
                                             nyc_ela_memb[3,6] - nyc_ela_summary[1,5],
                                             nyc_ela_memb[4,6] - nyc_ela_summary[1,5],
                                             nyc_ela_memb[5,6] - nyc_ela_summary[1,5],
                                             nyc_ela_memb[6,6] - nyc_ela_summary[1,5]))
nyc_ela_memb$difflvl_public = as.numeric(c(nyc_ela_memb[1,6] - nyc_ela_summary[2,5],
                                            nyc_ela_memb[2,6] - nyc_ela_summary[2,5],
                                            nyc_ela_memb[3,6] - nyc_ela_summary[2,5],
                                            nyc_ela_memb[4,6] - nyc_ela_summary[2,5],
                                            nyc_ela_memb[5,6] - nyc_ela_summary[2,5],
                                            nyc_ela_memb[6,6] - nyc_ela_summary[2,5]))
nyc_ela_memb <- nyc_ela_memb %>% 
  mutate(stdlvl_charter = difflvl_charter / as.numeric(c(nyc_ela_summary[1,6],
                                                         nyc_ela_summary[1,6],
                                                         nyc_ela_summary[1,6],
                                                         nyc_ela_summary[1,6],
                                                         nyc_ela_summary[1,6],
                                                         nyc_ela_summary[1,6])),
         stdlvl_public = difflvl_public / as.numeric(c(nyc_ela_summary[2,6],
                                                       nyc_ela_summary[2,6],
                                                       nyc_ela_summary[2,6],
                                                       nyc_ela_summary[2,6],
                                                       nyc_ela_summary[2,6],
                                                       nyc_ela_summary[2,6]))) %>% 
  select(school, n_ela, 
         mean_score, diffscore_charter, stdscore_charter, diffscore_public, stdscore_public,
         perc_lvl3and4, difflvl_charter, stdlvl_charter, difflvl_public, stdlvl_public) %>% 
  arrange(school)
nyc_achievement <- nyc_ela_memb
names(nyc_achievement)[3:12] <- c("mean_ela","diffela_charter","stdela_charter",
                                  "diffela_public", "stdela_public",
                                  "elaperc_lvl34", "diffelalvl_charter","stdelalvl_charter",
                                  "diffelalvl_public","stdela_public")
y <- read.csv("standardized_NYC_mem.csv")
nyc_achievement <- merge(y,nyc_achievement, by="school")
write.csv(nyc_achievement, file = file.path('standardized_NYC_mem.csv'), row.names = FALSE)
nyc_mathela_summary <- merge(nyc_math_summary,nyc_ela_summary,by="comparison")
write.csv(nyc_mathela_summary, file = file.path('NYCsummarystats.csv'), row.names = FALSE)


# 
#-------
# CALIFORNIA - achievement on CAASPP
caaspp <- read.csv("sb_ca2018_all_csv_v3/sb_ca2018_all_csv_v3.txt")
summary(caaspp)
names(caaspp)<- tolower(names(caaspp))
str(caaspp) # change data formatting to factor or numeric
caaspp$test.id <- as.factor(caaspp$test.id)
levels(caaspp$test.id) <- c("ela","math")
caaspp$percentage.standard.met.and.above <- as.numeric(caaspp$percentage.standard.met.and.above) * .0001
caaspp$mean.scale.score <- as.numeric(as.character(caaspp$mean.scale.score))

caaspp_ent <- read.csv("sb_ca2018_all_csv_v3/sb_ca2018entities_csv.txt")
caaspp_dist_ela <- caaspp %>% 
  filter(district.code == "64733" & subgroup.id == "1" & test.id == "ela"| # LA Unified
          district.code == "68338" & subgroup.id == "1" & test.id == "ela" | # San Diego Unified
           district.code == "76471" & subgroup.id == "1" & test.id == "ela"
           ) %>% # "all students" id
  arrange(school.code) %>% 
  select(district.code, school.code, test.year,subgroup.id,test.id,
         total.tested.at.entity.level,total.tested.with.scores,grade,
         caaspp.reported.enrollment,students.with.scores,mean.scale.score,percentage.standard.exceeded,
         percentage.standard.met, percentage.standard.met.and.above,percentage.standard.nearly.met,
         percentage.standard.not.met)
caaspp_dist_ela$district.code[caaspp_dist_ela$district.code=="64733"] <- "Los Angeles Unified"
caaspp_dist_ela$district.code[caaspp_dist_ela$district.code=="68338"] <- "San Diego Unified"
caaspp_dist_ela$district.code[caaspp_dist_ela$district.code=="76471"] <- "San Diego Unified"
browseURL("https://caaspp.cde.ca.gov/sb2018/UnderstandingCAASPPReports") # scoring definitions
browseURL("https://caaspp.cde.ca.gov/sb2018/research_fixfileformat18") # files record definitions
caaspp_distela_sum <- caaspp_dist_ela %>% 
  group_by(district.code, grade) %>% 
  summarize(mean_ela = mean(mean.scale.score, na.rm = TRUE),
            sd_ela = sd(mean.scale.score, na.rm = TRUE),
            mean_perclvl = mean(percentage.standard.met.and.above),
            sd_perclvl = sd(percentage.standard.met.and.above))
caaspp_memb <- caaspp_dist_ela %>% # Select the members
  filter(school.code == "122556"| # COTW Hollywood
           school.code == "126177" | # COTW Silver Lake
           school.code == "126193" | # COTW Mar Vista
           school.code == "127886" | # CLIC
           school.code == "134148" | # The City
           school.code == "100677" | # High Tech LA
           school.code == "101204" | # High Tech Middle
           school.code == "106732" | # High Tech Int'l
           school.code == "107573" | # High Tech Middle Media Arts
           school.code == "108787" | # HTH Media Arts
           school.code == "131565" | # High Tech Elem
           school.code == "3731247" | # HTH
           school.code == "6117683" | # High Tech Elem Expl
           school.code == "114678" | # HTH Chula Vista
           school.code == "114694" | # HTH North County
           school.code == "119271" | # High Tech Middle North County
           school.code == "123042" | # High Tech Middle Chula Vista
           school.code == "123059" | # High Tech Elem Chula Vista
           school.code == "127605" | # High Tech Elem North County
           school.code == "122754" | # Valley Elem
           school.code == "122838") %>%  # Valley Middle
  arrange(district.code, school.code, grade)
str(caaspp_memb)  
caaspp_memb$school.name = as.character(caaspp_memb$school.code)
caaspp_memb$school.name[caaspp_memb$school.name=="122838"] <- "Valley Charter Middle"
caaspp_memb$school.name[caaspp_memb$school.name=="122556"] <- "Citizens of the World Hollywood"
caaspp_memb$school.name[caaspp_memb$school.name=="126177"] <- "Citizens of the World Silver Lake"
caaspp_memb$school.name[caaspp_memb$school.name=="126193"] <- "Citizens of the World Mar Vista"
caaspp_memb$school.name[caaspp_memb$school.name=="127886"] <- "City Language Immersion Charter"
caaspp_memb$school.name[caaspp_memb$school.name=="134148"] <- "The City"
caaspp_memb$school.name[caaspp_memb$school.name=="100677"] <- "High Tech LA"
caaspp_memb$school.name[caaspp_memb$school.name=="101204"] <- "High Tech Middle"
caaspp_memb$school.name[caaspp_memb$school.name=="106732"] <- "High Tech International"
caaspp_memb$school.name[caaspp_memb$school.name=="107573"] <- "High Tech Middle Media Arts"
caaspp_memb$school.name[caaspp_memb$school.name=="108787"] <- "High Tech High Media Arts"
caaspp_memb$school.name[caaspp_memb$school.name=="131565"] <- "High Tech Elementary"
caaspp_memb$school.name[caaspp_memb$school.name=="3731247"] <- "High Tech High"
caaspp_memb$school.name[caaspp_memb$school.name=="6117683"] <- "High Tech Elementary Explorer"
caaspp_memb$school.name[caaspp_memb$school.name=="114678"] <- "High Tech High Chula Vista"
caaspp_memb$school.name[caaspp_memb$school.name=="114694"] <- "High Tech High North County"
caaspp_memb$school.name[caaspp_memb$school.name=="123042"] <- "High Tech Middle Chula Vista"
caaspp_memb$school.name[caaspp_memb$school.name=="123059"] <- "High Tech Elementary Chula Vista"
caaspp_memb$school.name[caaspp_memb$school.name=="127605"] <- "High Tech Elementary North County"
caaspp_memb$school.name[caaspp_memb$school.name=="122754"] <- "Valley Charter Elementary"
caaspp_memb$school.name[caaspp_memb$school.name=="119271"] <- "High Tech Middle North County"
caaspp_memb <- caaspp_memb %>% 
  select(district.code, school.name, test.id,grade, caaspp.reported.enrollment,
         students.with.scores,mean.scale.score,percentage.standard.met.and.above) %>% 
  arrange(district.code, school.name)
names(caaspp_memb) <- c("district","school","subject","grade","enrolled","n_ela","mean_ela","elaperc_lvl")
caaspp_memb <- caaspp_memb %>% 
  select(district, school, subject, grade, enrolled, n_ela, mean_ela, elaperc_lvl) %>% 
  arrange(district, grade)
caaspp_memb$diffela_charter = caaspp_memb$mean_ela
names(caaspp_memb)
caaspp_memb[1:5, 9] <- as.numeric(c(caaspp_memb[1,7] - caaspp_distela_sum[1,3], # 3rd grade scores in LA
                                    caaspp_memb[2,7] - caaspp_distela_sum[1,3],
                                    caaspp_memb[3,7] - caaspp_distela_sum[1,3], 
                                    caaspp_memb[4,7] - caaspp_distela_sum[1,3],
                                    caaspp_memb[5,7] - caaspp_distela_sum[1,3]))
caaspp_memb[6:10, 9] <- as.numeric(c(caaspp_memb[6,7] - caaspp_distela_sum[2,3], # 4th grade scores in LA 
                                    caaspp_memb[7,7] - caaspp_distela_sum[2,3],
                                    caaspp_memb[8,7] - caaspp_distela_sum[2,3], 
                                    caaspp_memb[9,7] - caaspp_distela_sum[2,3],
                                    caaspp_memb[10,7] - caaspp_distela_sum[2,3]))
caaspp_memb[11:15, 9] <- as.numeric(c(caaspp_memb[11,7] - caaspp_distela_sum[3,3], # 5th grade scores in LA 
                                     caaspp_memb[12,7] - caaspp_distela_sum[3,3],
                                     caaspp_memb[13,7] - caaspp_distela_sum[3,3], 
                                     caaspp_memb[14,7] - caaspp_distela_sum[3,3],
                                     caaspp_memb[15,7] - caaspp_distela_sum[3,3]))
caaspp_memb[16:19, 9] <- as.numeric(c(caaspp_memb[16,7] - caaspp_distela_sum[4,3], # 6th grade scores in LA 
                                      caaspp_memb[17,7] - caaspp_distela_sum[4,3],
                                      caaspp_memb[18,7] - caaspp_distela_sum[4,3], 
                                      caaspp_memb[19,7] - caaspp_distela_sum[4,3]))
caaspp_memb[20:22, 9] <- as.numeric(c(caaspp_memb[20,7] - caaspp_distela_sum[5,3], # 7th grade scores in LA 
                                      caaspp_memb[21,7] - caaspp_distela_sum[5,3],
                                      caaspp_memb[22,7] - caaspp_distela_sum[5,3]))
caaspp_memb[23:25, 9] <- as.numeric(c(caaspp_memb[23,7] - caaspp_distela_sum[6,3], # 8th grade scores in LA 
                                      caaspp_memb[24,7] - caaspp_distela_sum[6,3],
                                      caaspp_memb[25,7] - caaspp_distela_sum[6,3]))
caaspp_memb[26, 9] <- as.numeric(c(caaspp_memb[26,7] - caaspp_distela_sum[7,3])) # 11th grade scores in LA
caaspp_memb$diffelalvl_public = caaspp_memb$elaperc_lvl
caaspp_memb[1:5, 10] <- as.numeric(c(caaspp_memb[1,8] - caaspp_distela_sum[1,5], # 3rd grade perc met/above LA
                                    caaspp_memb[2,8] - caaspp_distela_sum[1,5],
                                    caaspp_memb[3,8] - caaspp_distela_sum[1,5], 
                                    caaspp_memb[4,8] - caaspp_distela_sum[1,5],
                                    caaspp_memb[5,8] - caaspp_distela_sum[1,5]))
caaspp_memb[6:10, 10] <- as.numeric(c(caaspp_memb[6,8] - caaspp_distela_sum[2,5], # 4th grade perc met/above LA
                                     caaspp_memb[7,8] - caaspp_distela_sum[2,5],
                                     caaspp_memb[8,8] - caaspp_distela_sum[2,5], 
                                     caaspp_memb[9,8] - caaspp_distela_sum[2,5],
                                     caaspp_memb[10,8] - caaspp_distela_sum[2,5]))
caaspp_memb[11:15, 10] <- as.numeric(c(caaspp_memb[11,8] - caaspp_distela_sum[3,5], # 5th grade perc met/above LA
                                      caaspp_memb[12,8] - caaspp_distela_sum[3,5],
                                      caaspp_memb[13,8] - caaspp_distela_sum[3,5], 
                                      caaspp_memb[14,8] - caaspp_distela_sum[3,5],
                                      caaspp_memb[15,8] - caaspp_distela_sum[3,5]))
caaspp_memb[16:19, 10] <- as.numeric(c(caaspp_memb[16,8] - caaspp_distela_sum[4,5], # 6th grade perc met/above LA
                                      caaspp_memb[17,8] - caaspp_distela_sum[4,5],
                                      caaspp_memb[18,8] - caaspp_distela_sum[4,5], 
                                      caaspp_memb[19,8] - caaspp_distela_sum[4,5]))
caaspp_memb[20:22, 10] <- as.numeric(c(caaspp_memb[20,8] - caaspp_distela_sum[5,5], # 7th grade perc met/above LA
                                      caaspp_memb[21,8] - caaspp_distela_sum[5,5],
                                      caaspp_memb[22,8] - caaspp_distela_sum[5,5]))
caaspp_memb[23:25, 10] <- as.numeric(c(caaspp_memb[23,8] - caaspp_distela_sum[6,5], # 8th grade perc met/above LA
                                      caaspp_memb[24,8] - caaspp_distela_sum[6,5],
                                      caaspp_memb[25,8] - caaspp_distela_sum[6,5]))
caaspp_memb[26, 10] <- as.numeric(c(caaspp_memb[26,8] - caaspp_distela_sum[7,5])) # 11th grade perc met/above LA
caaspp_memb[35:38, 9] <- as.numeric(c(caaspp_memb[35,7] - caaspp_distela_sum[9,3], #3rd grade scores SD
                                      caaspp_memb[36,7] - caaspp_distela_sum[9,3],
                                      caaspp_memb[37,7] - caaspp_distela_sum[9,3],
                                      caaspp_memb[38,7] - caaspp_distela_sum[9,3]))
caaspp_memb[39:42, 9] <- as.numeric(c(caaspp_memb[39,7] - caaspp_distela_sum[10,3], # 4th grade scores SD
                                      caaspp_memb[40,7] - caaspp_distela_sum[10,3],
                                      caaspp_memb[41,7] - caaspp_distela_sum[10,3],
                                      caaspp_memb[42,7] - caaspp_distela_sum[10,3]))
caaspp_memb[43:46, 9] <- as.numeric(c(caaspp_memb[43,7] - caaspp_distela_sum[11,3], # 5th grade scores SD
                                      caaspp_memb[44,7] - caaspp_distela_sum[11,3],
                                      caaspp_memb[45,7] - caaspp_distela_sum[11,3],
                                      caaspp_memb[46,7] - caaspp_distela_sum[11,3]))
caaspp_memb[47:50, 9] <- as.numeric(c(caaspp_memb[47,7] - caaspp_distela_sum[12,3], # 6th grade scores SD
                                      caaspp_memb[48,7] - caaspp_distela_sum[12,3],
                                      caaspp_memb[49,7] - caaspp_distela_sum[12,3],
                                      caaspp_memb[50,7] - caaspp_distela_sum[12,3]))
caaspp_memb[51:54, 9] <- as.numeric(c(caaspp_memb[51,7] - caaspp_distela_sum[13,3], # 7th grade scores SD
                                      caaspp_memb[52,7] - caaspp_distela_sum[13,3],
                                      caaspp_memb[53,7] - caaspp_distela_sum[13,3],
                                      caaspp_memb[54,7] - caaspp_distela_sum[13,3]))
caaspp_memb[55:58, 9] <- as.numeric(c(caaspp_memb[55,7] - caaspp_distela_sum[14,3], # 8th grade scores SD
                                      caaspp_memb[56,7] - caaspp_distela_sum[14,3],
                                      caaspp_memb[57,7] - caaspp_distela_sum[14,3],
                                      caaspp_memb[58,7] - caaspp_distela_sum[14,3]))
caaspp_memb[59:63, 9] <- as.numeric(c(caaspp_memb[59,7] - caaspp_distela_sum[15,3], # 11th grade scores SD
                                      caaspp_memb[60,7] - caaspp_distela_sum[15,3],
                                      caaspp_memb[61,7] - caaspp_distela_sum[15,3],
                                      caaspp_memb[62,7] - caaspp_distela_sum[15,3],
                                      caaspp_memb[63,7] - caaspp_distela_sum[15,3]))
caaspp_memb[27:34, 10] <- as.numeric(c(caaspp_memb[27,8] - caaspp_distela_sum[8,5], # perc met above LA
                                       caaspp_memb[28,8] - caaspp_distela_sum[8,5],
                                       caaspp_memb[29,8] - caaspp_distela_sum[8,5],
                                       caaspp_memb[30,8] - caaspp_distela_sum[8,5],
                                       caaspp_memb[31,8] - caaspp_distela_sum[8,5],
                                       caaspp_memb[32,8] - caaspp_distela_sum[8,5],
                                       caaspp_memb[33,8] - caaspp_distela_sum[8,5],
                                       caaspp_memb[34,8] - caaspp_distela_sum[8,5]))
caaspp_memb[64:76, 10] <- as.numeric(c(caaspp_memb[64,8] - caaspp_distela_sum[16,5], # perc met above SD
                                       caaspp_memb[65,8] - caaspp_distela_sum[16,5],
                                       caaspp_memb[66,8] - caaspp_distela_sum[16,5],
                                       caaspp_memb[67,8] - caaspp_distela_sum[16,5],
                                       caaspp_memb[68,8] - caaspp_distela_sum[16,5],
                                       caaspp_memb[69,8] - caaspp_distela_sum[16,5],
                                       caaspp_memb[70,8] - caaspp_distela_sum[16,5],
                                       caaspp_memb[71,8] - caaspp_distela_sum[16,5],
                                       caaspp_memb[72,8] - caaspp_distela_sum[16,5],
                                       caaspp_memb[73,8] - caaspp_distela_sum[16,5],
                                       caaspp_memb[74,8] - caaspp_distela_sum[16,5],
                                       caaspp_memb[75,8] - caaspp_distela_sum[16,5],
                                       caaspp_memb[76,8] - caaspp_distela_sum[16,5]))
caaspp_memb_ela <- caaspp_memb %>% 
  mutate(stdela_public = diffela_charter / as.numeric(c(caaspp_distela_sum[1,4],caaspp_distela_sum[1,4],
                                             caaspp_distela_sum[1,4],caaspp_distela_sum[1,4],
                                             caaspp_distela_sum[1,4],caaspp_distela_sum[2,4],
                                             caaspp_distela_sum[2,4],caaspp_distela_sum[2,4],
                                             caaspp_distela_sum[2,4],caaspp_distela_sum[2,4],
                                             caaspp_distela_sum[3,4],caaspp_distela_sum[3,4],
                                             caaspp_distela_sum[3,4],caaspp_distela_sum[3,4],
                                             caaspp_distela_sum[3,4],caaspp_distela_sum[4,4],
                                             caaspp_distela_sum[4,4],caaspp_distela_sum[4,4],
                                             caaspp_distela_sum[4,4],caaspp_distela_sum[5,4],
                                             caaspp_distela_sum[5,4],caaspp_distela_sum[5,4],
                                             caaspp_distela_sum[6,4],caaspp_distela_sum[6,4],
                                             caaspp_distela_sum[6,4],caaspp_distela_sum[7,4],
                                             1,1,1,1,1,1,1,1,caaspp_distela_sum[9,4],
                                             caaspp_distela_sum[9,4],caaspp_distela_sum[9,4],
                                             caaspp_distela_sum[9,4],caaspp_distela_sum[10,4],
                                             caaspp_distela_sum[10,4],caaspp_distela_sum[10,4],
                                             caaspp_distela_sum[10,4],caaspp_distela_sum[11,4],
                                             caaspp_distela_sum[11,4],caaspp_distela_sum[11,4],
                                             caaspp_distela_sum[11,4],caaspp_distela_sum[12,4],
                                             caaspp_distela_sum[12,4],caaspp_distela_sum[12,4],
                                             caaspp_distela_sum[12,4],caaspp_distela_sum[13,4],
                                             caaspp_distela_sum[13,4],caaspp_distela_sum[13,4],
                                             caaspp_distela_sum[13,4],caaspp_distela_sum[14,4],
                                             caaspp_distela_sum[14,4],caaspp_distela_sum[14,4],
                                             caaspp_distela_sum[14,4],caaspp_distela_sum[15,4],
                                             caaspp_distela_sum[15,4],caaspp_distela_sum[15,4],
                                             caaspp_distela_sum[15,4],caaspp_distela_sum[15,4],
                                             1,1,1,1,1,1,1,1,1,1,1,1,1)),
         stdelalvl_public = diffelalvl_public / as.numeric(c(caaspp_distela_sum[1,6],caaspp_distela_sum[1,6],
                                                             caaspp_distela_sum[1,6],caaspp_distela_sum[1,6],
                                                             caaspp_distela_sum[1,6],caaspp_distela_sum[2,6],
                                                             caaspp_distela_sum[2,6],caaspp_distela_sum[2,6],
                                                             caaspp_distela_sum[2,6],caaspp_distela_sum[2,6],
                                                             caaspp_distela_sum[3,6],caaspp_distela_sum[3,6],
                                                             caaspp_distela_sum[3,6],caaspp_distela_sum[3,6],
                                                             caaspp_distela_sum[3,6],caaspp_distela_sum[4,6],
                                                             caaspp_distela_sum[4,6],caaspp_distela_sum[4,6],
                                                             caaspp_distela_sum[4,6],caaspp_distela_sum[5,6],
                                                             caaspp_distela_sum[5,6],caaspp_distela_sum[5,6],
                                                             caaspp_distela_sum[6,6],caaspp_distela_sum[6,6],
                                                             caaspp_distela_sum[6,6],caaspp_distela_sum[7,6],
                                                             1,1,1,1,1,1,1,1,caaspp_distela_sum[9,6],
                                                             caaspp_distela_sum[9,6],caaspp_distela_sum[9,6],
                                                             caaspp_distela_sum[9,6],caaspp_distela_sum[10,6],
                                                             caaspp_distela_sum[10,6],caaspp_distela_sum[10,6],
                                                             caaspp_distela_sum[10,6],caaspp_distela_sum[11,6],
                                                             caaspp_distela_sum[11,6],caaspp_distela_sum[11,6],
                                                             caaspp_distela_sum[11,6],caaspp_distela_sum[12,6],
                                                             caaspp_distela_sum[12,6],caaspp_distela_sum[12,6],
                                                             caaspp_distela_sum[12,6],caaspp_distela_sum[13,6],
                                                             caaspp_distela_sum[13,6],caaspp_distela_sum[13,6],
                                                             caaspp_distela_sum[13,6],caaspp_distela_sum[14,6],
                                                             caaspp_distela_sum[14,6],caaspp_distela_sum[14,6],
                                                             caaspp_distela_sum[14,6],caaspp_distela_sum[15,6],
                                                             caaspp_distela_sum[15,6],caaspp_distela_sum[15,6],
                                                             caaspp_distela_sum[15,6],caaspp_distela_sum[15,6],
                                                             1,1,1,1,1,1,1,1,1,1,1,1,1))) %>% 
  select(district, school, grade, n_ela, mean_ela, diffela_charter,stdela_public,
         elaperc_lvl, diffelalvl_public, stdelalvl_public)

caaspp_dist_math <- caaspp %>% 
  filter(district.code == "64733" & subgroup.id == "1" & test.id == "math"| # LA Unified
           district.code == "68338" & subgroup.id == "1" & test.id == "math" | # San Diego Unified
           district.code == "76471" & subgroup.id == "1" & test.id == "math"
  ) %>% # "all students" id
  arrange(school.code) %>% 
  select(district.code, school.code, test.year,subgroup.id,test.id,
         total.tested.at.entity.level,total.tested.with.scores,grade,
         caaspp.reported.enrollment,students.with.scores,mean.scale.score,percentage.standard.exceeded,
         percentage.standard.met, percentage.standard.met.and.above,percentage.standard.nearly.met,
         percentage.standard.not.met)
caaspp_dist_math$district.code[caaspp_dist_math$district.code=="64733"] <- "Los Angeles Unified"
caaspp_dist_math$district.code[caaspp_dist_math$district.code=="68338"] <- "San Diego Unified"
caaspp_dist_math$district.code[caaspp_dist_math$district.code=="76471"] <- "San Diego Unified"
browseURL("https://caaspp.cde.ca.gov/sb2018/UnderstandingCAASPPReports") # scoring definitions
browseURL("https://caaspp.cde.ca.gov/sb2018/research_fixfileformat18") # files record definitions
caaspp_distmath_sum <- caaspp_dist_math %>% 
  group_by(district.code, grade) %>% 
  summarize(mean_math = mean(mean.scale.score, na.rm = TRUE),
            sd_math = sd(mean.scale.score, na.rm = TRUE),
            mean_perclvl = mean(percentage.standard.met.and.above),
            sd_perclvl = sd(percentage.standard.met.and.above))
caaspp_memb <- caaspp_dist_math %>% # Select the members
  filter(school.code == "122556"| # COTW Hollywood
           school.code == "126177" | # COTW Silver Lake
           school.code == "126193" | # COTW Mar Vista
           school.code == "127886" | # CLIC
           school.code == "134148" | # The City
           school.code == "100677" | # High Tech LA
           school.code == "101204" | # High Tech Middle
           school.code == "106732" | # High Tech Int'l
           school.code == "107573" | # High Tech Middle Media Arts
           school.code == "108787" | # HTH Media Arts
           school.code == "131565" | # High Tech Elem
           school.code == "3731247" | # HTH
           school.code == "6117683" | # High Tech Elem Expl
           school.code == "114678" | # HTH Chula Vista
           school.code == "114694" | # HTH North County
           school.code == "119271" | # High Tech Middle North County
           school.code == "123042" | # High Tech Middle Chula Vista
           school.code == "123059" | # High Tech Elem Chula Vista
           school.code == "127605" | # High Tech Elem North County
           school.code == "122754" | # Valley Elem
           school.code == "122838") %>%  # Valley Middle
  arrange(district.code, school.code, grade)
str(caaspp_memb)  
caaspp_memb$school.name = as.character(caaspp_memb$school.code)
caaspp_memb$school.name[caaspp_memb$school.name=="122838"] <- "Valley Charter Middle"
caaspp_memb$school.name[caaspp_memb$school.name=="122556"] <- "Citizens of the World Hollywood"
caaspp_memb$school.name[caaspp_memb$school.name=="126177"] <- "Citizens of the World Silver Lake"
caaspp_memb$school.name[caaspp_memb$school.name=="126193"] <- "Citizens of the World Mar Vista"
caaspp_memb$school.name[caaspp_memb$school.name=="127886"] <- "City Language Immersion Charter"
caaspp_memb$school.name[caaspp_memb$school.name=="134148"] <- "The City"
caaspp_memb$school.name[caaspp_memb$school.name=="100677"] <- "High Tech LA"
caaspp_memb$school.name[caaspp_memb$school.name=="101204"] <- "High Tech Middle"
caaspp_memb$school.name[caaspp_memb$school.name=="106732"] <- "High Tech International"
caaspp_memb$school.name[caaspp_memb$school.name=="107573"] <- "High Tech Middle Media Arts"
caaspp_memb$school.name[caaspp_memb$school.name=="108787"] <- "High Tech High Media Arts"
caaspp_memb$school.name[caaspp_memb$school.name=="131565"] <- "High Tech Elementary"
caaspp_memb$school.name[caaspp_memb$school.name=="3731247"] <- "High Tech High"
caaspp_memb$school.name[caaspp_memb$school.name=="6117683"] <- "High Tech Elementary Explorer"
caaspp_memb$school.name[caaspp_memb$school.name=="114678"] <- "High Tech High Chula Vista"
caaspp_memb$school.name[caaspp_memb$school.name=="114694"] <- "High Tech High North County"
caaspp_memb$school.name[caaspp_memb$school.name=="123042"] <- "High Tech Middle Chula Vista"
caaspp_memb$school.name[caaspp_memb$school.name=="123059"] <- "High Tech Elementary Chula Vista"
caaspp_memb$school.name[caaspp_memb$school.name=="127605"] <- "High Tech Elementary North County"
caaspp_memb$school.name[caaspp_memb$school.name=="122754"] <- "Valley Charter Elementary"
caaspp_memb$school.name[caaspp_memb$school.name=="119271"] <- "High Tech Middle North County"
caaspp_memb <- caaspp_memb %>% 
  select(district.code, school.name, test.id,grade, caaspp.reported.enrollment,
         students.with.scores,mean.scale.score,percentage.standard.met.and.above) %>% 
  arrange(district.code, school.name)
names(caaspp_memb) <- c("district","school","subject","grade","enrolled","n_math","mean_math","mathperc_lvl")
caaspp_memb <- caaspp_memb %>% 
  select(district, school, subject, grade, enrolled, n_math, mean_math, mathperc_lvl) %>% 
  arrange(district, grade)
caaspp_memb$diffmath_charter = caaspp_memb$mean_math
names(caaspp_memb)
caaspp_memb[1:5, 9] <- as.numeric(c(caaspp_memb[1,7] - caaspp_distmath_sum[1,3], # 3rd grade scores in LA
                                    caaspp_memb[2,7] - caaspp_distmath_sum[1,3],
                                    caaspp_memb[3,7] - caaspp_distmath_sum[1,3], 
                                    caaspp_memb[4,7] - caaspp_distmath_sum[1,3],
                                    caaspp_memb[5,7] - caaspp_distmath_sum[1,3]))
caaspp_memb[6:10, 9] <- as.numeric(c(caaspp_memb[6,7] - caaspp_distmath_sum[2,3], # 4th grade scores in LA 
                                     caaspp_memb[7,7] - caaspp_distmath_sum[2,3],
                                     caaspp_memb[8,7] - caaspp_distmath_sum[2,3], 
                                     caaspp_memb[9,7] - caaspp_distmath_sum[2,3],
                                     caaspp_memb[10,7] - caaspp_distmath_sum[2,3]))
caaspp_memb[11:15, 9] <- as.numeric(c(caaspp_memb[11,7] - caaspp_distmath_sum[3,3], # 5th grade scores in LA 
                                      caaspp_memb[12,7] - caaspp_distmath_sum[3,3],
                                      caaspp_memb[13,7] - caaspp_distmath_sum[3,3], 
                                      caaspp_memb[14,7] - caaspp_distmath_sum[3,3],
                                      caaspp_memb[15,7] - caaspp_distmath_sum[3,3]))
caaspp_memb[16:19, 9] <- as.numeric(c(caaspp_memb[16,7] - caaspp_distmath_sum[4,3], # 6th grade scores in LA 
                                      caaspp_memb[17,7] - caaspp_distmath_sum[4,3],
                                      caaspp_memb[18,7] - caaspp_distmath_sum[4,3], 
                                      caaspp_memb[19,7] - caaspp_distmath_sum[4,3]))
caaspp_memb[20:22, 9] <- as.numeric(c(caaspp_memb[20,7] - caaspp_distmath_sum[5,3], # 7th grade scores in LA 
                                      caaspp_memb[21,7] - caaspp_distmath_sum[5,3],
                                      caaspp_memb[22,7] - caaspp_distmath_sum[5,3]))
caaspp_memb[23:25, 9] <- as.numeric(c(caaspp_memb[23,7] - caaspp_distmath_sum[6,3], # 8th grade scores in LA 
                                      caaspp_memb[24,7] - caaspp_distmath_sum[6,3],
                                      caaspp_memb[25,7] - caaspp_distmath_sum[6,3]))
caaspp_memb[26, 9] <- as.numeric(c(caaspp_memb[26,7] - caaspp_distmath_sum[7,3])) # 11th grade scores in LA
caaspp_memb$diffmathlvl_public = caaspp_memb$mathperc_lvl
caaspp_memb[1:5, 10] <- as.numeric(c(caaspp_memb[1,8] - caaspp_distmath_sum[1,5], # 3rd grade perc met/above LA
                                     caaspp_memb[2,8] - caaspp_distmath_sum[1,5],
                                     caaspp_memb[3,8] - caaspp_distmath_sum[1,5], 
                                     caaspp_memb[4,8] - caaspp_distmath_sum[1,5],
                                     caaspp_memb[5,8] - caaspp_distmath_sum[1,5]))
caaspp_memb[6:10, 10] <- as.numeric(c(caaspp_memb[6,8] - caaspp_distmath_sum[2,5], # 4th grade perc met/above LA
                                      caaspp_memb[7,8] - caaspp_distmath_sum[2,5],
                                      caaspp_memb[8,8] - caaspp_distmath_sum[2,5], 
                                      caaspp_memb[9,8] - caaspp_distmath_sum[2,5],
                                      caaspp_memb[10,8] - caaspp_distmath_sum[2,5]))
caaspp_memb[11:15, 10] <- as.numeric(c(caaspp_memb[11,8] - caaspp_distmath_sum[3,5], # 5th grade perc met/above LA
                                       caaspp_memb[12,8] - caaspp_distmath_sum[3,5],
                                       caaspp_memb[13,8] - caaspp_distmath_sum[3,5], 
                                       caaspp_memb[14,8] - caaspp_distmath_sum[3,5],
                                       caaspp_memb[15,8] - caaspp_distmath_sum[3,5]))
caaspp_memb[16:19, 10] <- as.numeric(c(caaspp_memb[16,8] - caaspp_distmath_sum[4,5], # 6th grade perc met/above LA
                                       caaspp_memb[17,8] - caaspp_distmath_sum[4,5],
                                       caaspp_memb[18,8] - caaspp_distmath_sum[4,5], 
                                       caaspp_memb[19,8] - caaspp_distmath_sum[4,5]))
caaspp_memb[20:22, 10] <- as.numeric(c(caaspp_memb[20,8] - caaspp_distmath_sum[5,5], # 7th grade perc met/above LA
                                       caaspp_memb[21,8] - caaspp_distmath_sum[5,5],
                                       caaspp_memb[22,8] - caaspp_distmath_sum[5,5]))
caaspp_memb[23:25, 10] <- as.numeric(c(caaspp_memb[23,8] - caaspp_distmath_sum[6,5], # 8th grade perc met/above LA
                                       caaspp_memb[24,8] - caaspp_distmath_sum[6,5],
                                       caaspp_memb[25,8] - caaspp_distmath_sum[6,5]))
caaspp_memb[26, 10] <- as.numeric(c(caaspp_memb[26,8] - caaspp_distmath_sum[7,5])) # 11th grade perc met/above LA
caaspp_memb[35:38, 9] <- as.numeric(c(caaspp_memb[35,7] - caaspp_distmath_sum[9,3], #3rd grade scores SD
                                      caaspp_memb[36,7] - caaspp_distmath_sum[9,3],
                                      caaspp_memb[37,7] - caaspp_distmath_sum[9,3],
                                      caaspp_memb[38,7] - caaspp_distmath_sum[9,3]))
caaspp_memb[39:42, 9] <- as.numeric(c(caaspp_memb[39,7] - caaspp_distmath_sum[10,3], # 4th grade scores SD
                                      caaspp_memb[40,7] - caaspp_distmath_sum[10,3],
                                      caaspp_memb[41,7] - caaspp_distmath_sum[10,3],
                                      caaspp_memb[42,7] - caaspp_distmath_sum[10,3]))
caaspp_memb[43:46, 9] <- as.numeric(c(caaspp_memb[43,7] - caaspp_distmath_sum[11,3], # 5th grade scores SD
                                      caaspp_memb[44,7] - caaspp_distmath_sum[11,3],
                                      caaspp_memb[45,7] - caaspp_distmath_sum[11,3],
                                      caaspp_memb[46,7] - caaspp_distmath_sum[11,3]))
caaspp_memb[47:50, 9] <- as.numeric(c(caaspp_memb[47,7] - caaspp_distmath_sum[12,3], # 6th grade scores SD
                                      caaspp_memb[48,7] - caaspp_distmath_sum[12,3],
                                      caaspp_memb[49,7] - caaspp_distmath_sum[12,3],
                                      caaspp_memb[50,7] - caaspp_distmath_sum[12,3]))
caaspp_memb[51:54, 9] <- as.numeric(c(caaspp_memb[51,7] - caaspp_distmath_sum[13,3], # 7th grade scores SD
                                      caaspp_memb[52,7] - caaspp_distmath_sum[13,3],
                                      caaspp_memb[53,7] - caaspp_distmath_sum[13,3],
                                      caaspp_memb[54,7] - caaspp_distmath_sum[13,3]))
caaspp_memb[55:58, 9] <- as.numeric(c(caaspp_memb[55,7] - caaspp_distmath_sum[14,3], # 8th grade scores SD
                                      caaspp_memb[56,7] - caaspp_distmath_sum[14,3],
                                      caaspp_memb[57,7] - caaspp_distmath_sum[14,3],
                                      caaspp_memb[58,7] - caaspp_distmath_sum[14,3]))
caaspp_memb[59:63, 9] <- as.numeric(c(caaspp_memb[59,7] - caaspp_distmath_sum[15,3], # 11th grade scores SD
                                      caaspp_memb[60,7] - caaspp_distmath_sum[15,3],
                                      caaspp_memb[61,7] - caaspp_distmath_sum[15,3],
                                      caaspp_memb[62,7] - caaspp_distmath_sum[15,3],
                                      caaspp_memb[63,7] - caaspp_distmath_sum[15,3]))
caaspp_memb[27:34, 10] <- as.numeric(c(caaspp_memb[27,8] - caaspp_distmath_sum[8,5], # perc met above LA
                                       caaspp_memb[28,8] - caaspp_distmath_sum[8,5],
                                       caaspp_memb[29,8] - caaspp_distmath_sum[8,5],
                                       caaspp_memb[30,8] - caaspp_distmath_sum[8,5],
                                       caaspp_memb[31,8] - caaspp_distmath_sum[8,5],
                                       caaspp_memb[32,8] - caaspp_distmath_sum[8,5],
                                       caaspp_memb[33,8] - caaspp_distmath_sum[8,5],
                                       caaspp_memb[34,8] - caaspp_distmath_sum[8,5]))
caaspp_memb[64:76, 10] <- as.numeric(c(caaspp_memb[64,8] - caaspp_distmath_sum[16,5], # perc met above SD
                                       caaspp_memb[65,8] - caaspp_distmath_sum[16,5],
                                       caaspp_memb[66,8] - caaspp_distmath_sum[16,5],
                                       caaspp_memb[67,8] - caaspp_distmath_sum[16,5],
                                       caaspp_memb[68,8] - caaspp_distmath_sum[16,5],
                                       caaspp_memb[69,8] - caaspp_distmath_sum[16,5],
                                       caaspp_memb[70,8] - caaspp_distmath_sum[16,5],
                                       caaspp_memb[71,8] - caaspp_distmath_sum[16,5],
                                       caaspp_memb[72,8] - caaspp_distmath_sum[16,5],
                                       caaspp_memb[73,8] - caaspp_distmath_sum[16,5],
                                       caaspp_memb[74,8] - caaspp_distmath_sum[16,5],
                                       caaspp_memb[75,8] - caaspp_distmath_sum[16,5],
                                       caaspp_memb[76,8] - caaspp_distmath_sum[16,5]))
caaspp_memb_math <- caaspp_memb %>% 
  mutate(stdmath_public = diffmath_charter / as.numeric(c(caaspp_distmath_sum[1,4],caaspp_distmath_sum[1,4],
                                                        caaspp_distmath_sum[1,4],caaspp_distmath_sum[1,4],
                                                        caaspp_distmath_sum[1,4],caaspp_distmath_sum[2,4],
                                                        caaspp_distmath_sum[2,4],caaspp_distmath_sum[2,4],
                                                        caaspp_distmath_sum[2,4],caaspp_distmath_sum[2,4],
                                                        caaspp_distmath_sum[3,4],caaspp_distmath_sum[3,4],
                                                        caaspp_distmath_sum[3,4],caaspp_distmath_sum[3,4],
                                                        caaspp_distmath_sum[3,4],caaspp_distmath_sum[4,4],
                                                        caaspp_distmath_sum[4,4],caaspp_distmath_sum[4,4],
                                                        caaspp_distmath_sum[4,4],caaspp_distmath_sum[5,4],
                                                        caaspp_distmath_sum[5,4],caaspp_distmath_sum[5,4],
                                                        caaspp_distmath_sum[6,4],caaspp_distmath_sum[6,4],
                                                        caaspp_distmath_sum[6,4],caaspp_distmath_sum[7,4],
                                                        1,1,1,1,1,1,1,1,caaspp_distmath_sum[9,4],
                                                        caaspp_distmath_sum[9,4],caaspp_distmath_sum[9,4],
                                                        caaspp_distmath_sum[9,4],caaspp_distmath_sum[10,4],
                                                        caaspp_distmath_sum[10,4],caaspp_distmath_sum[10,4],
                                                        caaspp_distmath_sum[10,4],caaspp_distmath_sum[11,4],
                                                        caaspp_distmath_sum[11,4],caaspp_distmath_sum[11,4],
                                                        caaspp_distmath_sum[11,4],caaspp_distmath_sum[12,4],
                                                        caaspp_distmath_sum[12,4],caaspp_distmath_sum[12,4],
                                                        caaspp_distmath_sum[12,4],caaspp_distmath_sum[13,4],
                                                        caaspp_distmath_sum[13,4],caaspp_distmath_sum[13,4],
                                                        caaspp_distmath_sum[13,4],caaspp_distmath_sum[14,4],
                                                        caaspp_distmath_sum[14,4],caaspp_distmath_sum[14,4],
                                                        caaspp_distmath_sum[14,4],caaspp_distmath_sum[15,4],
                                                        caaspp_distmath_sum[15,4],caaspp_distmath_sum[15,4],
                                                        caaspp_distmath_sum[15,4],caaspp_distmath_sum[15,4],
                                                        1,1,1,1,1,1,1,1,1,1,1,1,1)),
         stdmathlvl_public = diffmathlvl_public / as.numeric(c(caaspp_distmath_sum[1,6],caaspp_distmath_sum[1,6],
                                                             caaspp_distmath_sum[1,6],caaspp_distmath_sum[1,6],
                                                             caaspp_distmath_sum[1,6],caaspp_distmath_sum[2,6],
                                                             caaspp_distmath_sum[2,6],caaspp_distmath_sum[2,6],
                                                             caaspp_distmath_sum[2,6],caaspp_distmath_sum[2,6],
                                                             caaspp_distmath_sum[3,6],caaspp_distmath_sum[3,6],
                                                             caaspp_distmath_sum[3,6],caaspp_distmath_sum[3,6],
                                                             caaspp_distmath_sum[3,6],caaspp_distmath_sum[4,6],
                                                             caaspp_distmath_sum[4,6],caaspp_distmath_sum[4,6],
                                                             caaspp_distmath_sum[4,6],caaspp_distmath_sum[5,6],
                                                             caaspp_distmath_sum[5,6],caaspp_distmath_sum[5,6],
                                                             caaspp_distmath_sum[6,6],caaspp_distmath_sum[6,6],
                                                             caaspp_distmath_sum[6,6],caaspp_distmath_sum[7,6],
                                                             1,1,1,1,1,1,1,1,caaspp_distmath_sum[9,6],
                                                             caaspp_distmath_sum[9,6],caaspp_distmath_sum[9,6],
                                                             caaspp_distmath_sum[9,6],caaspp_distmath_sum[10,6],
                                                             caaspp_distmath_sum[10,6],caaspp_distmath_sum[10,6],
                                                             caaspp_distmath_sum[10,6],caaspp_distmath_sum[11,6],
                                                             caaspp_distmath_sum[11,6],caaspp_distmath_sum[11,6],
                                                             caaspp_distmath_sum[11,6],caaspp_distmath_sum[12,6],
                                                             caaspp_distmath_sum[12,6],caaspp_distmath_sum[12,6],
                                                             caaspp_distmath_sum[12,6],caaspp_distmath_sum[13,6],
                                                             caaspp_distmath_sum[13,6],caaspp_distmath_sum[13,6],
                                                             caaspp_distmath_sum[13,6],caaspp_distmath_sum[14,6],
                                                             caaspp_distmath_sum[14,6],caaspp_distmath_sum[14,6],
                                                             caaspp_distmath_sum[14,6],caaspp_distmath_sum[15,6],
                                                             caaspp_distmath_sum[15,6],caaspp_distmath_sum[15,6],
                                                             caaspp_distmath_sum[15,6],caaspp_distmath_sum[15,6],
                                                             1,1,1,1,1,1,1,1,1,1,1,1,1))) %>% 
  select(district, school, grade, n_math, mean_math, diffmath_charter,stdmath_public,
         mathperc_lvl, diffmathlvl_public, stdmathlvl_public)
caaspp_memb <- merge(caaspp_memb_math,caaspp_memb_ela,by=c("district","school","grade"))
write.csv(caaspp_memb, file = file.path('caaspp_memb.csv'), row.names = FALSE)
caaspp_distsum <- merge(caaspp_distmath_sum,caaspp_distela_sum,by=c("district.code","grade"))
write.csv(caaspp_distsum, file = file.path('CAsummarystats.csv'), row.names = FALSE)

#-------------
# Colorado CMAS achievement data
browseURL("https://www.cde.state.co.us/assessment/cmas_sdf_2019") # data file notes
co_cmas <- read_excel("CO_CMAS_2019.xlsx")
co_cmas <- co_cmas[-c(1:9),]
colnames(co_cmas) = co_cmas[c(1),]
co_cmas <- co_cmas[-c(1),]
names(co_cmas) <- tolower(names(co_cmas))
names(co_cmas)[2:5] <- c("district_code","district_name","school_code","school_name")
names(co_cmas)[8:29] <- c("num_records","num_validscores","num_noscores","partic_rate",
                          "mean_score2019","sd_score","mean_score2018","count_notmeet","perc_notmeet",
                          "count_partial","perc_partial","count_approach","perc_approach",
                          "count_met","perc_met","count_exceed","perc_exceed","count_metexceed","perc_metexceed",
                          "count_metexceed2018","perc_metexceed2018","changeperc_metexceed")
str(co_cmas)
co_cmas$num_records <- as.numeric(gsub(",","",co_cmas$num_records))
co_cmas$num_validscores <- as.numeric(gsub(",","",co_cmas$num_validscores))
co_cmas$mean_score2019 <- as.numeric(co_cmas$mean_score2019)
co_cmas$sd_score <- as.numeric(co_cmas$sd_score)
co_cmas$count_metexceed <- as.numeric(gsub(",","",co_cmas$count_metexceed))
co_cmas$perc_metexceed <- as.numeric(co_cmas$perc_metexceed) * .01
co_cmas$district_name <- as.factor(co_cmas$district_name)
levels(co_cmas$district_name)
co_dist_math <- co_cmas %>% # Denver schools as a comparison district
  filter(district_name == "DENVER COUNTY 1" &
           grade == "All Grades" &
           subject == "Mathematics") %>% 
  select(school_name,num_validscores,subject,grade,mean_score2019,sd_score,count_metexceed,perc_metexceed) %>% 
  arrange(school_name)
co_dist_math$stdmathlvl_public <- sd(co_dist_math$perc_metexceed, na.rm = TRUE) # sd of portion students in the school that meet or exceed
co_dist_math <- co_dist_math %>% arrange(school_name)
co_memb_math <- co_dist_math[44:51,]
names(co_dist_math)
co_memb_math$diffmath_public = as.numeric(co_memb_math$mean_score2019) - as.numeric(co_dist_math[3,5])
co_memb_math$diffmathlvl_public = as.numeric(co_memb_math$perc_metexceed) - as.numeric(co_dist_math[3,8])
co_memb_math <- co_memb_math %>% 
  mutate(stdmath_public = diffmath_public / as.numeric(co_dist_math[3,6]),
         stdmathlvl_public = diffmathlvl_public / as.numeric(co_dist_math[3,9])) %>% 
  select(school_name, num_validscores, mean_score2019, diffmath_public, stdmath_public,
         perc_metexceed, diffmathlvl_public, stdmathlvl_public)
co_dist_ela <- co_cmas %>% # Denver schools as a comparison district
  filter(district_name == "DENVER COUNTY 1" &
           grade == "All Grades" &
           subject == "English Language Arts") %>% 
  select(school_name,num_validscores,subject,grade,mean_score2019,sd_score,count_metexceed,perc_metexceed) %>% 
  arrange(school_name)
co_dist_ela$stdelalvl_public <- sd(co_dist_ela$perc_metexceed, na.rm = TRUE) # sd of portion students in the school that meet or exceed
co_dist_ela <- co_dist_ela %>% arrange(school_name)
co_memb_ela <- co_dist_ela[44:51,]
names(co_dist_ela)
co_memb_ela$diffela_public = as.numeric(co_memb_ela$mean_score2019) - as.numeric(co_dist_ela[3,5])
co_memb_ela$diffelalvl_public = as.numeric(co_memb_ela$perc_metexceed) - as.numeric(co_dist_ela[3,8])
co_memb_ela <- co_memb_ela %>% 
  mutate(stdela_public = diffela_public / as.numeric(co_dist_ela[3,6]),
         stdelalvl_public = diffelalvl_public / as.numeric(co_dist_ela[3,9])) %>% 
  select(school_name, num_validscores, mean_score2019, diffela_public, stdela_public,
         perc_metexceed, diffelalvl_public, stdelalvl_public)
cmas_memb <- merge(co_memb_math,co_memb_ela,by="school_name")
names(cmas_memb)[2:3] <- c("n_math","mean_math")
names(cmas_memb)[9:10] <- c("n_ela","mean_ela")
write.csv(cmas_memb, file = file.path('co_cmas_memb.csv'), row.names = FALSE)
#--------
# Louisiana LEAP 2025
la_ela <- read_excel("LA_LEAP_2019.xlsx", sheet = "ELA")
la_ela <- la_ela[-c(1),]
names(la_ela) <- la_ela[c(1),]
la_ela <- la_ela[-c(1),]
names(la_ela) <- tolower(names(la_ela))
names(la_ela) <- c("sys_code","sys","site_code","school","2018perc_mastery","2019perc_mastery",
                   "1819percchange_mastery","2018perc_mastery_eng12","2019perc_mastery_eng12",
                   "1819percchange_master_eng12")
la_ela$`2019perc_mastery` <- as.numeric(la_ela$`2019perc_mastery`)
la_ela$`2019perc_mastery_eng12` <- as.numeric(la_ela$`2019perc_mastery_eng12`)
la_sys_ela <- la_ela %>% 
  group_by(sys) %>% 
  summarize(meanelalvl = mean(`2019perc_mastery`, na.rm = TRUE),
            sdelalvl = sd(`2019perc_mastery`, na.rm = TRUE),
            meanenglish = mean(`2019perc_mastery_eng12`, na.rm = TRUE),
            sdenglish = sd(`2019perc_mastery_eng12`, na.rm = TRUE)) %>% 
  filter(sys == "Orleans Parish")
la_memb_ela <- la_ela %>% 
  filter(school == "Morris Jeff Community School"|
           school == "International High School of New Orleans") %>% 
  arrange(school) %>% 
  select(school, `2019perc_mastery_eng12`)
names(la_memb_ela)[2] <- "elaperc_lvl34"
names(la_sys_ela)
la_memb_ela <- la_memb_ela %>% 
  mutate(diffelalvl_public = elaperc_lvl34 - as.numeric(c(la_sys_ela[1,4],la_sys_ela[1,2])),
         stdelalvl_public = diffelalvl_public / as.numeric(c(la_sys_ela[1,5],la_sys_ela[1,3])))

la_math <- read_excel("LA_LEAP_2019.xlsx", sheet = "Math")
la_math <- la_math[-c(1),]
names(la_math) <- la_math[c(1),]
la_math <- la_math[-c(1),]
names(la_math) <- tolower(names(la_math))
names(la_math) <- c("sys_code","sys","site_code","school","2018perc_mastery","2019perc_mastery",
                   "1819percchange_mastery","2018perc_mastery_alggeom","2019perc_mastery_alggeom",
                   "1819percchange_master_alggeom")
la_math$`2019perc_mastery` <- as.numeric(la_math$`2019perc_mastery`)
la_math$`2019perc_mastery_alggeom` <- as.numeric(la_math$`2019perc_mastery_alggeom`)
la_sys_math <- la_math %>% 
  group_by(sys) %>% 
  summarize(meanmathlvl = mean(`2019perc_mastery`, na.rm = TRUE),
            sdmathlvl = sd(`2019perc_mastery`, na.rm = TRUE),
            meanalggeom = mean(`2019perc_mastery_alggeom`, na.rm = TRUE),
            sdalggeom = sd(`2019perc_mastery_alggeom`, na.rm = TRUE)) %>% 
  filter(sys == "Orleans Parish")
la_memb_math <- la_math %>% 
  filter(school == "Morris Jeff Community School"|
           school == "International High School of New Orleans") %>% 
  arrange(school) %>% 
  select(school, `2019perc_mastery_alggeom`)
names(la_memb_math)[2] <- "mathperc_lvl34"
names(la_sys_math)
la_memb_math <- la_memb_math %>% 
  mutate(diffmathlvl_public = mathperc_lvl34 - as.numeric(c(la_sys_math[1,4],la_sys_math[1,2])),
         stdmathlvl_public = diffmathlvl_public / as.numeric(c(la_sys_math[1,5],la_sys_math[1,3])))
la_memb <- merge(la_memb_math,la_memb_ela,by="school")
write.csv(la_memb,file = file.path('la_memb_leap.csv'), row.names = FALSE)
