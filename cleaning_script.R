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
# CALIFORNIA
california <- read.csv("CA_enrl_2019-03-28.csv")
View(california)
names(california) <- tolower(names(california))
names(california)
california <- california %>% 
  select(school, county, district, ethnic, enr_total)
View(california)
str(california)
names(california)
california$ethnic <- as.factor(california$ethnic)
levels(california$ethnic) <- c("unreported","amind","asian","hawpi","filipino","hispanic","black","white","multiple")
head(california)
levels(california$ethnic)[5] <- "asian"
california <- california %>% 
  distinct(school, district, ethnic, .keep_all = TRUE) %>% 
  spread(key = ethnic,
         value = enr_total,
         fill = 0) %>% 
  mutate(total = unreported + amind + asian + hawpi + hispanic + black + white + multiple)
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
california_members <- california[c(1666:1668,1683,3922:3937,9345,9695,9697),]
names(california_members)
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
california_members[,4:37] <- sapply(california_members[,4:37],as.numeric)
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
california_members$std_total = c(california_members[1,38] / y_mem[1, 3], # LA Unified Schools:
                                california_members[2,38] / y_mem[1, 3], # COTW Hollywood, Mar Vista
                                california_members[3,38] / y_mem[1, 3], # Silver Lake
                                california_members[4,38] / y_mem[1, 3], # CLIC
                                california_members[5,38] / y_mem[1, 3], # HTH LA
                                california_members[6,38] / y_mem[1, 3], # HTH LA Middle
                                california_members[7,38] / y_mem[1, 3], # The City
                                california_members[8,38] / y_mem[1, 3], # Valley ES
                                california_members[9,38] / y_mem[1, 3], # Valley MS
                                california_members[10,38] / y_mem[2, 3], # San Diego Schools: HTH Elem
                                california_members[11,38] / y_mem[2, 3], # HTH Elem Explorer
                                california_members[12,38] / y_mem[2, 3], # HTH
                                california_members[13,38] / y_mem[2, 3], # HTH International
                                california_members[14,38] / y_mem[2, 3], # HTH Media Arts
                                california_members[15,38] / y_mem[2, 3], # HT Middle
                                california_members[16,38] / y_mem[2, 3], # HT Middle Media Arts
                                california_members[17,38] / y_mem[2, 3], # HT Elem Chula Vista
                                california_members[18,38] / y_mem[2, 3], # HT Elem North County
                                california_members[19,38] / y_mem[2, 3], # HTH Chula Vista 
                                california_members[20,38] / y_mem[2, 3], # HTH Mesa
                                california_members[21,38] / y_mem[2, 3], # HTH North County
                                california_members[22,38] / y_mem[2, 3], # HT Middle Chula Vista
                                california_members[23,38] / y_mem[2, 3]) # HT Middle North County
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
