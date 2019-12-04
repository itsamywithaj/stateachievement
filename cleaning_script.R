rm(list = ls()) # clear working environment
getwd() # check working directory
setwd("~/Downloads/States/")
install.packages("tidyverse") # install tidyverse
library(tidyverse)
install.packages("readxl") # CRAN version
library(readxl)
louisiana_sys <- read_excel("LA_enrl_2019-10.xlsx", sheet = "Total by School System") # Read Louisiana's data
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
