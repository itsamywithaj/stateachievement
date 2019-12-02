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

