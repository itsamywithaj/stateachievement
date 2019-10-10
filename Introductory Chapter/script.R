rm(list = ls())
# -------------------------------------------------------------------------
# CHAPTER 1-2 ######
# Set working directory and then have subfolders
getwd()
dir.create("data")
dir.create("data_output")
dir.create("fig_output")

# Download data, install packages
download.file("https://ndownloader.figshare.com/files/11492171","data/SAFI_clean.csv",mode="wb")
install.packages("tidyverse")
# -------------------------------------------------------------------------
# Play with objects & variables
length <- 3
width <- 19
area <- length * width
area
length <- 2
width <- 1
area
# -------------------------------------------------------------------------
# Vectors and data types
hh_members <- c(3, 7, 10, 6)
hh_members
respondent_wall_type <- c("muddaub","burntbricks","sunbricks")
respondent_wall_type
length(hh_members)
class(hh_members)
class(respondent_wall_type)
str(hh_members)
possessions <- c("bicycle","radio","television")
possessions <- c(possessions, "mobile_phone")
possessions <- c("car", possessions)
possessions
typeof(possessions)
num_char <- c(1,2,3,"a")
num_logical <- c(1,2,3,TRUE)
char_logical <- c("a","b","c",TRUE)
tricky <- c(1,2,3,"4")
class(num_char)
num_char
num_logical
class(num_logical)
class(char_logical)
char_logical
class(tricky)
tricky
combined_logical <- c(num_logical, char_logical)
class(combined_logical)
combined_logical
# -------------------------------------------------------------------------
# Missing Data
rooms <- c(2,1,1,NA,4)
mean(rooms)
mean(rooms, na.rm=TRUE) # calculates while ignoring missing values
max(rooms,na.rm=TRUE)
rooms <- c(1,2,1,1,NA,3,1,3,2,1,1,8,3,1,NA,1)
rooms <- rooms[!is.na(rooms)] # extracts missing values
rooms
median(rooms)
rooms[rooms > 2]
length(rooms[rooms > 2])
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# ### CHAPTER 3 #### Starting with Data!
library(tidyverse) # then import data
browseURL("https://datacarpentry.org/socialsci-workshop/data/") # the data information
interviews <- read_csv("data/SAFI_clean.csv", na = "NULL") # assumes fields are delineated by commas; also stores objects in classes of tbl_df, tbl, and data.frame
view(interviews) # see it pop up as a table
head(interviews) # shows a tibble 6 x 14 --> the 1st 6 rows
class(interviews) # shows the classes we made from read_csv()
dim(interviews) # returns vector with number of rows, number of columns
nrow(interviews) # returns number of rows
ncol(interviews) # number of columns
tail(interviews) # Shows the last 6 rows
names(interviews) # column names
str(interviews) # structure of the object and information about the class, length and content of each column
summary(interviews) # summary stats
# -------------------------------------------------------------------------
# Indexing and subsetting data frames
interviews[1,6] # 1st element in 6th column
interviews[[1]] # 1st column of data frame as a vector
interviews [1] # 1st column of data frame as a data.frame
interviews[1:3,7] # 1st 3 elements in the 7th column as a vector
interviews[3, ] # 3rd row of data frame as a data.frame
interviews[ ,-1] # whole data frame, except the 1st column
interviews["village"] # a data frame
interviews[,"village"] # another data frame
interviews[["village"]] # a vector
interviews$village # a vector
head_interviews <- interviews[1:6, ] # equivalent to head_interviews <- head(interviews)
head_interviews
interviews_100 <- interviews[100, ] ##1 data frame of row 100 of interviews dataset
nrow(interviews) ##2 pull out last row data frame with nrow
interviews[131, ]
tail(interviews)
interviews_last <- interviews[nrow(interviews), ]
interviews_middle <-interviews[nrow(interviews)*.5,] ##3 use nrow to extract middle and store in an object
head(interviews)
interviews_head <- interviews[-(7:nrow(interviews)), ]
interviews_head
# -------------------------------------------------------------------------
# Factors. A categorical data class
respondent_floor_type <- factor(c("earth","cement","cement","earth"))
levels(respondent_floor_type) # What are the categories?
nlevels(respondent_floor_type) # How many categories?
respondent_floor_type <- factor(respondent_floor_type,levels = c("earth","cement")) # change the order
levels(respondent_floor_type)[2]
# -------------------------------------------------------------------------
# Converting factors to character vector
as.character(respondent_floor_type)
year_fct <- factor(c(1990,1983,1977,1998,1990))
as.numeric(year_fct)
as.numeric(as.character(year_fct))
as.numeric(levels(year_fct))[year_fct]
## create a vector from data frame column "memb_assoc"
memb_assoc <- interviews$memb_assoc
## convert it to a factor
memb_assoc <- as.factor(memb_assoc)
memb_assoc
# bar plot number of interview respondents who were members of irrigation association
plot(memb_assoc)
## go back to vector from data frame column. then replace missing data
memb_assoc[is.na(memb_assoc)] <- "undetermined"
## rename the levels of the factor to have the first letter uppercase
memb_assoc <- interviews$memb_assoc
memb_assoc[is.na(memb_assoc)] <- "Undetermined"
memb_assoc[memb_assoc=="no"] <- "No"
memb_assoc[memb_assoc=="yes"] <- "Yes"
memb_assoc <- factor(memb_assoc, levels = c("No","Yes","Undetermined"))
memb_assoc
plot(memb_assoc)
