# Other options for downloading data from BARD
# G. Singer
# Wed Jun 24 10:18:21 2020 ------------------------------

# set up ------------------------------------------------------------------
# only need to run once per session, so skip if already done
# source("script/utils.R")
# check_and_install_packages()

# run if we haven't already created a 'data' directory
# dir.create("data", showWarnings = F)


# direct download by receiver and time ------------------------------------

download.file(url = "http://cftc.metro.ucdavis.edu/biotelemetry-autonomous-real-time-database/memo/DLDetLoxMetaCSV/2019-06-24%2010:15:33/2020-06-24%2010:15:33/total/null/SR_ButteBr,SR_BlwIrvineFinch/", 
              destfile = "data/upper_river_dets.csv")

download.file(url =  "http://cftc.metro.ucdavis.edu/biotelemetry-autonomous-real-time-database/memo/DLDetLoxMetaCSV/2011-06-24%2010:15:33/2011-09-24%2010:15:33/total/null/GG4,GG5/", 
              destfile = "data/GG_2011.csv")





# download by location, date, and filter by tagIDs ------------------------
# What if you want to query receivers at CVP and SWP to see if your fish intereacted with these facilities?
# ***reads directly into environment

# make list of locations to query
recs <- c("CC_InFB_N,CC_InFB_S,Tracy_Fish_Facility_1,Tracy_Fish_Facility_2,Tracy_Fish_Facility_3")

# make a list of tags from your study
my_tags <- paste0("A69-1303-", 46611:46639) # green sturgeon rescue fish from 2011

my_tags

dat <- read_csv(paste0("http://cftc.metro.ucdavis.edu/biotelemetry-autonomous-real-time-database/memo/DLDetLoxMetaCSV/2011-06-24%2010:15:33/2018-09-24%2010:15:33/total/null/", recs, "/")) %>% 
  filter(TagID_inclCodespace %in% my_tags)


ggplot(dat) + 
    geom_point(aes(x = Detection_DateTime, y = Detection_Location_Name)) +
    scale_x_datetime( labels = date_format("%Y-%m-%d"))





# download and save summary by tag ID -------------------------------------

download.file(url = "http://cftc.metro.ucdavis.edu/biotelemetry-autonomous-real-time-database/memo/getVisitsCSV/A69-1303-12345,A69-1206-260,A69-1303-46638/2010-01-01%2000:00:00/2020-06-24%2010:46:01/", 
              destfile = "data/tag12345_ect.csv")




# read file directly into environment -------------------------------------

tag12345 <- read_csv("http://cftc.metro.ucdavis.edu/biotelemetry-autonomous-real-time-database/memo/getVisitsCSV/A69-1303-12345,A180-9002-54321,A180-1702-37192,A69-1206-260,A69-1303-46638/2010-01-01%2000:00:00/2020-06-24%2010:46:01/")



