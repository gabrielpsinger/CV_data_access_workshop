# Plotting cumulative survival to a receiver location using BARD data

# Set up:

# make list of required packages
list.of.packages <- c("tidyverse",
                      "lubridate",
                      "ggplot2", 
                      "fishualize"
)

# make list of packages that are required, but not already downloaded
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

# download packages that are not already present in the library
if(length(new.packages))
  install.packages(new.packages)

# load packages
packages_load <- lapply(list.of.packages, require, character.only = T)

# print warning if there is a problem with installing/loading some of packages
if(any(as.numeric(packages_load)==0)){
  warning(paste("Package/s: ", paste(list.of.packages[packages_load != T], sep = ","), "not loaded!"))
}else{
  print("All packages were successfully loaded.")
}

# Get Data:
dets<- read_csv("2019_UCD_SJR_ProcessedforBARD.csv") # THIS WILL NEED TO CHANGE, need to pull from BARD

#1.
# Prep Data:
dets$DetectPotential <-ifelse(dets$RiverKm > 211, 350, 700) # 2 release groups means not all receivers had opportunity to capture all 700 fish

#2.
# Choose whether you want to look at survival to a specific receiver or a general location
loc <- dets$General_Location # general locations
#loc <- dets$SN # specific receiver ID

#3.
# Get first detection from tags at each location:
visits<- dets %>% 
  mutate(combo = (paste0(TagID,"_", loc))) %>% 
  arrange(TagID,combo,DetectDate)

visits$counter <- sequence(rle(visits$combo)$lengths) 

visits_first <- visits %>% 
  filter(counter==1) %>% 
  arrange(TagID, DetectDate)

#4.
# Determine receivers to plot
# Look at options:
unique(visits_first$General_Location) # General locations
#unique(visits_first$SN) # specific receiver IDS

# Choose:
gen_rec_list <- c("Blw_Release1", "Mossdale", "JP_US", "GoldenGateW") # list the receivers that you want to plot, these are just examples

#5.
# Make datafile
plot_df <- NULL

for (rec in gen_rec_list) {
  print(rec)
  #rec_df <- subset(visits_first, grepl(rec, visits_first$General_Location)) 
  rec_df <- subset(visits_first, visits_first$General_Location == rec) %>% 
    mutate(Date = ymd(format(as.Date(DetectDate), '%Y-%m-%d'))) %>% 
    arrange(TagID, Date)
  
  dets.by.day <- rec_df %>% 
    group_by(Date, General_Location, DetectPotential) %>% 
    tally() %>% 
    mutate(prop.det = n/DetectPotential) %>% 
    transform(Percent = ave(prop.det, FUN = cumsum)) %>% 
    mutate(Percent = Percent*100)
  plot_df <- rbind(plot_df, data.frame(dets.by.day))
}

#6.
# Plot data
ggplot(data = plot_df) +
  geom_line(aes(x = Date, y = Percent, color = General_Location), size = 1) +
  scale_color_fish_d(option = "Oncorhynchus_tshawytscha") +
  ylim(0, 100)+
  ylab("% Detected (Cumulative)")+
  xlab("Date")+
  ggtitle("Cumulative Detections per Location")+
  theme_bw() 

