# Plotting cumulative survival to a receiver location using BARD d --------
# Written by C. Hause
# Edited by G. Singer
# Mon Jun 22 14:59:35 2020 ------------------------------


# set up ------------------------------------------------------------------
# Only run the following lines, if starting with this script file. Otherwise, the set up is already complete
# source("script/utils.R")
# check_and_install_packages()


# download data from BARD -------------------------------------------------

download_salmon_data()


# read csv into work space (also reducing the # of detections -------------
# filter receivers by rkm to reduce file size loaded 

dets <- read_csv_chunked(
  'data/2019_UCD_SJR.csv',
  callback = DataFrameCallback$new(
    function(x, pos)
      subset(x, RiverKm == 270.52 | RiverKm == 163.12 | RiverKm < 94)
  ),
  progress = T
)


# 1. choose whether you want to look at survival to a specific rec --------

loc <- dets$General_Location # general locations
#loc <- dets$SN # specific receiver ID


# 2. get first detection from tags at each location ------------------------

visits <- dets %>%
  mutate(combo = (paste0(TagID, "_", loc))) %>%
  arrange(TagID, combo, DetectDate)

visits$counter <- sequence(rle(visits$combo)$lengths) 

visits_first <- visits %>% 
  filter(counter==1) %>% 
  arrange(TagID, DetectDate)


# 3. determine which receivers to plot ------------------------------------

# Look at options:
unique(visits_first$General_Location) # General locations
#unique(visits_first$SN) # specific receiver IDS

# Choose:
gen_rec_list <- c("Blw_Release1", "Mossdale", "JP_US", "GoldenGateW") # list the receivers that you want to plot, these are just examples


# 4. make data file -------------------------------------------------------
plot_df <- NULL

for (rec in gen_rec_list) {
  print(rec)
  rec_df <-
    subset(visits_first, visits_first$General_Location == rec) %>%
    mutate(Date = ymd(format(as.Date(DetectDate), '%Y-%m-%d'))) %>%
    arrange(TagID, Date)
  
  dets.by.day <- rec_df %>%
    group_by(Date, General_Location) %>%
    tally()
  
  dets.by.day$total_detected <- sum(dets.by.day$n)
  
  prop.detected <- dets.by.day %>%
    mutate(prop.det = n / total_detected) %>%
    transform(Percent = ave(prop.det, FUN = cumsum)) %>%
    mutate(Percent = Percent * 100)
  prop.detected$labels <- prop.detected$Date[nrow(prop.detected)]
  plot_df <- rbind(plot_df, data.frame(prop.detected))
}

# 5. plot -----------------------------------------------------------------

ggplot(data = plot_df) +
  geom_line(aes(x = Date, y = Percent, color = General_Location), size = 1.1) +
  scale_color_fish_d(option = "Oncorhynchus_tshawytscha") +
  ylim(0, 105) +
  ylab("% Detected (Cumulative)") +
  xlab("Date") +
  ggtitle("Cumulative Detections per Location") +
  annotate(
    "text",
    x = unique(plot_df$labels),
    y = 105 ,
    label = paste0("n = ", unique(plot_df$total_detected)),
    size = 3.5
  ) +
  theme_bw() +
  theme(text = element_text(size = 16))

