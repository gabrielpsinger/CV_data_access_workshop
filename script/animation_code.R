# Animate tracks of Green Sturgeon detections downloaded from BARD
# G. Singer
# Thu Jun 18 10:57:04 2020 ------------------------------

source("script/utils.R")
check_and_install_packages()


# get data ----------------------------------------------------------------

# set up temporary file
tempdl <- tempfile()

# download zipped data file from BARD website
download.file('http://cftc.metro.ucdavis.edu/biotelemetry-autonomous-real-time-database/resources/csvs/UCD_GS_RESCUE.zip', 
              tempdl, 
              mode = 'wb')

# unzip into data folder
unzip(tempdl, "UCD_GS_RESCUE.csv", exdir = "data")

# read csv into work space (also reducing the # of TagIDs read to simplify)
gs_dat <- read_csv_chunked('data/UCD_GS_RESCUE.csv', callback = DataFrameCallback$new(function(x, pos) subset(x, TagID>46635)),  progress = T)


# prep data for plotting --------------------------------------------------

# subset rec data by general location to make for a cleaner map (quick version)
recs_short <- gs_dat %>% 
  select(General_Location, Lat, Lon) %>% 
  group_by(General_Location) %>% 
  summarize(lat = mean(Lat), lon = mean(Lon))

# summarize detection data
dets <- gs_dat %>%
  select(TagID, DetectionLocation, DetectDate, Lat, Lon) %>%  #reduce the fields
  mutate(dt = as_date(DetectDate)) %>%                        # make date only column to group dets by
  group_by(dt) %>%                                            # group detections
  filter(DetectDate == min (DetectDate)) %>%                  # take the first detection per hour to simplify data for animations
  ungroup() %>%
  select(TagID, DetectionLocation, DetectDate, Lat, Lon)

# subset a single fish
GS46638 <- dets %>% 
  filter(TagID==46638)


# Begin plotting ----------------------------------------------------------

# Plot code adapted from https://medium.com/@mueller.johannes.j/use-r-and-gganimate-to-make-an-animated-map-of-european-students-and-their-year-abroad-517ad75dca06).

# get outline of CA
CA_map<- map_data('state', region = 'california')                                     

# convert format for plotting
CA <- c(
    geom_polygon(
      aes(long, lat, group = group),
      
      size = 0.1,
      colour = "#090D2A",
      fill = "#090D2A",
      alpha = 0.8,
      data = CA_map
    )
  )       



plot <- ggplot() +
  CA +                                                                               # add CA outline to plot
  geom_point(data = recs_short,                                                      # add receiver array to plot
             aes(x = lon, y = lat), 
             color = "white") +           
  geom_point(data = GS46638,                                                         # add detections to plot
             aes(x = Lon, y = Lat),
             color = "red",
             size = 3.5) +
  annotate(                                                                         # Add text label for TagID
    "text",
    label = paste("TagID:", GS46638$TagID[1]),
    color =  "white",
    x = -125,
    y = 37.1,
    size = 6
  ) +
  theme(
    panel.background =  element_rect(fill = 'gray40', colour = 'gray40'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  coord_cartesian(xlim = c(-125.5,-119), ylim = c(37, 41)) +                        # crop map of CA
  transition_manual(frames = DetectDate) +
  labs(title = 'Location: {GS46638$DetectionLocation[current_frame]}',              # add location and time to title
       subtitle = 'Date: {current_frame}')

# animate plot
animate(plot, fps=2)

# create fig_output directory
dir.create("fig_output")

# save animation to directory
anim_save("fig_output/GS46638.gif", plot)

# # optional: can save as mp4 to pause and click through frame by frame, with av renderer



# Faceted plot ------------------------------------------------------------
# plot 4 fish at once, keep time relative to explore differences in migration timing

# subset data by time  - keep post rescue detections; not entire detection history
rescue_year <- dets %>% 
  filter(DetectDate<= "2012-01-01 00:00:00")


plot2 <- ggplot() +
  CA +                                                                               # ad CA outline to plot
  geom_point(data = recs_short, aes(x = lon, y = lat), color = "white") +            # add receiver array to plot
  geom_point(data = rescue_year, aes(x = Lon, y = Lat, color = factor(TagID)),
             size = 3.5, show.legend = F) + 
  theme(panel.background =  element_rect(fill='gray40',colour='gray40'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) + 
  coord_cartesian(xlim = c(-125.5, -119), ylim = c(37,41)) +
  facet_wrap(~TagID) + 
  transition_time(DetectDate) + 
  labs(title = "Date: {frame_time}")

# animate plot
animate(plot2, fps=2, renderer = gifski_renderer(loop = F))

