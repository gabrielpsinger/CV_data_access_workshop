#--------------------------------------------#
# M. Johnston 
# Utility/convenience functions for analysis 
# Friday 2020-06-19 11:14:28 ----------------# 
# Edited by G. Singer 
# Mon Jun 22 14:39:13 2020 ------------------------------

#-------------------------------------------------------#
check_and_install_packages = function() {
  
  list.of.packages <- c("tidyverse",
                        "lubridate",
                        "maps", 
                        "rgeos",
                        "maptools",
                        "ggmap",
                        "gganimate", 
                        "sf", 
                        "rgdal", 
                        "gifski", 
                        "fishualize", 
                        "scales", 
                        "rvest")
 


# make list of packages that are required, but not already downloaded
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
# download packages that are not already present in the library
if(length(new.packages))
  install.packages(new.packages)
# load packages
packages_load <- lapply(list.of.packages, require, character.only = TRUE)
# print warning if there is a problem with installing/loading some of packages
if(any(as.numeric(packages_load)==0)){
  warning(paste("Package/s: ", paste(list.of.packages[packages_load != TRUE], sep = ","), "not loaded!"))
}else{
  print("All packages were successfully loaded.")
}
}
#-------------------------------------------------------#
download_telemetry_data = function() {
  
  # unzip into data folder
  if(
    !file.exists("data/UCD_GS_RESCUE.csv")){
    tempdl <- tempfile()
    
    # download zipped data file from BARD website
    download.file(
      'http://cftc.metro.ucdavis.edu/biotelemetry-autonomous-real-time-database/resources/csvs/UCD_GS_RESCUE.zip',
      tempdl,
      mode = 'wb'
    )
    unzip(tempdl, "UCD_GS_RESCUE.csv", exdir = "data")
    print("Data was successfully added to your /data directory.")}
  else
    print("data already exists")
  
}

# slight change of Myfanwy's code, so that we can show the download process one time
download_salmon_data = function() {
  
  # unzip into data folder
  if(
    !file.exists("data/2019_UCD_SJR.csv")){
    tempdl <- tempfile()
    
    # download zipped data file from BARD website
    download.file(
      'http://cftc.metro.ucdavis.edu/biotelemetry-autonomous-real-time-database/resources/csvs/2019_UCD_SJR.zip',
      tempdl,
      mode = 'wb'
    )
    unzip(tempdl, "2019_UCD_SJR.csv", exdir = "data")
    print("Data was successfully added to your /data directory.")}
  else
    print("data already exists")
  
}

# quickly 'vet' a dataframe by previewing rows at the head, middle, and tail (M. Johnston):
#-------------------------------------------------------#
vet <- function(d, n = 4L) {
  if(class(d) != 'data.frame') stop('vet() can only vet dataframes')
  left <- as.integer(nrow(d) / 2 - n / 2)
  torso = d[seq_len(n) + left - 1L,]
  rbind(head(d, n), torso, tail(d, n))
}
# typing shortcuts (M. Johnston)
#--------------------------------------------#
len <- function(x){length(unique(x))}
csn <- function(x){colSums(is.na(x))}
rsn <- function(x){rowSums(is.na(x))}



# scrape CDFW salvage site ------------------------------------------------


scrape_CDFW_salvage = function(year, month, day){
url <- paste0("https://apps.wildlife.ca.gov/Salvage/Project/DailySummary?EndDate=Sat%20", tolower(month.abb[month]), "%20", day, "%20", year, "&sortOrder=")
html <- read_html(url)
salvage <- html_nodes(html, "td")

text <- html_text(salvage, trim = T)

acre_ft <- as.data.frame(matrix(text[1:6], nrow = 2, byrow = T))

colnames(acre_ft) <- as.character(unlist(acre_ft[1,]))
acre_ft = acre_ft[-1, ]




species <- as.data.frame(matrix(text[7:30], ncol= 3, byrow = T))
colnames(species) <- as.character(unlist(species[1,]))
species = species[-1, ]

sal_list <- list(acre_ft, species)

return(sal_list)

}

scrape_CDFW_salvage(2016, 10, 15)

# old plotting code -------------------------------------------------------

# plot <- ggplot() +
#   CA +                                                                               # ad CA outline to plot
#   geom_point(data = recs_short,                                                      #  add receiver array to plot
#              aes(x = lon, y = lat), 
#              color = "white") +           
#   geom_point(data = GS46638,
#              aes(x = Lon, y = Lat),
#              color = "red",
#              size = 3.5) +
#   annotate(                                                                         # Add text label for TagID
#     "text",
#     label = paste("TagID:", GS46638$TagID[1]),
#     color =  "white",
#     x = -125,
#     y = 37.1,
#     size = 6
#   ) +
#   theme(
#     panel.background =  element_rect(fill = 'gray40', colour = 'gray40'),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank()
#   ) +
#   coord_cartesian(xlim = c(-125.5,-119), ylim = c(37, 41)) +
#   theme(
#     axis.line = element_blank(),
#     axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank(),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank()
#   ) +
#   transition_manual(frames = DetectDate) +
#   labs(title = 'Location: {GS46638$DetectionLocation[current_frame]}',
#        subtitle = 'Date: {current_frame}')

# no date input
# scrape_CDFW_salvage = function(){
#   url <- "https://apps.wildlife.ca.gov/Salvage/Project/DailySummary?EndDate=Sat%20Oct%2015%202016&sortOrder="
#   html <- read_html(url)
#   salvage <- html_nodes(html, "td")
#   
#   text <- html_text(salvage, trim = T)
#   
#   acre_ft <- as.data.frame(matrix(text[1:6], nrow = 2, byrow = T))
#   
#   colnames(acre_ft) <- as.character(unlist(acre_ft[1,]))
#   acre_ft = acre_ft[-1, ]
#   
#   
#   
#   
#   species <- as.data.frame(matrix(text[7:30], ncol= 3, byrow = T))
#   colnames(species) <- as.character(unlist(species[1,]))
#   species = species[-1, ]
#   
#   sal_list <- list(acre_ft, species)
#   
#   return(sal_list)
#   
# }