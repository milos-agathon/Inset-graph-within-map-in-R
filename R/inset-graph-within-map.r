################################################################################
#                  Inset graph within map in R
#                  Milos Popovic
#                  2022/02/12
################################################################################

# my favorite font ❤️
windowsFonts(georg = windowsFont('Georgia'))

# libraries we need
libs <- c("tidyverse", "sf", "grid", "gridExtra", "giscoR", "classInt")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)) {
  install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = TRUE))

# 1. LOAD DATA
#---------

#list of European countries
europeList <- function(urlfile, iso2) {
  urlfile <-'https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv'
  iso2 <- read.csv(urlfile) %>%
        filter(region=="Europe" | alpha.2 == "TR" | alpha.2 == "CY") %>% #filter Europe, Cyprus and Turkey
        select("alpha.2") %>%
        rename(CNTR_ID = alpha.2)
return(iso2)
}

countries <- europeList()

# load national map of Europe
europeMap <- function(europe, eur) {
  
  europe <- giscoR::gisco_get_countries(
  year = "2016",
  epsg = "4326",
  resolution = "10",
  region = c("Europe", "Asia")
) %>%
  mutate(FID = recode(FID, 'UK'='GB')) %>% 
  mutate(FID = recode(FID, 'EL'='GR'))

eur <- europe %>% dplyr::filter(FID%in%countries$CNTR_ID)
return(eur)
}

e <- europeMap()

# UNESCO data
UNESCOdata <- function(unesco_data_url, unesco_data) {
  unesco_data_url <- "https://raw.githubusercontent.com/milos-agathon/Inset-graph-within-map-in-R/main/data/women_in_science.csv"
  unesco_data <- read.csv(unesco_data_url) %>%
        rename(ISO3_CODE = ISO3) %>%
        select(ISO3_CODE, womensci)
return(unesco_data)
}

df <- UNESCOdata()

# 4. BREAKS
#----------
makeIntervals <- function(ni, labels, d, lvl) {
  d <- drop_na(df)
  # let's find a natural interval with quantile breaks
  ni <- classIntervals(d$womensci, 
              n = 6, 
              style = 'jenks')$brks
  # create categories
  labels <- c()
  for(i in 1:length(ni)){
    labels <- c(labels, paste0(round(ni[i], 0), 
                             "–", 
                             round(ni[i + 1], 0)))
  }
  labels <- labels[1:length(labels)-1]

  # finally, carve out the categorical variable 
  # based on the breaks and labels above
  d$cat <- cut(d$womensci, 
              breaks = ni, 
              labels = labels, 
              include.lowest = T)
  levels(d$cat) # let's check how many levels it has (6)

  # label NAs, too
  lvl <- levels(d$cat)
  lvl[length(lvl) + 1] <- "No data"
  d$cat <- factor(d$cat, levels = lvl)
  d$cat[is.na(d$cat)] <- "No data"
  levels(d$cat)
return(d)
}

w <- makeIntervals()

# 2. JOIN DATA
#----------
crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

spatialJoin <- function(shp, data, sf_data) {
  shp <- e
  data <- w
  sf_data <- shp %>% left_join(data, by="ISO3_CODE") %>%
      st_as_sf() %>%
      st_transform(crs = crsLAEA) %>%
      drop_na() %>%
      select(NAME_ENGL, womensci, cat, geometry) %>%
      mutate(NAME_ENGL = replace(NAME_ENGL, str_detect(NAME_ENGL, "Bosnia and Herzegovina"), "BIH")) %>% 
      mutate(NAME_ENGL = replace(NAME_ENGL, str_detect(NAME_ENGL, "Russian Federation"), "Russia")) %>%  #shorten names
      mutate(NAME_ENGL = replace(NAME_ENGL, str_detect(NAME_ENGL, "North Macedonia"), "N. Macedonia")) %>%
      mutate(NAME_ENGL = replace(NAME_ENGL, str_detect(NAME_ENGL, "United Kingdom"), "UK"))
return(sf_data)
}

sfd <- spatialJoin()

# 3. BOUNDING BOX
#----------
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

bbox <- function(bb, laeabb, b) {

bb <- st_sfc(
  st_polygon(list(cbind(
    c(-10.6600, 33.00, 33.00, -10.6600, -10.6600),
    c(32.5000, 32.5000, 71.0500, 71.0500, 32.5000) 
    ))),
  crs = crsLONGLAT)

laeabb <- st_transform(bb, crs = crsLAEA)
box <- st_bbox(laeabb)
return(box)
}

b <- bbox()

# 5. PLOT
#----------

orderDF <- function(a, aa) {
  a <- sfd %>% st_drop_geometry()
  a$womensci <- as.numeric(as.character(a$womensci))
  a <- subset(a, !is.na(womensci))
  attach(a)
  a <- a[order(NAME_ENGL, womensci),] 
return(a)
}

nd <- orderDF()

# boxplot
source("R/women_science_boxplot.r") # load script
boxp <- women_science_boxplot()

#map
source("R/women_science_map.r") # load script
map <- women_science_map()

#combine
InsetGraphMap <- function(m, bp, vp) {
  m <- map
  bp <- boxp 
  vp <- viewport(width=0.3, height=0.85, x=0.15, y=0.5)
  png("female_researchers.png", height=4200, width=4000, res=600)
  # clear current device
  grid.newpage()
  print(m)
  print(bp+labs(title=""), vp=vp)
  dev.off()
  return()
}

InsetGraphMap()
