################################################################################
#                  Inset graph within map in R
#                  Milos Popovic
#                  2022/02/12
################################################################################

setwd("C:/Users/milos/Downloads")

windowsFonts(georg = windowsFont('Georgia'))

# libraries we need
libs <- c("tidyverse", "sf", "grid", "gridExtra", "giscoR", "classInt", "cshapes")

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

      select(NAME_ENGL, womensci, cat, geometry)
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
women_science_boxplot <- function(l) {
  dat <- nd
  l <- ggplot(dat, aes(x=reorder(NAME_ENGL, womensci), 
    y=womensci, fill=cat)) + 
    geom_bar(stat='identity') + 
  geom_text(data=subset(dat, womensci<45), 
    aes(label = womensci),
      position = position_stack(vjust = .5), 
      hjust=0.5,
      size=2.75,
      color="grey10",
      family="georg") +
  geom_text(data=subset(dat, womensci>45), 
    aes(label = womensci),
      position = position_stack(vjust = .5), 
      hjust=0.5,
      size=2.75,
      color="white",
      family="georg") +
  scale_fill_manual( guide = guide_legend(),
    values = rev(c("#3f1651", "#612b70", "#963586", "#cb4978", "#e9716a", "#f89f5b")),
    name=""  
    ) +
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major=element_blank(),
              panel.border=element_blank(),
              text = element_text(family = "georg"),
              strip.text = element_text(size=12),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.line   = element_line(colour=NA),
              axis.line.x = element_blank(),
              axis.line.y = element_blank(),
              axis.text.y = element_text(margin = unit(c(3, 0, 0, 0), "mm"),colour="grey10", size=8, hjust=0),
              axis.text.x = element_blank(), 
              axis.ticks = element_blank(),
        legend.title = element_text(),
        plot.title = element_text(size=8, color="grey20", hjust=.5),
        legend.key=element_rect(fill=NA),
        legend.position="none", legend.direction="horizontal") + 
        xlab("") + 
        ylab("") + 
        coord_flip()
return(l)
}

women_science_boxplot()

#map 
women_science_map <- function(p) {             
  shp_data <- sfd

  p <- 
    ggplot() +
    geom_sf(data=shp_data, 
      aes(fill=cat), 
        color="white", 
        size=0.15) +
    coord_sf(crs = crsLAEA, 
      xlim = c(b["xmin"], b["xmax"]), 
      ylim = c(b["ymin"], b["ymax"])) +
    labs(y="", subtitle="",
      x = "",
      title="Female researchers as a % of total researchers\n(2019 or latest year available)",
      caption="©2022 Milos Popovic https://milospopovic.net\nSource: UNESCO Institute for Statistics, June 2019.\nhttp://uis.unesco.org/sites/default/files/documents/fs55-women-in-science-2019-en.pdf")+
    scale_fill_manual(name= "",
      values = rev(c("grey80", "#3f1651", "#612b70", "#963586", "#cb4978", "#e9716a", "#f89f5b")),
      drop=F)+
    guides(fill=guide_legend(
      direction = "horizontal",
      keyheight = unit(1.15, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = .5,
      nrow =1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
        )
      ) +
    theme_minimal() +
    theme(text = element_text(family = "georg"),
      panel.background = element_blank(), 
      legend.background = element_blank(),
      legend.position = c(.45, -.02),
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.title = element_text(size=14, color="#3f1651", hjust=0.5, vjust=1),
      plot.subtitle = element_text(size=11, color="#7a2b41", hjust=0.5, vjust=0, face="bold"),
      plot.caption = element_text(size=8, color="grey60", hjust=0, vjust=-6),
      axis.title.x = element_text(size=10, color="grey20", hjust=0.5, vjust=-6),
      legend.text = element_text(size=9, color="grey20"),
      legend.title = element_text(size=11, color="grey20"),
      strip.text = element_text(size=12),
      plot.margin = unit(c(t=-4, r=0, b=-4, l=10), "lines"),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank())
return(p)
}

women_science_map()

#combine

vp <- viewport(width=0.3,height=0.85,x=0.15,y=0.5)
png("female_researchers_1c.png", height=4200, width=4000, res=600)
# clear current device
grid.newpage()
print(p)
print(l+labs(title=""),vp=vp)
dev.off()