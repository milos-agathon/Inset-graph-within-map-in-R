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
      title="Female researchers as a % of total researchers\n(2017 or latest year available)",
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
