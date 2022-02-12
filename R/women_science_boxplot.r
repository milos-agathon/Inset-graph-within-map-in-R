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