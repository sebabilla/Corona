map_15d <- function(i){ 
  table15d <-  table_places %>% filter(リリース日 >= start_date + i - 15 & リリース日 <= start_date + i)
  hok_map %>% ggplot(aes(long, lat)) +
    geom_polygon(fill = "white", colour = "grey50") +
    xlim(139, 146.5) +
    ylim(40.5, 46.5) +
    coord_quickmap() +
    geom_jitter(data = table15d, aes(x = long, y = lat, color = 性別), width = 0.2, height = 0.2, alpha = 0.3) +
    scale_color_manual(values = c("Red", "Blue"), limits = c("女性", "男性"))
}
for (i in count_days) {
  #for (i in 95) {  
  graph_inter <- map_15d(i)
  filename <- ifelse(i<10, 
                     paste("Images/graph00", i, ".png", sep = ""), 
                     ifelse(i<100, 
                            paste("Images/graph0", i, ".png", sep = ""), 
                            paste("Images/graph", i, ".png", sep = "")))
  ggsave(filename, plot = graph_inter, width = 15, height = 10, units = "cm")
}