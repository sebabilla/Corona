map_15d <- function(i){ 
  table15d <-  table_places %>% filter(リリース日 >= start_date + i - 15 & リリース日 <= start_date + i)
  hok_map %>% ggplot(aes(long, lat)) +
    geom_polygon(fill = "white", colour = "grey50") +
    xlim(139.2, 146.3) +
    ylim(40.7, 46.3) +
    coord_quickmap() +
    geom_jitter(data = table15d, aes(x = long, y = lat, colour = 性別), width = 0.1, height = 0.1, shape = 6, alpha = 0.5, size = 1.5) +
    scale_color_discrete("", limits = c("女性", "男性", NA)) +
    geom_text(data = ext_hok, aes(x = long, y = lat, label = location)) +
    theme_void()
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