map_15d <- function(i){ 
  table15d <-  table_places %>% 
    filter(リリース日 >= start_date + i - 15 & リリース日 <= start_date + i) %>%
    group_by(place) %>% mutate(n = n()) %>%
    ungroup()
  table15d1 <- table15d %>% filter(n==1)
  table15d10 <- table15d %>% filter(n>1 & n<=10)
  table15dx <- table15d %>% filter(n>10)
  hok_map %>% ggplot(aes(long, lat)) +
    geom_polygon(fill = "white", colour = "grey50") +
    xlim(139.2, 146.3) +
    ylim(40.9, 45.6) +
    coord_quickmap() +
    geom_jitter(data = table15d1, aes(x = long, y = lat, colour = 性別), width = 0.01, height = 0.01, shape = 6, alpha = 1, size = 2) +
    geom_jitter(data = table15d10, aes(x = long, y = lat, colour = 性別), width = 0.05, height = 0.05, shape = 6, alpha = 0.6, size = 2) +
    geom_jitter(data = table15dx, aes(x = long, y = lat, colour = 性別), width = 0.1, height = 0.1, shape = 6, alpha = 0.3, size = 2) +
    scale_color_discrete("", limits = c("女性", "男性", NA)) +
    geom_text(data = ext_hok, aes(x = long, y = lat, label = location)) +
    theme_void() +
    theme(legend.position="top")
}