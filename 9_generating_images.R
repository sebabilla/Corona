#grouping all the graphs in one, making on image per day, export it
for (i in count_days) {
#for (i in max(count_days)) {  
  gr_date <- start_date + i
  title <- paste("北海道におけるコロナウイルスの普及    ", year(gr_date), "年", month(gr_date), "月", day(gr_date), "日まで", sep = "")
  g1 <- graph_sex_job_age(i)
  g2 <- map_15d(i)
  g3 <- graph_daily_v(i)
  g4 <- graph_casuality(i)
  lay = rbind(c(2,2,2,2,1,1,1,1,1,1), c(2,2,2,2,1,1,1,1,1,1), c(3,3,3,3,3,3,3,3,4,4))
  graph_inter <- grid.arrange(g1, g2, g3, g4, layout_matrix = lay, top = title)
  filename <- ifelse(i<10, 
                     paste("Images/graph00", i, ".png", sep = ""), 
                     ifelse(i<100, 
                            paste("Images/graph0", i, ".png", sep = ""), 
                            paste("Images/graph", i, ".png", sep = "")))
  ggsave(filename, plot = graph_inter, width = 30, height = 15, units = "cm")
  print(i)
}