#grouping all the graphs in one, making on image per day, export it
for (i in count_days) {
#for (i in 95) {  
    graph_inter <- graph_sex_job_age(i)
    filename <- ifelse(i<10, 
                       paste("Images/graph00", i, ".png", sep = ""), 
                       ifelse(i<100, 
                              paste("Images/graph0", i, ".png", sep = ""), 
                              paste("Images/graph", i, ".png", sep = "")))
    ggsave(filename, plot = graph_inter, width = 15, height = 10, units = "cm")
  }