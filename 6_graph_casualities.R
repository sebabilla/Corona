#limits of the graph
maxd <- max(death$死亡累計)
nrows <- ceiling(sqrt(maxd))
death2 <- death %>% mutate(red = 日死亡数, grey = 死亡累計 - 日死亡数, white = nrows^2 - 死亡累計) %>%
  select(grey, red, white)

#function at t = i
graph_casuality <- function(i){
  j=1+i
  vgrey <- rep(1, death2$grey[j], length.out = NA)
  vred <- rep(2, death2$red[j], length.out = NA)
  vwhite <- rep(3, death2$white[j], length.out = NA)
  state <- c(vgrey, vred, vwhite)
  state <- state[!is.na(state)]
  df <- expand.grid(y = 1:nrows, x = 1:nrows)
  df <- df %>% mutate(state = as.character(state))
  ggplot(df, aes(x = x, y = y, fill = state)) + 
    geom_tile(color = "white", size = 0.5) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = c("grey30", "red", "white"), limits = c("1","2","3")) +
    ggtitle("死亡") +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())
}
