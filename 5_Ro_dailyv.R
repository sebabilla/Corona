#functions to calculate log(Ro)
fk <- function(nt, Nbis)log(Nbis[nt]) - log(Nbis[nt-1])
fKtpond <- function(t, Kt, tau){
  mean(Kt[t:(t-tau+1)])
}
calclogRot <- function(N, tau){
  Nbis <- c(c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01), N)
  Nbis <- ifelse(Nbis == 0, 0.01, Nbis)
  nt <- 2:length(Nbis)
  Kt <- sapply(nt, fk, Nbis)
  t <- 7:length(Kt)
  Ktpond  <- sapply(t, fKtpond, Kt, tau)
  logRot <- Ktpond*tau
  logRot
}

#calculate log(Ro(t)) 
timetable <- ch %>% select(リリース日) %>% group_by(リリース日) %>% 
  summarize(n = n()) %>% mutate(リリース日 = ymd(リリース日))
timetable <- tibble(リリース日 = count_days + start_date) %>% 
  left_join(. , timetable) %>%
  mutate(n = ifelse(is.na(n), 0.01, n)) %>%
  mutate(logRo = calclogRot(n, 7))
max_n <- max(timetable$n)
timelimits <- c(start_date - 2, start_date + max(count_days) + 2)

graph_daily_v <- function(i){
  dailyv <- timetable %>% filter(リリース日 <= start_date + i)
  logRo <- last(dailyv$logRo)
  labelRo <- paste("log(Ro) ~ ", round(logRo, digits = 1))
  signRo <- ifelse(logRo > 0, "red3", "green4")
  dailyv %>% ggplot(aes(リリース日, n)) + 
    geom_col(fill = "blue4") +
    geom_text(aes(x=start_date + 10, y = 38, label = "基本再生産数"), col = "black", fontface = "plain", size = 4) +
    geom_text(aes(x=start_date + 10, y = 30, label = labelRo), col = signRo, fontface = "plain", family = "serif", size = 5) +
    scale_x_date(date_labels = "%m-%d", date_breaks = "2 weeks", limits = timelimits) +
    ylim(0, max_n) +
    ylab("人数") +
    theme_bw()
}