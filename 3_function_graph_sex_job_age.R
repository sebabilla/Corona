#function for doing graph of sex, age, job at t+i
graph_sex_job_age <- function(i){
  gr_date <- start_date + i
  title <- paste("北海道の患者 ", year(gr_date), "年", month(gr_date), "月", day(gr_date), "日", sep = "")
  daily_table <- table_sex_job_age %>% filter(リリース日 <= start_date + i)
  daily_job <- daily_table %>%
    select(属性) %>% 
    mutate(job = str_sub(属性, end = 2)) %>%
    group_by(job) %>%
    summarize(n = n()) %>% 
    arrange(desc(n)) %>%
    left_join(jobmax_12, .)
  legend <- daily_job %>% unite("legend", job, n, sep = " ") %>% pull(legend)
  daily_table %>% select(-n) %>%
    left_join(. , daily_job, by = "job") %>% 
    unite("daily_job", job, n, sep = " ") %>%
    ggplot(aes(年代, fill = fct_infreq(daily_job))) + 
    geom_bar(color="black") +
    facet_wrap( ~ 性別, ncol=2) +
    scale_x_discrete(limits = c("0", "10", "20", "30", "40", "50", "60", "70", "80", "90", "100", NA)) +
    ylab("人数") +
    ggtitle(title) +
    scale_fill_brewer("    属", palette = "Set3", limits = legend) +
    theme_bw() +
    theme(text = element_text(size = 11))
}