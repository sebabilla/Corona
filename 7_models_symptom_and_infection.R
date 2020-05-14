#at the end of the script, the data need to be generate manually!!

#real data of infected people, making broad categories, filter NA, non informative data
infected <- patients %>% select(リリース日, 性別, 属性, 年代, 居住地,備考) %>%
  filter(!is.na(性別)) %>% filter(!is.na(年代)) %>% filter(!is.na(居住地)) %>% filter(!居住地 %in% c("茨城県", "神奈川県", "東京都", "大阪府", "京都府", "中国武漢市")) %>%
  filter(!属性 %in% c("不明", NA)) %>%
  mutate(occupation = ifelse(属性 %in% c("中学生", "大学生", "学生", "専門学校生", "小学生", "未就学", "未就学児", "高校生"), "student", 
                               ifelse(属性 %in% c("アルバイト", "パート", "パート従業員", "パート職員"), "parttime",
                                        ifelse(属性 %in% c("介護老人保健施設入所者", "無職", "患者"), "nothing", 
                                                 ifelse(属性 %in% "主婦", "housewife",
                                                          ifelse(属性 %in% c("医師", "医療スタッフ", "医療ソーシャルワーカー", "医療従事者", "医療機関職員", "医療関係職員", "検疫官", "歯科医師", "病院職員", "看護助手", "看護師", "薬剤師", "開業医"),"healthcare", "fulltime")))))) %>%
  mutate(年代 = ifelse(年代 %in% c(90, 100), 80, 年代)) %>%
  mutate(age = as.numeric(年代), sex = ifelse(性別 == "女性", "Female", "Male"), date = ymd(リリース日), place = 居住地, infected = 1) %>%
  mutate(date = as.numeric(date - start_date)) %>%
  mutate(symptom = ifelse(備考 == "有症", 1, ifelse(備考 == "無症", 0, NA))) %>%
  select(date, sex, occupation, age, place, infected, symptom)

#function that will be fit the best glm
#may need to be rerun several times if unknown datas appears in the test set (small sample)
#will use the last tibble named infected 2
bestglm <- function(ind, dataset){
  test_index <- createDataPartition(dataset$output, times = 1, p = 0.1, list = FALSE)
  train_set <- dataset %>% slice(-test_index)
  test_set <- dataset %>% slice(test_index)
  glm_fit <- train_set %>% 
    glm(output ~ date+sex+occupation+age+place, data=., family = "binomial")
  p_hat_glm <- predict(glm_fit, newdata = test_set, type = "response")
  y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
  confmat <- confusionMatrix(y_hat_glm, as.factor(test_set$output))
  #making the output
  sensspec <- confmat$byClass[1:2]
  cmtable <- confmat$table
  sumglm <- summary(glm_fit)
  hotpoint <- tibble(category = rownames(sumglm$coefficients), estimate = sumglm$coefficients[,1], p = sumglm$coefficients[,4]) %>%
    filter(category != "(Intercept)", p <= 0.05) %>% arrange(p)
  outputlist <- list(sensspec, cmtable, hotpoint)
  if (outputlist[[1]][1]>0.25 & outputlist[[1]][1]>0.25) {
    outputlist
  }
}

#try to establish a model for probability to get symptoms
#1. defing the sets
infected2 <- infected %>% filter(!is.na(symptom)) %>% 
  group_by(place) %>% mutate(n=(n())) %>% filter(n>1) %>% ungroup() %>%
  mutate(output = symptom) %>% select(-infected, -symptom)
#2. may need to be run several times to not have errors
###!!!!
patientsbestmodel <- sapply(1:20, bestglm, infected2)
###!!!!
#3. extract the model with best sensibility (that's the wickness here)
patientsbestmodel <- patientsbestmodel %>% discard(is.null) #not the list nuber of the best fit, put it in the line below to uncomment them 
maxsensitivity <- which.max(lapply(1:length(patientsbestmodel), function(x) patientsbestmodel[[x]][[1]][1]))
patss <- tibble(class = names(patientsbestmodel[[maxsensitivity]][[1]]), value = signif(patientsbestmodel[[maxsensitivity]][[1]], digit = 3))
patconfmat <- as_tibble(patientsbestmodel[[maxsensitivity]][[2]]) %>% rename(predict = Prediction, ref = Reference)
patcoef<- patientsbestmodel[[maxsensitivity]][[3]] %>% mutate(category = str_sub(category, -10), estimate = signif(estimate, digits = 2), p = signif(p, digits = 2)) %>% rename("p<.05" = p, βhat = estimate)
patientsfit_grob <- grid.arrange(
  tableGrob(patss, rows = NULL, theme = ttheme_minimal(colhead=list(bg_params = list(col="black")))),
  tableGrob(patconfmat, rows = NULL, theme = ttheme_minimal(colhead=list(bg_params = list(col="black")))),
  tableGrob(patcoef, rows = NULL, theme = ttheme_minimal(colhead=list(bg_params = list(col="black")))),
  nrow=1, layout_matrix = rbind(c(1,1,2,2,3,3,3)), top = "モデル①の結果　どちらかというと、ベストフィットはいつも悪くなりました。\nというのは、様子についての一番大切な情報を持てなくて、推測できません。")
 
##try to establish a model for probability to get infected
#simulate data of non infected people according to demographics 
#1. generationg odds
occup_hok_sex_age <- read_csv("Data/occup_hok_sex_age.csv") #build with data from 2017 Employment Status Survey Results for Prefectures, Human resources for health country profiles: Japan. Manila, Philippines: World HealthOrganization Regional Office for the Western Pacific; 2017 and Table 15.  Population by Age (Five-Year Groups) and Sex, and Sex Ratio - Prefectures (2010 and 2015)
occup_hok_sex_age <- occup_hok_sex_age %>% gather("age", "count", `00`: `80`) %>% unite("s_o_a", sex:age)
sumohsac <- sum(occup_hok_sex_age$count)
occup_hok_sex_age <- occup_hok_sex_age %>% mutate(odd = count/sumohsac)
vec_soa <- occup_hok_sex_age$s_o_a
vec_soa_odd <- occup_hok_sex_age$odd
places_pop <- places %>% filter(!is.na(population))
sumh <- sum(places_pop$population)
places_pop <- places_pop %>% select(place, population) %>%
  mutate(pourcent = population/sumh) %>% filter(!is.na(pourcent))
vec_city <- places_pop$place
vec_city_odd <- places_pop$pourcent
#2. generating data
nnot_infected <- nrow(infected)*3
city <- sample(vec_city, nnot_infected, replace = TRUE, prob = vec_city_odd)
soa <- sample(vec_soa, nnot_infected, replace = TRUE, prob = vec_soa_odd)
sdate <- sample(count_days, nnot_infected, replace = TRUE)
not_infected <- tibble(date = sdate, place = city, soa = soa) %>% 
  separate(soa, c("sex", "occupation", "age"), sep = "_") %>%
  mutate(age = as.numeric(age), infected = 0)
#3. finally a dataset to feed the model
infected3 <- infected %>% add_row(not_infected) %>%
  arrange(date) %>% mutate(age = as.numeric(age), output = as.factor(infected)) %>%
  group_by(place) %>% mutate(n=n()) %>% filter(n>1) %>% (ungroup)
#4. give it to the model, may need to be run several times to not have errors
###!!!
infectedbestmodel <- sapply(1:20, bestglm, infected3)
###!!!
#5. extract the model with best specificity (that's the wickness here)
maxspecificity <- which.max(lapply(1:20, function(x) infectedbestmodel[,x][[1]][2]))
infss <- tibble(class = names(infectedbestmodel[,maxspecificity][[1]]), value = signif(infectedbestmodel[,maxspecificity][[1]], digit = 3))
infconfmat <- as_tibble(infectedbestmodel[,maxspecificity][[2]]) %>% rename(Pred = Prediction, Ref = Reference)
infcoef<- infectedbestmodel[,maxspecificity][[3]] %>% mutate(category = str_sub(category, -10), estimate = signif(estimate, digits = 2), p = signif(p, digits = 2)) %>% filter(estimate>0) %>% rename("p<.05" = p, βhat = estimate)
inffit_grob <- grid.arrange(
  tableGrob(infss, rows = NULL, theme = ttheme_minimal(colhead=list(bg_params = list(col="black")))),
  tableGrob(infconfmat, rows = NULL, theme = ttheme_minimal(colhead=list(bg_params = list(col="black")))),
  tableGrob(infcoef, rows = NULL, theme = ttheme_minimal(colhead=list(bg_params = list(col="black")))),
  nrow=1, layout_matrix = rbind(c(1,1,2,2,3,3,3)), top = "モデル②の結果　もちろん完璧ではありませんが、だれの状態が\nよりよい危険か安全だが少し言えます。ここでは危険な状態（β＞０）だけ見せます。")
