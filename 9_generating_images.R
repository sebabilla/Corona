#presenting the 1st model : chance than people get symptoms
textmodel1 <- grid.arrange(text_grob("モデルコーナ  モデル①", face = "bold"),
                           text_grob("まずは、P(症状｜状態) ＝ β1時間 + β2性別 + β3年代 + Σβi属性 + Σβj居住地 + βo", face = "italic", color="darkblue"),
                           text_grob("をやってみました。方法は「glm」+トレーンセット＝90%"),
                           text_grob("設定\n属性（無、学生、パート、フール、健康）居住地（様々な市町村）\nカテゴリ変数から量的変数までを変更してみても、結果は変わりませんでした。"), layout_matrix = rbind(c(1),c(2), c(3), c(4), c(4)),ncol = 1)


#presenting the 2nd model : chance than people become infected
textmodel2 <- grid.arrange(text_grob("モデルコーナ  モデル②", face = "bold"),
                           text_grob("次は、P(感染｜状態) ＝ β1時間 + β2性別 + β3年代 + Σβi属性 + Σβj居住地 + βo", face = "italic", color="darkblue"),
                           text_grob("をやってみました。もう一回「glm」+トレーンセット＝90%"),
                           text_grob("問題\n「陽性患者属性」というデータベースで、アウトプットは患者に限ります。\n健常者のデータベースは国勢調査などを基づいて作りました。"), layout_matrix = rbind(c(1),c(2), c(3), c(4), c(4)),ncol = 1)

#Conclusion
textconclusion <- grid.arrange(text_grob(" 結論  持っている情報の上では\n(症状｜状態)が全然推測できますんが、(感染｜状態)が少し推測できます。\nもちろん人々の様子・関係により、その可能は非常に変更すると思います。", face = "bold", color="darkblue"),
                           text_grob("この動画はただの学生の遊びだけですね。"),
                           text_grob("made by Sébastien Abilla, 博士課程2年製\n北海道オーペンデータポータル(2020)新型コロナウイルス感染症に関するデータ\n総務省統計局(2017)、総合政策部(2019)、Irizarry(2020)、Wikipedia、..."), layout_matrix = rbind(c(1),c(1), c(1), c(2), c(3), c(3)),ncol = 1)



#grouping all the graphs in one, making on image per day, export it
for (i in count_days) {
#for (i in max(count_days)) {  
    gr_date <- start_date + i
    title <- paste("北海道における新型コロナウイルス感染症の普及    ", year(gr_date), "年", month(gr_date), "月", day(gr_date), "日まで", sep = "")
    g1 <- graph_sex_job_age(i)
    g2 <- map_15d(i)
    g3 <- graph_daily_v(i)
    g4 <- graph_casuality(i)
    if (i<0.25*max(count_days)) {
      g5 <- textmodel1
    }
    if (i>=0.25*max(count_days)&i<0.5*max(count_days)) {
      g5 <- patientsfit_grob
    }
    if (i>=0.5*max(count_days)&i<0.75*max(count_days)) {
      g5 <- textmodel2
    }
    if (i>=0.75*max(count_days)&i<max(count_days)) {
      g5 <- inffit_grob
    }
    if (i==max(count_days)) {
      g5 <- textconclusion
    }
    lay = rbind(c(2,2,2,2,1,1,1,1,1,1), c(2,2,2,2,1,1,1,1,1,1), c(3,3,3,4,4,5,5,5,5,5))
    graph_inter <- grid.arrange(g1, g2, g3, g4, g5, layout_matrix = lay, top = title)
    filename <- ifelse(i<10, 
                       paste("Images/graph00", i, ".png", sep = ""), 
                       ifelse(i<100, 
                              paste("Images/graph0", i, ".png", sep = ""), 
                              paste("Images/graph", i, ".png", sep = "")))
    ggsave(filename, plot = graph_inter, width = 30, height = 15, units = "cm")
    print(i)
  }