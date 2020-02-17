require("data.table")
require("dplyr")
require("tidyr")
require("stringr")
require("lubridate")
require("ggplot2")
require("plotly")




options(scipen = 10)




# population data

# preprocessing

popul = fread("D:/멤버십/세대구성별_가구_및_가구원__시군구_20191015174451.csv")

popul_solo = data.frame(year = unique(unlist(popul[1,-1]) %>% as.numeric()),
                        num = popul %>% select(popul[2, ] %>% str_detect("1인가구원") %>% 
                                                 which()) %>% slice(3) %>% unlist() %>% as.numeric())


# one-person households

ggplotly(popul_solo %>% ggplot(aes(x = year, y = num)) + 
           geom_line(color = "#33CCFF") + geom_point(color = "#33CCFF") +
           ggtitle("연도별 1인 가구") +
           theme(plot.title = element_text(hjust = 0.5)) +
           labs(x = "연도", y = "1인 가구(명)", fill = ""))




# convenience store data

# preprocessing

cvs = fread("D:/멤버십/편의점_매출_동향_품목별__20191015174131.csv", header = T) %>% 
  slice(1, 7, 8)

cvs_ratio = data.frame(year = rep(2013:2018, 2), 
                       ratio = c(1, 1*(1.095), 1*(1.095)*(1.145), 1*(1.095)*(1.145)*(1.118),
                                 1*(1.095)*(1.145)*(1.118)*(1.151),1*(1.095)*(1.145)*(1.118)*(1.151)*(1.081),
                                 1, 1*(1.047), 1*(1.047)*(1.182), 1*(1.047)*(1.182)*(1.943),
                                 1*(1.047)*(1.182)*(1.943)*(1.148),1*(1.047)*(1.182)*(1.943)*(1.148)*(1.118)),
                       cat = rep(c("가공식품", "즉석·신선"), each = 6))


# cvs sales

ggplotly(cvs_ratio %>% ggplot(aes(x = year, y = ratio, color = cat)) + 
           geom_line() + geom_point() +
           ggtitle(label = "CU 연도별 가공, 즉석식품 매출 성장") +
           theme(plot.title = element_text(hjust = 0.5),
                 plot.subtitle = element_text(hjust = 1, size = 8)) +
           labs(x = "연도", y = "매출 성장", color = ""))




# occupancy data

# preprocess

occup = data.frame(carrier = c("SKT", "KT", "LG", "MVNO"),
                   num = c(27992484, 17919083, 13856518, 8030007))


# occupancy ratio

occup %>% plot_ly(labels = ~carrier, values = ~num) %>% 
  add_pie(hole = 0.6,
          marker = list(colors = c("#3366FF", "#FF3333", "#FF66CC", "#66FFCC"))) %>% 
  layout(title = "Donut Chart for Carriers Occupancy Ratio")
  



# membership data

# preprocessing

memb_count = data.frame(age = paste(seq(10, 60, by = 10), "대", sep = ""),
                        count = c(4.62, 4.56, 4.32, 3.34, 2.63, 2.28))

memb05 = fread("D:/멤버십/MBR_CVS_05MONTH.csv", header = T, encoding = "UTF-8")
memb06 = fread("D:/멤버십/MBR_CVS_06MONTH.csv", header = T, encoding = "UTF-8")
memb07 = fread("D:/멤버십/MBR_CVS_07MONTH.csv", header = T, encoding = "UTF-8")
memb08 = fread("D:/멤버십/MBR_CVS_08MONTH.csv", header = T, encoding = "UTF-8")
memb09 = fread("D:/멤버십/MBR_CVS_09MONTH.csv", header = T, encoding = "UTF-8")

memb = bind_rows(memb05, memb06) %>% 
  bind_rows(memb07) %>% 
  bind_rows(memb08) %>% 
  bind_rows(memb09) %>%
  unite(col = "date", colnames(.)[2:4], sep = "-") %>% 
  mutate(date = ymd(date)) %>% 
  arrange(date)


# count

ggplotly(memb_count %>% ggplot(aes(x = age, y = count, fill = age)) + geom_col() +
           ggtitle("연령대별 평균 멤버십 이용 횟수") +
           theme(plot.title = element_text(hjust = 0.55)) +
           labs(x = "연령대", y = "평균 사용 횟수(월)", fill = ""))

# by region

ggplotly(memb %>% group_by(`지역-시도`) %>% summarise(ratio = mean(`이용비율(%)`)) %>% 
           ggplot(aes(x = `지역-시도`, y = ratio, fill = `지역-시도`)) + geom_col() +
           ggtitle("지역별 평균 멤버십 사용 비율") +
           theme(plot.title = element_text(hjust = 0.55)) +
           labs(y = "사용비율(월)", fill = ""))

# by gender

ggplotly(memb %>% group_by(성별) %>% summarise(ratio = mean(`이용비율(%)`)) %>% 
           ggplot(aes(x = 성별, y = ratio, fill = 성별)) + geom_col() +
           ggtitle("성별 평균 멤버십 사용 비율") +
           theme(plot.title = element_text(hjust = 0.5)) +
           scale_fill_manual(values = c("#33CCFF", "#FF6699")) +
           labs(y = "사용비율(월)", fill = ""))

# by age

ggplotly(memb %>% group_by(연령대) %>% summarise(ratio = mean(`이용비율(%)`)) %>% 
           ggplot(aes(x = 연령대, y = ratio, fill = 연령대)) + geom_col() +
           ggtitle("연령별 평균 멤버십 사용 비율") +
           theme(plot.title = element_text(hjust = 0.5)) +
           labs(y = "사용비율(월)", fill = ""))

# by age, agender

ggplotly(memb %>% group_by(연령대, 성별) %>% summarise(ratio = mean(`이용비율(%)`)) %>% 
  ggplot(aes(x = 연령대, y = ratio, fill = 성별)) + geom_col(position = "dodge") +
    scale_fill_manual(values = c("#33CCFF", "#FF6699")) +
    ggtitle("성별 연령별 평균 멤버십 사용 비율") +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(y = "사용비율(월)", fill = ""))