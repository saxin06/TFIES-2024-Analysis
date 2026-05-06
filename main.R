library(dplyr)
library(haven)
library(labelled)
library(ggplot2)

setwd('/Users/sandyhsu/Desktop/社會統計/4:29/2024年家庭收支調查')
TFIES <- read_spss(file='inc113.sav')
summary(TFIES$itm400)
summary(TFIES$itm600)

TFIES2024 <- mutate(.data=TFIES, itm600=ifelse(is.na(itm600),0,itm600),
                     income=itm400-itm600)

#所得總額
#家庭戶數
#每戶人數
#每人所得總額
#每戶所得總額
#每戶可支配所得平均數
#每人可支配所得平均數
result <- reframe(.data=TFIES2024,
                  所得總額= weighted.mean(x=TFIES$itm500, w=TFIES$a20),
                  家庭戶數= sum(TFIES$a20),
                  每戶人數= weighted.mean(x=a8, w=a20),
                  每人所得總額= sum(itm500*a20)/sum(a8*a20),
                  每戶所得總額= sum(itm500*a20)/sum(a20),
                  每戶可支配所得平均數= sum(income*a20)/sum(a20))|>
  mutate(每人所得總額=每戶所得總額/每戶人數,
         每人可支配所得平均數=每戶可支配所得平均數/每戶人數)
result

#每人可支配所得中位數
MedianIncome <- TFIES2024 |>
  mutate(income=income/a8) |>
  arrange(income) |>
  mutate(CumPersons=cumsum(a8*a20)) |>
  summarise(每人可支配所得中位數=
              min(income[CumPersons>=sum(a8*a20)/2]))
MedianIncome

TFIES2024 |>
  mutate(income=income/a8) |>
  arrange(income) |>
  mutate(CumPersons=cumsum(a8*a20)) |>
  select(income, CumPersons)

#每戶可支配所得中位數
MedianIncome <- TFIES2024 |>
  mutate(income) |>
  arrange(income) |>
  mutate(CumHouseholds= cumsum(a20)) |>
  reframe(每戶可支配所得中位數=
              min(income[CumHouseholds>=sum(a20)/2]))

ggplot(data = filter(TFIES2024, income < quantile(income, 0.99, na.rm = TRUE)), 
       aes(x = income, weight = a20)) +
  geom_histogram(fill = "#4A4A4A", color = "white", bins = 50) +
  theme_minimal() +
  labs(title = "2024年家庭可支配所得分配直方圖",
       x = "可支配所得",
       y = "加權戶數") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        text = element_text(family = "Heiti TC")) 
