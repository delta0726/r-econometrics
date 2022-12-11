# ***********************************************************************************************
# Title   : 計量経済学
# Chapter : 5 重回帰回帰モデルの推定と検定
# Theme   : 実証例5.8 非線形モデルにおける結合仮説の検定
# Date    : 2022/12/12
# Page    : P178
# URL     : https://ritsu1997.github.io/r-for-nlas-econometrics/index.html
# ***********************************************************************************************


# ＜目次＞
# 0 準備
# 1 モデル構築


# 0 準備 ---------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(estimatr)
library(modelsummary)
library(gridExtra)


# データロード
df <- read_csv("csv/youdou.csv")

# データ準備
df <-
  df %>%
    mutate(lny80 = log(y80),
           lny99 = log(y99),
           lny90 = log(y90),
           growthrate8099 = (lny99 - lny80) / 19  * 100,
           growthrate8090 = (lny90 - lny80) / 10)

# データ確認
df %>% print()
df %>% glimpse()


# 1 モデル構築 -------------------------------------------------------

# ＜ポイント＞
#


# モデル構築
model <- lm_robust(growthrate8099 ~ y80 + I(y80^2), se_type = "stata", data = df)


# 結果比較
model %>% summary()
model %>% glance()
