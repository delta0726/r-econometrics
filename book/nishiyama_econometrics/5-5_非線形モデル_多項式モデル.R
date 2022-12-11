# ***********************************************************************************************
# Title   : 計量経済学
# Chapter : 5 重回帰回帰モデルの推定と検定
# Theme   : 実証例5.5 信頼と規範が経済成長に与える影響の多項式モデル
# Date    : 2022/12/12
# Page    : P170 - P171
# URL     : https://ritsu1997.github.io/r-for-nlas-econometrics/index.html
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 モデル構築
# 2 プロット比較


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
# - 2乗項はI()を使ってフォーミュラで表現するか、データフレームで二乗の列を作成する


# データフレームの編集
# --- 2乗項の追加
df_2 <- df %>% mutate(y80_2 = y80^2)


# モデル構築
# --- モデル1   ：線形回帰モデル
# --- モデル2-1 ：多項式モデル（フォーミュラで表現）
# --- モデル2-2 ：多項式モデル（データフレームで表現）
model_1   <- lm_robust(growthrate8099 ~ y80, data = df, se_type = "stata")
model_2_1 <- lm_robust(growthrate8099 ~ y80 + I(y80^2), data = df, se_type = "stata")
model_2_2 <- lm_robust(growthrate8099 ~ y80 + y80_2, data = df_2, se_type = "stata")

# 結果比較
list(model_1 = model_1, model_2_1 = model_2_1, model_2_2 = model_2_2) %>%
  modelsummary(statistic = "{std.error}({statistic}){stars}")


# 2 プロット作成 ----------------------------------------------------------

p1 <-
  df %>%
    ggplot(aes(x = y80, y = growthrate8099)) +
      geom_point() +
      stat_smooth(method = "lm", formula = y ~ x) +
      xlim(c(2.5, 5.5)) +
      ylim(c(2, 4))

p2 <-
  df %>%
    ggplot(aes(x = y80, y = growthrate8099)) +
      geom_point() +
      stat_smooth(method = "lm", formula = y ~ x + I(x^2)) +
      ylim(c(2, 4)) +
      xlim(c(2.5, 5.5)) +
      ylim(c(2, 4))

grid.arrange(p1, p2, ncol = 2)