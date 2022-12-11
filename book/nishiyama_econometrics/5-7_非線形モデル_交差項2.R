# ***********************************************************************************************
# Title   : 計量経済学
# Chapter : 5 重回帰回帰モデルの推定と検定
# Theme   : 実証例5.7 都市化の度合いと初期時点GDPのダミー変数同士の交互作用
# Date    : 2022/12/12
# Page    : P175
# URL     : https://ritsu1997.github.io/r-for-nlas-econometrics/index.html
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 モデル構築


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


# 1 データ準備 ---------------------------------------------------------

# ＜ポイント＞
# - ダミー変数のためのカテゴリカルデータを作成


# データフレームの編集
# --- did列をフラグをカテゴリ変数に変換
df_model <-
  df %>%
    select(growthrate8099, lny80, did) %>%
    mutate(D1 = did > 0.4,
           D2 = lny80 > 1.8)

# データ確認
df_model %>% print()
df_model %>% glimpse()


# 2 モデル構築 -------------------------------------------------------

# ＜ポイント＞
# -


# モデル構築
model_1   <- lm(growthrate8099 ~ D1 * D2)

# 結果比較
list(model_1 = model_1) %>%
  modelsummary(statistic = "{std.error}({statistic}){stars}")
