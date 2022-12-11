# ***********************************************************************************************
# Title   : 計量経済学
# Chapter : 5 重回帰回帰モデルの推定と検定
# Theme   : 実証例5.6 都市化の度合いと初期時点GDPの交互作用
# Date    : 2022/12/12
# Page    : P173 - P174
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
    mutate(D = did > 0.4)

# データ確認
df_model %>% print()
df_model %>% glimpse()


# 2 モデル構築 -------------------------------------------------------

# ＜ポイント＞
# -


# モデル構築
# --- モデル1   ：交差項モデル
# --- モデル2-1 ：ダミー変数でデータ抽出したモデル
# --- モデル2-2 ：ダミー変数でデータ抽出したモデル
model_1   <- lm(growthrate8099 ~ D * lny80, data = df_model)
model_2_1 <- lm(growthrate8099 ~ lny80, data = subset(df_model, D == 0))
model_2_2 <- lm(growthrate8099 ~ lny80, data = subset(df_model, D == 1))

# 結果比較
list(model_1 = model_1, model_2_1 = model_2_1, model_2_2 = model_2_2) %>%
  modelsummary(statistic = "{std.error}({statistic}){stars}")
