# ***********************************************************************************************
# Title   : 計量経済学
# Chapter : 5 重回帰回帰モデルの推定と検定
# Theme   : 実証例5.1 信頼と規範が経済成長に与える影響の重回帰分析
# Date    : 2022/12/11
# Page    : P151 - P152
# URL     : https://ritsu1997.github.io/r-for-nlas-econometrics/index.html
# ***********************************************************************************************


# ＜概要＞
# - 複数のモデルを比較することで、説明変数による結果の歪みを確認する（欠落変数バイアス）
#   --- 非説明変数との相関が高い変数ほど、欠落変数バイアスが高い


# ＜目次＞
# 0 準備
# 1 欠落変数バイアス
# 2 説明変数を変更


# 0 準備 ---------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(modelsummary)
library(psych)


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


# 1 欠落変数バイアス ------------------------------------------------

# ＜ポイント＞
# - 線形回帰モデルは欠落変数バイアスの影響により結果が大きく異なる場合がある
#   --- Yと相関の強い変数を追加した場合に生じる


# データ確認
df %>%
  select(growthrate8099, trust80, education80, lny80) %>%
  pairs.panels()

# モデル構築
# --- モデル1： 単回帰モデル（P144 5.1）
# --- モデル2： 重回帰モデル
model_1 <- lm(growthrate8099 ~ trust80, data = df)
model_2 <- lm(growthrate8099 ~ trust80 + education80 + lny80, data = df)

# モデル比較
list(model_1 = model_1, model_2 = model_2) %>% modelsummary()


# 2 説明変数を変更 -------------------------------------------------

# ＜ポイント＞
# - trust80をnorm80に変更してみても、結果はそれほど大きく変化しない
#   --- norm80はtrust80と相関が高く、他の変数とも相関が低い


# データ確認
df %>%
  select(growthrate8099, trust80, norm80, education80, lny80) %>%
  pairs.panels()

# モデル構築
# --- モデル2： 重回帰モデル
# --- モデル3： 単回帰モデル（trust80をnorm80に変更）
model_2 <- lm(growthrate8099 ~ trust80 + education80 + lny80, data = df)
model_3 <- lm(growthrate8099 ~ norm80 + education80 + lny80, data = df)

# モデル比較
list(model_2 = model_2, model_3 = model_3) %>% modelsummary()
