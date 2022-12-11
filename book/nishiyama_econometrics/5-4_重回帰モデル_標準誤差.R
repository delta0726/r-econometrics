# ***********************************************************************************************
# Title   : 計量経済学
# Chapter : 5 重回帰回帰モデルの推定と検定
# Theme   : 実証例5.4 信頼と規範が経済成長に与える影響の重回帰分析の標準誤差
# Date    : 2022/12/11
# Page    : P163
# URL     : https://ritsu1997.github.io/r-for-nlas-econometrics/index.html
# ***********************************************************************************************


# ＜概要＞
# - 回帰分析における標準誤差とは傾きのバラツキを意味する
#   --- 線形回帰モデルでは誤差項の分散均一の仮定が満たされないと不偏推定量とならない
#   --- 分散不均一に頑健な標準誤差を用いて推定することでモデル評価の信頼性を高める


# ＜参考資料＞
# 回帰分析ではlm()ではなくestimatr::lm_robust()を使おう
# https://speakerdeck.com/dropout009/tokyor100
# ⇒標準誤差とは何かを教えてくれている


# ＜目次＞
# 0 準備
# 1 モデル構築


# 0 準備 ---------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(estimatr)
library(modelsummary)


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
# - 重回帰分析を用いて欠落変数バイアスを避けることの重要性を確認
# - lm_robust()で分散不均一のロバスト標準誤差を用いて評価する（計量経済学は分散均一の仮定が満たしにくい）


# モデル構築
model_1 <- lm_robust(growthrate8099 ~ trust80 + education80 + lny80, data = df, se_type = "stata")
model_2 <- lm_robust(growthrate8099 ~ norm80 + education80 + lny80, data = df, se_type = "stata")

# 結果比較
list(model_1 = model_1, model_2 = model_2) %>%
  modelsummary(statistic = "{std.error}({statistic}){stars}")