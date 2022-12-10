# ***********************************************************************************************
# Title   : 計量経済学
# Chapter : 5 重回帰回帰モデルの推定と検定
# Theme   : 実証例5.2 FWL定理の確認
# Date    : 2022/12/11
# Page    : P154
# URL     : https://ritsu1997.github.io/r-for-nlas-econometrics/index.html
# ***********************************************************************************************


# ＜概要＞
# - FWL定理は重回帰モデルのOLS推定量が単回帰分析を繰り返すことで導くことができることを示す
#   --- 欠落変数バイアスのメカニズムを知るうえで重要


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


# 1 モデル構築 -------------------------------------------------------

# ＜ポイント＞
# - FWL定理でアプローチするtrust80の回帰係数を確認する


# ベースモデルの構築
model <- lm(formula = growthrate8099 ~ trust80 + education80 + lny80, data = df)

# 回帰係数の確認
# --- trust80の回帰係数は0.0206
model %>% tidy() %>% filter(term == "trust80")


# 2 FWL定理によるアプローチ ---------------------------------------------

# ＜ポイント＞
# - FWL定理は重回帰モデルのOLS推定量が単回帰分析を繰り返すことで導くことができることを示す


# ＜アプローチ＞
# - reg2 ~ 0 + reg1 によりターゲット変数を再現することができる
#   --- ターゲット変数で他の変数を回帰した残差(reg1)
#   --- ターゲット変数を除いて回帰した残差(reg2)


# モデル構築
# --- trust80を残りの変数で回帰する
# --- ベースモデルをtrust80を除外して回帰する
reg1 <- lm(trust80 ~ education80 + lny80, data = df)
reg2 <- lm(growthrate8099 ~ education80 + lny80, data = df)

# 残差
res1 <- reg1$residuals
res2 <- reg2$residuals

# 定数項を0として推定
# --- res1の回帰係数は0.0206
model3 <- lm(res2 ~ 0 + res1)
model3 %>% tidy()
