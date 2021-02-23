#***************************************************************************************
# Title     : 効果検証入門
# Chapter   : 3章 傾向スコアを用いた分析
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/13
# URL       :
#***************************************************************************************


# ＜目次＞
# 0 準備
# 1 データ加工
# 1 傾向スコアの仕組み
# 2 傾向スコアを利用した効果の推定
# 3 機械学習を利用したメールマーケティング施策の効果推定
# 4 LaLondeデータセットの分析



# 0 準備 ------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(haven)
library(broom)
library(MatchIt)
library(WeightIt)
library(cobalt)

# データ読み込み
cps1_data <- read_dta("https://users.nber.org/~rdehejia/data/cps_controls.dta")
cps3_data <- read_dta("https://users.nber.org/~rdehejia/data/cps_controls3.dta")
nswdw_data <- read_dta("https://users.nber.org/~rdehejia/data/nsw_dw.dta")

# データ加工
cps1_data


# 1 データ加工 ------------------------------------------------------------------

# データ加工
# --- NSWデータから介入グループだけ取り出してCPS1と付ける
cps1_nsw_data <- n
swdw_data %>%
  filter(treat == 1) %>%
  rbind(cps1_data)

# データ加工
# --- NSWデータから介入グループだけ取り出してCPS3と付ける
cps3_nsw_data <-
  nswdw_data %>%
    filter(treat == 1) %>%
    rbind(cps3_data)




# (5) RCT データでの分析
## 共変量なしの回帰分析
nsw_nocov <-
  lm(re78 ~ treat, data = nswdw_data) %>%
    tidy()

## 共変量付きの回帰分析
nsw_cov <-
   lm(re78 ~ treat + re74 + re75 + age + education + black + hispanic + nodegree + married,
      data = nswdw_data) %>%
     tidy() %>%
     filter(term == "treat")

# (6) バイアスのあるデータでの回帰分析
## CPS1の分析結果
cps1_reg <-
   lm(re78 ~ treat + re74 + re75 + age + education + black + hispanic + nodegree + married,
      data = cps1_nsw_data) %>%
     tidy() %>%
     filter(term == "treat")

## CPS3の分析結果
cps3_reg <-
   lm(re78 ~ treat + re74 + re75 + age + education + black + hispanic + nodegree + married,
      data = cps3_nsw_data) %>%
     tidy() %>%
     filter(term == "treat")

# (7) 傾向スコアマッチングによる効果推定
## 傾向スコアを用いたマッチング
m_near <-
  matchit(treat ~ age + education + black + hispanic + nodegree +
          married + re74 + re75 + I(re74^2) + I(re75^2), data = cps1_nsw_data, method = "nearest")

## 共変量のバランスを確認
m_near %>% love.plot(threshold = .1)

## マッチング後のデータを作成
matched_data <- m_near %>% match.data()

## マッチング後のデータで効果の推定
PSM_result_cps1 <-
   lm(re78 ~ treat, data = matched_data) %>%
     tidy()

# (8) IPWによる効果推定
## 重みの推定
weighting <-
  weightit(treat ~ age + education + black + hispanic + nodegree
                   + married + re74 + re75 + I(re74^2) + I(re75^2),
           data = cps1_nsw_data,
           method = "ps",
           estimand = "ATE")

## 共変量のバランスを確認
weighting %>% love.plot(threshold = .1)

## 重み付きデータでの効果の推定
IPW_result <-
   lm(re78 ~ treat, data = cps1_nsw_data, weights = weighting$weights) %>%
     tidy()

# (9) IPWによる効果推定(ATT)
## 重みの推定
weighting <-
  weightit(treat ~ age + education + black + hispanic + nodegree
           + married + re74 + re75 + I(re74^2) + I(re75^2),
           data = cps1_nsw_data,
           method = "ps",
           estimand = "ATT")

## 共変量のバランスを確認
weighting %>% love.plot(threshold = .1)

## 重み付きデータでの効果の推定
IPW_result <-
   lm(re78 ~ treat, data = cps1_nsw_data, weights = weighting$weights) %>%
     tidy()
