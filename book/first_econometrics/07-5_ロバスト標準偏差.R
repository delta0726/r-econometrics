# ***********************************************************************************************
# Title   : 計量経済学の第一歩
# Chapter : 7 重回帰分析の応用
# Theme   : 例7.5 分散が不均一な時のロバスト標準偏差
# Date    : 2022/12/06
# Page    : P178 - P181
# URL     : http://www.yuhikaku.co.jp/static/studia_ws/index.html#isbn_9784641150287
# ***********************************************************************************************


# ＜概要＞
# - ｢誤差分散が不均一｣という仮定は共変量の有無によって満たすことが難しくなりがち
# - 最小二乗推定量は分散均一前提として標準誤差を計算してる（t検定やF検定の信頼性も揺らぐ）
# - 分散不均一の場合は誤差項の分散ではなく、 最小二乗推定量の分散を推定する（ロバスト標準誤差）


# ＜参考資料＞
# 回帰分析ではlm()ではなくestimatr::lm_robust()を使おう
# https://speakerdeck.com/dropout009/tokyor100
# ⇒標準誤差とは何かを教えてくれている


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 線形確率モデルの構築


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(broom)
library(modelsummary)
library(lmtest)
library(sandwich)
library(estimatr)
library(ggfortify)


# データロード
df <- read_csv("csv/6_1_income.csv")


# 1 データ確認 ------------------------------------------------------------------

# ＜データ内容＞
# exper  ：就業可能年数
# exper2 ：就業可能年数^2
# yeduc  ：就学年数
# income ：賃金
# lincome：ln(賃金)

# データ確認
df %>% print()
df %>% glimpse()


# 2 通常の線形回帰モデル ----------------------------------------------------------

# ＜命題＞
# - ミンサー方程式をOLSで重回帰


# モデル構築
reg1 <- lm(lincome ~ yeduc + exper + exper2, data = df)

# 結果確認
list(reg1 = reg1) %>% modelsummary(statistic = "({statistic}){stars}")

# 残差プロット
reg1 %>% autoplot()


# 3 ホワイト修正によるモデル推定 ----------------------------------------------------

# ＜ポイント＞
# - 標準誤差のみが修正されている
#   --- 元の標準偏差よりも大きくなることもあれば、小さくなることもある


# 分散共分散行列を計算
vcov <- reg1 %>% vcovHC(type = "HC1")

# 重回帰分析の結果とvcovから標準誤差をホワイト修正
reg_rob <- reg1 %>% coeftest(vcov)

# 結果確認
reg_rob %>% print()
reg_rob %>% class()

# 結果比較
list(reg1 = reg1, reg_rob  = reg_rob) %>%
  modelsummary(statistic = "({statistic}){stars}")


# 4 estimatrのlm_robust()による回帰 -------------------------------------------------

# ＜ポイント＞
# - stats::lm()をestimatr::lm_robust()に置き換えるだけで、ロバスト標準誤差の推定ができる
#   --- lm_robust()を用いると｢分散均一の仮定｣を緩和することができる


# モデル構築
reg_rob2 <- lm_robust(lincome ~ yeduc + exper + exper2, data = df)

# 結果確認
list(reg1 = reg1, reg_rob  = reg_rob, reg_rob2  = reg_rob2) %>%
  modelsummary(statistic = "({statistic}){stars}")
