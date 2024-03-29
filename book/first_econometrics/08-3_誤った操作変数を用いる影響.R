# ***********************************************************************************************
# Title   : 計量経済学の第一歩
# Chapter : 8 操作変数法
# Theme   : 例8.3 誤った操作変数を使ったら
# Date    : 2022/12/08
# Page    : P202 - P205
# URL     : http://www.yuhikaku.co.jp/static/studia_ws/index.html#isbn_9784641150287
# ***********************************************************************************************


# ＜概要＞
# - 誤った操作変数は回帰係数を大きく歪めかねない
#   --- 内生性があっても適切な操作変数がなければ、操作変数法は使わないほうが良い
#   --- 説明変数(X)と操作変数(U)の相関が低いと相関係数項が発散する（P202 式）


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 ベースモデルの構築
# 3 誤った操作変数を選択した影響


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(broom)
library(estimatr)


# データロード
df <- read_csv("csv/8_income.csv")


# 1 データ確認 ----------------------------------------------------------------

# ＜データ内容＞
# - lincome ：収入(対数値)
# - payeduc ：使用しない
# - yeduc   ：就学年数(本人)
# - exper   ：使用しない
# - exper2  ：使用しない
# - mbirth  ：生まれ月
# - moyeduc ：使用しない
# - sibs    ：使用しない

df %>% print()
df %>% glimpse()


# 2 ベースモデルの構築 -------------------------------------------------------

# ＜ポイント＞
# - 政策変数yeduc(X)は、結果変数lincome(Y)を統計的に有意に説明している


# モデル構築
# --- ベースモデル
reg1 <- lm(lincome ~ yeduc, data = df)
reg1 %>% tidy()


# 3 誤った操作変数を選択した影響 -----------------------------------------------

# ＜ポイント＞
# - 誤った操作変数を用いるとyeduc(X)は0.055から0.298に急増するが、統計的有意ではなくなる
#   --- t値が下がると操作変数としては不適切


# 操作変数の適正性評価
# --- 政策変数を操作変数で回帰
# --- 統計的に有意でない（誤った操作変数）
reg2 <- lm(yeduc ~ mbirth, data = df)
reg2 %>% tidy()

# モデル構築
# --- 誤った操作変数を使って教育の収益率を推定
ivreg1 <- ivreg(lincome ~ yeduc | mbirth, data = df)

# 結果比較
list(reg1 = reg1, ivreg1 = ivreg1) %>%
  modelsummary(statistic = "({statistic}){stars}")
