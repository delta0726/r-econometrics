# ***********************************************************************************************
# Title   : 計量経済学の第一歩
# Chapter : 9 パネルデータ分析
# Theme   : 例9.2：2期間パネルによる政策評価
# Date    : 2022/12/02
# Page    : P227 - P229
# URL     : http://www.yuhikaku.co.jp/static/studia_ws/index.html#isbn_9784641150287
# ***********************************************************************************************


# ＜概要＞
# - ダミー変数を導入することで政策有無についての効果を算出する
#   --- 自己選択がある時に｢差の差の推定法｣を使って政策効果を評価したい場合は、外的条件をできるだけ制御する
#   --- パネルデータの場合、時間を通じて変化しない個別効果を制御しながら政策評価を行う


# ＜目次＞
# 0 準備
# 1 パネルデータの確認
# 2 線形回帰によるモデル構築
# 3 パネル回帰によるモデル構築
# 4 パネル回帰を再現する


# 0 準備 ---------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(broom)
library(plm)
library(modelsummary)


# データロード
df <- read_csv("csv/9_2_life_xt.csv")


# 1 パネルデータの確認 ---------------------------------------------------

# ＜ポイント＞
# - パネルデータとは、各時点において同じidの値を観測したデータセットのことをいう


# データ確認
df %>% print()
df %>% glimpse()

# 各idごとにt1とt2が観測されている
df %>% select(id, t) %>% table() %>% head()

# shockのダミーフラグ
# --- t2時点から見て過去1年に病気やけがで休んだ場合に1を付ける
# --- t1時点のレコードにも同様にフラグが付いている
df %>% filter(t == 2) %>% group_by(shock) %>% tally()
df %>% filter(t == 1) %>% group_by(shock) %>% tally()

# shock_y2のダミーフラグ
# --- shockのうちt2時点のレコードのみフラグが付いている
df %>% filter(t == 2) %>% group_by(shock_y2) %>% tally()
df %>% filter(t == 1) %>% group_by(shock_y2) %>% tally()


# 2 線形回帰によるモデル構築 -----------------------------------------

# ＜ポイント＞
# - まずは、固定効果を考慮せずにt=2のクロスセクションデータで回帰する


# モデル構築
#t=2期目（2009年）のデータを使って最小2乗法で回帰
reg1 <- lm(life ~ shock + income, data = subset(df, t == 2))

# 結果確認
# --- shockの回帰係数はマイナスで統計的に有意
list(t2 = reg1) %>% modelsummary(statistic = "({statistic}){stars}")


# 3 パネル回帰によるモデル構築 ----------------------------------------

# ＜ポイント＞
# - idとtで個別効果(I)を測定する
# - modelをfdとすることで1階差分法を用いる


# モデル構築
# --- 1階差分法（fd）による回帰
preg1 <- plm(life ~ shock_y2 + income, data = df, effect = "individual",
             model = "fd", index = c("id", "t"))

# 結果確認
list(t2 = reg1, preg1 = preg1) %>%
  modelsummary(statistic = "({statistic}){stars}")


# 4 パネル回帰を再現する -------------------------------------------

# ＜ポイント＞
# - 個別効果を考慮するため2時点のデータを使って1階差分を取る
#   --- 最小2乗法で回帰する形で1階差分法の実行


# データを抽出
# --- t=1
# --- t=2
df_s1 <- subset(df, t == 1)
df_s2 <- subset(df, t == 2)

# t=2とt=1の差分を取る
df_ds <- df_s2 - df_s1

#差分を取ったデータを使って最小2乗法で回帰
replicate <- lm(life ~ shock_y2 + income, data = df_ds)

# 結果確認
list(preg1 = preg1, replicate = replicate) %>%
  modelsummary(statistic = "({statistic}){stars}")
