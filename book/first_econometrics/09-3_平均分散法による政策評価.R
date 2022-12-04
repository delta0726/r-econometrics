# ***********************************************************************************************
# Title   : 計量経済学の第一歩
# Chapter : 9 パネルデータ分析
# Theme   : 例9.3：平均差分法
# Date    : 2022/12/03
# Page    : P229 - P232
# URL     : http://www.yuhikaku.co.jp/static/studia_ws/index.html#isbn_9784641150287
# ***********************************************************************************************


# ＜概要＞
# - 平均差分法を用いても、時間を通じて変化しない個別効果を制御することができる
#   --- 個別効果を消去するために、各個人について被説明変数と説明変数全ての時間平均を作る
#   --- それをそれぞれの重回帰モデルから引くことによって個別効果を消去する


# ＜目次＞
# 0 準備
# 1 パネルデータの確認
# 2 固定効果モデルの構築
# 3 モデル評価


# 0 準備 ---------------------------------------------------------------

# ライブラリ
library(tidyverse)
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

# 交差項の検証
# --- shock_y2はshockとy2の交差項（積により算出されている）
df %>%
  select(shock, y2, shock_y2) %>%
  mutate(shock_y2_calc = shock * y2,
         diff = shock_y2 - shock_y2_calc)


# 2 固定効果モデルの構築 --------------------------------------------------------

# ＜ポイント＞
# - 固定効果モデル(within)を用いた回帰
# - インデックスにはidとtを設定


# モデル構築
preg1 <- plm(life ~ shock + y2 + shock_y2 + income, data = df, effect ="individual",
             model = "within", index = c("id", "t"))

# 結果確認
preg1 %>% print()


# 3 モデル評価 -------------------------------------------------------------

# ＜ポイント＞
# - ｢shock｣という変数は時間を通じて変化しないため、個別効果とともに取り除かれた
# - shock_y2はマイナス0.14で統計的に有意（例2のshockと同じ値）

# モデルサマリー
list(preg1 = preg1) %>% modelsummary(statistic = "({statistic}){stars}")

# n人分の切片平均値出力
preg1 %>% fixef() %>% mean()
