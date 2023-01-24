# ***********************************************************************************************
# Title   : 計量経済学の第一歩
# Chapter : 7 重回帰分析の応用
# Theme   : ダミー変数を活用した推定
# Date    : 2022/12/05
# Page    : P166 - P171
# URL     : http://www.yuhikaku.co.jp/static/studia_ws/index.html#isbn_9784641150287
# ***********************************************************************************************


# ＜概要＞
# - ダミー変数をモデルに追加することで、グループによる効果の違いを測定することができる
#   --- 政策効果がグループ間で異なるかどうかを調べる際に使う（グループごとに切片がシフトする）


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 ダミー変数と交差項を含んだモデル構築


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(broom)
library(psych)
library(modelsummary)


# データロード
df <- read_csv("csv/7_1_income.csv")


# 1 データ確認 --------------------------------------------------------------

# ＜データ内容＞
# female       ：女性ダミー
# yeduc        ：就学年数
# lincome      ：収入
# female_yeduc ：女性ダミー * 就学年数

# データ確認
df %>% print()
df %>% glimpse()

# female_yeduc
df %>% filter(female == 1) %>% select(female, yeduc, female_yeduc)


# 2 ダミー変数と交差項を含んだモデル構築 ----------------------------------------

# ＜命題＞
# - 就学年数よるの収益率の男女差を推定
#   --- 女性ダミー(female)で男女格差を表現
#   --- 交差項(female_yeduc)で就学年数における収入格差を確認

# ＜ポイント＞
# - 女性ダミー(female)は有意にマイナス（女性は男性に比べて有意に年収が低い）
# - 交差項(female_yeduc)は+0.09と統計的に有意（女性の就学年数あたりの収益率は男性よりも高い）
#   --- 女性の教育収益率は11.4%(+0.024+0.09)


# データ確認
df %>%
  select(lincome, yeduc, female, female_yeduc) %>%
  pairs.panels()

# モデル構築
reg1 <- lm(lincome ~ yeduc + female + female_yeduc, data = df)

# 結果確認
reg1 %>% glance()
reg1 %>% tidy()

# サマリー出力
list(reg1 = reg1) %>% modelsummary(statistic = "({statistic}){stars}")
