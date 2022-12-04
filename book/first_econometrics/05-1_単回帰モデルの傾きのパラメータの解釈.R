# ***********************************************************************************************
# Title   : 計量経済学の第一歩
# Chapter : 5 単回帰分析
# Theme   : 例5.1 単回帰モデルの傾きのパラメータの解釈
# Date    : 2022/12/04
# Page    : P110 - P116
# URL     : http://www.yuhikaku.co.jp/static/studia_ws/index.html#isbn_9784641150287
# ***********************************************************************************************


# ＜概要＞
# - 線形回帰モデルはのβ1は政策変数(X)と成果変数(Y)の間の統計的な関係を示している
# - β1の推定値の解釈は説明変数と被説明変数のそれぞれの単位によって決まる
#   --- 単位の変更により、解釈が煩雑になることがある（万円⇒千円など）
#   --- 説明変数や被説明変数を対数変換することで｢％変化｣の解釈に変換することができる


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 モデル1（income = a + b yeduc + u）
# 3 モデル2（lincome = a + b yeduc + u）
# 4 モデル3（income = a + b lyeduc + u）
# 5 モデル4（lincome = a + b lyeduc + u）

# 0 準備 ------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(psych)
library(modelsummary)


# データロード
df <- read_csv("csv/5_1_income.csv")


# 1 データ確認 ----------------------------------------------------------

# ＜データ概要＞
# - income ：年収(万円)
# - yeduc  ：就学年数
# - lincome：log(年収(万円))
# - lyeduc ：log(就学年数)


# データ確認
df %>% head()
df %>% glimpse()

# 対数値の確認
df %>%
  mutate(log_income = log(income),
         log_yeduc = log(yeduc)) %>%
  head()


# 2 モデル1（income = a + b yeduc + u） ----------------------------------

# ＜ポイント＞
# - 就学年数が1年伸びると、年収が23.1万円増加する
#   --- 元データは正規分布とは違って偏りが発生している


# データ確認
df %>% select(income, yeduc) %>% pairs.panels()

# モデル構築
reg1 <- lm(income ~ yeduc, data = df)

# 結果確認
list(reg1 = reg1) %>% modelsummary(statistic = "({statistic}){stars}")


# 3 モデル2（lincome = a + b yeduc + u） ---------------------------------

# ＜ポイント＞
# - 就学年数が1年伸びると、年収が6.5%増加する


# データ確認
df %>% select(lincome, yeduc) %>% pairs.panels()

# モデル構築
reg2 <- lm(lincome ~ yeduc, data = df)

# 結果確認
list(reg2 = reg2) %>% modelsummary(statistic = "({statistic}){stars}")


# 4 モデル3（income = a + b lyeduc + u） ---------------------------------

# ＜ポイント＞
# - 就学年数が1%(0.01)伸びると、年収が2.97万円(2.97 * 100)増加する


# データ確認
df %>% select(income, lyeduc) %>% pairs.panels()

# モデル構築
reg3 <- lm(income ~ lyeduc, data = df)

# 結果確認
list(reg2 = reg2) %>% modelsummary(statistic = "({statistic}){stars}")


# 5 モデル4（lincome = a + b lyeduc + u） ---------------------------------

# ＜ポイント＞
# - 就学年数が1%(0.01)伸びると、年収が2.97万円(2.97 * 100)増加する


# データ確認
df %>% select(lincome, lyeduc) %>% pairs.panels()

# モデル構築
reg4<-lm(lincome ~ lyeduc, data = df)

# 結果確認
list(reg2 = reg2) %>% modelsummary(statistic = "({statistic}){stars}")
