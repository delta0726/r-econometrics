# ***********************************************************************************************
# Title   : 計量経済学の第一歩
# Chapter : 7 重回帰分析の応用
# Theme   : チョウ検定
# Date    : 2022/01/25
# Page    : P171 - P174
# URL     : http://www.yuhikaku.co.jp/static/studia_ws/index.html#isbn_9784641150287
# ***********************************************************************************************


# ＜概要＞
# - グループ間で重回帰モデルの回帰パラメータが異なるかを調べたい場合はチョウ検定を使う
#   --- 説明変数が多くて、ダミー変数との交差項を全て考慮すると推定するパラメータが多くなる場合に便利


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 パターン別にモデル構築
# 3 チョウ検定統計量の計算
# 4 ANOVA分析によるチョウ検定


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(broom)
library(estimatr)


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


# 2 パターン別にモデル構築 ------------------------------------------------

# モデル構築
# --- 男性のみのデータを使って単回帰で推定
# --- 女性のみのデータを使って単回帰で推定
# --- すべてのデータを使って単回帰で推定
reg1 <- lm(lincome ~ yeduc, data = subset(df, female == 0))
reg2 <- lm(lincome ~ yeduc, data = subset(df, female == 1))
reg3 <- lm(lincome ~ yeduc, data = df)

# サマリー出力
list(reg1 = reg1, reg2 = reg2, reg3 = reg3) %>%
  modelsummary(statistic = "({statistic}){stars}")


# 3 チョウ検定統計量の計算 -------------------------------------------------

# 残差2乗和を計算
SSR1 <- reg1 %>% deviance()
SSR2 <- reg2 %>% deviance()
SSR <- reg3 %>% deviance()

# ランク数
k <- reg3$rank

# チョウ検定統計量の計算
# --- F統計量
Fstat <- (SSR - (SSR1 + SSR2)) / (SSR1 + SSR2) * (nrow(df) - 2 * k) / k
Fstat

# p値
1 - pf(Fstat, k, (nrow(df) - 2 * k))


# 4 ANOVA分析によるチョウ検定 ---------------------------------------------

# モデル構築
reg4 <- lm(lincome ~ yeduc + female + female_yeduc, data = df)

# 分散分析
result <- anova(reg3, reg4)

# 結果確認
result %>% tidy()
