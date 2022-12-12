# ***********************************************************************************************
# Title   : 計量経済学
# Chapter : 6 パネルデータ分析
# Theme   : 実証例6.2 保育所が母親の就業に与える影響の固定効果推定の標準誤差
# Date    : 2022/12/13
# Page    : P231
# URL     : https://ritsu1997.github.io/r-for-nlas-econometrics/index.html
# ***********************************************************************************************


# ＜目次＞
# 0 準備
# 1 固定効果の推定
# 2 推定結果の確認


# 0 準備 ---------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(estimatr)
library(modelsummary)
library(gridExtra)


# データロード
df <- read_csv("csv/yamaguchi_2.csv")

# データ準備
df <- df %>% filter(year > 1995 & hh.type == "all")

# データ確認
df %>% print()
df %>% glimpse()


# 1 固定効果の推定 ---------------------------------------------------------------


# データ確認
df_model <- df %>% select(pref, emp.rate, cap.rate)

# モデル構築
model1 <- lm_robust(emp.rate ~ cap.rate, data = df_model, clusters = pref,
                    fixed_effects = ~ pref, se_type = "stata")


# 2 推定結果の確認 ---------------------------------------------------------------

# 結果確認
model1 %>% print()
model1 %>% summary()

# パラメータ取得
# --- 回帰係数
# --- 標準誤差
model1$std.error
