# ***********************************************************************************************
# Title   : 計量経済学
# Chapter : 7 操作変数法
# Theme   : 実証例7.2 操作変数推定量の標準誤差
# Date    : 2022/12/13
# Page    : P280
# URL     : https://ritsu1997.github.io/r-for-nlas-econometrics/index.html
# ***********************************************************************************************


# ＜目次＞
# 0 準備
# 1 モデルデータの準備
# 2 操作変数法によるモデルの構築


# 0 準備 ---------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(estimatr)
library(modelsummary)
library(gridExtra)


# データロード
df <- read_csv("csv/ipehd_qje2009_master.csv")

# データ確認
df %>% print()
df %>% glimpse()


# 1 モデルデータの準備 ------------------------------------------------------

# モデル用のデータ
df_model <- df %>% select(f_rw, f_prot, kmwittenberg)

# データ確認
df_model %>% print()
df_model %>% glimpse()


# 2 操作変数法によるモデルの構築------------------------------------------------

# モデル構築
iv1 <- iv_robust(f_rw ~ f_prot | kmwittenberg, data = df_model, se_type = "stata")

# 結果確認
iv1 %>% print()
iv1 %>% summary()

# 標準誤差
iv1 %>% tidy()
