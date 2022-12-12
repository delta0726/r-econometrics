# ***********************************************************************************************
# Title   : 計量経済学
# Chapter : 7 操作変数法
# Theme   : 実証例7.4 操作変数の強さ
# Date    : 2022/12/13
# Page    : P298 - P299
# URL     : https://ritsu1997.github.io/r-for-nlas-econometrics/index.html
# ***********************************************************************************************


# ＜目次＞
# 0 準備
# 1 モデルデータの準備
# 2 操作変数法によるモデルの構築
# 3 操作変数の強さ


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
df_model <-
  df %>%
    select(f_prot, kmwittenberg, f_young, f_jew, f_fem, f_ortsgeb, f_pruss,
           hhsize, lnpop, gpop, f_blind, f_deaf, f_dumb, f_miss)

# データ確認
df_model %>% print()
df_model %>% glimpse()


# 2 操作変数法によるモデルの構築------------------------------------------------

fs <- lm_robust(f_prot ~ kmwittenberg + f_young + f_jew + f_fem +
                f_ortsgeb + f_pruss + hhsize + lnpop + gpop +
                f_blind + f_deaf + f_dumb + f_miss,
                data = df_model, se_type = "stata")


# 3 操作変数の強さ ----------------------------------------------------------

# t値の算出
beta1_hat <- fs$coefficients[2] %>% round( 3)
se <- fs$std.error[2] %>% round( 3)
t_value <- beta1_hat / se
t_value

# F値の算出
f_value <- t_value ^ 2
f_value