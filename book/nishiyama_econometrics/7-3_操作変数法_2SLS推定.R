# ***********************************************************************************************
# Title   : 計量経済学
# Chapter : 7 操作変数法
# Theme   : 実証例7.3 2段階最小二乗法
# Date    : 2022/00/00
# Page    : P295 - P293
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
df_model <-
  df %>%
    select(f_rw, f_prot, kmwittenberg, f_young, f_jew, f_fem, f_ortsgeb, f_pruss,
           hhsize, lnpop, gpop, f_blind, f_deaf, f_dumb, f_miss)

# データ確認
df_model %>% print()
df_model %>% glimpse()


# 2 操作変数法によるモデルの構築------------------------------------------------

# モデル構築
iv2 <- iv_robust(f_rw ~  f_prot + f_young + f_jew + f_fem + f_ortsgeb +
                 f_pruss + hhsize + lnpop + gpop + f_blind + f_deaf + f_dumb + f_miss |

                 kmwittenberg + f_young + f_jew + f_fem + f_ortsgeb +
                 f_pruss + hhsize + lnpop + gpop + f_blind + f_deaf + f_dumb + f_miss,
                 data = df_model, se_type = "stata")

# 結果確認
iv2 %>% print()
iv2 %>% summary()

# 標準誤差
iv2 %>% tidy()
