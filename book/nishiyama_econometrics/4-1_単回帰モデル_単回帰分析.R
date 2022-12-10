# ***********************************************************************************************
# Title   : 計量経済学
# Chapter : 4 線形単回帰モデルの推定と検定
# Theme   : 実証例4.1 労働生産性と実質賃金の関係
# Date    : 2022/12/10
# Page    : P128 - P129
# URL     : https://ritsu1997.github.io/r-for-nlas-econometrics/index.html
# ***********************************************************************************************


# ＜概要＞
# - 計量経済学で用いられる分散不均一にロバスト標準誤差を用いた回帰を確認する
# - 回帰分析におけるt検定の意味を確認する


# ＜目次＞
# 0 準備
# 1 lm()による線形回帰
# 2 lm.robust()による線形回帰
# 3 回帰係数のt検定


# 0 準備 ---------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(broom)
library(estimatr)
library(modelsummary)


# データロード
df <- read_csv("csv/ch04_wage.csv")

# データ確認
df %>% print()
df %>% glimpse()


# 1 lm()による線形回帰 -------------------------------------------------

# ＜ポイント＞
# - ベーシックな線形回帰の場合、回帰係数は一致するが標準誤差やt値は一致しない
#   --- 書籍は不均一分散にロバスト標準誤差を使っている


# 線形回帰
result <- lm(formula = wage ~ productivity, data = df)

# 結果確認
result %>% glance()
result %>% tidy()


# 2 lm.robust()による線形回帰 -------------------------------------------

# ＜ポイント＞
# - 書籍に合わせて分散不均一に対してロバストな標準誤差を用いて線形回帰を行う
#   --- estimater::lm_robust()ではロバスト標準誤差のタイプを選択することができる


# 線形回帰
result_rob <- lm_robust(formula = wage ~ productivity, data = df, se_type = "stata")

# 結果確認
result_rob %>% glance()
result_rob %>% tidy()

# 結果比較
list(result = result, result_rob = result_rob) %>%
  modelsummary(statistic = "({statistic}){stars}")


# 3 回帰係数のt検定 -----------------------------------------------------

# ＜ポイント＞
# - 回帰分析のデフォルトのt検定は対象となる項が必要かどうかを評価している
#   --- 帰無仮説でβ=0 / 対立仮説β≒0を設定して、帰無仮説を棄却する
#   --- β=0と統計的には言えない ⇒ 回帰係数は存在する意義がある

# ＜解釈＞
# - β=0のt値は+26.7、β=1のt値は-22.1となっている
# - βの信頼区間が0.507-0.587なので、β=0とβ=1でt値の符号が変わる
#   --- (beta - beta・null) / se


# 回帰係数の確認
# --- β1 = 0.5468196
result_rob %>% tidy()

# デフォルトのt検定
# --- β=0を帰無仮説として検定（対象の回帰係数の必要性を評価）
result_rob %>%
  tidy() %>%
  filter(term == "productivity") %>%
  select(term, estimate, std.error, statistic, conf.low, conf.high)

# 帰無仮説を変更してt検定
# --- β=1を帰無仮説として検定
beta1 <- result_rob$coefficients["productivity"]
se <- result_rob$std.error["productivity"]
tstat <- (beta1 - 1) / se
tstat

# 信頼区間の算出
upper <- beta1 + 1.96 * se
lower <- beta1 - 1.96 * se
tibble(lower = lower, upper = upper) %>% print()
