#***************************************************************************************
# Title     : 効果検証入門
# Chapter   : 2章 介入効果を測るための回帰分析
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/13
# URL       :
#***************************************************************************************

# ＜目次＞
# 2.1 回帰分析の導入
# 2.1.5 メールマーケティングデータの分析(回帰分析)

# 2.2 回帰分析におけるバイアス
# 2.3 回帰分析を利用した探索的な効果検証
# 2.4 回帰分析に関する様々な議論



# 2.1 回帰分析の導入 ------------------------------------

# 2.1.5 メールマーケティングデータの分析(回帰分析) ------------------------------------

# 2.1.5.1 準備 -------------------------------

library("tidyverse")
library("broom")


# データ準備
# --- ECサイトのユーザーに対してRCTを適用したメールマーケティングを行ったデータ
email_data <- read_csv("csv/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge.csv")


# データ概要
email_data %>% as_tibble()
email_data %>% glimpse()


# マーケティング選択肢の確認
email_data$segment %>% unique()


# データの準備
# --- 女性向けメールが配信されたデータを削除したデータを作成
# --- 問題の単純化するため
# --- treatment: 男性でEmailを送った人
male_df <-
  email_data %>%
    filter(segment != "Womens E-Mail") %>%
    mutate(treatment = ifelse(segment == "Mens E-Mail", 1, 0))


# 確認
male_df %>% as_tibble()
male_df %>% glimpse()


# seedを固定する
set.seed(1)


# 閾値の設定
# --- 条件に反応するサンプルの量を半分にする
obs_rate_c <- 0.5
obs_rate_t <- 0.5


## バイアスのあるデータの作成
# --- 購入しそうな顧客をフィルタリング
# --- history: 昨年の購入額（多い）
# --- recency: 経過月数（短い）
# --- channel: 購入チャネル（マルチチャネル）
# --- treatment: 1(配信した) / 1(配信しなかった)
biased_data <-
  male_df %>%
    mutate(obs_rate_c = ifelse((history > 300) | (recency < 6) | (channel == "Multichannel"), obs_rate_c, 1),
           obs_rate_t = ifelse((history > 300) | (recency < 6) | (channel == "Multichannel"), 1, obs_rate_t),
           random_number = runif(n = NROW(male_df))) %>%
    filter((treatment == 0 & random_number < obs_rate_c) |
             (treatment == 1 & random_number < obs_rate_t) )



# 2.1.5.2 回帰分析の実行 -------------------------------

# バイアスのあるデータでの回帰分析
# --- 目的変数： spend:(購入額)
# --- 介入変数： treatment: 1(配信した) / 0(配信しなかった)
# --- 共変量  ： history: 昨年の購入額
biased_reg <- lm(data = biased_data, formula = spend ~ treatment + history)
biased_reg %>% print()


# 分析結果のレポート
biased_reg %>% summary()


# 回帰パラメーターをtibbleで抽出
# --- treatmentは0.903
# --- 本書では回帰係数以外はほとんど見ない
biased_reg_coef <- biased_reg %>% tidy()
biased_reg_coef %>% print()




# 2.2 回帰分析におけるバイアス --------------------------------



# (7) RCTデータでの回帰分析とバイアスのあるデータでの回帰分析の比較
## RCTデータでの単回帰
rct_reg_coef <- lm(spend ~ treatment, male_df) %>% tidy()
rct_reg_coef %>% print()


## バイアスのあるデータでの単回帰
nonrct_reg_coef <- lm(spend ~ treatment, biased_data) %>% tidy()
nonrct_reg_coef %>% print()



## バイアスのあるデータでの重回帰
nonrct_mreg_coef <- lm(spend ~ treatment + recency + channel + history, biased_data) %>% tidy()
nonrct_mreg_coef %>% print()


# (8) OVBの確認
## (a) history抜きの回帰分析とパラメーターの取り出し
short_coef <-
  biased_data %>%
    lm(spend ~ treatment + recency + channel, .) %>%
    tidy()

short_coef %>% print()


## aの結果から介入効果に関するパラメーターのみを取り出す
alpha_1 <-
  short_coef %>%
    filter(term == "treatment") %>%
    pull(estimate)

## (b) historyを追加した回帰分析とパラメーターの取り出し
long_coef <-
  biased_data %>%
    lm(spend ~ treatment + recency + channel + history, .) %>%
    tidy()

long_coef %>% print()


## bの結果から介入とhistoryに関するパラメーターを取り出す
beta_1 <- long_coef %>% filter(term == "treatment") %>% pull(estimate)
beta_2 <- long_coef %>% filter(term == "history") %>% pull(estimate)


## (c) 脱落した変数と介入変数での回帰分析
omitted_coef <-
  biased_data %>%
    lm(data = ., formula = history ~ treatment + channel + recency) %>%
    tidy()


## cの結果から介入変数に関するパラメーターを取り出す
gamma_1 <- omitted_coef %>% filter(term == "treatment") %>% pull(estimate)

## OVBの確認
beta_2*gamma_1
alpha_1 - beta_1


## モデル式のベクトルを用意
formula_vec <-
  c(spend ~ treatment + recency + channel,
    spend ~ treatment + recency + channel + history,
    history ~ treatment + channel + recency) %>%
    set_names(paste("reg", LETTERS[1:3], sep ="_"))


## モデル式のデータフレーム化
models <-
  formula_vec %>%
    enframe(name = "model_index", value = "formula")


## まとめて回帰分析を実行
df_models <-
  models %>%
    mutate(model = map(.x = formula, .f = lm, data = biased_data)) %>%
    mutate(lm_result = map(.x = model, .f = tidy))


## モデルの結果を整形
df_results <-
  df_models %>%
    mutate(formula = as.character(formula)) %>%
    select(formula, model_index, lm_result) %>%
    unnest(cols = lm_result)

## モデルA,B,Cでのtreatmentのパラメータを抜き出す
treatment_coef <-
  df_results %>%
    filter(term == "treatment") %>%
    pull(estimate)


## モデルBからhistoryのパラメータを抜き出す
history_coef <-
  df_results %>%
    filter(model_index == "reg_B", term == "history") %>%
    pull(estimate)


## OVBの確認
OVB <- history_coef*treatment_coef[3]
coef_gap <- treatment_coef[1] - treatment_coef[2]
OVB # beta_2*gamma_1
coef_gap # alpha_1 - beta_1


# (10) 入れてはいけない変数を入れてみる
#visitとtreatmentとの相関
cor_visit_treatment <-
   lm(treatment ~ visit + channel + recency + history, biased_data) %>%
   tidy()

# visitを入れた回帰分析を実行
bad_control_reg <-
   lm(spend ~ treatment + channel + recency + history + visit, biased_data) %>%
   tidy()
