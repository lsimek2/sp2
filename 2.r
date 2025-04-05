library('MFDFA')
library('data.table')
library('dplyr')

df <- read.csv('train_logs.csv')
df_copy <- copy(df) # ako treba df vratiti u pocetno stanje
# promijeniti? - id, activiy, down_event, up_event u factor?
df_scores <- read.csv('train_scores.csv')
n <- dim(df_scores)[1]  # broj sudionika

# samo prvih 100 za testove
prvih100 <- df_scores$id[1:100]
df_skraceni <- df %>% filter(id %in% prvih100)
df_scores_skraceni <- df_scores %>% slice(1:100)

df <- df_skraceni  ##
df_scores <- df_scores_skraceni ##

# neki jednostavni featuresi
df <- (
  df %>% group_by(id)
    %>% mutate(total_words = last(word_count), wordspct = word_count*100/total_words)
)

# summarisano dodati na df_scores
# ovdje npr total_time aproksimira dio vremena od formlanih 30min doista potrosen na pisanje
df_dod <- (
  df %>% filter(wordspct >= 5 & wordspct < 95)
    %>% summarise(total_time = last(down_time) - first(down_time), total_words=last(total_words))
)
df_scores <- inner_join(df_scores, df_dod, by='id')


df <- df %>% mutate(iki=c(-1, diff(down_time))) %>% filter(event_id > 1)
df <- df %>% ungroup()

v_H <- c()
v_tau <- c()
v_r2 <- c()
s = c(10, 12, 15, 23, 34, 56, 80, 100, 140, 200, 250)
for (curr_id in df_scores$id[1:20]) {
  print(curr_id)
  mfdfa = MFDFA(
    tsx=(df %>% filter(id == curr_id))$iki,
    s=s,
    q=2
  )
  
  v_H <- c(v_H, mfdfa$Hq)
  v_tau <- c(v_tau, mfdfa$tau_q)
  v_r2 <- c(v_r2, cor(log(s), mfdfa$line[,1])^2)

}

# ovo ispada beskorisno
# df_scores$iki_t_coef = v

# total_time ispada beskoristan
summary(lm(score ~ total_time + total_words + total_words * total_time, data=df_scores))

df_scores$H = v_H
df_scores$tau = v_tau
df_scores$r2 = v_r2

summary(lm(score ~ total_words + H, data=df_scores))
summary(lm(score ~ total_words + H + tau + r2, data=df_scores))