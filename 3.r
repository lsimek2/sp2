# ovdje cemo pokusati s drugim paketom wol-fi/mutltifractal
# sada pomocu lower-case mfdfa 
# radimo "ozbiljno"
# ovo je bez varijacija, basic

# ideje: promjena s, izgladjivanje, maknuti kad iki < 2, gledati samo dio, promjena parametra m

library('dplyr')
library('MFDFA')
library('multifractal')  # alternativa za MFDFA, komanda je lower-case mfdfa
library('caret')

df <- read.csv('train_logs.csv')
df$id <- as.factor(df$id)
# promijeniti? - id, activity, down_event, up_event u factor?
df_scores <- read.csv('train_scores.csv')
n <- dim(df_scores)[1]  # broj sudionika

iki <- df %>% mutate(iki=c(-1, diff(down_time))) %>% select(id, iki) %>% slice(-1) 

qvec <- -5:5
feature_names <- c() # za sve q dodajemo Hq, R2q, tauq, alfaq, jos Hmax, Hmin, alphamax, alphamin

for (q in qvec) {
  feature_names <- c(feature_names,
                    paste('H', q, sep='_'),
                    paste('tau', q, sep='_'),
                    paste('alpha', q, sep='_'),
                    paste('R2', q, sep='_'),
                    paste('f_alpha', q, sep='_')
                  )
}

feature_names <- c(feature_names, 'Hmax', 'Hmin', 'alphamax', 'alphamin')

i=1
for (curr_id in df_scores$id) {
  print(paste(i, '/', n, ' id=', curr_id, sep=''))
  mfwrapper <- mfdfa(
    x = (iki %>% filter(id == curr_id))$iki,
    scale=NA,
    q=qvec,
    m=1,
    overlap=FALSE
  )
  
  feature_vals <- c()
  
  # j = 1
  for (j in 1:length(qvec)) {
    feature_vals <- c(feature_vals,
                     mfwrapper$Hq[j],
                     mfwrapper$tq[j],
                     mfwrapper$alpha[j],
                     mfwrapper$R2[j],
                     mfwrapper$f_alpha[j]
                    )
    # j = j + 1
  }
  
  feature_vals <- c(feature_vals, max(mfwrapper$Hq), min(mfwrapper$Hq),
                    max(mfwrapper$alpha), min(mfwrapper$alpha))
  
  df_scores[i, feature_names] <- feature_vals
  
  i <- i + 1
}

df_scores <- df_scores %>% select(-c('alpha_5', 'f_alpha_5'))

filename <-  gsub("[: ]", "-", date())
filename <- paste('features', filename, '.csv', sep='')
write.csv(df_scores, filename )

# summary(lm(score~., data=df_scores %>% select(-id)))
# r2 0.2157, adj. 0.1977 za puni model
# sada s validacijom
train_idx <- sample(n, n*0.7, replace=F)
train_scores <- df_scores[train_idx,]
valid_scores <- df_scores[-train_idx,]

model <- lm(score~., data=train_scores %>% select(-id))

pred_train <- predict(model, newdata=train_scores)
pred_valid <- predict(model, newdata=valid_scores)

postResample(pred=pred_train, obs=train_scores$score)
postResample(pred=pred_valid, obs=valid_scores$score)

# ispitati interakcije
df_scores_wint <- df_scores
for (i in 3:ncol(df_scores)) {
  for (j in 3:ncol(df_scores)) {
    if (i != j) {
      name = paste(colnames(df_scores)[i], colnames(df_scores)[j], sep='X')
      df_scores_wint[, name] <- df_scores[, i] * df_scores[, j]
    }
  }  
}

# summary(lm(score~., data=df_scores_wint %>% select(-id)))
# sada s validacijom
train_idx <- sample(n, n*0.7, replace=F)
train_scores <- df_scores_wint[train_idx,]
valid_scores <- df_scores_wint[-train_idx,]

model <- lm(score~., data=train_scores %>% select(-id))

pred_train <- predict(model, newdata=train_scores)
pred_valid <- predict(model, newdata=valid_scores)

postResample(pred=pred_train, obs=train_scores$score)
postResample(pred=pred_valid, obs=valid_scores$score)
# potpuno overfittano
# dosta NA-ova?


# mnlogit?
