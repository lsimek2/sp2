library('dplyr')
library('MFDFA')

df <- read.csv('train_logs.csv')
df$id <- as.factor(df$id)
# promijeniti? - id, activiy, down_event, up_event u factor?
df_scores <- read.csv('train_scores.csv')
n = dim(df_scores)[1]  # broj sudionika

samo_prvi = df %>% filter(id == '0042269b')
iki = (
  samo_prvi 
  %>% mutate(iki=c(-1,diff(down_time)), t=1)
  %>% slice(2:(dim(samo_prvi)[1]))
  %>% select(id,iki, t)
  %>% mutate(t=cumsum(t))
)

plot(1:length(iki$iki), iki$iki, 'l')

# x = MFDFA(iki$iki, c(10, 20, 30, 40, 50, 100, 150, 200), q=2)
# ovisnost o odabiru scaleova

iki2 = (
  iki %>% mutate(iki_cumsum = cumsum(iki))
  %>% mutate(cumpercent = iki_cumsum * 100 / sum(iki))
)

plot(1:length(iki2$cumpercent), iki2$cumpercent, 'l')
iki22 = iki2 %>% filter(cumpercent >= 5 & cumpercent <= 95)
lines(iki22$t, iki22$cumpercent, col='red')
# jos gledati i -- koliko traje "interkvartil" u vremenu pisanja (mozda ga def. po #rijeci?)
# lijepo se vidi fraktalnost za id = 0042269b (ocjena 6)
# za 0093f095 (ocj 4.5) vidimo da se vecina toga dogodi na kraju
# za 026be946 (ocj. 5) vidimo "dva ritma" -- probati uvijek odvojiti
# 044c5c54 (ocj. 1) pokazuje veliku pauzu izmedju
# 098f7878 (ocj. 5) dosta monotono kroz cijelo vrijeme
# jos jedna mogucnost -- gledati kako cumsum(iki) ovisi o t -- ako je linearno odn. nije

plot(1:length(iki2$cumpercent), iki2$iki, 'l')

# na konkretnom pogledati kako procjena H varira ovisno o odabiru s
s = c(5, 10, 15, 20, 25, 30, 35)
x = MFDFA(iki$iki, s=s, q=2)
print(c(x$Hq, cor(s, x$Fq)^2))
s = c(10, 30, 60, 100, 150, 250)
x = MFDFA(iki$iki, s=s, q=2)
print(c(x$Hq, cor(s, x$Fq)^2))
s = c(50, 100, 250, 400)
x = MFDFA(iki$iki, s=s, q=2)
print(c(x$Hq, cor(s, x$Fq)^2))
s = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)
x = MFDFA(iki$iki, s=s, q=2)
print(c(x$Hq, cor(s, x$Fq)^2))

# funkcija za dobiti mfdfa (lower-case) 
# za dani id

# skratiti s? - konzistentno se vidi da se
# pravac pokvari za vece s

single <- function(curr_id) {
  samo_prvi = df %>% filter(id == curr_id)
  iki = (
    samo_prvi 
    %>% mutate(iki=c(-1,diff(down_time)), t=1)
    %>% slice(2:(dim(samo_prvi)[1]))
  )
  
  # print('pocinjem mfdfa')
  return (mfdfa(iki$iki))
}

ikiplot <- function(curr_id, pct=10) {
  samo_prvi = df %>% filter(id == curr_id)
  iki = (
    samo_prvi 
    %>% mutate(iki=c(-1,diff(down_time)), t=1)
    %>% slice(2:(dim(samo_prvi)[1]))
  )
  
  # print('pocinjem mfdfa')
  plot(cumsum(iki$iki), type='l')
  abline(v=quantile(samo_prvi$event_id, pct/100),col='red')
  abline(v=quantile(samo_prvi$event_id, 1-2*pct/100), col='red')
}