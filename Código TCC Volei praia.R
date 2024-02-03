# PASSOS INICIAIS ----

## Chamando as bibliotecas necessárias ----
library(RCurl)
library(stringr)
library(tidyverse)
library(tidyr)
library(MASS)
library(ggplot2)
if(!require("TTR")) install.packages("TTR")
library("TTR")
library(glmnet)
library(caret)
library(glmtoolbox)
library(pscl)
library(corrplot)
library("PerformanceAnalytics")
library(tidymodels)
library(rpart.plot)
library(vip)
library(readxl)
library(gt)
library(hrbrthemes)
library(gridExtra)


## Puxando a base de dados do URL ----
x <- getURL("https://raw.githubusercontent.com/BigTimeStats/beach-volleyball/master/data/full_archive/full_archive.csv")


## Criando os objetos necessários ----
best_lambdas1 <- c()
coefs1 <- c()
best_lambdas2 <- c()
coefs2 <- c()
compl_nao_forward1 <- cbind(c(),c())
compl_nao_backward1 <- cbind(c(),c())
compl_nao_forward2 <- cbind(c(),c())
compl_nao_backward2 <- cbind(c(),c())
avgs_full_model <- c()
avgs_bck_model <- c()
sens_full_model_masc <- c()
sens_bck_model_masc <- c()
spec_full_model_masc <- c()
spec_bck_model_masc <- c()
aic_full_model <- c()
aic_bck_model <- c()
avgs_dentro_full_masc <- c()
avgs_dentro_bck_masc <- c()
r2_mcfad_full_model <- c()
r2_mcfad_bck_model <- c()

best_lambdas1_avp <- c()
coefs1_avp <- c()
best_lambdas2_avp <- c()
coefs2_avp <- c()
compl_nao_forward1_avp <- cbind(c(),c())
compl_nao_backward1_avp <- cbind(c(),c())
compl_nao_forward2_avp <- cbind(c(),c())
compl_nao_backward2_avp <- cbind(c(),c())
avgs_full_model_avp <- c()
avgs_bck_model_avp <- c()
aic_full_model_avp <- c()
aic_bck_model_avp <- c()
avgs_dentro_full_masc_avp <- c()
avgs_dentro_bck_masc_avp <- c()
r2_mcfad_full_model_avp <- c()
r2_mcfad_bck_model_avp <- c()

best_lambdas1_fivb <- c()
coefs1_fivb <- c()
best_lambdas2_fivb <- c()
coefs2_fivb <- c()
compl_nao_forward1_fivb <- cbind(c(),c())
compl_nao_backward1_fivb <- cbind(c(),c())
compl_nao_forward2_fivb <- cbind(c(),c())
compl_nao_backward2_fivb <- cbind(c(),c())
avgs_full_model_fivb <- c()
avgs_bck_model_fivb <- c()
aic_full_model_fivb <- c()
aic_bck_model_fivb <- c()
avgs_dentro_full_masc_fivb <- c()
avgs_dentro_bck_masc_fivb <- c()
r2_mcfad_full_model_fivb <- c()
r2_mcfad_bck_model_fivb <- c()

best_lambdas1_fem <- c()
coefs1_fem <- c()
best_lambdas2_fem <- c()
coefs2_fem <- c()
compl_nao_forward1_fem <- cbind(c(),c())
compl_nao_backward1_fem <- cbind(c(),c())
compl_nao_forward2_fem <- cbind(c(),c())
compl_nao_backward2_fem <- cbind(c(),c())
avgs_full_model_fem <- c()
avgs_bck_model_fem <- c()
sens_full_model_fem <- c()
sens_bck_model_fem <- c()
spec_full_model_fem <- c()
spec_bck_model_fem <- c()
aic_full_model_fem <- c()
aic_bck_model_fem <- c()
avgs_dentro_full_fem <- c()
avgs_dentro_bck_fem <- c()
r2_mcfad_full_model_fem <- c()
r2_mcfad_bck_model_fem <- c()

best_lambdas1_avp_fem <- c()
coefs1_avp_fem <- c()
best_lambdas2_avp_fem <- c()
coefs2_avp_fem <- c()
compl_nao_forward1_avp_fem <- cbind(c(),c())
compl_nao_backward1_avp_fem <- cbind(c(),c())
compl_nao_forward2_avp_fem <- cbind(c(),c())
compl_nao_backward2_avp_fem <- cbind(c(),c())
avgs_full_model_avp_fem <- c()
avgs_bck_model_avp_fem <- c()
aic_full_model_avp_fem <- c()
aic_bck_model_avp_fem <- c()
avgs_dentro_full_fem_avp <- c()
avgs_dentro_bck_fem_avp <- c()
r2_mcfad_full_model_avp_fem <- c()
r2_mcfad_bck_model_avp_fem <- c()

best_lambdas1_fivb_fem <- c()
coefs1_fivb_fem <- c()
best_lambdas2_fivb_fem <- c()
coefs2_fivb_fem <- c()
compl_nao_forward1_fivb_fem <- cbind(c(),c())
compl_nao_backward1_fivb_fem <- cbind(c(),c())
compl_nao_forward2_fivb_fem <- cbind(c(),c())
compl_nao_backward2_fivb_fem <- cbind(c(),c())
avgs_full_model_fivb_fem <- c()
avgs_bck_model_fivb_fem <- c()
aic_full_model_fivb_fem <- c()
aic_bck_model_fivb_fem <- c()
avgs_dentro_full_fem_fivb <- c()
avgs_dentro_bck_fem_fivb <- c()
r2_mcfad_full_model_fivb_fem <- c()
r2_mcfad_bck_model_fivb_fem <- c()

percs_dentro_full_masc <- c()
percs_dentro_bck_masc <- c()
percs_dentro_full_masc_avp <- c()
percs_dentro_bck_masc_avp <- c()
percs_dentro_full_masc_fivb <- c()
percs_dentro_bck_masc_fivb <- c()

percs_dentro_full_fem <- c()
percs_dentro_bck_fem <- c()
percs_dentro_full_fem_avp <- c()
percs_dentro_bck_fem_avp <- c()
percs_dentro_full_fem_fivb <- c()
percs_dentro_bck_fem_fivb <- c()



## Chamando a base geral e a base para preencher alturas vazias nos dados

dados <- read.csv(text = x)
alturas <- read_excel("alturas vazias.xlsx", sheet = "Planilha1", col_names = TRUE)


# INICIANDO A LIMPEZA E AJUSTES NA BASE ----

## Convertendo as alturas para o encoding correto, depois convertendo o campo para numérico ----

alts <- as.data.frame(iconv(alturas$hgt, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
nomes_alts <- as.data.frame(iconv(alturas$Nome, from = 'UTF-8', to = 'ASCII//TRANSLIT'))

alturas <- cbind(nomes_alts, alts)
colnames(alturas) <- c('Nome', 'hgt')


for(i in 1:nrow(dados)){
  if(dados[i,"w_player1"] %in% alturas$Nome && is.na(dados[i,"w_p1_hgt"])){
    dados[i,"w_p1_hgt"] <- alturas[alturas$Nome == dados[i, "w_player1"], "hgt"]
  }
  if(dados[i,"w_player2"] %in% alturas$Nome && is.na(dados[i,"w_p2_hgt"])){
    dados[i,"w_p2_hgt"] <- alturas[alturas$Nome == dados[i, "w_player2"], "hgt"]
  }
  if(dados[i,"l_player1"] %in% alturas$Nome && is.na(dados[i,"l_p1_hgt"])){
    dados[i,"l_p1_hgt"] <- alturas[alturas$Nome == dados[i, "l_player1"], "hgt"]
  }
  if(dados[i,"l_player2"] %in% alturas$Nome && is.na(dados[i,"l_p2_hgt"])){
    dados[i,"l_p2_hgt"] <- alturas[alturas$Nome == dados[i, "l_player2"], "hgt"]
  }
}

dados$w_p1_hgt <- as.numeric(dados$w_p1_hgt)
dados$w_p2_hgt <- as.numeric(dados$w_p2_hgt)
dados$l_p1_hgt <- as.numeric(dados$l_p1_hgt)
dados$l_p2_hgt <- as.numeric(dados$l_p2_hgt)


## Convertendo os campos de data de nascimento para o formato de data ----

dados$date <- as.Date(dados$date)
dados$w_p1_birthdate <- as.Date(dados$w_p1_birthdate)
dados$w_p2_birthdate <- as.Date(dados$w_p2_birthdate)
dados$l_p1_birthdate <- as.Date(dados$l_p1_birthdate)
dados$l_p2_birthdate <- as.Date(dados$l_p2_birthdate)

for(i in 1:nrow(dados)){
  if(dados[i,"w_player1"] == 'Mike Whitmarsh'){
    dados[i,"w_p1_birthdate"] <- to_date('1962-05-18')
    dados[i,"w_p1_age"] <- (dados[i,"w_p1_birthdate"] - dados[i,"date"])/365.25
  }
  if(dados[i,"w_player2"] == 'Mike Whitmarsh'){
    dados[i,"w_p2_birthdate"] <- as.Date('1962-05-18')
    dados[i,"w_p2_age"] <- (dados[i,"w_p2_birthdate"] - dados[i,"date"])/365.25
  }
  if(dados[i,"l_player1"] == 'Mike Whitmarsh'){
    dados[i,"l_p1_birthdate"] <- as.Date('1962-05-18')
    dados[i,"l_p1_age"] <- (dados[i,"l_p1_birthdate"] - dados[i,"date"])/365.25
  }
  if(dados[i,"l_player2"] == 'Mike Whitmarsh'){
    dados[i,"l_p2_birthdate"] <- as.Date('1962-05-18')
    dados[i,"l_p2_age"] <- (dados[i,"l_p2_birthdate"] - dados[i,"date"])/365.25
  }
}


## Removendo torneios do tipo "rei da praia" (se um mesmo jogador tiver mais de uma dupla em um mesmo torneio, remover torneio) ----
# Isso é feito para que o "rank" se refira obrigatoriamete ao rank da dupla, nao o de apenas um jogador da dupla

dados$tourn_id <- paste(dados$circuit, dados$tournament, dados$country, dados$year, dados$gender, sep = ' - ')


dados <- dados[which(dados$bracket != "Qualifier Bracket"),]
dados_dupl_w <- dados[,c("tourn_id", "match_num", "date", "w_player1", "w_player2")]
colnames(dados_dupl_w) <-  c("tourn_id", "match_num", "date", "player1", "player2")
dados_dupl_l <- dados[,c("tourn_id", "match_num", "date", "l_player1", "l_player2")]
colnames(dados_dupl_l) <-  c("tourn_id", "match_num", "date", "player1", "player2")

dados_dupl <- rbind(dados_dupl_w, dados_dupl_l)

dados_dupl_grp <- as.data.frame(unique(dados_dupl[,c("tourn_id", "date", "player1", "player2")]))
colnames(dados_dupl_grp) <- c("tourn_id", "date", "player1", "player2")

dados_dupl_grp_p1 <- dados_dupl_grp %>% group_by(tourn_id, date, player1) %>% summarise(
  total  = n()
) %>% filter(total>=2)

dados_dupl_grp_p2 <- dados_dupl_grp %>% group_by(tourn_id, date, player2) %>% summarise(
  total  = n()
) %>% filter(total>=2)

dados_dupl_remov_torneio <- rbind(dados_dupl_grp_p1, dados_dupl_grp_p2)

dados_dupl_remov_torneio$chave_exc <- paste(dados_dupl_remov_torneio$tourn_id, dados_dupl_remov_torneio$date, sep = ' - ')


dados$tourn_id <- paste(dados$circuit, dados$tournament, dados$country, dados$year, dados$gender, dados$date, sep = ' - ')

dados <- dados %>% filter(!(tourn_id %in% dados_dupl_remov_torneio$chave_exc))

dados <- dados[,-ncol(dados)]


## Removendo scores vazios e jogos que não foram finalizados ----

dados <- dados[which(dados$score != 'Forfeit or other'),]
dados <- dados[!is.na(dados$score),]
dados <- dados[which(dados$score != ""),]
dados <- dados[which(!grepl("retired", dados$score)), ]


## Separando os placares em sets ----

dados[c("set_1", "set_2", "set_3")] <- str_split_fixed(dados$score, ", ", n=3)
dados$score_in_sets <- ifelse(dados$set_3 == "", "2-0", "2-1")
dados[which(dados$set_3 == ""), "set_3"] <- "0-0"
dados[which(dados$set_2 == ""), "set_2"] <- "0-0"
dados[c("w_set1_score", "l_set1_score")] <- as.numeric(str_split_fixed(dados$set_1, "-", n=2))
dados[c("w_set2_score", "l_set2_score")] <- as.numeric(str_split_fixed(dados$set_2, "-", n=2))
dados[c("w_set3_score", "l_set3_score")] <- as.numeric(str_split_fixed(dados$set_3, "-", n=2))
dados[c("w_score_in_sets", "l_score_in_sets")] <- as.numeric(str_split_fixed(dados$score_in_sets, "-", n=2))



## Removendo vazios das variáveis de interesse ----

dados_sem_na <- dados[!is.na(dados$w_p1_tot_attacks),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_p1_tot_aces),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_p1_tot_blocks),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_p1_tot_kills),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_p1_tot_errors),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_p1_tot_attacks),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_p1_tot_digs),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_p1_tot_hitpct),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_p1_tot_serve_errors),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_p1_age),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_p1_country),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_p1_hgt),]

dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_p2_tot_aces),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_p2_tot_blocks),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_p2_tot_kills),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_p2_tot_errors),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_p2_tot_attacks),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_p2_tot_digs),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_p2_tot_hitpct),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_p2_tot_serve_errors),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_p2_age),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_p2_country),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_p2_hgt),]

dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_p1_tot_aces),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_p1_tot_blocks),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_p1_tot_kills),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_p1_tot_errors),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_p1_tot_attacks),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_p1_tot_digs),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_p1_tot_hitpct),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_p1_tot_serve_errors),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_p1_age),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_p1_country),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_p1_hgt),]

dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_p2_tot_aces),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_p2_tot_blocks),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_p2_tot_kills),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_p2_tot_errors),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_p2_tot_attacks),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_p2_tot_digs),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_p2_tot_hitpct),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_p2_tot_serve_errors),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_p2_age),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_p2_country),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_p2_hgt),]


dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$w_rank),]
dados_sem_na <- dados_sem_na[which(dados_sem_na$w_rank != ''),]



## Ajustando os ranks para considerar somente a seed da dupla no torneio no momento do jogo, removendo "qualifiers" ----

for(i in 1:nrow(dados_sem_na)){
  dados_sem_na[i,"w_rank"] <- strsplit(dados_sem_na[i, "w_rank"], "[,]")[[1]][[1]]
  dados_sem_na[i,"l_rank"] <- strsplit(dados_sem_na[i, "l_rank"], "[,]")[[1]][[1]]
}


dados_sem_na <- dados_sem_na[which(!grepl('Q',dados_sem_na$w_rank)),]
dados_sem_na <- dados_sem_na[!is.na(dados_sem_na$l_rank),]
dados_sem_na <- dados_sem_na[which(dados_sem_na$l_rank != ''),]
dados_sem_na <- dados_sem_na[which(!grepl('Q',dados_sem_na$l_rank)),]


## Criando variável do número de sets e pontos jogados e id da partida ---

dados_sem_na$sets_played <- dados_sem_na$l_score_in_sets + dados_sem_na$w_score_in_sets

dados_sem_na$points_played <- dados_sem_na$w_set1_score + dados_sem_na$w_set2_score + dados_sem_na$w_set3_score + dados_sem_na$l_set1_score + dados_sem_na$l_set2_score + dados_sem_na$l_set3_score

dados_sem_na$match_id <- paste(dados_sem_na$circuit, dados_sem_na$tournament, dados_sem_na$country, dados_sem_na$year,
                               dados_sem_na$gender, dados_sem_na$match_num, sep = ' - ')



## Definindo qual será a dupla de referência para as previsões a partir do row_number do dataframe ordenado por circuito, torneio, país, ano, data, gênero e núm. da partida ----

dados_sem_na <- dados_sem_na %>% arrange(circuit, tournament, country, year, date, gender, match_num)

dados_sem_na$dupla_de_referencia <- ifelse(row_number(dados_sem_na)%%2 ==0, 'W', 'L')

dados_sem_na$l_rank <- as.integer(dados_sem_na$l_rank)
dados_sem_na$w_rank <- as.integer(dados_sem_na$w_rank)



## Seperando os dados por circuito ----

dados_fivb_sem_na <- dados_sem_na[which(dados_sem_na$circuit == 'FIVB'),]
dados_avp_sem_na <- dados_sem_na[which(dados_sem_na$circuit == 'AVP'),]

## Somente AVP - TALVEZ REMOVER DEPOIS ----
dados_sem_na <- dados_avp_sem_na


## Cuidando dos jogos perdidos ----
dados_todos <- dados
dados <- dados[which(dados$circuit == 'AVP'),]
dados$tourn_id <- paste(dados$circuit, dados$tournament, dados$country, dados$year,  dados$gender, sep = ' - ') 

# Remove-se os "Qualifier Bracket" para se evitar problemas com as variáveis de rank
dados <- dados[which(dados$bracket != "Qualifier Bracket"),]

# A lógica é fazer um df com todos os jogadores, ordená-lo e ver quantas partidas foram perdidas entre um jogo e outro devido à remoção das variáveis com vazios



# Começando com a base geral, pré-remoção da NA's

dados1_w1 <- dados[,c("tourn_id", "match_num", "date", "w_player1", "w_p1_hgt", "w_p1_age")]
colnames(dados1_w1) <- c("tourn_id", "match_num", "date", "player", "hgt","age")
dados1_w2 <- dados[,c("tourn_id", "match_num", "date", "w_player2", "w_p2_hgt", "w_p2_age")]
colnames(dados1_w2) <- c("tourn_id", "match_num", "date", "player", "hgt", "age")
dados1_l1 <- dados[,c("tourn_id", "match_num", "date","l_player1", "l_p1_hgt", "l_p1_age")]
colnames(dados1_l1) <- c("tourn_id", "match_num", "date", "player", "hgt", "age")
dados1_l2 <- dados[,c("tourn_id", "match_num","date", "l_player2", "l_p2_hgt", "l_p2_age")]
colnames(dados1_l2) <- c("tourn_id", "match_num", "date", "player", "hgt", "age")

dados_duplas_w <- dados[,c("tourn_id", "match_num", "date", "w_player1", "w_player2")]
dados_duplas_w$result <- 'W'
colnames(dados_duplas_w) <- c("match_id", "match_num", "date", "player1","player2", "result")
dados_duplas_l <- dados[,c("tourn_id", "match_num", "date", "l_player1", "l_player2")]
dados_duplas_l$result <- 'L'
colnames(dados_duplas_l) <- c("match_id", "match_num", "date", "player1","player2", "result")
dados_gerais_duplas <- rbind(dados_duplas_w, dados_duplas_l)
dados_gerais_duplas$match_id <- paste(dados_gerais_duplas$match_id, dados_gerais_duplas$match_num, sep = ' - ')


jog_tds_jogos <- rbind(dados1_w1, dados1_w2, dados1_l1, dados1_l2)

jog_tds_jogos$player_tourn_id <- paste(jog_tds_jogos$tourn_id, jog_tds_jogos$date, jog_tds_jogos$player, sep = ' - ')
jog_tds_jogos$match_player_id <- paste(jog_tds_jogos$tourn_id, jog_tds_jogos$match_num, 
                                       jog_tds_jogos$player, jog_tds_jogos$date, jog_tds_jogos$match_num,
                                       sep = ' - ')

jog_tds_jogos <- jog_tds_jogos[which(!duplicated(jog_tds_jogos$match_player_id)),]

jog_tds_jogos <- jog_tds_jogos %>% arrange(player, date, match_num)



# Criando o "ID" da partida por jogador

jog_tds_jogos$cont_player <- 1

for(i in 1:(nrow(jog_tds_jogos)-1)){
  if(i %% 10000 == 0) {print(i)}
  if(jog_tds_jogos[i+1, "player"] == jog_tds_jogos[i,"player"]){
    jog_tds_jogos[i+1, "cont_player"] <- jog_tds_jogos[i,"cont_player"] +1
  }
}



# Fazendo o mesmo que foi feito anteriormente, mas com os dados já sem vazios

dados_sem_na <- dados_sem_na[which(dados_sem_na$circuit == 'AVP'),]
dados_sem_na$tourn_id <- paste(dados_sem_na$circuit, dados_sem_na$tournament, dados_sem_na$country, dados_sem_na$year,  dados_sem_na$gender, sep = ' - ') 
dados_sem_na <- dados_sem_na[which(dados_sem_na$bracket != "Qualifier Bracket"),]
dados_sem_na1_w1 <- dados_sem_na[,c("tourn_id", "match_num", "date", "w_player1", "w_p1_hgt", "w_p1_age")]
colnames(dados_sem_na1_w1) <- c("tourn_id", "match_num", "date", "player", "hgt","age")
dados_sem_na1_w2 <- dados_sem_na[,c("tourn_id", "match_num", "date", "w_player2", "w_p2_hgt", "w_p2_age")]
colnames(dados_sem_na1_w2) <- c("tourn_id", "match_num", "date", "player", "hgt", "age")
dados_sem_na1_l1 <- dados_sem_na[,c("tourn_id", "match_num", "date","l_player1", "l_p1_hgt", "l_p1_age")]
colnames(dados_sem_na1_l1) <- c("tourn_id", "match_num", "date", "player", "hgt", "age")
dados_sem_na1_l2 <- dados_sem_na[,c("tourn_id", "match_num","date", "l_player2", "l_p2_hgt", "l_p2_age")]
colnames(dados_sem_na1_l2) <- c("tourn_id", "match_num", "date", "player", "hgt", "age")

jog_tds_jogos_sem_na <- rbind(dados_sem_na1_w1, dados_sem_na1_w2, dados_sem_na1_l1, dados_sem_na1_l2)

jog_tds_jogos_sem_na$player_tourn_id <- paste(jog_tds_jogos_sem_na$tourn_id, jog_tds_jogos_sem_na$date, jog_tds_jogos_sem_na$player, sep = ' - ')
jog_tds_jogos_sem_na$match_player_id <- paste(jog_tds_jogos_sem_na$tourn_id, jog_tds_jogos_sem_na$match_num, 
                                              jog_tds_jogos_sem_na$player, jog_tds_jogos_sem_na$date, jog_tds_jogos_sem_na$match_num,
                                              sep = ' - ')


jog_tds_jogos_sem_na <- jog_tds_jogos_sem_na %>% arrange(player, date, match_num)

jog_tds_jogos_sem_na$jogos_nao_contabilizados_torneio <- c()

jog_tds_jogos_sem_na[1,'jogos_nao_contabilizados_torneio'] <- '1o jogo contabilizado'
jog_tds_jogos_sem_na$qtd_jogos_nao_contabilizados_torneio <- 0



# Juntando com os "ID's" por jogador da base completa

jog_tds_jogos_sem_na <- jog_tds_jogos_sem_na %>% 
  left_join(jog_tds_jogos[,c("match_player_id", "cont_player")], by = 'match_player_id')



# Criando colunas para verificar se o jogador teve jogos perdidos

jog_tds_jogos_sem_na$qtd_total_jogos_perdidos <- 0
jog_tds_jogos_sem_na$teve_jogos_perdidos <- 'não'

for(i in 2:nrow(jog_tds_jogos_sem_na)){
  if(jog_tds_jogos_sem_na[i, "player"] == jog_tds_jogos_sem_na[i-1, "player"] ){
    jog_tds_jogos_sem_na[i, 'qtd_total_jogos_perdidos'] <- jog_tds_jogos_sem_na[i,"cont_player"] - jog_tds_jogos_sem_na[i-1,"cont_player"]
    jog_tds_jogos_sem_na[i,'teve_jogos_perdidos'] <- ifelse(jog_tds_jogos_sem_na[i,"qtd_total_jogos_perdidos"] > 1, 'sim', 'não')
  }
}

jog_tds_jogos_sem_na$col_aux_jogos_perd <- ifelse(jog_tds_jogos_sem_na$qtd_total_jogos_perdidos %in% c(0,1),0,jog_tds_jogos_sem_na$qtd_total_jogos_perdidos)


# cont_player_sem_na se refere à "ID" do jogo por jogador, mas da base já sem vazios

jog_tds_jogos_sem_na$cont_player_sem_na <- 1

for(i in 1:(nrow(jog_tds_jogos_sem_na)-1)){
  if(i %% 10000 == 0) {print(i)}
  if(jog_tds_jogos_sem_na[i+1, "player"] == jog_tds_jogos_sem_na[i,"player"]){
    jog_tds_jogos_sem_na[i+1, "cont_player_sem_na"] <- jog_tds_jogos_sem_na[i,"cont_player_sem_na"] +1
  }
}



# Se o jogador perdeu algum dos últimos 4 jogos anteriores ao respectivo jogo atual, esta partida será desconsiderada depois

jog_tds_jogos_sem_na$jogos_perd_ult_4 <- 0

jog_tds_jogos_sem_na$jogos_perd_ult_4 <- ifelse(jog_tds_jogos_sem_na$cont_player_sem_na == 1, 
                                                jog_tds_jogos_sem_na$qtd_total_jogos_perdidos,
                                                0)

for(i in 1:(nrow(jog_tds_jogos_sem_na))){
  
  if(jog_tds_jogos_sem_na[i,"cont_player_sem_na"] != 1){
    if(jog_tds_jogos_sem_na[i,"cont_player_sem_na"] %in% c(2,3,4,5)){
      jog_tds_jogos_sem_na[i,"jogos_perd_ult_4"] <- jog_tds_jogos_sem_na[i,"col_aux_jogos_perd"] + jog_tds_jogos_sem_na[i-1,"jogos_perd_ult_4"]
    } else{
      jog_tds_jogos_sem_na[i,"jogos_perd_ult_4"] <- jog_tds_jogos_sem_na[i-1,"jogos_perd_ult_4"] - jog_tds_jogos_sem_na[i-4,"col_aux_jogos_perd"] + jog_tds_jogos_sem_na[i,"col_aux_jogos_perd"] 
    }
  }
}


# Essas partidas só serão desconsideradas após a realização do cálculo das médias móveis, de modo que estas mesmas partidas sejam contabilizadas nos cálculos das médias móveis das partidas posteriores

jogos_sem_na_desconsiderar <- jog_tds_jogos_sem_na %>% filter(jogos_perd_ult_4 >0)

# As colunas a seguir serão utilizadas posteriormente para remoção das partidas com gap entre jogos maior que 1

dados_sem_na$w1_match_player_id <- paste(dados_sem_na$tourn_id, dados_sem_na$match_num, 
                                         dados_sem_na$w_player1, dados_sem_na$date,
                                         dados_sem_na$match_num, sep = ' - ')
dados_sem_na$w2_match_player_id <- paste(dados_sem_na$tourn_id, dados_sem_na$match_num, 
                                         dados_sem_na$w_player2, dados_sem_na$date,
                                         dados_sem_na$match_num, sep = ' - ')
dados_sem_na$l1_match_player_id <- paste(dados_sem_na$tourn_id, dados_sem_na$match_num, 
                                         dados_sem_na$l_player1, dados_sem_na$date,
                                         dados_sem_na$match_num, sep = ' - ')
dados_sem_na$l2_match_player_id <- paste(dados_sem_na$tourn_id, dados_sem_na$match_num, 
                                         dados_sem_na$l_player2, dados_sem_na$date,
                                         dados_sem_na$match_num, sep = ' - ')


# COMEÇANDO A ORGANIZAR PARA ANÁLISES ----

## Separando os dados por gênero e criando tabela com os valores compilados por jogador ----

### MASCULINOS ----
dados_sem_na_m <- dados_sem_na[which(dados_sem_na$gender == 'M'), ]

wp1m <- dados_sem_na_m[,c("match_id", "match_num", "date", "points_played", "sets_played", "w_player1", "w_p1_age", "w_p1_hgt",
                          "w_p1_tot_aces", "w_p1_tot_attacks", "w_p1_tot_kills","w_p1_tot_blocks", 
                          "w_p1_tot_digs", "w_p1_tot_errors", "w_p1_tot_serve_errors", "w_p1_tot_hitpct")]
colnames(wp1m) <- c('match_id',"match_num", "date", "points_played", "sets_played", 'player', 'age', 'hgt', 'tot_aces', 'tot_attacks', 
                    'tot_kills', 'tot_blocks', 'tot_digs', 'tot_errors', 'tot_serve_errors', "tot_hitpct")

wp1m$nr_player <- 'W1'

wp2m <- dados_sem_na_m[,c("match_id","match_num", "date", "points_played", "sets_played", "w_player2", "w_p2_age", "w_p2_hgt",
                          "w_p2_tot_aces", "w_p2_tot_attacks", "w_p2_tot_kills","w_p2_tot_blocks", 
                          "w_p2_tot_digs", "w_p2_tot_errors", "w_p2_tot_serve_errors", "w_p2_tot_hitpct")]
wp2m$nr_player <- 'W2'
colnames(wp2m) <- colnames(wp1m)


lp1m <- dados_sem_na_m[,c("match_id","match_num", "date", "points_played", "sets_played", "l_player1", "l_p1_age", "l_p1_hgt",
                          "l_p1_tot_aces", "l_p1_tot_attacks", "l_p1_tot_kills", "l_p1_tot_blocks", 
                          "l_p1_tot_digs", "l_p1_tot_errors", "l_p1_tot_serve_errors", "l_p1_tot_hitpct")]
lp1m$nr_player <- 'L1'
colnames(lp1m) <- colnames(wp1m)


lp2m <- dados_sem_na_m[,c("match_id","match_num", "date", "points_played", "sets_played", "l_player2", "l_p2_age", "l_p2_hgt",
                          "l_p2_tot_aces", "l_p2_tot_attacks", "l_p2_tot_kills", "l_p2_tot_blocks", 
                          "l_p2_tot_digs", "l_p2_tot_errors", "l_p2_tot_serve_errors", "l_p2_tot_hitpct")]
lp2m$nr_player <- 'L2'
colnames(lp2m) <- colnames(wp1m)


dados_jogadores_m <- rbind(wp1m,wp2m,lp1m,lp2m)



### FEMININOS ----
dados_sem_na_w <- dados_sem_na[which(dados_sem_na$gender == 'W'), ]

wp1w <- dados_sem_na_w[,c("match_id","match_num", "date", "points_played", "sets_played", "w_player1", "w_p1_age", "w_p1_hgt",
                          "w_p1_tot_aces", "w_p1_tot_attacks", "w_p1_tot_kills","w_p1_tot_blocks", 
                          "w_p1_tot_digs", "w_p1_tot_errors", "w_p1_tot_serve_errors", "w_p1_tot_hitpct")]
colnames(wp1w) <- c('match_id',"match_num", "date", "points_played", "sets_played", 'player', 'age', 'hgt', 'tot_aces', 'tot_attacks', 
                    'tot_kills', 'tot_blocks', 'tot_digs', 'tot_errors', 'tot_serve_errors', "tot_hitpct")

wp1w$nr_player <- 'W1'

wp2w <- dados_sem_na_w[,c("match_id","match_num", "date", "points_played", "sets_played", "w_player2", "w_p2_age", "w_p2_hgt",
                          "w_p2_tot_aces", "w_p2_tot_attacks", "w_p2_tot_kills","w_p2_tot_blocks", 
                          "w_p2_tot_digs", "w_p2_tot_errors", "w_p2_tot_serve_errors", "w_p2_tot_hitpct")]
wp2w$nr_player <- 'W2'
colnames(wp2w) <- colnames(wp1w)


lp1w <- dados_sem_na_w[,c("match_id","match_num", "date", "points_played", "sets_played", "l_player1", "l_p1_age", "l_p1_hgt",
                          "l_p1_tot_aces", "l_p1_tot_attacks", "l_p1_tot_kills", "l_p1_tot_blocks", 
                          "l_p1_tot_digs", "l_p1_tot_errors", "l_p1_tot_serve_errors", "l_p1_tot_hitpct")]
lp1w$nr_player <- 'L1'
colnames(lp1w) <- colnames(wp1w)


lp2w <- dados_sem_na_w[,c("match_id","match_num", "date", "points_played", "sets_played", "l_player2", "l_p2_age", "l_p2_hgt",
                          "l_p2_tot_aces", "l_p2_tot_attacks", "l_p2_tot_kills", "l_p2_tot_blocks", 
                          "l_p2_tot_digs", "l_p2_tot_errors", "l_p2_tot_serve_errors", "l_p2_tot_hitpct")]
lp2w$nr_player <- 'L2'
colnames(lp2w) <- colnames(wp1w)


dados_jogadores_w <- rbind(wp1w,wp2w,lp1w,lp2w)




## Filtrando por jogadores com ao menos 5 jogos ----

### MASCULINO ----
dados_jogadores_m <- dados_jogadores_m %>% arrange(player, date, match_num, match_id)

# Criando média geral para colocar nos primeiros jogos, filtrando por jogadores com 
dados_jogadores_grp_m <- dados_jogadores_m %>% group_by(player, hgt) %>% summarise(
  tot_games = n()
) %>%filter(tot_games>=5)

for( i in 1:nrow(dados_sem_na_m)){
  if(dados_sem_na_m[i,"w_player1"] %in% dados_jogadores_grp_m$player){
    dados_sem_na_m[i, 'consid_jog_w1'] <- 'sim'
  }
  if(dados_sem_na_m[i,"w_player2"] %in% dados_jogadores_grp_m$player){
    dados_sem_na_m[i, 'consid_jog_w2'] <- 'sim'
  }
  if(dados_sem_na_m[i,"l_player1"] %in% dados_jogadores_grp_m$player){
    dados_sem_na_m[i, 'consid_jog_l1'] <- 'sim'
  }
  if(dados_sem_na_m[i,"l_player2"] %in% dados_jogadores_grp_m$player){
    dados_sem_na_m[i, 'consid_jog_l2'] <- 'sim'
  }
}


dados_sem_na_m_n_jogos <- dados_sem_na_m[which(dados_sem_na_m$consid_jog_w1 == 'sim'),]
dados_sem_na_m_n_jogos <- dados_sem_na_m_n_jogos[which(dados_sem_na_m_n_jogos$consid_jog_w2 == 'sim'),]
dados_sem_na_m_n_jogos <- dados_sem_na_m_n_jogos[which(dados_sem_na_m_n_jogos$consid_jog_l1 == 'sim'),]
dados_sem_na_m_n_jogos <- dados_sem_na_m_n_jogos[which(dados_sem_na_m_n_jogos$consid_jog_l2 == 'sim'),]


### FEMININO ----
dados_jogadores_w <- dados_jogadores_w %>% arrange(player, date,match_num, match_id)

dados_jogadores_grp_w <- dados_jogadores_w %>% group_by(player, hgt) %>% summarise(
  tot_games = n()
) %>%filter(tot_games>=5)


for( i in 1:nrow(dados_sem_na_w)){
  if(dados_sem_na_w[i,"w_player1"] %in% dados_jogadores_grp_w$player){
    dados_sem_na_w[i, 'consid_jog_w1'] <- 'sim'
  }
  if(dados_sem_na_w[i,"w_player2"] %in% dados_jogadores_grp_w$player){
    dados_sem_na_w[i, 'consid_jog_w2'] <- 'sim'
  }
  if(dados_sem_na_w[i,"l_player1"] %in% dados_jogadores_grp_w$player){
    dados_sem_na_w[i, 'consid_jog_l1'] <- 'sim'
  }
  if(dados_sem_na_w[i,"l_player2"] %in% dados_jogadores_grp_w$player){
    dados_sem_na_w[i, 'consid_jog_l2'] <- 'sim'
  }
}

dados_sem_na_w_n_jogos <- dados_sem_na_w[which(dados_sem_na_w$consid_jog_w1 == 'sim'),]
dados_sem_na_w_n_jogos <- dados_sem_na_w_n_jogos[which(dados_sem_na_w_n_jogos$consid_jog_w2 == 'sim'),]
dados_sem_na_w_n_jogos <- dados_sem_na_w_n_jogos[which(dados_sem_na_w_n_jogos$consid_jog_l1 == 'sim'),]
dados_sem_na_w_n_jogos <- dados_sem_na_w_n_jogos[which(dados_sem_na_w_n_jogos$consid_jog_l2 == 'sim'),]


## TRABALHANDO COM MÉDIAS MÓVEIS ----

### MASCULINO ----

#### Criando médias por pontos jogados em cada jogo ----

dados_jogadores_m$avg_atual_aces_points = dados_jogadores_m$tot_aces/dados_jogadores_m$points_played
dados_jogadores_m$avg_atual_attacks_points = dados_jogadores_m$tot_attacks/dados_jogadores_m$points_played
dados_jogadores_m$avg_atual_kills_points = dados_jogadores_m$tot_kills/dados_jogadores_m$points_played
dados_jogadores_m$avg_atual_blocks_points = dados_jogadores_m$tot_blocks/dados_jogadores_m$points_played
dados_jogadores_m$avg_atual_digs_points = dados_jogadores_m$tot_digs/dados_jogadores_m$points_played
dados_jogadores_m$avg_atual_errors_points = dados_jogadores_m$tot_errors/dados_jogadores_m$points_played
dados_jogadores_m$avg_atual_serve_errors_points = dados_jogadores_m$tot_serve_errors/dados_jogadores_m$points_played


# Filtrando somente por jogadores com ao menos 5 jogos na base
dados_jogadores_m_n_jogos <- dados_jogadores_m[dados_jogadores_m$player %in% dados_jogadores_grp_m$player,]


# Inputando as médias móveis das variáveis
for(i in 1:nrow(dados_jogadores_m_n_jogos)){
  #Travando para os casos em que é a primeira vez que aparece o jogador
  if(i!=1 && dados_jogadores_m_n_jogos[i-1, "player"] == dados_jogadores_m_n_jogos[i,"player"]){
    next
  }
  else{ 
    print(i)
    # j é a quantidade de jogos do jogador i
    j = length(dados_jogadores_m_n_jogos[which(dados_jogadores_m_n_jogos$player == dados_jogadores_m_n_jogos[i,"player"]), 'player'])
    # de i até ((i+j)-1) significa da primeira vez em que aparece o jogador até o último registro dele
    
    # Média móvel da hitting percentage
    dados_jogadores_m_n_jogos[i:(i+j-1), 'sma_hitpct'] <- (SMA(dados_jogadores_m_n_jogos[which(dados_jogadores_m_n_jogos$player == dados_jogadores_m_n_jogos[i,"player"]), 'tot_hitpct'], n=5)*5-dados_jogadores_m_n_jogos[i:(i+j-1),"tot_hitpct"])/4
    
    # Média móvel das médias por ponto jogado
    dados_jogadores_m_n_jogos[i:(i+j-1), 'sma_avg_aces_points'] <- (SMA(dados_jogadores_m_n_jogos[which(dados_jogadores_m_n_jogos$player == dados_jogadores_m_n_jogos[i,"player"]), 'avg_atual_aces_points'], n=5)*5-dados_jogadores_m_n_jogos[i:(i+j-1),"avg_atual_aces_points"])/4
    dados_jogadores_m_n_jogos[i:(i+j-1), 'sma_avg_attacks_points'] <- (SMA(dados_jogadores_m_n_jogos[which(dados_jogadores_m_n_jogos$player == dados_jogadores_m_n_jogos[i,"player"]), "avg_atual_attacks_points"], n=5)*5-dados_jogadores_m_n_jogos[i:(i+j-1),"avg_atual_attacks_points"])/4
    dados_jogadores_m_n_jogos[i:(i+j-1), 'sma_avg_kills_points'] <- (SMA(dados_jogadores_m_n_jogos[which(dados_jogadores_m_n_jogos$player == dados_jogadores_m_n_jogos[i,"player"]), 'avg_atual_kills_points'], n=5)*5-dados_jogadores_m_n_jogos[i:(i+j-1),"avg_atual_kills_points"])/4
    dados_jogadores_m_n_jogos[i:(i+j-1), 'sma_avg_blocks_points'] <- (SMA(dados_jogadores_m_n_jogos[which(dados_jogadores_m_n_jogos$player == dados_jogadores_m_n_jogos[i,"player"]), 'avg_atual_blocks_points'], n=5)*5-dados_jogadores_m_n_jogos[i:(i+j-1),"avg_atual_blocks_points"])/4
    dados_jogadores_m_n_jogos[i:(i+j-1), 'sma_avg_digs_points'] <- (SMA(dados_jogadores_m_n_jogos[which(dados_jogadores_m_n_jogos$player == dados_jogadores_m_n_jogos[i,"player"]), 'avg_atual_digs_points'], n=5)*5-dados_jogadores_m_n_jogos[i:(i+j-1),"avg_atual_digs_points"])/4
    dados_jogadores_m_n_jogos[i:(i+j-1), 'sma_avg_errors_points'] <- (SMA(dados_jogadores_m_n_jogos[which(dados_jogadores_m_n_jogos$player == dados_jogadores_m_n_jogos[i,"player"]), 'avg_atual_errors_points'], n=5)*5-dados_jogadores_m_n_jogos[i:(i+j-1),"avg_atual_errors_points"])/4
    dados_jogadores_m_n_jogos[i:(i+j-1), 'sma_avg_serve_errors_points'] <- (SMA(dados_jogadores_m_n_jogos[which(dados_jogadores_m_n_jogos$player == dados_jogadores_m_n_jogos[i,"player"]), 'avg_atual_serve_errors_points'], n=5)*5-dados_jogadores_m_n_jogos[i:(i+j-1),"avg_atual_serve_errors_points"])/4
    
    # Média móvel dos totais
    dados_jogadores_m_n_jogos[i:(i+j-1), 'sma_attacks'] <- (SMA(dados_jogadores_m_n_jogos[which(dados_jogadores_m_n_jogos$player == dados_jogadores_m_n_jogos[i,"player"]), "tot_attacks"], n=5)*5-dados_jogadores_m_n_jogos[i:(i+j-1),"tot_attacks"])/4
    dados_jogadores_m_n_jogos[i:(i+j-1), 'sma_kills'] <- (SMA(dados_jogadores_m_n_jogos[which(dados_jogadores_m_n_jogos$player == dados_jogadores_m_n_jogos[i,"player"]), 'tot_kills'], n=5)*5-dados_jogadores_m_n_jogos[i:(i+j-1),"tot_kills"])/4
    dados_jogadores_m_n_jogos[i:(i+j-1), 'sma_errors'] <- (SMA(dados_jogadores_m_n_jogos[which(dados_jogadores_m_n_jogos$player == dados_jogadores_m_n_jogos[i,"player"]), 'tot_errors'], n=5)*5-dados_jogadores_m_n_jogos[i:(i+j-1),"tot_errors"])/4
  }
}



# ID das partidas por jogador
dados_jogadores_m_n_jogos$match_player_id <- paste(dados_jogadores_m_n_jogos$match_id,
                                                   dados_jogadores_m_n_jogos$player,
                                                   dados_jogadores_m_n_jogos$date,
                                                   dados_jogadores_m_n_jogos$match_num,
                                                   sep=" - ")

# Removendo partidas com ID-partida-jogador duplicado
dados_jogadores_m_n_jogos <- dados_jogadores_m_n_jogos[!duplicated(dados_jogadores_m_n_jogos$match_player_id),]


#### Juntando as variáveis de médias móveis no dataframe com os jogos masculinos ----

# ID-partida-jogador para este dataframe também
dados_sem_na_m_n_jogos$match_player_id <- paste(dados_sem_na_m_n_jogos$match_id,
                                                dados_sem_na_m_n_jogos$w_player1,
                                                dados_sem_na_m_n_jogos$date,
                                                dados_sem_na_m_n_jogos$match_num,
                                                sep=" - ")

# Também removendo os ID-partida-jogador duplicados
dados_sem_na_m_n_jogos <- dados_sem_na_m_n_jogos[!duplicated(dados_sem_na_m_n_jogos$match_player_id),]



#### Inserindo os dados para os jogadores identificados como 'Jogador 1 - vencedor' ----
jogadores_filtrados_w1 <- dados_jogadores_m_n_jogos %>% filter(nr_player == 'W1')
jogadores_filtrados_w1 <- jogadores_filtrados_w1[,-2]

colnames(jogadores_filtrados_w1)[5] <- 'w_player1'

n = ncol(dados_sem_na_m_n_jogos)

dados_sem_na_m_n_jogos <- dados_sem_na_m_n_jogos %>%
  left_join(jogadores_filtrados_w1[,c(17:35)], by= 'match_player_id')


# Renomeando as variáveis para constar o 'w1' antes
for(i in (n+1):ncol(dados_sem_na_m_n_jogos)){
  colnames(dados_sem_na_m_n_jogos)[i] <- paste('w1_', colnames(dados_sem_na_m_n_jogos)[i], sep='' )
}


#### Inserindo os dados para os jogadores identificados como 'Jogador 2 - vencedor' ----

dados_sem_na_m_n_jogos$match_player_id <- paste(dados_sem_na_m_n_jogos$match_id,
                                                dados_sem_na_m_n_jogos$w_player2,
                                                dados_sem_na_m_n_jogos$date,
                                                dados_sem_na_m_n_jogos$match_num,
                                                sep=" - ")

jogadores_filtrados_w2 <- dados_jogadores_m_n_jogos %>% filter(nr_player == 'W2')
jogadores_filtrados_w2 <- jogadores_filtrados_w2[,-2]

colnames(jogadores_filtrados_w2)[5] <- 'w_player2'

n = ncol(dados_sem_na_m_n_jogos)

dados_sem_na_m_n_jogos <- dados_sem_na_m_n_jogos %>%
  left_join(jogadores_filtrados_w2[,c(17:35)], by= 'match_player_id')



# Renomeando as variáveis para constar o 'w2' antes
for(i in (n+1):ncol(dados_sem_na_m_n_jogos)){
  colnames(dados_sem_na_m_n_jogos)[i] <- paste('w2_', colnames(dados_sem_na_m_n_jogos)[i], sep='' )
}




#### Inserindo os dados para os jogadores identificados como 'Jogador 1 - perdedor' ----

dados_sem_na_m_n_jogos$match_player_id <- paste(dados_sem_na_m_n_jogos$match_id,
                                                dados_sem_na_m_n_jogos$l_player1,
                                                dados_sem_na_m_n_jogos$date,
                                                dados_sem_na_m_n_jogos$match_num,
                                                sep=" - ")

jogadores_filtrados_l1 <- dados_jogadores_m_n_jogos %>% filter(nr_player == 'L1')
jogadores_filtrados_l1 <- jogadores_filtrados_l1[,-2]

colnames(jogadores_filtrados_l1)[5] <- 'l_player1'

n = ncol(dados_sem_na_m_n_jogos)

dados_sem_na_m_n_jogos <- dados_sem_na_m_n_jogos %>%
  left_join(jogadores_filtrados_l1[,c(17:35)], by= 'match_player_id')


# Renomeando as variáveis para constar o 'l1' antes
for(i in (n+1):ncol(dados_sem_na_m_n_jogos)){
  colnames(dados_sem_na_m_n_jogos)[i] <- paste('l1_', colnames(dados_sem_na_m_n_jogos)[i], sep='' )
}




#### Inserindo os dados para os jogadores identificados como 'Jogador 2 - perdedor' ----

dados_sem_na_m_n_jogos$match_player_id <- paste(dados_sem_na_m_n_jogos$match_id,
                                                dados_sem_na_m_n_jogos$l_player2,
                                                dados_sem_na_m_n_jogos$date,
                                                dados_sem_na_m_n_jogos$match_num,
                                                sep=" - ")

jogadores_filtrados_l2 <- dados_jogadores_m_n_jogos %>% filter(nr_player == 'L2')
jogadores_filtrados_l2 <- jogadores_filtrados_l2[,-2]

colnames(jogadores_filtrados_l2)[5] <- 'l_player2'

n = ncol(dados_sem_na_m_n_jogos)

dados_sem_na_m_n_jogos <- dados_sem_na_m_n_jogos %>%
  left_join(jogadores_filtrados_l2[,c(17:35)], by= 'match_player_id')


# Renomeando as variáveis para constar o 'l2' antes
for(i in (n+1):ncol(dados_sem_na_m_n_jogos)){
  colnames(dados_sem_na_m_n_jogos)[i] <- paste('l2_', colnames(dados_sem_na_m_n_jogos)[i], sep='' )
}


#### Inserindo os dados relativos às duplas ----

duplas_todas_masc <- dados_gerais_duplas

duplas_todas_masc$match_id <- paste(duplas_todas_masc$match_id, duplas_todas_masc$date, sep = ' - ')

duplas_todas_masc <- duplas_todas_masc %>% arrange(player1, player2, date, match_num, match_id)

duplas_todas_masc$chave_dupla <- paste(duplas_todas_masc$player1, duplas_todas_masc$player2, sep = ' - ')


# Número de jogos juntos da dupla
duplas_todas_masc$nr_jogos <- 1

for(m in 1:(nrow(duplas_todas_masc)-1)){
  if(duplas_todas_masc[m+1, "chave_dupla"] == duplas_todas_masc[m, "chave_dupla"]){
    duplas_todas_masc[m+1, "nr_jogos"] <- duplas_todas_masc[m, "nr_jogos"] +1
  }
}


duplas_todas_masc$chave_dupla_result <- paste(duplas_todas_masc$player1, duplas_todas_masc$player2, duplas_todas_masc$result, sep = ' - ')


# Sequência de vitórias da duplas
duplas_todas_masc$streak_aux <- ifelse(duplas_todas_masc$result == 'L',-1,1)

duplas_todas_masc$streak <- 0

for(m in 2:nrow(duplas_todas_masc)){
  if(duplas_todas_masc[m-1, "chave_dupla"] == duplas_todas_masc[m, "chave_dupla"]){
    if(duplas_todas_masc[m-1,"streak_aux"] == -1){
      if(duplas_todas_masc[m-1,"streak"] >= 0){
        duplas_todas_masc[m, "streak"] <- -1
      } else{
        duplas_todas_masc[m, "streak"] <- duplas_todas_masc[m-1, "streak"] - 1
      }
    } else{
      if(duplas_todas_masc[m-1,"streak_aux"] == 1){
        if(duplas_todas_masc[m-1,"streak"] < 0){
          duplas_todas_masc[m, "streak"] <- 1
        } else{
          duplas_todas_masc[m, "streak"] <- duplas_todas_masc[m-1, "streak"] + 1
        }
      }
    }
  }
}


# Tempo entre cada jogo da dupla
duplas_todas_masc$tempo_entre_jogos <- 0

for(m in 1:(nrow(duplas_todas_masc)-1)){
  if(duplas_todas_masc[m+1, "chave_dupla"] == duplas_todas_masc[m, "chave_dupla"]){
    duplas_todas_masc[m+1,'tempo_entre_jogos'] <- difftime(duplas_todas_masc[m+1,"date"],duplas_todas_masc[m,"date"], units = 'days')
  }
}


# Número de títulos da dupla
dados_campeoes <- dados %>% group_by(tourn_id) %>% summarise(
  ult_jogo = max(match_num)
)

dados_campeoes$tourn_match_id <- paste(dados_campeoes$tourn_id, dados_campeoes$ult_jogo, sep = ' - ')

dados$tourn_match_id <- paste(dados$tourn_id, dados$match_num, sep = ' - ')

dados$jogo_titulo <- ifelse(dados$tourn_match_id %in% dados_campeoes$tourn_match_id, 'sim', 'nao')

dados_ult_jogo <- dados %>% filter(jogo_titulo == 'sim')

dados_ult_jogo$dupla_campea <- paste(dados_ult_jogo$w_player1, dados_ult_jogo$w_player2, sep = ' - ')

duplas_todas_masc$titulos <- 0

for(i in 1:nrow(duplas_todas_masc)){
  duplas_todas_masc[i,"titulos"] <- nrow(dados_ult_jogo[which(dados_ult_jogo$dupla_campea == duplas_todas_masc[i, 'chave_dupla'] & 
                                                                dados_ult_jogo$date<duplas_todas_masc[i,"date"]),])
}



# Juntando as variáveis criadas ao dataframe com os jogos

duplas_todas_masc_w <- duplas_todas_masc %>% filter(result == 'W')
duplas_todas_masc_l <- duplas_todas_masc %>% filter(result == 'L')

dados_sem_na_m_n_jogos$match_id <- paste(dados_sem_na_m_n_jogos$match_id, dados_sem_na_m_n_jogos$date, sep=' - ')

dados_sem_na_m_n_jogos2 <- dados_sem_na_m_n_jogos %>%
  left_join(duplas_todas_masc_w[,c(1,8,11,12,13)], by= 'match_id')

colnames(dados_sem_na_m_n_jogos2)[c((ncol(dados_sem_na_m_n_jogos2)-3), (ncol(dados_sem_na_m_n_jogos2)-2), (ncol(dados_sem_na_m_n_jogos2)-1), ncol(dados_sem_na_m_n_jogos2))] <- c('nr_jogos_dupla_w', 'streak_dupla_w', 'tempo_entre_jogos_dupla_w', 'titulos_w')

dados_sem_na_m_n_jogos2 <- dados_sem_na_m_n_jogos2 %>%
  left_join(duplas_todas_masc_l[,c(1,8,11,12,13)], by= 'match_id')

colnames(dados_sem_na_m_n_jogos2)[c((ncol(dados_sem_na_m_n_jogos2)-3), (ncol(dados_sem_na_m_n_jogos2)-2), (ncol(dados_sem_na_m_n_jogos2)-1), ncol(dados_sem_na_m_n_jogos2))] <- c('nr_jogos_dupla_l', 'streak_dupla_l', 'tempo_entre_jogos_dupla_l', 'titulos_l')


# tste <- as.data.frame(table(duplas_todas_masc$tempo_entre_jogos))
# tot <- sum(tste$Freq)
# 
# tste$Var2 <- as.numeric(tste$Var1)
# tste$perc <- (tste$Freq/tot)*100
# sum(tste[which(as.numeric(tste$Var2)>130), "perc"])
# tste[which(tste$Var1==3758), "Freq"]



#### Removendo todos os vazios e também os jogos em que houve descontinuidade antes ----

dados_masc_sexto_mais_jogo <- dados_sem_na_m_n_jogos2 %>% na.omit() %>% arrange(date, tournament, match_num)

dados_masc_sexto_mais_jogo <- dados_masc_sexto_mais_jogo[which(!(dados_masc_sexto_mais_jogo$w1_match_player_id %in% jogos_sem_na_desconsiderar$match_player_id) &
                                                                 !(dados_masc_sexto_mais_jogo$w2_match_player_id %in% jogos_sem_na_desconsiderar$match_player_id) &
                                                                 !(dados_masc_sexto_mais_jogo$l1_match_player_id %in% jogos_sem_na_desconsiderar$match_player_id) &
                                                                 !(dados_masc_sexto_mais_jogo$l2_match_player_id %in% jogos_sem_na_desconsiderar$match_player_id)),]


# Criando variável para definir se algum jogador de cada dupla está jogando em casa
dados_masc_sexto_mais_jogo$dupla_w_home <- ifelse(dados_masc_sexto_mais_jogo$w_p1_country == dados_masc_sexto_mais_jogo$country | dados_masc_sexto_mais_jogo$w_p2_country == dados_masc_sexto_mais_jogo$country, 1,0)
dados_masc_sexto_mais_jogo$dupla_l_home <- ifelse(dados_masc_sexto_mais_jogo$l_p1_country == dados_masc_sexto_mais_jogo$country | dados_masc_sexto_mais_jogo$l_p2_country == dados_masc_sexto_mais_jogo$country, 1,0)

# Criando variáveis agregadas para a dupla vencedora
dados_masc_sexto_mais_jogo$dupla_w_avg_age <- (dados_masc_sexto_mais_jogo$w_p1_age + dados_masc_sexto_mais_jogo$w_p2_age)/2
dados_masc_sexto_mais_jogo$dupla_w_avg_hgt <- (dados_masc_sexto_mais_jogo$w_p1_hgt + dados_masc_sexto_mais_jogo$w_p2_hgt)/2
dados_masc_sexto_mais_jogo$dupla_w_sma_avg_aces_points <- dados_masc_sexto_mais_jogo$w1_sma_avg_aces_points + dados_masc_sexto_mais_jogo$w2_sma_avg_aces_points
dados_masc_sexto_mais_jogo$dupla_w_sma_avg_attacks_points <- dados_masc_sexto_mais_jogo$w1_sma_avg_attacks_points + dados_masc_sexto_mais_jogo$w2_sma_avg_attacks_points
dados_masc_sexto_mais_jogo$dupla_w_sma_avg_kills_points <- dados_masc_sexto_mais_jogo$w1_sma_avg_kills_points + dados_masc_sexto_mais_jogo$w2_sma_avg_kills_points
dados_masc_sexto_mais_jogo$dupla_w_sma_avg_blocks_points <- dados_masc_sexto_mais_jogo$w1_sma_avg_blocks_points + dados_masc_sexto_mais_jogo$w2_sma_avg_blocks_points
dados_masc_sexto_mais_jogo$dupla_w_sma_avg_digs_points <- dados_masc_sexto_mais_jogo$w1_sma_avg_digs_points + dados_masc_sexto_mais_jogo$w2_sma_avg_digs_points
dados_masc_sexto_mais_jogo$dupla_w_sma_avg_errors_points <- dados_masc_sexto_mais_jogo$w1_sma_avg_errors_points + dados_masc_sexto_mais_jogo$w2_sma_avg_errors_points
dados_masc_sexto_mais_jogo$dupla_w_sma_avg_serve_errors_points <- dados_masc_sexto_mais_jogo$w1_sma_avg_serve_errors_points + dados_masc_sexto_mais_jogo$w2_sma_avg_serve_errors_points
dados_masc_sexto_mais_jogo$dupla_w_sma_hitpct <- ((dados_masc_sexto_mais_jogo$w1_sma_kills+dados_masc_sexto_mais_jogo$w2_sma_kills)
                                                  - (dados_masc_sexto_mais_jogo$w1_sma_errors + dados_masc_sexto_mais_jogo$w2_sma_errors))/
  (dados_masc_sexto_mais_jogo$w1_sma_attacks + dados_masc_sexto_mais_jogo$w2_sma_attacks)


# Criando variáveis agregadas para a dupla perdedora
dados_masc_sexto_mais_jogo$dupla_l_avg_age <- (dados_masc_sexto_mais_jogo$l_p1_age + dados_masc_sexto_mais_jogo$l_p2_age)/2
dados_masc_sexto_mais_jogo$dupla_l_avg_hgt <- (dados_masc_sexto_mais_jogo$l_p1_hgt + dados_masc_sexto_mais_jogo$l_p2_hgt)/2
dados_masc_sexto_mais_jogo$dupla_l_sma_avg_aces_points <- dados_masc_sexto_mais_jogo$l1_sma_avg_aces_points + dados_masc_sexto_mais_jogo$l2_sma_avg_aces_points
dados_masc_sexto_mais_jogo$dupla_l_sma_avg_attacks_points <- dados_masc_sexto_mais_jogo$l1_sma_avg_attacks_points + dados_masc_sexto_mais_jogo$l2_sma_avg_attacks_points
dados_masc_sexto_mais_jogo$dupla_l_sma_avg_kills_points <- dados_masc_sexto_mais_jogo$l1_sma_avg_kills_points + dados_masc_sexto_mais_jogo$l2_sma_avg_kills_points
dados_masc_sexto_mais_jogo$dupla_l_sma_avg_blocks_points <- dados_masc_sexto_mais_jogo$l1_sma_avg_blocks_points + dados_masc_sexto_mais_jogo$l2_sma_avg_blocks_points
dados_masc_sexto_mais_jogo$dupla_l_sma_avg_digs_points <- dados_masc_sexto_mais_jogo$l1_sma_avg_digs_points + dados_masc_sexto_mais_jogo$l2_sma_avg_digs_points
dados_masc_sexto_mais_jogo$dupla_l_sma_avg_errors_points <- dados_masc_sexto_mais_jogo$l1_sma_avg_errors_points + dados_masc_sexto_mais_jogo$l2_sma_avg_errors_points
dados_masc_sexto_mais_jogo$dupla_l_sma_avg_serve_errors_points <- dados_masc_sexto_mais_jogo$l1_sma_avg_serve_errors_points + dados_masc_sexto_mais_jogo$l2_sma_avg_serve_errors_points
dados_masc_sexto_mais_jogo$dupla_l_sma_hitpct <- ((dados_masc_sexto_mais_jogo$l1_sma_kills+dados_masc_sexto_mais_jogo$l2_sma_kills)
                                                  - (dados_masc_sexto_mais_jogo$l1_sma_errors + dados_masc_sexto_mais_jogo$l2_sma_errors))/
  (dados_masc_sexto_mais_jogo$l1_sma_attacks + dados_masc_sexto_mais_jogo$l2_sma_attacks)


# Definindo variáveis para a dupla de referência -- dupla perdedora se variável "dupla_de_referencia" == 'L', dupla vencedora caso contrário

for(i in 1:nrow(dados_masc_sexto_mais_jogo)){
  if(dados_masc_sexto_mais_jogo[i,"dupla_de_referencia"] == 'L'){
    
    dados_masc_sexto_mais_jogo[i,"dupla_ref_titulos"] <- dados_masc_sexto_mais_jogo[i,"titulos_l"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_nr_jogos"] <- dados_masc_sexto_mais_jogo[i,"nr_jogos_dupla_l"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_streak"] <- dados_masc_sexto_mais_jogo[i,"streak_dupla_l"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_rank"] <- dados_masc_sexto_mais_jogo[i,"l_rank"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_home"] <- dados_masc_sexto_mais_jogo[i, "dupla_l_home"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_avg_age"] <- dados_masc_sexto_mais_jogo[i, "dupla_l_avg_age"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_avg_hgt"] <- dados_masc_sexto_mais_jogo[i, "dupla_l_avg_hgt"]
    
    dados_masc_sexto_mais_jogo[i,"dupla_ref_sma_avg_aces_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_l_sma_avg_aces_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_sma_avg_attacks_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_l_sma_avg_attacks_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_sma_avg_kills_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_l_sma_avg_kills_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_sma_avg_blocks_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_l_sma_avg_blocks_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_sma_avg_digs_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_l_sma_avg_digs_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_sma_avg_errors_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_l_sma_avg_errors_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_sma_avg_serve_errors_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_l_sma_avg_serve_errors_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_sma_hitpct"] <- dados_masc_sexto_mais_jogo[i,"dupla_l_sma_hitpct"]
    
    
    dados_masc_sexto_mais_jogo[i,"dupla_opp_titulos"] <- dados_masc_sexto_mais_jogo[i,"titulos_w"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_nr_jogos"] <- dados_masc_sexto_mais_jogo[i,"nr_jogos_dupla_w"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_streak"] <- dados_masc_sexto_mais_jogo[i,"streak_dupla_w"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_rank"] <- dados_masc_sexto_mais_jogo[i,"w_rank"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_home"] <- dados_masc_sexto_mais_jogo[i, "dupla_w_home"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_avg_age"] <- dados_masc_sexto_mais_jogo[i, "dupla_w_avg_age"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_avg_hgt"] <- dados_masc_sexto_mais_jogo[i, "dupla_w_avg_hgt"]
    
    dados_masc_sexto_mais_jogo[i,"dupla_opp_sma_avg_aces_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_w_sma_avg_aces_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_sma_avg_attacks_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_w_sma_avg_attacks_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_sma_avg_kills_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_w_sma_avg_kills_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_sma_avg_blocks_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_w_sma_avg_blocks_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_sma_avg_digs_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_w_sma_avg_digs_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_sma_avg_errors_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_w_sma_avg_errors_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_sma_avg_serve_errors_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_w_sma_avg_serve_errors_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_sma_hitpct"] <- dados_masc_sexto_mais_jogo[i,"dupla_w_sma_hitpct"]
    
  }
  else{
    
    dados_masc_sexto_mais_jogo[i,"dupla_ref_titulos"] <- dados_masc_sexto_mais_jogo[i,"titulos_w"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_nr_jogos"] <- dados_masc_sexto_mais_jogo[i,"nr_jogos_dupla_w"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_streak"] <- dados_masc_sexto_mais_jogo[i,"streak_dupla_w"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_rank"] <- dados_masc_sexto_mais_jogo[i,"w_rank"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_home"] <- dados_masc_sexto_mais_jogo[i, "dupla_w_home"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_avg_age"] <- dados_masc_sexto_mais_jogo[i, "dupla_w_avg_age"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_avg_hgt"] <- dados_masc_sexto_mais_jogo[i, "dupla_w_avg_hgt"]
    
    dados_masc_sexto_mais_jogo[i,"dupla_ref_sma_avg_aces_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_w_sma_avg_aces_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_sma_avg_attacks_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_w_sma_avg_attacks_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_sma_avg_kills_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_w_sma_avg_kills_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_sma_avg_blocks_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_w_sma_avg_blocks_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_sma_avg_digs_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_w_sma_avg_digs_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_sma_avg_errors_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_w_sma_avg_errors_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_sma_avg_serve_errors_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_w_sma_avg_serve_errors_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_ref_sma_hitpct"] <- dados_masc_sexto_mais_jogo[i,"dupla_w_sma_hitpct"]
    
    
    dados_masc_sexto_mais_jogo[i,"dupla_opp_titulos"] <- dados_masc_sexto_mais_jogo[i,"titulos_l"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_nr_jogos"] <- dados_masc_sexto_mais_jogo[i,"nr_jogos_dupla_l"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_streak"] <- dados_masc_sexto_mais_jogo[i,"streak_dupla_l"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_rank"] <- dados_masc_sexto_mais_jogo[i,"l_rank"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_home"] <- dados_masc_sexto_mais_jogo[i, "dupla_l_home"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_avg_age"] <- dados_masc_sexto_mais_jogo[i, "dupla_l_avg_age"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_avg_hgt"] <- dados_masc_sexto_mais_jogo[i, "dupla_l_avg_hgt"]
    
    
    dados_masc_sexto_mais_jogo[i,"dupla_opp_sma_avg_aces_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_l_sma_avg_aces_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_sma_avg_attacks_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_l_sma_avg_attacks_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_sma_avg_kills_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_l_sma_avg_kills_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_sma_avg_blocks_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_l_sma_avg_blocks_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_sma_avg_digs_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_l_sma_avg_digs_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_sma_avg_errors_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_l_sma_avg_errors_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_sma_avg_serve_errors_points"] <- dados_masc_sexto_mais_jogo[i,"dupla_l_sma_avg_serve_errors_points"]
    dados_masc_sexto_mais_jogo[i,"dupla_opp_sma_hitpct"] <- dados_masc_sexto_mais_jogo[i,"dupla_l_sma_hitpct"]
    
    
  }
  
}

dados_masc_sexto_mais_jogo$dupla_ref_home <- as.factor(dados_masc_sexto_mais_jogo$dupla_ref_home)
dados_masc_sexto_mais_jogo$dupla_opp_home <- as.factor(dados_masc_sexto_mais_jogo$dupla_opp_home)
dados_masc_sexto_mais_jogo$dupla_de_referencia <- as.factor(dados_masc_sexto_mais_jogo$dupla_de_referencia)


#### Montando os modelos ----


variaveis_tentativa1_masc_ambos <- dados_masc_sexto_mais_jogo[,c("dupla_de_referencia","dupla_ref_nr_jogos", "dupla_ref_streak",
                                                                 "dupla_ref_rank", 'dupla_ref_titulos',
                                                                 "dupla_ref_home", 
                                                                 "dupla_ref_avg_age",
                                                                 "dupla_ref_avg_hgt", "dupla_ref_sma_avg_aces_points",
                                                                 "dupla_ref_sma_avg_blocks_points",
                                                                 "dupla_ref_sma_avg_digs_points", "dupla_ref_sma_hitpct",
                                                                 "dupla_ref_sma_avg_serve_errors_points", "dupla_opp_nr_jogos",
                                                                 "dupla_opp_streak",
                                                                 "dupla_opp_rank", 'dupla_opp_titulos',
                                                                 "dupla_opp_home",
                                                                 "dupla_opp_avg_age","dupla_opp_avg_hgt", 
                                                                 "dupla_opp_sma_avg_aces_points",
                                                                 "dupla_opp_sma_avg_blocks_points",
                                                                 "dupla_opp_sma_avg_digs_points", "dupla_opp_sma_hitpct",
                                                                 
                                                                 "dupla_opp_sma_avg_serve_errors_points")]
##### Testando para vários dados de treino e teste ----

for(z in 1:200){
  
  print(paste('z = ', z, '; masc ambos', sep=''))
  
  training.samples <- variaveis_tentativa1_masc_ambos$dupla_de_referencia %>%
    createDataPartition(p = 0.8, list = FALSE)
  train.data  <- variaveis_tentativa1_masc_ambos[training.samples, ]
  
  train.masc.ambos <- train.data
  
  
  
  # Modelo completo
  full.model2 <- glm(dupla_de_referencia ~., data = train.data, family = binomial)
  
  # Modelo backward stepwise
  step.model.bck2 <- full.model2 %>% stepAIC(trace = FALSE, direction = 'backward')
  
  
  # Estão no modelo completo e não no modelo stepwise backward
  for(i in 1:length(coef(full.model2))){
    if(!(names(coef(full.model2))[i] %in% names(coef(step.model.bck2)))){
      compl_nao_backward2 <- rbind(compl_nao_backward2, cbind(names(coef(full.model2))[i], z))
    }
  }
  
  
}



### FEMININO ----

#### Criando médias por pontos jogados em cada jogo ----

dados_jogadores_w$avg_atual_aces_points = dados_jogadores_w$tot_aces/dados_jogadores_w$points_played
dados_jogadores_w$avg_atual_attacks_points = dados_jogadores_w$tot_attacks/dados_jogadores_w$points_played
dados_jogadores_w$avg_atual_kills_points = dados_jogadores_w$tot_kills/dados_jogadores_w$points_played
dados_jogadores_w$avg_atual_blocks_points = dados_jogadores_w$tot_blocks/dados_jogadores_w$points_played
dados_jogadores_w$avg_atual_digs_points = dados_jogadores_w$tot_digs/dados_jogadores_w$points_played
dados_jogadores_w$avg_atual_errors_points = dados_jogadores_w$tot_errors/dados_jogadores_w$points_played
dados_jogadores_w$avg_atual_serve_errors_points = dados_jogadores_w$tot_serve_errors/dados_jogadores_w$points_played


# Filtrando somente por jogadores com ao menos 5 jogos na base
dados_jogadores_w_n_jogos <- dados_jogadores_w[dados_jogadores_w$player %in% dados_jogadores_grp_w$player,]


# Inputando as médias móveis das variáveis
for(i in 1:nrow(dados_jogadores_w_n_jogos)){
  #Travando para os casos em que é a primeira vez que aparece o jogador
  if(i!=1 && dados_jogadores_w_n_jogos[i-1, "player"] == dados_jogadores_w_n_jogos[i,"player"]){
    next
  }
  else{ 
    print(i)
    # j é a quantidade de jogos do jogador i
    j = length(dados_jogadores_w_n_jogos[which(dados_jogadores_w_n_jogos$player == dados_jogadores_w_n_jogos[i,"player"]), 'player'])
    # de i até ((i+j)-1) significa da primeira vez em que aparece o jogador até o último registro dele
    
    # Média móvel da hitting percentage
    dados_jogadores_w_n_jogos[i:(i+j-1), 'sma_hitpct'] <- (SMA(dados_jogadores_w_n_jogos[which(dados_jogadores_w_n_jogos$player == dados_jogadores_w_n_jogos[i,"player"]), 'tot_hitpct'], n=5)*5-dados_jogadores_w_n_jogos[i:(i+j-1),"tot_hitpct"])/4
    
    # Média móvel das médias por ponto jogado
    dados_jogadores_w_n_jogos[i:(i+j-1), 'sma_avg_aces_points'] <- (SMA(dados_jogadores_w_n_jogos[which(dados_jogadores_w_n_jogos$player == dados_jogadores_w_n_jogos[i,"player"]), 'avg_atual_aces_points'], n=5)*5-dados_jogadores_w_n_jogos[i:(i+j-1),"avg_atual_aces_points"])/4
    dados_jogadores_w_n_jogos[i:(i+j-1), 'sma_avg_attacks_points'] <- (SMA(dados_jogadores_w_n_jogos[which(dados_jogadores_w_n_jogos$player == dados_jogadores_w_n_jogos[i,"player"]), "avg_atual_attacks_points"], n=5)*5-dados_jogadores_w_n_jogos[i:(i+j-1),"avg_atual_attacks_points"])/4
    dados_jogadores_w_n_jogos[i:(i+j-1), 'sma_avg_kills_points'] <- (SMA(dados_jogadores_w_n_jogos[which(dados_jogadores_w_n_jogos$player == dados_jogadores_w_n_jogos[i,"player"]), 'avg_atual_kills_points'], n=5)*5-dados_jogadores_w_n_jogos[i:(i+j-1),"avg_atual_kills_points"])/4
    dados_jogadores_w_n_jogos[i:(i+j-1), 'sma_avg_blocks_points'] <- (SMA(dados_jogadores_w_n_jogos[which(dados_jogadores_w_n_jogos$player == dados_jogadores_w_n_jogos[i,"player"]), 'avg_atual_blocks_points'], n=5)*5-dados_jogadores_w_n_jogos[i:(i+j-1),"avg_atual_blocks_points"])/4
    dados_jogadores_w_n_jogos[i:(i+j-1), 'sma_avg_digs_points'] <- (SMA(dados_jogadores_w_n_jogos[which(dados_jogadores_w_n_jogos$player == dados_jogadores_w_n_jogos[i,"player"]), 'avg_atual_digs_points'], n=5)*5-dados_jogadores_w_n_jogos[i:(i+j-1),"avg_atual_digs_points"])/4
    dados_jogadores_w_n_jogos[i:(i+j-1), 'sma_avg_errors_points'] <- (SMA(dados_jogadores_w_n_jogos[which(dados_jogadores_w_n_jogos$player == dados_jogadores_w_n_jogos[i,"player"]), 'avg_atual_errors_points'], n=5)*5-dados_jogadores_w_n_jogos[i:(i+j-1),"avg_atual_errors_points"])/4
    dados_jogadores_w_n_jogos[i:(i+j-1), 'sma_avg_serve_errors_points'] <- (SMA(dados_jogadores_w_n_jogos[which(dados_jogadores_w_n_jogos$player == dados_jogadores_w_n_jogos[i,"player"]), 'avg_atual_serve_errors_points'], n=5)*5-dados_jogadores_w_n_jogos[i:(i+j-1),"avg_atual_serve_errors_points"])/4
    
    # Média móvel dos totais
    dados_jogadores_w_n_jogos[i:(i+j-1), 'sma_attacks'] <- (SMA(dados_jogadores_w_n_jogos[which(dados_jogadores_w_n_jogos$player == dados_jogadores_w_n_jogos[i,"player"]), "tot_attacks"], n=5)*5-dados_jogadores_w_n_jogos[i:(i+j-1),"tot_attacks"])/4
    dados_jogadores_w_n_jogos[i:(i+j-1), 'sma_kills'] <- (SMA(dados_jogadores_w_n_jogos[which(dados_jogadores_w_n_jogos$player == dados_jogadores_w_n_jogos[i,"player"]), 'tot_kills'], n=5)*5-dados_jogadores_w_n_jogos[i:(i+j-1),"tot_kills"])/4
    dados_jogadores_w_n_jogos[i:(i+j-1), 'sma_errors'] <- (SMA(dados_jogadores_w_n_jogos[which(dados_jogadores_w_n_jogos$player == dados_jogadores_w_n_jogos[i,"player"]), 'tot_errors'], n=5)*5-dados_jogadores_w_n_jogos[i:(i+j-1),"tot_errors"])/4
  }
}



# ID das partidas por jogador
dados_jogadores_w_n_jogos$match_player_id <- paste(dados_jogadores_w_n_jogos$match_id,
                                                   dados_jogadores_w_n_jogos$player,
                                                   dados_jogadores_w_n_jogos$date,
                                                   dados_jogadores_w_n_jogos$match_num,
                                                   sep=" - ")

# Removendo partidas com ID-partida-jogador duplicado
dados_jogadores_w_n_jogos <- dados_jogadores_w_n_jogos[!duplicated(dados_jogadores_w_n_jogos$match_player_id),]


#### Juntando as variáveis de médias móveis no dataframe com os jogos femininos ----

# ID-partida-jogador para este dataframe também
dados_sem_na_w_n_jogos$match_player_id <- paste(dados_sem_na_w_n_jogos$match_id,
                                                dados_sem_na_w_n_jogos$w_player1,
                                                dados_sem_na_w_n_jogos$date,
                                                dados_sem_na_w_n_jogos$match_num,
                                                sep=" - ")

# Também removendo os ID-partida-jogador duplicados
dados_sem_na_w_n_jogos <- dados_sem_na_w_n_jogos[!duplicated(dados_sem_na_w_n_jogos$match_player_id),]



#### Inserindo os dados para os jogadores identificados como 'Jogador 1 - vencedor' ----
jogadores_filtrados_w1 <- dados_jogadores_w_n_jogos %>% filter(nr_player == 'W1')
jogadores_filtrados_w1 <- jogadores_filtrados_w1[,-2]

colnames(jogadores_filtrados_w1)[5] <- 'w_player1'

n = ncol(dados_sem_na_w_n_jogos)

dados_sem_na_w_n_jogos <- dados_sem_na_w_n_jogos %>%
  left_join(jogadores_filtrados_w1[,c(17:35)], by= 'match_player_id')


# Renomeando as variáveis para constar o 'w1' antes
for(i in (n+1):ncol(dados_sem_na_w_n_jogos)){
  colnames(dados_sem_na_w_n_jogos)[i] <- paste('w1_', colnames(dados_sem_na_w_n_jogos)[i], sep='' )
}


#### Inserindo os dados para os jogadores identificados como 'Jogador 2 - vencedor' ----

dados_sem_na_w_n_jogos$match_player_id <- paste(dados_sem_na_w_n_jogos$match_id,
                                                dados_sem_na_w_n_jogos$w_player2,
                                                dados_sem_na_w_n_jogos$date,
                                                dados_sem_na_w_n_jogos$match_num,
                                                sep=" - ")

jogadores_filtrados_w2 <- dados_jogadores_w_n_jogos %>% filter(nr_player == 'W2')
jogadores_filtrados_w2 <- jogadores_filtrados_w2[,-2]

colnames(jogadores_filtrados_w2)[5] <- 'w_player2'

n = ncol(dados_sem_na_w_n_jogos)

dados_sem_na_w_n_jogos <- dados_sem_na_w_n_jogos %>%
  left_join(jogadores_filtrados_w2[,c(17:35)], by= 'match_player_id')


# Renomeando as variáveis para constar o 'w2' antes
for(i in (n+1):ncol(dados_sem_na_w_n_jogos)){
  colnames(dados_sem_na_w_n_jogos)[i] <- paste('w2_', colnames(dados_sem_na_w_n_jogos)[i], sep='' )
}



#### Inserindo os dados para os jogadores identificados como 'Jogador 1 - perdedor' ----

dados_sem_na_w_n_jogos$match_player_id <- paste(dados_sem_na_w_n_jogos$match_id,
                                                dados_sem_na_w_n_jogos$l_player1,
                                                dados_sem_na_w_n_jogos$date,
                                                dados_sem_na_w_n_jogos$match_num,
                                                sep=" - ")

jogadores_filtrados_l1 <- dados_jogadores_w_n_jogos %>% filter(nr_player == 'L1')
jogadores_filtrados_l1 <- jogadores_filtrados_l1[,-2]

colnames(jogadores_filtrados_l1)[5] <- 'l_player1'

n = ncol(dados_sem_na_w_n_jogos)

dados_sem_na_w_n_jogos <- dados_sem_na_w_n_jogos %>%
  left_join(jogadores_filtrados_l1[,c(17:35)], by= 'match_player_id')


# Renomeando as variáveis para constar o 'l1' antes
for(i in (n+1):ncol(dados_sem_na_w_n_jogos)){
  colnames(dados_sem_na_w_n_jogos)[i] <- paste('l1_', colnames(dados_sem_na_w_n_jogos)[i], sep='' )
}



#### Inserindo os dados para os jogadores identificados como 'Jogador 2 - perdedor' ----

dados_sem_na_w_n_jogos$match_player_id <- paste(dados_sem_na_w_n_jogos$match_id,
                                                dados_sem_na_w_n_jogos$l_player2,
                                                dados_sem_na_w_n_jogos$date,
                                                dados_sem_na_w_n_jogos$match_num,
                                                sep=" - ")

jogadores_filtrados_l2 <- dados_jogadores_w_n_jogos %>% filter(nr_player == 'L2')
jogadores_filtrados_l2 <- jogadores_filtrados_l2[,-2]

colnames(jogadores_filtrados_l2)[5] <- 'l_player2'

n = ncol(dados_sem_na_w_n_jogos)

dados_sem_na_w_n_jogos <- dados_sem_na_w_n_jogos %>%
  left_join(jogadores_filtrados_l2[,c(17:35)], by= 'match_player_id')


# Renomeando as variáveis para constar o 'l2' antes
for(i in (n+1):ncol(dados_sem_na_w_n_jogos)){
  colnames(dados_sem_na_w_n_jogos)[i] <- paste('l2_', colnames(dados_sem_na_w_n_jogos)[i], sep='' )
}


#### Inserindo os dados relativos às duplas ----

duplas_todas_fem <- dados_gerais_duplas

duplas_todas_fem$match_id <- paste(duplas_todas_fem$match_id, duplas_todas_fem$date, sep = ' - ')

duplas_todas_fem <- duplas_todas_fem %>% arrange(player1, player2, date, match_num, match_id)

duplas_todas_fem$chave_dupla <- paste(duplas_todas_fem$player1, duplas_todas_fem$player2, sep = ' - ')


# Número de jogos juntos da dupla
duplas_todas_fem$nr_jogos <- 1

for(m in 1:(nrow(duplas_todas_fem)-1)){
  if(duplas_todas_fem[m+1, "chave_dupla"] == duplas_todas_fem[m, "chave_dupla"]){
    duplas_todas_fem[m+1, "nr_jogos"] <- duplas_todas_fem[m, "nr_jogos"] +1
  }
}


duplas_todas_fem$chave_dupla_result <- paste(duplas_todas_fem$player1, duplas_todas_fem$player2, duplas_todas_fem$result, sep = ' - ')


# Sequência de vitórias da duplas
duplas_todas_fem$streak_aux <- ifelse(duplas_todas_fem$result == 'L',-1,1)

duplas_todas_fem$streak <- 0

for(m in 2:nrow(duplas_todas_fem)){
  if(duplas_todas_fem[m-1, "chave_dupla"] == duplas_todas_fem[m, "chave_dupla"]){
    if(duplas_todas_fem[m-1,"streak_aux"] == -1){
      if(duplas_todas_fem[m-1,"streak"] >= 0){
        duplas_todas_fem[m, "streak"] <- -1
      } else{
        duplas_todas_fem[m, "streak"] <- duplas_todas_fem[m-1, "streak"] - 1
      }
    } else{
      if(duplas_todas_fem[m-1,"streak_aux"] == 1){
        if(duplas_todas_fem[m-1,"streak"] < 0){
          duplas_todas_fem[m, "streak"] <- 1
        } else{
          duplas_todas_fem[m, "streak"] <- duplas_todas_fem[m-1, "streak"] + 1
        }
      }
    }
  }
}


# Tempo entre cada jogo da dupla
duplas_todas_fem$tempo_entre_jogos <- 0

for(m in 1:(nrow(duplas_todas_fem)-1)){
  if(duplas_todas_fem[m+1, "chave_dupla"] == duplas_todas_fem[m, "chave_dupla"]){
    duplas_todas_fem[m+1,'tempo_entre_jogos'] <- difftime(duplas_todas_fem[m+1,"date"],duplas_todas_fem[m,"date"], units = 'days')
  }
}


duplas_todas_fem$titulos <- 0

for(i in 1:nrow(duplas_todas_fem)){
  duplas_todas_fem[i,"titulos"] <- nrow(dados_ult_jogo[which(dados_ult_jogo$dupla_campea == duplas_todas_fem[i, 'chave_dupla'] & 
                                                               dados_ult_jogo$date<duplas_todas_fem[i,"date"]),])
}



# Juntando as variáveis criadas ao dataframe com os jogos

duplas_todas_fem_w <- duplas_todas_fem %>% filter(result == 'W')
duplas_todas_fem_l <- duplas_todas_fem %>% filter(result == 'L')

dados_sem_na_w_n_jogos$match_id <- paste(dados_sem_na_w_n_jogos$match_id, dados_sem_na_w_n_jogos$date, sep=' - ')

dados_sem_na_w_n_jogos2 <- dados_sem_na_w_n_jogos %>%
  left_join(duplas_todas_fem_w[,c(1,8,11,12,13)], by= 'match_id')

colnames(dados_sem_na_w_n_jogos2)[c((ncol(dados_sem_na_w_n_jogos2)-3), (ncol(dados_sem_na_w_n_jogos2)-2), (ncol(dados_sem_na_w_n_jogos2)-1), ncol(dados_sem_na_w_n_jogos2))] <- c('nr_jogos_dupla_w', 'streak_dupla_w', 'tempo_entre_jogos_dupla_w', 'titulos_w')

dados_sem_na_w_n_jogos2 <- dados_sem_na_w_n_jogos2 %>%
  left_join(duplas_todas_fem_l[,c(1,8,11,12,13)], by= 'match_id')

colnames(dados_sem_na_w_n_jogos2)[c((ncol(dados_sem_na_w_n_jogos2)-3), (ncol(dados_sem_na_w_n_jogos2)-2), (ncol(dados_sem_na_w_n_jogos2)-1), ncol(dados_sem_na_w_n_jogos2))] <- c('nr_jogos_dupla_l', 'streak_dupla_l', 'tempo_entre_jogos_dupla_l', 'titulos_l')


#### Removendo todos os vazios e também os jogos em que houve descontinuidade antes ----

dados_fem_sexto_mais_jogo <- dados_sem_na_w_n_jogos2 %>% na.omit() %>% arrange(date, tournament, match_num)

dados_fem_sexto_mais_jogo <- dados_fem_sexto_mais_jogo[which(!(dados_fem_sexto_mais_jogo$w1_match_player_id %in% jogos_sem_na_desconsiderar$match_player_id) &
                                                               !(dados_fem_sexto_mais_jogo$w2_match_player_id %in% jogos_sem_na_desconsiderar$match_player_id) &
                                                               !(dados_fem_sexto_mais_jogo$l1_match_player_id %in% jogos_sem_na_desconsiderar$match_player_id) &
                                                               !(dados_fem_sexto_mais_jogo$l2_match_player_id %in% jogos_sem_na_desconsiderar$match_player_id)),]


# Criando variável para definir se algum jogador de cada dupla está jogando em casa
dados_fem_sexto_mais_jogo$dupla_w_home <- ifelse(dados_fem_sexto_mais_jogo$w_p1_country == dados_fem_sexto_mais_jogo$country | dados_fem_sexto_mais_jogo$w_p2_country == dados_fem_sexto_mais_jogo$country, 1,0)
dados_fem_sexto_mais_jogo$dupla_l_home <- ifelse(dados_fem_sexto_mais_jogo$l_p1_country == dados_fem_sexto_mais_jogo$country | dados_fem_sexto_mais_jogo$l_p2_country == dados_fem_sexto_mais_jogo$country, 1,0)

# Criando variáveis agregadas para a dupla vencedora
dados_fem_sexto_mais_jogo$dupla_w_avg_age <- (dados_fem_sexto_mais_jogo$w_p1_age + dados_fem_sexto_mais_jogo$w_p2_age)/2
dados_fem_sexto_mais_jogo$dupla_w_avg_hgt <- (dados_fem_sexto_mais_jogo$w_p1_hgt + dados_fem_sexto_mais_jogo$w_p2_hgt)/2
dados_fem_sexto_mais_jogo$dupla_w_sma_avg_aces_points <- dados_fem_sexto_mais_jogo$w1_sma_avg_aces_points + dados_fem_sexto_mais_jogo$w2_sma_avg_aces_points
dados_fem_sexto_mais_jogo$dupla_w_sma_avg_attacks_points <- dados_fem_sexto_mais_jogo$w1_sma_avg_attacks_points + dados_fem_sexto_mais_jogo$w2_sma_avg_attacks_points
dados_fem_sexto_mais_jogo$dupla_w_sma_avg_kills_points <- dados_fem_sexto_mais_jogo$w1_sma_avg_kills_points + dados_fem_sexto_mais_jogo$w2_sma_avg_kills_points
dados_fem_sexto_mais_jogo$dupla_w_sma_avg_blocks_points <- dados_fem_sexto_mais_jogo$w1_sma_avg_blocks_points + dados_fem_sexto_mais_jogo$w2_sma_avg_blocks_points
dados_fem_sexto_mais_jogo$dupla_w_sma_avg_digs_points <- dados_fem_sexto_mais_jogo$w1_sma_avg_digs_points + dados_fem_sexto_mais_jogo$w2_sma_avg_digs_points
dados_fem_sexto_mais_jogo$dupla_w_sma_avg_errors_points <- dados_fem_sexto_mais_jogo$w1_sma_avg_errors_points + dados_fem_sexto_mais_jogo$w2_sma_avg_errors_points
dados_fem_sexto_mais_jogo$dupla_w_sma_avg_serve_errors_points <- dados_fem_sexto_mais_jogo$w1_sma_avg_serve_errors_points + dados_fem_sexto_mais_jogo$w2_sma_avg_serve_errors_points
dados_fem_sexto_mais_jogo$dupla_w_sma_hitpct <- ((dados_fem_sexto_mais_jogo$w1_sma_kills+dados_fem_sexto_mais_jogo$w2_sma_kills)
                                                 - (dados_fem_sexto_mais_jogo$w1_sma_errors + dados_fem_sexto_mais_jogo$w2_sma_errors))/
  (dados_fem_sexto_mais_jogo$w1_sma_attacks + dados_fem_sexto_mais_jogo$w2_sma_attacks)


# Criando variáveis agregadas para a dupla perdedora
dados_fem_sexto_mais_jogo$dupla_l_avg_age <- (dados_fem_sexto_mais_jogo$l_p1_age + dados_fem_sexto_mais_jogo$l_p2_age)/2
dados_fem_sexto_mais_jogo$dupla_l_avg_hgt <- (dados_fem_sexto_mais_jogo$l_p1_hgt + dados_fem_sexto_mais_jogo$l_p2_hgt)/2
dados_fem_sexto_mais_jogo$dupla_l_sma_avg_aces_points <- dados_fem_sexto_mais_jogo$l1_sma_avg_aces_points + dados_fem_sexto_mais_jogo$l2_sma_avg_aces_points
dados_fem_sexto_mais_jogo$dupla_l_sma_avg_attacks_points <- dados_fem_sexto_mais_jogo$l1_sma_avg_attacks_points + dados_fem_sexto_mais_jogo$l2_sma_avg_attacks_points
dados_fem_sexto_mais_jogo$dupla_l_sma_avg_kills_points <- dados_fem_sexto_mais_jogo$l1_sma_avg_kills_points + dados_fem_sexto_mais_jogo$l2_sma_avg_kills_points
dados_fem_sexto_mais_jogo$dupla_l_sma_avg_blocks_points <- dados_fem_sexto_mais_jogo$l1_sma_avg_blocks_points + dados_fem_sexto_mais_jogo$l2_sma_avg_blocks_points
dados_fem_sexto_mais_jogo$dupla_l_sma_avg_digs_points <- dados_fem_sexto_mais_jogo$l1_sma_avg_digs_points + dados_fem_sexto_mais_jogo$l2_sma_avg_digs_points
dados_fem_sexto_mais_jogo$dupla_l_sma_avg_errors_points <- dados_fem_sexto_mais_jogo$l1_sma_avg_errors_points + dados_fem_sexto_mais_jogo$l2_sma_avg_errors_points
dados_fem_sexto_mais_jogo$dupla_l_sma_avg_serve_errors_points <- dados_fem_sexto_mais_jogo$l1_sma_avg_serve_errors_points + dados_fem_sexto_mais_jogo$l2_sma_avg_serve_errors_points
dados_fem_sexto_mais_jogo$dupla_l_sma_hitpct <- ((dados_fem_sexto_mais_jogo$l1_sma_kills+dados_fem_sexto_mais_jogo$l2_sma_kills)
                                                 - (dados_fem_sexto_mais_jogo$l1_sma_errors + dados_fem_sexto_mais_jogo$l2_sma_errors))/
  (dados_fem_sexto_mais_jogo$l1_sma_attacks + dados_fem_sexto_mais_jogo$l2_sma_attacks)


# Definindo variáveis para a dupla de referência -- dupla perdedora se variável "dupla_de_referencia" == 'L', dupla vencedora caso contrário

for(i in 1:nrow(dados_fem_sexto_mais_jogo)){
  if(dados_fem_sexto_mais_jogo[i,"dupla_de_referencia"] == 'L'){
    
    dados_fem_sexto_mais_jogo[i,"dupla_ref_titulos"] <- dados_fem_sexto_mais_jogo[i,"titulos_l"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_nr_jogos"] <- dados_fem_sexto_mais_jogo[i,"nr_jogos_dupla_l"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_streak"] <- dados_fem_sexto_mais_jogo[i,"streak_dupla_l"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_rank"] <- dados_fem_sexto_mais_jogo[i,"l_rank"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_home"] <- dados_fem_sexto_mais_jogo[i, "dupla_l_home"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_avg_age"] <- dados_fem_sexto_mais_jogo[i, "dupla_l_avg_age"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_avg_hgt"] <- dados_fem_sexto_mais_jogo[i, "dupla_l_avg_hgt"]
    
    dados_fem_sexto_mais_jogo[i,"dupla_ref_sma_avg_aces_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_l_sma_avg_aces_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_sma_avg_attacks_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_l_sma_avg_attacks_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_sma_avg_kills_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_l_sma_avg_kills_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_sma_avg_blocks_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_l_sma_avg_blocks_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_sma_avg_digs_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_l_sma_avg_digs_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_sma_avg_errors_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_l_sma_avg_errors_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_sma_avg_serve_errors_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_l_sma_avg_serve_errors_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_sma_hitpct"] <- dados_fem_sexto_mais_jogo[i,"dupla_l_sma_hitpct"]
    
    
    dados_fem_sexto_mais_jogo[i,"dupla_opp_titulos"] <- dados_fem_sexto_mais_jogo[i,"titulos_w"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_nr_jogos"] <- dados_fem_sexto_mais_jogo[i,"nr_jogos_dupla_w"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_streak"] <- dados_fem_sexto_mais_jogo[i,"streak_dupla_w"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_rank"] <- dados_fem_sexto_mais_jogo[i,"w_rank"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_home"] <- dados_fem_sexto_mais_jogo[i, "dupla_w_home"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_avg_age"] <- dados_fem_sexto_mais_jogo[i, "dupla_w_avg_age"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_avg_hgt"] <- dados_fem_sexto_mais_jogo[i, "dupla_w_avg_hgt"]
    
    dados_fem_sexto_mais_jogo[i,"dupla_opp_sma_avg_aces_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_w_sma_avg_aces_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_sma_avg_attacks_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_w_sma_avg_attacks_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_sma_avg_kills_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_w_sma_avg_kills_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_sma_avg_blocks_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_w_sma_avg_blocks_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_sma_avg_digs_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_w_sma_avg_digs_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_sma_avg_errors_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_w_sma_avg_errors_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_sma_avg_serve_errors_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_w_sma_avg_serve_errors_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_sma_hitpct"] <- dados_fem_sexto_mais_jogo[i,"dupla_w_sma_hitpct"]
    
  }
  else{
    
    dados_fem_sexto_mais_jogo[i,"dupla_ref_titulos"] <- dados_fem_sexto_mais_jogo[i,"titulos_w"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_nr_jogos"] <- dados_fem_sexto_mais_jogo[i,"nr_jogos_dupla_w"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_streak"] <- dados_fem_sexto_mais_jogo[i,"streak_dupla_w"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_rank"] <- dados_fem_sexto_mais_jogo[i,"w_rank"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_home"] <- dados_fem_sexto_mais_jogo[i, "dupla_w_home"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_avg_age"] <- dados_fem_sexto_mais_jogo[i, "dupla_w_avg_age"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_avg_hgt"] <- dados_fem_sexto_mais_jogo[i, "dupla_w_avg_hgt"]
    
    dados_fem_sexto_mais_jogo[i,"dupla_ref_sma_avg_aces_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_w_sma_avg_aces_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_sma_avg_attacks_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_w_sma_avg_attacks_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_sma_avg_kills_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_w_sma_avg_kills_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_sma_avg_blocks_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_w_sma_avg_blocks_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_sma_avg_digs_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_w_sma_avg_digs_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_sma_avg_errors_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_w_sma_avg_errors_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_sma_avg_serve_errors_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_w_sma_avg_serve_errors_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_ref_sma_hitpct"] <- dados_fem_sexto_mais_jogo[i,"dupla_w_sma_hitpct"]
    
    
    dados_fem_sexto_mais_jogo[i,"dupla_opp_titulos"] <- dados_fem_sexto_mais_jogo[i,"titulos_l"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_nr_jogos"] <- dados_fem_sexto_mais_jogo[i,"nr_jogos_dupla_l"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_streak"] <- dados_fem_sexto_mais_jogo[i,"streak_dupla_l"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_rank"] <- dados_fem_sexto_mais_jogo[i,"l_rank"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_home"] <- dados_fem_sexto_mais_jogo[i, "dupla_l_home"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_avg_age"] <- dados_fem_sexto_mais_jogo[i, "dupla_l_avg_age"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_avg_hgt"] <- dados_fem_sexto_mais_jogo[i, "dupla_l_avg_hgt"]
    
    
    dados_fem_sexto_mais_jogo[i,"dupla_opp_sma_avg_aces_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_l_sma_avg_aces_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_sma_avg_attacks_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_l_sma_avg_attacks_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_sma_avg_kills_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_l_sma_avg_kills_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_sma_avg_blocks_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_l_sma_avg_blocks_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_sma_avg_digs_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_l_sma_avg_digs_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_sma_avg_errors_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_l_sma_avg_errors_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_sma_avg_serve_errors_points"] <- dados_fem_sexto_mais_jogo[i,"dupla_l_sma_avg_serve_errors_points"]
    dados_fem_sexto_mais_jogo[i,"dupla_opp_sma_hitpct"] <- dados_fem_sexto_mais_jogo[i,"dupla_l_sma_hitpct"]
    
    
  }
  
}

dados_fem_sexto_mais_jogo$dupla_ref_home <- as.factor(dados_fem_sexto_mais_jogo$dupla_ref_home)
dados_fem_sexto_mais_jogo$dupla_opp_home <- as.factor(dados_fem_sexto_mais_jogo$dupla_opp_home)
dados_fem_sexto_mais_jogo$dupla_de_referencia <- as.factor(dados_fem_sexto_mais_jogo$dupla_de_referencia)


#### Montando os modelos ----


variaveis_tentativa1_fem_ambos <- dados_fem_sexto_mais_jogo[,c("dupla_de_referencia","dupla_ref_nr_jogos", "dupla_ref_streak",
                                                               "dupla_ref_rank", 'dupla_ref_titulos',
                                                               "dupla_ref_home", 
                                                               "dupla_ref_avg_age",
                                                               "dupla_ref_avg_hgt", "dupla_ref_sma_avg_aces_points",
                                                               "dupla_ref_sma_avg_blocks_points",
                                                               "dupla_ref_sma_avg_digs_points", "dupla_ref_sma_hitpct",
                                                               "dupla_ref_sma_avg_serve_errors_points", "dupla_opp_nr_jogos",
                                                               "dupla_opp_streak",
                                                               "dupla_opp_rank", 'dupla_opp_titulos',
                                                               "dupla_opp_home",
                                                               "dupla_opp_avg_age","dupla_opp_avg_hgt", 
                                                               "dupla_opp_sma_avg_aces_points",
                                                               "dupla_opp_sma_avg_blocks_points",
                                                               "dupla_opp_sma_avg_digs_points", "dupla_opp_sma_hitpct",
                                                               "dupla_opp_sma_avg_serve_errors_points")]
##### Testando para vários dados de treino e teste ----

for(z in 1:200){
  
  print(paste('z = ', z, '; fem ambos', sep=''))
  
  training.samples <- variaveis_tentativa1_fem_ambos$dupla_de_referencia %>%
    createDataPartition(p = 0.8, list = FALSE)
  train.data  <- variaveis_tentativa1_fem_ambos[training.samples, ]
  
  train.fem.ambos <- train.data
  
  
  # Modelo completo
  
  full.model2_fem <- glm(dupla_de_referencia ~., data = train.data, family = binomial)
  
  # Modelo backward stepwise
  step.model.bck2_fem <- full.model2_fem %>% stepAIC(trace = FALSE, direction = 'backward')
  
  
  # Estão no modelo completo e não no modelo stepwise backward
  for(i in 1:length(coef(full.model2_fem))){
    if(!(names(coef(full.model2_fem))[i] %in% names(coef(step.model.bck2_fem)))){
      compl_nao_backward2_fem <- rbind(compl_nao_backward2_fem, cbind(names(coef(full.model2_fem))[i], z))
    }
  }
  
}


##### Selecionando variáveis ----

compl_nao_backward_masc_grp <- as.data.frame(compl_nao_backward2) %>% group_by(V1) %>% summarise(
  tot=n()
) %>% arrange(-tot)

compl_nao_backward_masc_grp[which(compl_nao_backward_masc_grp$V1 == 'dupla_opp_home1'),'V1'] <- 'dupla_opp_home'
compl_nao_backward_masc_grp[which(compl_nao_backward_masc_grp$V1 == 'dupla_ref_home1'),'V1'] <- 'dupla_ref_home'


colnames(compl_nao_backward_masc_grp) <- c("Variável", 'Qtd Remoções')

compl_nao_backward_masc_grp%>%gt() %>%
  tab_style(
    style = list(cell_fill(color = 'red'),
                 cell_text(weight = 'bold')),
    locations = cells_body(columns = `Qtd Remoções`, 
                           rows = which(compl_nao_backward_masc_grp$`Qtd Remoções` >= 100))) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_column_labels()) %>%
  tab_options(
    table.font.size = 11,
    data_row.padding = px(3)
  )


compl_nao_backward_fem_grp <- as.data.frame(compl_nao_backward2_fem) %>% group_by(V1) %>% summarise(
  tot=n()
) %>% arrange(-tot)


compl_nao_backward_fem_grp[which(compl_nao_backward_fem_grp$V1 == 'dupla_opp_home1'),'V1'] <- 'dupla_opp_home'
compl_nao_backward_fem_grp[which(compl_nao_backward_fem_grp$V1 == 'dupla_ref_home1'),'V1'] <- 'dupla_ref_home'


colnames(compl_nao_backward_fem_grp) <- c("Variável", 'Qtd Remoções')

compl_nao_backward_fem_grp%>%gt() %>%
  tab_style(
    style = list(cell_fill(color = 'red'),
                 cell_text(weight = 'bold')),
    locations = cells_body(columns = `Qtd Remoções`, 
                           rows = which(compl_nao_backward_fem_grp$`Qtd Remoções` >= 100))) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_column_labels()) %>%
  tab_options(
    table.font.size = 11,
    data_row.padding = px(3)
  )



compl_nao_backward_masc_grp$desconsid <- ifelse(compl_nao_backward_masc_grp$`Qtd Remoções`>100, 'sim', 'nao')
compl_nao_backward_fem_grp$desconsid <- ifelse(compl_nao_backward_fem_grp$`Qtd Remoções`>100, 'sim', 'nao')



descons_masc <- compl_nao_backward_masc_grp[which(compl_nao_backward_masc_grp$desconsid == 'sim'), "Variável"]
descons_fem <- compl_nao_backward_fem_grp[which(compl_nao_backward_fem_grp$desconsid == 'sim'), "Variável"]


var_selec_masc <- variaveis_tentativa1_masc_ambos[,which(!(colnames(variaveis_tentativa1_masc_ambos) %in%
                                                             descons_masc$Variável))]

var_selec_fem <- variaveis_tentativa1_fem_ambos[,which(!(colnames(variaveis_tentativa1_fem_ambos) %in%
                                                           descons_fem$Variável))]




#### Comparando os modelos sem as variáveis do passo acima e o modelo com todas as variáveis ----

##### MASCULINO ----

for(z in 1:20){
  
  print(paste('z = ', z, '; masc', sep=''))
  variaveis_tentativa1_masc_ambos$dupla_ref_rank <- as.integer(variaveis_tentativa1_masc_ambos$dupla_ref_rank)
  variaveis_tentativa1_masc_ambos$dupla_opp_rank <- as.integer(variaveis_tentativa1_masc_ambos$dupla_opp_rank)
  
  
  training.samples <- variaveis_tentativa1_masc_ambos$dupla_de_referencia %>%
    createDataPartition(p = 0.8, list = FALSE)
  train.data  <- variaveis_tentativa1_masc_ambos[training.samples,-26 ]
  test.data <- variaveis_tentativa1_masc_ambos[-training.samples, -26]
  
  train.masc.ambos <- train.data
  test.masc.ambos <- test.data
  
  # Modelo completo
  
  full.model2_masc <- glm(dupla_de_referencia ~., data = train.data, family = binomial)
  
  # AIC e R2 para modelo completo
  aic_full_model[z] <- full.model2_masc[["aic"]]
  r2_mcfad_full_model[z] <- pR2(full.model2_masc)['McFadden']
  
  # Verificando os resíduos do modelo completo
  for(i in 1:10){
    env_full_masc<- as.data.frame(envelope(full.model2_masc, plot.it = F))
    env_full_masc$dentro <- ifelse(env_full_masc$Residuals <= env_full_masc$`Upper limit` & env_full_masc$Residuals >= env_full_masc$`Lower limit`, 'sim', 'nao')
    perc_dentro_full_masc <- table(env_full_masc$dentro)[2]/sum(table(env_full_masc$dentro))
    
    if(!is.na(perc_dentro_full_masc)){
      percs_dentro_full_masc[i] <- perc_dentro_full_masc
    } else{
      df_temp <- as.data.frame(table(env_full_masc$dentro))
      if(df_temp[1,"Var1"] == 'sim'){
        percs_dentro_full_masc[i] <- 1
      } else{
        percs_dentro_full_masc[i] <- 0
      }
    }
  }
  
  avgs_dentro_full_masc[z] <- mean(percs_dentro_full_masc)
  
  # Modelo backward stepwise
  
  var_selec_masc$dupla_ref_rank <- as.integer(var_selec_masc$dupla_ref_rank)
  var_selec_masc$dupla_opp_rank <- as.integer(var_selec_masc$dupla_opp_rank)
  
  train.data  <- var_selec_masc[training.samples, -26]
  test.data <- var_selec_masc[-training.samples, -26]
  
  train.masc.bck <- train.data
  test.masc.bck <- test.data
  
  
  var_selec_mod_masc <- glm(dupla_de_referencia ~., data = train.data, family = binomial)
  
  # AIC e R2 para modelo backward stepwise
  aic_bck_model[z] <- var_selec_mod_masc[["aic"]]
  r2_mcfad_bck_model[z] <- pR2(var_selec_mod_masc)['McFadden']
  
  # Verificando os resíduos do modelo backward stepwise
  for(i in 1:10){
    env_bck_masc<- as.data.frame(envelope(var_selec_mod_masc, plot.it = F))
    env_bck_masc$dentro <- ifelse(env_bck_masc$Residuals <= env_bck_masc$`Upper limit` & env_bck_masc$Residuals >= env_bck_masc$`Lower limit`, 'sim', 'nao')
    perc_dentro_bck_masc <- table(env_bck_masc$dentro)[2]/sum(table(env_bck_masc$dentro))
    
    if(!is.na(perc_dentro_bck_masc)){
      percs_dentro_bck_masc[i] <- perc_dentro_bck_masc
    } else{
      df_temp <- as.data.frame(table(env_bck_masc$dentro))
      if(df_temp[1,"Var1"] == 'sim'){
        percs_dentro_bck_masc[i] <- 1
      } else{
        percs_dentro_bck_masc[i] <- 0
      }
    }
  }
  
  avgs_dentro_bck_masc[z] <- mean(percs_dentro_bck_masc)
  
  
  # Predições modelo completo
  
  # Fazendo predições
  probabilities <- full.model2_masc %>% predict(test.masc.ambos, type = "response")
  predicted.classes <- ifelse(probabilities > 0.5, "W", "L")
  
  # # Medindo a acurácia
  observed.classes <- test.masc.ambos$dupla_de_referencia
  avg_full <- mean(predicted.classes == observed.classes)
  
  cm_full_masc <- confusionMatrix(as.factor(predicted.classes), observed.classes, positive = 'W')
  sens <- cm_full_masc$byClass["Sensitivity"]
  spec <- cm_full_masc$byClass["Specificity"]
  
  if(length(sens_full_model_masc)==0){
    sens_full_model_masc <- sens
  } else{
    sens_full_model_masc <- cbind(sens_full_model_masc, sens)
  }
  
  if(length(spec_full_model_masc)==0){
    spec_full_model_masc <- spec
  } else{
    spec_full_model_masc <- cbind(spec_full_model_masc, spec)
  }
  
  if(length(avgs_full_model)==0){
    avgs_full_model <- avg_full
  } else{
    avgs_full_model <- cbind(avgs_full_model, avg_full)
  }
  
  
  
  #Predições modelo backward stepwise
  
  # Fazendo predições
  probabilities <- var_selec_mod_masc %>% predict(test.masc.bck, type = "response")
  predicted.classes <- ifelse(probabilities > 0.5, "W", "L")
  
  # Medindo a acurácia
  observed.classes <- test.masc.bck$dupla_de_referencia
  avg_bck <- mean(predicted.classes == observed.classes)
  
  cm_bck_masc <- confusionMatrix(as.factor(predicted.classes), observed.classes, positive = 'W')
  sens <- cm_bck_masc$byClass["Sensitivity"]
  spec <- cm_bck_masc$byClass["Specificity"]
  
  if(length(sens_bck_model_masc)==0){
    sens_bck_model_masc <- sens
  } else{
    sens_bck_model_masc <- cbind(sens_bck_model_masc, sens)
  }
  
  if(length(spec_bck_model_masc)==0){
    spec_bck_model_masc <- spec
  } else{
    spec_bck_model_masc <- cbind(spec_bck_model_masc, spec)
  }
  
  if(length(avgs_bck_model)==0){
    avgs_bck_model <- avg_bck
  } else {
    avgs_bck_model <- cbind(avgs_bck_model, avg_bck)
  }
  
}

##### FEMININO ----

for(z in 1:20){
  
  print(paste('z = ', z, '; fem', sep=''))
  
  variaveis_tentativa1_fem_ambos$dupla_ref_rank <- as.integer(variaveis_tentativa1_fem_ambos$dupla_ref_rank)
  variaveis_tentativa1_fem_ambos$dupla_opp_rank <- as.integer(variaveis_tentativa1_fem_ambos$dupla_opp_rank)
  
  
  training.samples <- variaveis_tentativa1_fem_ambos$dupla_de_referencia %>%
    createDataPartition(p = 0.8, list = FALSE)
  train.data  <- variaveis_tentativa1_fem_ambos[training.samples, -26]
  test.data <- variaveis_tentativa1_fem_ambos[-training.samples, -26]
  
  train.fem.ambos <- train.data
  test.fem.ambos <- test.data
  
  # Modelo completo
  
  full.model2_fem <- glm(dupla_de_referencia ~., data = train.data, family = binomial)
  
  # AIC e R2 para modelo completo
  aic_full_model_fem[z] <- full.model2_fem[["aic"]]
  r2_mcfad_full_model_fem[z] <- pR2(full.model2_fem)['McFadden']
  
  # Verificando os resíduos do modelo completo
  for(i in 1:10){
    env_full_fem<- as.data.frame(envelope(full.model2_fem, plot.it = F))
    env_full_fem$dentro <- ifelse(env_full_fem$Residuals <= env_full_fem$`Upper limit` & env_full_fem$Residuals >= env_full_fem$`Lower limit`, 'sim', 'nao')
    perc_dentro_full_fem <- table(env_full_fem$dentro)[2]/sum(table(env_full_fem$dentro))
    
    if(!is.na(perc_dentro_full_fem)){
      percs_dentro_full_fem[i] <- perc_dentro_full_fem
    } else{
      df_temp <- as.data.frame(table(env_full_fem$dentro))
      if(df_temp[1,"Var1"] == 'sim'){
        percs_dentro_full_fem[i] <- 1
      } else{
        percs_dentro_full_fem[i] <- 0
      }
    }
  }
  
  avgs_dentro_full_fem[z] <- mean(percs_dentro_full_fem)
  
  # Modelo backward stepwise
  
  train.data  <- var_selec_fem[training.samples, ]
  test.data <- var_selec_fem[-training.samples, ]
  
  var_selec_fem$dupla_ref_rank <- as.integer(var_selec_fem$dupla_ref_rank)
  var_selec_fem$dupla_opp_rank <- as.integer(var_selec_fem$dupla_opp_rank)
  
  train.fem.bck <- train.data
  test.fem.bck <- test.data
  
  
  var_selec_mod_fem <- glm(dupla_de_referencia ~., data = train.data, family = binomial)
  
  # AIC e R2 para modelo backward stepwise
  aic_bck_model_fem[z] <- var_selec_mod_fem[["aic"]]
  r2_mcfad_bck_model_fem[z] <- pR2(var_selec_mod_fem)['McFadden']
  
  # Verificando os resíduos do modelo backward stepwise
  for(i in 1:10){
    env_bck_fem<- as.data.frame(envelope(var_selec_mod_fem, plot.it = F))
    env_bck_fem$dentro <- ifelse(env_bck_fem$Residuals <= env_bck_fem$`Upper limit` & env_bck_fem$Residuals >= env_bck_fem$`Lower limit`, 'sim', 'nao')
    perc_dentro_bck_fem <- table(env_bck_fem$dentro)[2]/sum(table(env_bck_fem$dentro))
    
    if(!is.na(perc_dentro_bck_fem)){
      percs_dentro_bck_fem[i] <- perc_dentro_bck_fem
    } else{
      df_temp <- as.data.frame(table(env_bck_fem$dentro))
      if(df_temp[1,"Var1"] == 'sim'){
        percs_dentro_bck_fem[i] <- 1
      } else{
        percs_dentro_bck_fem[i] <- 0
      }
    }
  }
  
  avgs_dentro_bck_fem[z] <- mean(percs_dentro_bck_fem)
  
  
  # Predições modelo completo
  
  # Fazendo predições
  probabilities <- full.model2_fem %>% predict(test.fem.ambos, type = "response")
  predicted.classes <- ifelse(probabilities > 0.5, "W", "L")
  
  # # Medindo a acurácia
  observed.classes <- test.fem.ambos$dupla_de_referencia
  avg_full <- mean(predicted.classes == observed.classes)
  
  cm_full_fem <- confusionMatrix(as.factor(predicted.classes), observed.classes, positive = 'W')
  sens <- cm_full_fem$byClass["Sensitivity"]
  spec <- cm_full_fem$byClass["Specificity"]
  
  if(length(sens_full_model_fem)==0){
    sens_full_model_fem <- sens
  } else{
    sens_full_model_fem <- cbind(sens_full_model_fem, sens)
  }
  
  if(length(spec_full_model_fem)==0){
    spec_full_model_fem <- spec
  } else{
    spec_full_model_fem <- cbind(spec_full_model_fem, spec)
  }
  
  if(length(avgs_full_model_fem)==0){
    avgs_full_model_fem <- avg_full
  } else{
    avgs_full_model_fem <- cbind(avgs_full_model_fem, avg_full)
  }
  
  
  
  #Predições modelo backward stepwise
  
  # Fazendo predições
  probabilities <- var_selec_mod_fem %>% predict(test.fem.bck, type = "response")
  predicted.classes <- ifelse(probabilities > 0.5, "W", "L")
  
  # Medindo a acurácia
  observed.classes <- test.fem.bck$dupla_de_referencia
  avg_bck <- mean(predicted.classes == observed.classes)
  
  
  cm_bck_fem <- confusionMatrix(as.factor(predicted.classes), observed.classes, positive = 'W')
  sens <- cm_bck_fem$byClass["Sensitivity"]
  spec <- cm_bck_fem$byClass["Specificity"]
  
  if(length(sens_bck_model_fem)==0){
    sens_bck_model_fem <- sens
  } else{
    sens_bck_model_fem <- cbind(sens_bck_model_fem, sens)
  }
  
  if(length(spec_bck_model_fem)==0){
    spec_bck_model_fem <- spec
  } else{
    spec_bck_model_fem <- cbind(spec_bck_model_fem, spec)
  }
  
  
  if(length(avgs_bck_model_fem)==0){
    avgs_bck_model_fem <- avg_bck
  } else {
    avgs_bck_model_fem <- cbind(avgs_bck_model_fem, avg_bck)
  }
  
}


#### Verificando a correlação entre as variáveis ----

# MASCULINO

# Modelo completo

correlacoes_full_masc_1 <- as.data.frame(variaveis_tentativa1_masc_ambos[,c(2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25)])

correlacoes_full_masc <- round(cor(correlacoes_full_masc_1),2)


rownames(correlacoes_full_masc) <- c('ref_nr_jogos','ref_streak', 'ref_rank', 'ref_titulos', 'ref_avg_age', 'ref_avg_hgt', 'ref_sma_avg_aces', 'ref_sma_avg_blocks',
                                     'ref_sma_avg_digs', 'ref_sma_hitpct', 'ref_sma_avg_serve_errors','opp_nr_jogos', 'opp_streak', 'opp_rank', 'opp_titulos', 'opp_avg_age',
                                     'opp_avg_hgt', 'opp_sma_avg_aces', 'opp_sma_avg_blocks', 'opp_sma_avg_digs',
                                     'opp_sma_hitpct', 'opp_sma_avg_serve_errors')

colnames(correlacoes_full_masc) <- rownames(correlacoes_full_masc)


corrplot(correlacoes_full_masc, type = "upper", order = "hclust", method = 'color',
         tl.col = "black", tl.srt = 45, tl.cex=0.6, diag= T, addCoef.col='black', 
         number.cex = 0.6)

max(correlacoes_full_masc[which(correlacoes_full_masc!=1)])
min(correlacoes_full_masc[which(correlacoes_full_masc!=1)])



# Modelo com seleção de variáveis

correlacoes_bck_masc_1 <- as.data.frame(var_selec_masc[, which(sapply(var_selec_masc,is.numeric))])

correlacoes_bck_masc <- round(cor(correlacoes_bck_masc_1),2)



corrplot(correlacoes_bck_masc, type = "upper", order = "hclust", method = 'color',
         tl.col = "black", tl.srt = 45, tl.cex=1, diag= T, addCoef.col='black', 
         number.cex = 1)

max(correlacoes_bck_masc[which(correlacoes_bck_masc!=1)])
min(correlacoes_bck_masc[which(correlacoes_bck_masc!=1)])




# FEMININO

# Modelo completo

correlacoes_full_fem_1 <- as.data.frame(variaveis_tentativa1_fem_ambos[,c(2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25)])

correlacoes_full_fem <- round(cor(correlacoes_full_fem_1),2)


rownames(correlacoes_full_fem) <- c('ref_nr_jogos','ref_streak', 'ref_rank', 'ref_titulos', 'ref_avg_age', 'ref_avg_hgt', 'ref_sma_avg_aces', 'ref_sma_avg_blocks',
                                    'ref_sma_avg_digs', 'ref_sma_hitpct', 'ref_sma_avg_serve_errors','opp_nr_jogos', 'opp_streak', 'opp_rank', 'opp_titulos', 'opp_avg_age',
                                    'opp_avg_hgt', 'opp_sma_avg_aces', 'opp_sma_avg_blocks', 'opp_sma_avg_digs',
                                    'opp_sma_hitpct', 'opp_sma_avg_serve_errors')

colnames(correlacoes_full_fem) <- rownames(correlacoes_full_fem)


corrplot(correlacoes_full_fem, type = "upper", order = "hclust", method = 'color',
         tl.col = "black", tl.srt = 45,  tl.cex=0.6, diag= T, addCoef.col='black', 
         number.cex = 0.6)

max(correlacoes_full_fem[which(correlacoes_full_fem!=1)])
min(correlacoes_full_fem[which(correlacoes_full_fem!=1)])



# Modelo com seleção de variáveis

correlacoes_bck_fem_1 <- as.data.frame(var_selec_fem[,which(sapply(var_selec_fem,is.numeric))])


correlacoes_bck_fem <- round(cor(correlacoes_bck_fem_1),2)



corrplot(correlacoes_bck_fem, type = "upper", order = "hclust", method = 'color',
         tl.col = "black", tl.srt = 45, tl.cex=1, diag= T, addCoef.col='black', 
         number.cex = 1)

max(correlacoes_bck_fem[which(correlacoes_bck_fem!=1)])
min(correlacoes_bck_fem[which(correlacoes_bck_fem!=1)])



# Nota-se uma diminuição no índice de correlação ao se remover as variáveis selecionadas


#### Verificando o diagnóstico dos resíduos ----

mean(avgs_dentro_full_masc)
envelope(full.model2_masc, plot.it = T, rep=100, ylab = 'Quantis Observados', xlab = 'Quantis Esperados', main = 'Modelo Completo - Masculino')

mean(avgs_dentro_bck_masc)
envelope(var_selec_mod_masc, plot.it = T, ylab = 'Quantis Observados', xlab = 'Quantis Esperados', main = 'Modelo com Variáveis Selecionadas - Masculino')
mean(avgs_dentro_full_fem)
envelope(full.model2_fem, plot.it = T, ylab = 'Quantis Observados', xlab = 'Quantis Esperados', main = 'Modelo Completo - Feminino')
mean(avgs_dentro_bck_fem)
envelope(var_selec_mod_fem, plot.it = T, ylab = 'Quantis Observados', xlab = 'Quantis Esperados', main = 'Modelo com Variáveis Selecionadas - Feminino')

par(mfrow=c(1,1))
diagn_resid <- as.data.frame(rbind(cbind('Completo - Masculino',round(mean(avgs_dentro_full_masc),3)),
                                   cbind('Seleção de Variáveis - Masculino',round(mean(avgs_dentro_bck_masc),3)),
                                   cbind('Completo - Feminino',round(mean(avgs_dentro_full_fem),3)),
                                   cbind('Seleção de Variáveis - Feminino',round(mean(avgs_dentro_bck_fem),3))))

colnames(diagn_resid) <- c('Modelo', '% dentro dos limites')

diagn_resid %>% gt() %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_body(columns = Modelo)) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_body(columns = `% dentro dos limites`)) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_column_labels())

boxplot(percs_dentro_bck_fem)
boxplot(avgs_dentro_bck_fem)

#### Comparando as medidas de ajuste dos modelos ----

# AIC
dif_aic_masc <-c()
dif_aic_fem <-c()

for(i in 1:length(aic_full_model)){
  if(aic_full_model[i] < aic_bck_model[i]){
    dif_aic_masc[i] <- 'Modelo completo melhor'
  } else{
    dif_aic_masc[i] <- 'Modelo backward melhor'
  }
  if(aic_full_model_fem[i] < aic_bck_model_fem[i]){
    dif_aic_fem[i] <- 'Modelo completo melhor'
  } else{
    dif_aic_fem[i] <- 'Modelo backward melhor'
  }
}


# Nota-se que os modelos sem as variaveis sempre apresentam menor aic
table(dif_aic_masc)
table(dif_aic_fem)
mean(aic_full_model)
mean(aic_bck_model)
mean(aic_full_model_fem)
mean(aic_bck_model_fem)


# R2 de McFadden
dif_r2_masc <-c()
dif_r2_fem <-c()

for(i in 1:length(r2_mcfad_full_model)){
  if(r2_mcfad_full_model[i] > r2_mcfad_bck_model[i]){
    dif_r2_masc[i] <- 'Modelo completo melhor'
  } else{
    dif_r2_masc[i] <- 'Modelo backward melhor'
  }
  if(r2_mcfad_full_model_fem[i] > r2_mcfad_bck_model_fem[i]){
    dif_r2_fem[i] <- 'Modelo completo melhor'
  } else{
    dif_r2_fem[i] <- 'Modelo backward melhor'
  }
}

# Nota-se que o modelo completo possui r2 levemente superior em todos os casos
table(dif_r2_masc)
table(dif_r2_fem)
mean(r2_mcfad_full_model)
mean(r2_mcfad_bck_model)
mean(r2_mcfad_full_model_fem)
mean(r2_mcfad_bck_model_fem)


# Montando a tabela com o r2 e o AIC para cada um dos modelos

ajuste_modelos <- as.data.frame(rbind(cbind('Completo - Masculino',round(mean(aic_full_model),3), round(mean(r2_mcfad_full_model),3)),
                                      cbind('Seleção de Variáveis - Masculino',round(mean(aic_bck_model),3), round(mean(r2_mcfad_bck_model),3)),
                                      cbind('Completo - Feminino',round(mean(aic_full_model_fem),3), round(mean(r2_mcfad_full_model_fem),3)),
                                      cbind('Seleção de Variáveis - Feminino',round(mean(aic_bck_model_fem),3), round(mean(r2_mcfad_bck_model_fem),3))))

colnames(ajuste_modelos) <- c('Modelo', 'AIC', 'R²')

ajuste_modelos %>% gt() %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_body(columns = Modelo)) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_column_labels())



#### Comparando os coeficientes dos modelos ----

#MASCULINO
sum_full_masc <- summary(full.model2_masc)
coef_full_masc <- as.data.frame(sum_full_masc[["coefficients"]])
coef_full_masc$Variável <- rownames(coef_full_masc)
coef_full_masc$Modelo = 'Completo'
sum_bck_masc <- summary(var_selec_mod_masc)
coef_bck_masc <- as.data.frame(sum_bck_masc[["coefficients"]])
coef_bck_masc$Variável <- rownames(coef_bck_masc)
coef_bck_masc$Modelo <- 'Seleção de Variáveis'


coefs_masc <- coef_full_masc[,c("Variável","Estimate", "Std. Error", 'Pr(>|z|)')] %>%
  left_join(coef_bck_masc[,c("Variável","Estimate", "Std. Error", 'Pr(>|z|)')], by = 'Variável')

coefs_masc$Variável <- c('Intercepto', 'ref_nr_jogos','ref_streak', 'ref_rank', 'ref_titulos', 
                         'ref_home', 'ref_avg_age', 'ref_avg_hgt', 'ref_sma_avg_aces', 
                         'ref_sma_avg_blocks','ref_sma_avg_digs', 'ref_sma_hitpct', 
                         'ref_sma_avg_serve_errors','opp_nr_jogos', 'opp_streak', 'opp_rank', 
                         'opp_titulos', 'opp_home', 'opp_avg_age','opp_avg_hgt', 'opp_sma_avg_aces', 
                         'opp_sma_avg_blocks', 'opp_sma_avg_digs','opp_sma_hitpct', 
                         'opp_sma_avg_serve_errors')

coefs_masc$Estimate.x <- format(round(coefs_masc$Estimate.x, digits=3), nsmalls=3)
coefs_masc$`Std. Error.x` <- format(round(coefs_masc$`Std. Error.x`, digits=3), nsmalls=3)
coefs_masc$`Pr(>|z|).x` <- format(round(coefs_masc$`Pr(>|z|).x`, digits=3), nsmalls=3)
coefs_masc$Estimate.y <- format(round(coefs_masc$Estimate.y, digits=3), nsmalls=3)
coefs_masc$`Std. Error.y` <- format(round(coefs_masc$`Std. Error.y`, digits=3), nsmalls=3)
coefs_masc$`Pr(>|z|).y` <- format(round(coefs_masc$`Pr(>|z|).y`, digits=3), nsmalls=3)


coefs_masc[coefs_masc$Estimate.y =='     NA', c("Estimate.y", "Std. Error.y", "Pr(>|z|).y")] <- '-'




coefs_masc %>% gt() %>%
  tab_spanner(
    label = 'Modelo Completo',
    columns = c(Estimate.x, `Std. Error.x`, `Pr(>|z|).x`)
  ) %>%
  tab_spanner(
    label = 'Modelo com Seleção de Variáveis',
    columns = c(Estimate.y, `Std. Error.y`, `Pr(>|z|).y`)
  ) %>%
  cols_label(
    Estimate.x = 'Estimativa',
    `Std. Error.x` = 'DP',
    `Pr(>|z|).x` = 'P-valor',
    Estimate.y = 'Estimativa',
    `Std. Error.y` = 'DP',
    `Pr(>|z|).y` = 'P-valor'
  ) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_row_groups( groups= everything())) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_column_spanners(spanners= everything())) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_column_labels()) %>%
  tab_options(
    table.font.size = 15,
    data_row.padding = px(3.5)
  )

# AGORA APENAS COM AS VARIAVEIS SELECIONADAS

coefs_masc_sel <- coefs_masc[which(coefs_masc$Estimate.y != '-'), c("Variável","Estimate.y", "Std. Error.y", 'Pr(>|z|).y')]

coefs_masc_sel %>% gt() %>%
  tab_spanner(
    label = 'Modelo com Seleção de Variáveis',
    columns = c(Estimate.y, `Std. Error.y`, `Pr(>|z|).y`)
  ) %>%
  cols_label(
    Estimate.y = 'Estimativa',
    `Std. Error.y` = 'DP',
    `Pr(>|z|).y` = 'P-valor'
  ) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_body(columns = Variável)) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_row_groups( groups= everything())) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_column_spanners(spanners= everything())) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_column_labels()) %>%
  tab_options(
    table.font.size = 12,
    data_row.padding = px(3)
  )




# FEMININO

sum_full_fem <- summary(full.model2_fem)
coef_full_fem <- as.data.frame(sum_full_fem[["coefficients"]])
coef_full_fem$Variável <- rownames(coef_full_fem)
coef_full_fem$Modelo = 'Completo'
sum_bck_fem <- summary(var_selec_mod_fem)
coef_bck_fem <- as.data.frame(sum_bck_fem[["coefficients"]])
coef_bck_fem$Variável <- rownames(coef_bck_fem)
coef_bck_fem$Modelo <- 'Seleção de Variáveis'


coefs_fem <- coef_full_fem[,c("Variável","Estimate", "Std. Error", 'Pr(>|z|)')] %>%
  left_join(coef_bck_fem[,c("Variável","Estimate", "Std. Error", 'Pr(>|z|)')], by = 'Variável')

coefs_fem$Variável <- c('Intercepto', 'ref_nr_jogos','ref_streak', 'ref_rank', 'ref_titulos', 
                        'ref_home', 'ref_avg_age', 'ref_avg_hgt', 'ref_sma_avg_aces', 
                        'ref_sma_avg_blocks','ref_sma_avg_digs', 'ref_sma_hitpct', 
                        'ref_sma_avg_serve_errors','opp_nr_jogos', 'opp_streak', 'opp_rank', 
                        'opp_titulos', 'opp_home', 'opp_avg_age','opp_avg_hgt', 'opp_sma_avg_aces', 
                        'opp_sma_avg_blocks', 'opp_sma_avg_digs','opp_sma_hitpct', 
                        'opp_sma_avg_serve_errors')

coefs_fem$Estimate.x <- format(round(coefs_fem$Estimate.x, digits=3), nsmalls=3)
coefs_fem$`Std. Error.x` <- format(round(coefs_fem$`Std. Error.x`, digits=3), nsmalls=3)
coefs_fem$`Pr(>|z|).x` <- format(round(coefs_fem$`Pr(>|z|).x`, digits=3), nsmalls=3)
coefs_fem$Estimate.y <- format(round(coefs_fem$Estimate.y, digits=3), nsmalls=3)
coefs_fem$`Std. Error.y` <- format(round(coefs_fem$`Std. Error.y`, digits=3), nsmalls=3)
coefs_fem$`Pr(>|z|).y` <- format(round(coefs_fem$`Pr(>|z|).y`, digits=3), nsmalls=3)


coefs_fem[coefs_fem$Estimate.y =='    NA', c("Estimate.y", "Std. Error.y", "Pr(>|z|).y")] <- '-'




coefs_fem %>% gt() %>%
  tab_spanner(
    label = 'Modelo Completo',
    columns = c(Estimate.x, `Std. Error.x`, `Pr(>|z|).x`)
  ) %>%
  tab_spanner(
    label = 'Modelo com Seleção de Variáveis',
    columns = c(Estimate.y, `Std. Error.y`, `Pr(>|z|).y`)
  ) %>%
  cols_label(
    Estimate.x = 'Estimativa',
    `Std. Error.x` = 'DP',
    `Pr(>|z|).x` = 'P-valor',
    Estimate.y = 'Estimativa',
    `Std. Error.y` = 'DP',
    `Pr(>|z|).y` = 'P-valor'
  ) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_row_groups( groups= everything())) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_column_spanners(spanners= everything())) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_column_labels()) %>%
  tab_options(
    table.font.size = 15,
    data_row.padding = px(3.5)
  )


# AGORA APENAS COM AS VARIAVEIS SELECIONADAS

coefs_fem_sel <- coefs_fem[which(coefs_fem$Estimate.y != '-'), c("Variável","Estimate.y", "Std. Error.y", 'Pr(>|z|).y')]

coefs_fem_sel %>% gt() %>%
  tab_spanner(
    label = 'Modelo com Seleção de Variáveis',
    columns = c(Estimate.y, `Std. Error.y`, `Pr(>|z|).y`)
  ) %>%
  cols_label(
    Estimate.y = 'Estimativa',
    `Std. Error.y` = 'DP',
    `Pr(>|z|).y` = 'P-valor'
  ) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_body(columns = Variável)) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_row_groups( groups= everything())) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_column_spanners(spanners= everything())) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_column_labels()) %>%
  tab_options(
    table.font.size = 12,
    data_row.padding = px(3)
  )



#### Comparando a precisão das previsões ----

dif_acc_masc <-c()
dif_acc_fem <-c()

for(i in 1:length(avgs_full_model)){
  if(avgs_full_model[i] >avgs_bck_model[i]){
    dif_acc_masc[i] <- 'Modelo completo melhor'
  } else{
    dif_acc_masc[i] <- 'Modelo backward melhor'
  }
  if(avgs_full_model_fem[i] > avgs_bck_model_fem[i]){
    dif_acc_fem[i] <- 'Modelo completo melhor'
  } else{
    dif_acc_fem[i] <- 'Modelo backward melhor'
  }
}

# Nota-se que o modelo onde foi feita a seleção de variáveis tendem a ter acurácia de previsões superior
df_dif_acc_masc <- as.data.frame(table(dif_acc_masc))
df_dif_acc_fem <- as.data.frame(table(dif_acc_fem))
mean(avgs_full_model)
mean(avgs_bck_model)
mean(avgs_full_model_fem)
mean(avgs_bck_model_fem)


# Montando a tabela com o r2 e o AIC para cada um dos modelos

precisao_previsoes <- as.data.frame(rbind(cbind('Completo - Masculino',format(round(mean(avgs_full_model),digits = 3), nsmall=3), paste(df_dif_acc_masc[which(df_dif_acc_masc$dif_acc_masc == 'Modelo completo melhor'), 'Freq'],
                                                                                                                                        ' (', round(100*(df_dif_acc_masc[which(df_dif_acc_masc$dif_acc_masc == 'Modelo completo melhor'), 'Freq']/20),3),
                                                                                                                                        '%)', sep=''), format(round(mean(sens_full_model_masc),digits = 3), nsmall=3), format(round(mean(spec_full_model_masc),digits = 3), nsmall=3)),
                                          cbind('Seleção de Variáveis - Masculino',format(round(mean(avgs_bck_model),digits = 3), nsmall=3), paste(df_dif_acc_masc[which(df_dif_acc_masc$dif_acc_masc == 'Modelo backward melhor'), 'Freq'],
                                                                                                                                                   ' (', round(100*(df_dif_acc_masc[which(df_dif_acc_masc$dif_acc_masc == 'Modelo backward melhor'), 'Freq']/20),3),
                                                                                                                                                   '%)', sep=''), format(round(mean(sens_bck_model_masc),digits = 3), nsmall=3), format(round(mean(spec_bck_model_masc),digits = 3), nsmall=3)),
                                          cbind('Completo - Feminino',format(round(mean(avgs_full_model_fem),3), nsmall=3), paste(df_dif_acc_fem[which(df_dif_acc_fem$dif_acc_fem == 'Modelo completo melhor'), 'Freq'],
                                                                                                                                  ' (', round(100*(df_dif_acc_fem[which(df_dif_acc_fem$dif_acc_fem == 'Modelo completo melhor'), 'Freq']/20),3),
                                                                                                                                  '%)', sep=''), format(round(mean(sens_full_model_fem),digits = 3), nsmall=3), format(round(mean(spec_full_model_fem),digits = 3), nsmall=3)),
                                          cbind('Seleção de Variáveis - Feminino',format(round(mean(avgs_bck_model_fem),3), nsmall=3), paste(df_dif_acc_fem[which(df_dif_acc_fem$dif_acc_fem == 'Modelo backward melhor'), 'Freq'],
                                                                                                                                             ' (', round(100*(df_dif_acc_fem[which(df_dif_acc_fem$dif_acc_fem == 'Modelo backward melhor'), 'Freq']/20),3),
                                                                                                                                             '%)', sep=''), format(round(mean(sens_bck_model_fem),digits = 3), nsmall=3), format(round(mean(spec_bck_model_fem),digits = 3), nsmall=3))))

colnames(precisao_previsoes) <- c('Modelo', 'Acurácia', 'Qtd com precisão superior (%)', 'Sensibilidade', 'Especificidade')

precisao_previsoes %>% gt() %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_body(columns = Modelo)) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_column_labels())


# Boxplots

masc_comp_prev <- as.data.frame(cbind('Completo - M', t(avgs_full_model)))
masc_bck_prev <- as.data.frame(cbind('Seleção de Variáveis - M', t(avgs_bck_model)))
fem_comp_prev <- as.data.frame(cbind('Completo - F', t(avgs_full_model_fem)))
fem_bck_prev <- as.data.frame(cbind('Seleção de Variáveis - F', t(avgs_bck_model_fem)))

previsoes <- rbind(masc_comp_prev, masc_bck_prev, fem_comp_prev, fem_bck_prev)

previsoes$V2 <- as.numeric(previsoes$V2)

colnames(previsoes) <- c('Modelo', 'Acurácia')

ggplot(previsoes, aes(x = Modelo, y = Acurácia, fill = Modelo)) +
  geom_boxplot() +
  scale_fill_manual(values = c("blue", "red", "green", 'purple')) +
  theme_classic(base_size=16.5)


boxplot(t(avgs_bck_model))
boxplot(t(avgs_bck_model_fem))




### ARVORE DE DECISÃO ----

# MASCULINO

acur_arvore_masc <- c()
sens_arvore_masc <- c()
spec_arvore_masc <- c()

for(r in 1:20){
  
  training.samples <- var_selec_masc$dupla_de_referencia %>%
    createDataPartition(p = 0.8, list = FALSE)
  train.data  <- var_selec_masc[training.samples, ]
  test.data <- var_selec_masc[-training.samples, ]
  
  train.masc.bck <- train.data
  test.masc.bck <- test.data

  # Criando o modelo da árvore
  tree_spec <- decision_tree() %>%
    set_engine("rpart") %>%
    set_mode("classification")
  
  
  # Ajustando o modelo
  tree_fit.masc.bck <- tree_spec %>%
    fit(dupla_de_referencia ~ ., data = train.masc.bck)
  
  
  
  # Fazendo previsões com árvore de decisão
  predictions <- tree_fit.masc.bck %>%
    predict(test.masc.bck) %>%
    pull(.pred_class)
  
  
  # Medidas de acurácia
  metrics <- metric_set(accuracy, sensitivity, specificity, kap)
  model_performance.masc.bck <- test.masc.bck %>%
    mutate(predictions = predictions) %>%
    metrics(truth = dupla_de_referencia, estimate = predictions)
  
  cm_arv_masc <- confusionMatrix(predictions, test.masc.bck$dupla_de_referencia, positive = 'W')
  
  if(length(acur_arvore_masc) == 0){
    acur_arvore_masc <- cm_arv_masc$overall["Accuracy"]
  } else{
    acur_arvore_masc <- cbind(acur_arvore_masc, cm_arv_masc$overall["Accuracy"])
  }
  
  if(length(sens_arvore_masc) == 0){
    sens_arvore_masc <- cm_arv_masc$byClass["Sensitivity"]
  } else{
    sens_arvore_masc <- cbind(sens_arvore_masc, cm_arv_masc$byClass["Sensitivity"])
  }
  
  if(length(spec_arvore_masc) == 0){
    spec_arvore_masc <- cm_arv_masc$byClass["Specificity"]
  } else{
    spec_arvore_masc <- cbind(spec_arvore_masc, cm_arv_masc$byClass["Specificity"])
  }

}

confusionMatrix(predictions, test.masc.bck$dupla_de_referencia, positive = 'W')
print(model_performance.masc.bck)

# Plotando a árvore de decisão
tree_plot.masc.bck <-rpart.plot(tree_fit.masc.bck$fit, type = 4, digits = 3,extra = 101, under = TRUE, cex = 0.55, box.palette = "auto", roundin=F)

# Descrevendo as regras da árvore
rules.masc.bck <- rpart.rules(tree_fit.masc.bck$fit)
print(rules.masc.bck)

# Importância das variáveis
var_importance.masc.bck <- vip::vip(tree_fit.masc.bck, num_features = 16)
print(var_importance.masc.bck)


# FEMININO

acur_arvore_fem <- c()
sens_arvore_fem <- c()
spec_arvore_fem <- c()

for(r in 1:20){
  
  training.samples <- var_selec_fem$dupla_de_referencia %>%
    createDataPartition(p = 0.8, list = FALSE)
  train.data  <- var_selec_fem[training.samples,-26 ]
  test.data <- var_selec_fem[-training.samples, -26]
  
  train.fem.bck <- train.data
  test.fem.bck <- test.data
  

  # Criando o modelo da árvore
  tree_spec <- decision_tree() %>%
    set_engine("rpart") %>%
    set_mode("classification")
  
  
  # Ajustando o modelo
  tree_fit.fem.bck <- tree_spec %>%
    fit(dupla_de_referencia ~ ., data = train.fem.bck)
  
  
  # Fazendo previsões com árvore de decisão
  predictions <- tree_fit.fem.bck %>%
    predict(test.fem.bck) %>%
    pull(.pred_class)
  
  
  # Medidas de acurácia
  metrics <- metric_set(accuracy, sensitivity, specificity, kap)
  model_performance.fem.bck <- test.fem.bck %>%
    mutate(predictions = predictions) %>%
    metrics(truth = dupla_de_referencia, estimate = predictions)
  
  cm_arv_fem <- confusionMatrix(predictions, test.fem.bck$dupla_de_referencia, positive = 'W')
  
  if(length(acur_arvore_fem) == 0){
    acur_arvore_fem <- cm_arv_fem$overall["Accuracy"]
  } else{
    acur_arvore_fem <- cbind(acur_arvore_fem, cm_arv_fem$overall["Accuracy"])
  }
  
  if(length(sens_arvore_fem) == 0){
    sens_arvore_fem <- cm_arv_fem$byClass["Sensitivity"]
  } else{
    sens_arvore_fem <- cbind(sens_arvore_fem, cm_arv_fem$byClass["Sensitivity"])
  }
  
  if(length(spec_arvore_fem) == 0){
    spec_arvore_fem <- cm_arv_fem$byClass["Specificity"]
  } else{
    spec_arvore_fem <- cbind(spec_arvore_fem, cm_arv_fem$byClass["Specificity"])
  }

}


# Plotando a árvore de decisão
tree_plot.fem.bck <-rpart.plot(tree_fit.fem.bck$fit, type = 4, digits = 3,extra = 101, under = TRUE, cex = 0.55, box.palette = "auto", roundin=F)

# Descrevendo as regras da árvore
rules.fem.bck <- rpart.rules(tree_fit.fem.bck$fit)
print(rules.fem.bck)

# Importância das variáveis
var_importance.fem.bck <- vip::vip(tree_fit.fem.bck, num_features = 16)
print(var_importance.fem.bck)



# Boxplots

masc_arv_prev <- as.data.frame(cbind('Masculino', t(acur_arvore_masc)))
fem_arv_prev <- as.data.frame(cbind('Feminino', t(acur_arvore_fem)))

previsoes_arv <- rbind(masc_arv_prev, fem_arv_prev)

previsoes_arv$Accuracy <- as.numeric(previsoes_arv$Accuracy)

colnames(previsoes_arv) <- c('Modelo', 'Acurácia')

ggplot(previsoes_arv, aes(x = Modelo, y = Acurácia, fill = Modelo)) +
  geom_boxplot() +
  scale_fill_manual(values = c("blue", "red")) +
  theme_classic(base_size=16.5)


# Juntando masculino e feminino

arvore_df <- data.frame(Gênero = rbind("Masculino", "Feminino"),
                        Acurácia = rbind(format(round(mean(acur_arvore_masc), digits = 3), nsmall=3),format(round(mean(acur_arvore_fem), digits=3), nsmall=3)),
                        Sensitividade = rbind(format(round(mean(sens_arvore_masc), digits = 3), nsmall=3),format(round(mean(sens_arvore_fem), digits=3), nsmall=3)),
                        Especificidade = rbind(format(round(mean(spec_arvore_masc), digits=3), nsmall=3), format(round(mean(spec_arvore_fem), digits=3), nsmall=3)))

colnames(arvore_df) <- c('Gênero', 'Acurácia', 'Sensibilidade', 'Especificidade')

arvore_df %>% gt() %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_body(columns = Gênero)) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_column_labels())





### KNN ----

# Masculino


acur_knn_masc <- c()
sens_knn_masc <- c()
spec_knn_masc <- c()
k_best_masc <- c()

for(r in 1:20){
  
  print(r)
  
  training.samples <- var_selec_masc$dupla_de_referencia %>%
    createDataPartition(p = 0.8, list = FALSE)
  train.data  <- var_selec_masc[training.samples, ]
  test.data <- var_selec_masc[-training.samples, ]
  
  train.masc.bck <- train.data
  test.masc.bck <- test.data
  

  preProcValues <- preProcess(train.masc.bck, method = c("center", "scale"))
  trainTransformed <- predict(preProcValues, train.masc.bck)
  testTransformed <- predict(preProcValues, test.masc.bck)
  
  knnModel <- train(
    dupla_de_referencia ~ ., 
    data = trainTransformed, 
    method = "knn", 
    trControl = trainControl(method = "cv"), 
    tuneGrid = data.frame(k = c(3,5,7,10,13,16,18,20))
  )
  
  k_masc <- knnModel$bestTune$k
  
  best_model<- knn3(
    dupla_de_referencia ~ .,
    data = trainTransformed,
    k = k_masc
  )
  
  
  predictions <- predict(best_model, testTransformed,type = "class")
  
  # Matriz de Confusão
  cm_masc <- confusionMatrix(predictions, testTransformed$dupla_de_referencia, positive = 'W')
  
  
  if(length(acur_knn_masc) == 0){
    acur_knn_masc <- cm_masc$overall["Accuracy"]
  } else{
    acur_knn_masc <- cbind(acur_knn_masc, cm_masc$overall["Accuracy"])
  }
  
  if(length(sens_knn_masc) == 0){
    sens_knn_masc <- cm_masc$byClass["Sensitivity"]
  } else{
    sens_knn_masc <- cbind(sens_knn_masc, cm_masc$byClass["Sensitivity"])
  }
  
  if(length(spec_knn_masc) == 0){
    spec_knn_masc <- cm_masc$byClass["Specificity"]
  } else{
    spec_knn_masc <- cbind(spec_knn_masc, cm_masc$byClass["Specificity"])
  }
  
  if(length(k_best_masc) == 0){
    k_best_masc <- k_masc
  } else{
    k_best_masc <- cbind(k_best_masc, k_masc)
  }

}

k_best_masc_grp <- data.frame(table(k_best_masc))
k_max_masc <- k_best_masc_grp[which.max(k_best_masc_grp$Freq), "k_best_masc"]
k_tab_masc <- paste(k_max_masc[1], ' (',100*(max(k_best_masc_grp$Freq)/20), '%)', sep='')




# FEMININO

acur_knn_fem <- c()
sens_knn_fem <- c()
spec_knn_fem <- c()
k_best_fem <- c()

for(r in 1:20){
  
  print(r)
  
  training.samples <- var_selec_fem$dupla_de_referencia %>%
    createDataPartition(p = 0.8, list = FALSE)
  train.data  <- var_selec_fem[training.samples,-26 ]
  test.data <- var_selec_fem[-training.samples, -26]
  
  train.fem.bck <- train.data
  test.fem.bck <- test.data

  preProcValues <- preProcess(train.fem.bck, method = c("center", "scale"))
  trainTransformed <- predict(preProcValues, train.fem.bck)
  testTransformed <- predict(preProcValues, test.fem.bck)
  
  knnModel <- train(
    dupla_de_referencia ~ ., 
    data = trainTransformed, 
    method = "knn", 
    trControl = trainControl(method = "cv"), 
    tuneGrid = data.frame(k = c(3,5,7,10,13,16,18,20))
  )
  
  k_fem <- knnModel$bestTune$k
  
  best_model<- knn3(
    dupla_de_referencia ~ .,
    data = trainTransformed,
    k = k_fem
  )
  
  predictions <- predict(best_model, testTransformed,type = "class")
  
  # Matriz de confusão
  cm_fem <- confusionMatrix(predictions, testTransformed$dupla_de_referencia, positive = 'W')
  
  if(length(acur_knn_fem) == 0){
    acur_knn_fem <- cm_fem$overall["Accuracy"]
  } else{
    acur_knn_fem <- cbind(acur_knn_fem, cm_fem$overall["Accuracy"])
  }
  
  if(length(sens_knn_fem) == 0){
    sens_knn_fem <- cm_fem$byClass["Sensitivity"]
  } else{
    sens_knn_fem <- cbind(sens_knn_fem, cm_fem$byClass["Sensitivity"])
  }
  
  if(length(spec_knn_fem) == 0){
    spec_knn_fem <- cm_fem$byClass["Specificity"]
  } else{
    spec_knn_fem <- cbind(spec_knn_fem, cm_fem$byClass["Specificity"])
  }
  
  if(length(k_best_fem) == 0){
    k_best_fem <- k_fem
  } else{
    k_best_fem <- cbind(k_best_fem, k_fem)
  }

}

# Boxplots

masc_knn_prev <- as.data.frame(cbind('Masculino', t(acur_knn_masc)))
fem_knn_prev <- as.data.frame(cbind('Feminino', t(acur_knn_fem)))

previsoes_knn <- rbind(masc_knn_prev, fem_knn_prev)

previsoes_knn$Accuracy <- as.numeric(previsoes_knn$Accuracy)

colnames(previsoes_knn) <- c('Modelo', 'Acurácia')

ggplot(previsoes_knn, aes(x = Modelo, y = Acurácia, fill = Modelo)) +
  geom_boxplot() +
  scale_fill_manual(values = c("blue", "red")) +
  theme_classic(base_size=16.5)



# Juntando masculino e feminino

# Juntando masculino e feminino

knn_df <- data.frame(Gênero = rbind("Masculino", "Feminino"),
                     `K ótimo` = rbind(k_tab_masc, k_tab_fem),
                     Acurácia = rbind(format(round(mean(acur_knn_masc), digits = 3), nsmall=3),format(round(mean(acur_knn_fem), digits=3), nsmall=3)),
                     Sensitividade = rbind(format(round(mean(sens_knn_masc), digits = 3), nsmall=3),format(round(mean(sens_knn_fem), digits=3), nsmall=3)),
                     Especificidade = rbind(format(round(mean(spec_knn_masc), digits=3), nsmall=3), format(round(mean(spec_knn_fem), digits=3), nsmall=3)))

colnames(knn_df) <- c('Gênero', 'K ótimo', 'Acurácia', 'Sensibilidade', 'Especificidade')

knn_df %>% gt() %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_body(columns = Gênero)) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_column_labels())


## Comparando todos os modelos estudados ----

df_tds_metodos2 <- data.frame(rbind(cbind('Masculino','Acurácia',format(round(mean(avgs_bck_model),digits = 3), nsmall=3), format(round(mean(acur_arvore_masc), digits=3), nsmall=3), format(round(mean(acur_knn_masc), digits = 3), nsmall=3)),
                                    cbind('Masculino', 'Sensibilidade', format(round(mean(sens_full_model_masc),digits = 3), nsmall=3), format(round(mean(sens_arvore_masc), digits=3), nsmall=3), format(round(mean(sens_knn_masc), digits = 3), nsmall=3)),
                                    cbind('Masculino', 'Especificidade', format(round(mean(spec_full_model_masc),digits = 3), nsmall=3), format(round(mean(spec_arvore_masc), digits=3), nsmall=3), format(round(mean(spec_knn_masc), digits = 3), nsmall=3)),
                                    cbind('Feminino','Acurácia',format(round(mean(avgs_bck_model_fem),digits = 3), nsmall=3), format(round(mean(acur_arvore_fem), digits=3), nsmall=3), format(round(mean(acur_knn_fem), digits = 3), nsmall=3)),
                                    cbind('Feminino', 'Sensibilidade', format(round(mean(sens_full_model_fem),digits = 3), nsmall=3), format(round(mean(sens_arvore_fem), digits=3), nsmall=3), format(round(mean(sens_knn_fem), digits = 3), nsmall=3)),
                                    cbind('Feminino', 'Especificidade', format(round(mean(spec_full_model_fem),digits = 3), nsmall=3), format(round(mean(spec_arvore_fem), digits=3), nsmall=3), format(round(mean(spec_knn_fem), digits = 3), nsmall=3)))
)

rownames(df_tds_metodos2) <- NULL
colnames(df_tds_metodos2) <- c('Gênero', 'Medida', 'Regressão Logística', 'Árvore de Decisão', 'KNN')


df_tds_metodos2 %>% gt(groupname_col = 'Gênero', row_group_as_column = TRUE) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_row_groups( groups= everything())) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_column_labels())




# Análise Exploratória ----

## Fazendo os mesmos procedimentos para FIVB ----

## Somente Fivb ----
dados_fivb_sem_na <- dados_fivb_sem_na


## Cuidando dos jogos perdidos ----

dados_fivb <- dados_todos[which(dados_todos$circuit == 'FIVB'),]
dados_fivb$tourn_id <- paste(dados_fivb$circuit, dados_fivb$tournament, dados_fivb$country, dados_fivb$year,  dados_fivb$gender, sep = ' - ') 

# Remove-se os "Qualifier Bracket" para se evitar problemas com as variáveis de rank
dados_fivb <- dados_fivb[which(dados_fivb$bracket != "Qualifier Bracket"),]

# A lógica é fazer um df com todos os jogadores, ordená-lo e ver quantas partidas foram perdidas entre um jogo e outro devido à remoção das variáveis com vazios



# Começando com a base geral, pré-remoção da NA's

dados_fivb1_w1 <- dados_fivb[,c("tourn_id", "match_num", "date", "w_player1", "w_p1_hgt", "w_p1_age")]
colnames(dados_fivb1_w1) <- c("tourn_id", "match_num", "date", "player", "hgt","age")
dados_fivb1_w2 <- dados_fivb[,c("tourn_id", "match_num", "date", "w_player2", "w_p2_hgt", "w_p2_age")]
colnames(dados_fivb1_w2) <- c("tourn_id", "match_num", "date", "player", "hgt", "age")
dados_fivb1_l1 <- dados_fivb[,c("tourn_id", "match_num", "date","l_player1", "l_p1_hgt", "l_p1_age")]
colnames(dados_fivb1_l1) <- c("tourn_id", "match_num", "date", "player", "hgt", "age")
dados_fivb1_l2 <- dados_fivb[,c("tourn_id", "match_num","date", "l_player2", "l_p2_hgt", "l_p2_age")]
colnames(dados_fivb1_l2) <- c("tourn_id", "match_num", "date", "player", "hgt", "age")

dados_fivb_duplas_w <- dados_fivb[,c("tourn_id", "match_num", "date", "w_player1", "w_player2")]
dados_fivb_duplas_w$result <- 'W'
colnames(dados_fivb_duplas_w) <- c("match_id", "match_num", "date", "player1","player2", "result")
dados_fivb_duplas_l <- dados_fivb[,c("tourn_id", "match_num", "date", "l_player1", "l_player2")]
dados_fivb_duplas_l$result <- 'L'
colnames(dados_fivb_duplas_l) <- c("match_id", "match_num", "date", "player1","player2", "result")
dados_fivb_gerais_duplas <- rbind(dados_fivb_duplas_w, dados_fivb_duplas_l)
dados_fivb_gerais_duplas$match_id <- paste(dados_fivb_gerais_duplas$match_id, dados_fivb_gerais_duplas$match_num, sep = ' - ')


jog_tds_jogos_fivb <- rbind(dados_fivb1_w1, dados_fivb1_w2, dados_fivb1_l1, dados_fivb1_l2)

jog_tds_jogos_fivb$player_tourn_id <- paste(jog_tds_jogos_fivb$tourn_id, jog_tds_jogos_fivb$date, jog_tds_jogos_fivb$player, sep = ' - ')
jog_tds_jogos_fivb$match_player_id <- paste(jog_tds_jogos_fivb$tourn_id, jog_tds_jogos_fivb$match_num, 
                                            jog_tds_jogos_fivb$player, jog_tds_jogos_fivb$date, jog_tds_jogos_fivb$match_num,
                                            sep = ' - ')

jog_tds_jogos_fivb <- jog_tds_jogos_fivb[which(!duplicated(jog_tds_jogos_fivb$match_player_id)),]

jog_tds_jogos_fivb <- jog_tds_jogos_fivb %>% arrange(player, date, match_num)



# Criando o "ID" da partida por jogador

jog_tds_jogos_fivb$cont_player <- 1

for(i in 1:(nrow(jog_tds_jogos_fivb)-1)){
  if(i %% 10000 == 0) {print(i)}
  if(jog_tds_jogos_fivb[i+1, "player"] == jog_tds_jogos_fivb[i,"player"]){
    jog_tds_jogos_fivb[i+1, "cont_player"] <- jog_tds_jogos_fivb[i,"cont_player"] +1
  }
}



# Fazendo o mesmo que foi feito anteriormente, mas com os dados já sem vazios

dados_fivb_sem_na <- dados_fivb_sem_na[which(dados_fivb_sem_na$circuit == 'FIVB'),]
dados_fivb_sem_na$tourn_id <- paste(dados_fivb_sem_na$circuit, dados_fivb_sem_na$tournament, dados_fivb_sem_na$country, dados_fivb_sem_na$year,  dados_fivb_sem_na$gender, sep = ' - ') 
dados_fivb_sem_na <- dados_fivb_sem_na[which(dados_fivb_sem_na$bracket != "Qualifier Bracket"),]
dados_fivb_sem_na1_w1 <- dados_fivb_sem_na[,c("tourn_id", "match_num", "date", "w_player1", "w_p1_hgt", "w_p1_age")]
colnames(dados_fivb_sem_na1_w1) <- c("tourn_id", "match_num", "date", "player", "hgt","age")
dados_fivb_sem_na1_w2 <- dados_fivb_sem_na[,c("tourn_id", "match_num", "date", "w_player2", "w_p2_hgt", "w_p2_age")]
colnames(dados_fivb_sem_na1_w2) <- c("tourn_id", "match_num", "date", "player", "hgt", "age")
dados_fivb_sem_na1_l1 <- dados_fivb_sem_na[,c("tourn_id", "match_num", "date","l_player1", "l_p1_hgt", "l_p1_age")]
colnames(dados_fivb_sem_na1_l1) <- c("tourn_id", "match_num", "date", "player", "hgt", "age")
dados_fivb_sem_na1_l2 <- dados_fivb_sem_na[,c("tourn_id", "match_num","date", "l_player2", "l_p2_hgt", "l_p2_age")]
colnames(dados_fivb_sem_na1_l2) <- c("tourn_id", "match_num", "date", "player", "hgt", "age")

jog_tds_jogos_fivb_sem_na <- rbind(dados_fivb_sem_na1_w1, dados_fivb_sem_na1_w2, dados_fivb_sem_na1_l1, dados_fivb_sem_na1_l2)

jog_tds_jogos_fivb_sem_na$player_tourn_id <- paste(jog_tds_jogos_fivb_sem_na$tourn_id, jog_tds_jogos_fivb_sem_na$date, jog_tds_jogos_fivb_sem_na$player, sep = ' - ')
jog_tds_jogos_fivb_sem_na$match_player_id <- paste(jog_tds_jogos_fivb_sem_na$tourn_id, jog_tds_jogos_fivb_sem_na$match_num, 
                                                   jog_tds_jogos_fivb_sem_na$player, jog_tds_jogos_fivb_sem_na$date, jog_tds_jogos_fivb_sem_na$match_num,
                                                   sep = ' - ')


jog_tds_jogos_fivb_sem_na <- jog_tds_jogos_fivb_sem_na %>% arrange(player, date, match_num)

jog_tds_jogos_fivb_sem_na$jogos_nao_contabilizados_torneio <- c()

jog_tds_jogos_fivb_sem_na[1,'jogos_nao_contabilizados_torneio'] <- '1o jogo contabilizado'
jog_tds_jogos_fivb_sem_na$qtd_jogos_nao_contabilizados_torneio <- 0



# Juntando com os "ID's" por jogador da base completa

jog_tds_jogos_fivb_sem_na <- jog_tds_jogos_fivb_sem_na %>% 
  left_join(jog_tds_jogos_fivb[,c("match_player_id", "cont_player")], by = 'match_player_id')



# Criando colunas para verificar se o jogador teve jogos perdidos

jog_tds_jogos_fivb_sem_na$qtd_total_jogos_perdidos <- 0
jog_tds_jogos_fivb_sem_na$teve_jogos_perdidos <- 'não'

for(i in 2:nrow(jog_tds_jogos_fivb_sem_na)){
  if(jog_tds_jogos_fivb_sem_na[i, "player"] == jog_tds_jogos_fivb_sem_na[i-1, "player"] ){
    jog_tds_jogos_fivb_sem_na[i, 'qtd_total_jogos_perdidos'] <- jog_tds_jogos_fivb_sem_na[i,"cont_player"] - jog_tds_jogos_fivb_sem_na[i-1,"cont_player"]
    jog_tds_jogos_fivb_sem_na[i,'teve_jogos_perdidos'] <- ifelse(jog_tds_jogos_fivb_sem_na[i,"qtd_total_jogos_perdidos"] > 1, 'sim', 'não')
  }
}

jog_tds_jogos_fivb_sem_na$col_aux_jogos_perd <- ifelse(jog_tds_jogos_fivb_sem_na$qtd_total_jogos_perdidos %in% c(0,1),0,jog_tds_jogos_fivb_sem_na$qtd_total_jogos_perdidos)


# cont_player_sem_na se refere à "ID" do jogo por jogador, mas da base já sem vazios

jog_tds_jogos_fivb_sem_na$cont_player_sem_na <- 1

for(i in 1:(nrow(jog_tds_jogos_fivb_sem_na)-1)){
  if(i %% 10000 == 0) {print(i)}
  if(jog_tds_jogos_fivb_sem_na[i+1, "player"] == jog_tds_jogos_fivb_sem_na[i,"player"]){
    jog_tds_jogos_fivb_sem_na[i+1, "cont_player_sem_na"] <- jog_tds_jogos_fivb_sem_na[i,"cont_player_sem_na"] +1
  }
}



# Se o jogador perdeu algum dos últimos 4 jogos anteriores ao respectivo jogo atual, esta partida será desconsiderada depois

jog_tds_jogos_fivb_sem_na$jogos_perd_ult_4 <- 0

jog_tds_jogos_fivb_sem_na$jogos_perd_ult_4 <- ifelse(jog_tds_jogos_fivb_sem_na$cont_player_sem_na == 1, 
                                                     jog_tds_jogos_fivb_sem_na$qtd_total_jogos_perdidos,
                                                     0)

for(i in 1:(nrow(jog_tds_jogos_fivb_sem_na))){
  
  if(jog_tds_jogos_fivb_sem_na[i,"cont_player_sem_na"] != 1){
    if(jog_tds_jogos_fivb_sem_na[i,"cont_player_sem_na"] %in% c(2,3,4,5)){
      jog_tds_jogos_fivb_sem_na[i,"jogos_perd_ult_4"] <- jog_tds_jogos_fivb_sem_na[i,"col_aux_jogos_perd"] + jog_tds_jogos_fivb_sem_na[i-1,"jogos_perd_ult_4"]
    } else{
      jog_tds_jogos_fivb_sem_na[i,"jogos_perd_ult_4"] <- jog_tds_jogos_fivb_sem_na[i-1,"jogos_perd_ult_4"] - jog_tds_jogos_fivb_sem_na[i-4,"col_aux_jogos_perd"] + jog_tds_jogos_fivb_sem_na[i,"col_aux_jogos_perd"] 
    }
  }
}


# Essas partidas só serão desconsideradas após a realização do cálculo das médias móveis, de modo que estas mesmas partidas sejam contabilizadas nos cálculos das médias móveis das partidas posteriores

jogos_fivb_sem_na_desconsiderar <- jog_tds_jogos_fivb_sem_na %>% filter(jogos_perd_ult_4 >0)

# As colunas a seguir serão utilizadas posteriormente para remoção das partidas com gap entre jogos maior que 1

dados_fivb_sem_na$w1_match_player_id <- paste(dados_fivb_sem_na$tourn_id, dados_fivb_sem_na$match_num, 
                                              dados_fivb_sem_na$w_player1, dados_fivb_sem_na$date,
                                              dados_fivb_sem_na$match_num, sep = ' - ')
dados_fivb_sem_na$w2_match_player_id <- paste(dados_fivb_sem_na$tourn_id, dados_fivb_sem_na$match_num, 
                                              dados_fivb_sem_na$w_player2, dados_fivb_sem_na$date,
                                              dados_fivb_sem_na$match_num, sep = ' - ')
dados_fivb_sem_na$l1_match_player_id <- paste(dados_fivb_sem_na$tourn_id, dados_fivb_sem_na$match_num, 
                                              dados_fivb_sem_na$l_player1, dados_fivb_sem_na$date,
                                              dados_fivb_sem_na$match_num, sep = ' - ')
dados_fivb_sem_na$l2_match_player_id <- paste(dados_fivb_sem_na$tourn_id, dados_fivb_sem_na$match_num, 
                                              dados_fivb_sem_na$l_player2, dados_fivb_sem_na$date,
                                              dados_fivb_sem_na$match_num, sep = ' - ')


# COMEÇANDO A ORGANIZAR PARA ANÁLISES ----

## Separando os dados por gênero e criando tabela com os valores compilados por jogador ----

### MASCULINOS ----
dados_fivb_sem_na_m <- dados_fivb_sem_na[which(dados_fivb_sem_na$gender == 'M'), ]

wp1m_fivb <- dados_fivb_sem_na_m[,c("match_id", "match_num", "date", "points_played", "sets_played", "w_player1", "w_p1_age", "w_p1_hgt",
                                    "w_p1_tot_aces", "w_p1_tot_attacks", "w_p1_tot_kills","w_p1_tot_blocks", 
                                    "w_p1_tot_digs", "w_p1_tot_errors", "w_p1_tot_serve_errors", "w_p1_tot_hitpct")]
colnames(wp1m_fivb) <- c('match_id',"match_num", "date", "points_played", "sets_played", 'player', 'age', 'hgt', 'tot_aces', 'tot_attacks', 
                         'tot_kills', 'tot_blocks', 'tot_digs', 'tot_errors', 'tot_serve_errors', "tot_hitpct")

wp1m_fivb$nr_player <- 'W1'

wp2m_fivb <- dados_fivb_sem_na_m[,c("match_id","match_num", "date", "points_played", "sets_played", "w_player2", "w_p2_age", "w_p2_hgt",
                                    "w_p2_tot_aces", "w_p2_tot_attacks", "w_p2_tot_kills","w_p2_tot_blocks", 
                                    "w_p2_tot_digs", "w_p2_tot_errors", "w_p2_tot_serve_errors", "w_p2_tot_hitpct")]
wp2m_fivb$nr_player <- 'W2'
colnames(wp2m_fivb) <- colnames(wp1m_fivb)


lp1m_fivb <- dados_fivb_sem_na_m[,c("match_id","match_num", "date", "points_played", "sets_played", "l_player1", "l_p1_age", "l_p1_hgt",
                                    "l_p1_tot_aces", "l_p1_tot_attacks", "l_p1_tot_kills", "l_p1_tot_blocks", 
                                    "l_p1_tot_digs", "l_p1_tot_errors", "l_p1_tot_serve_errors", "l_p1_tot_hitpct")]
lp1m_fivb$nr_player <- 'L1'
colnames(lp1m_fivb) <- colnames(wp1m_fivb)


lp2m_fivb <- dados_fivb_sem_na_m[,c("match_id","match_num", "date", "points_played", "sets_played", "l_player2", "l_p2_age", "l_p2_hgt",
                                    "l_p2_tot_aces", "l_p2_tot_attacks", "l_p2_tot_kills", "l_p2_tot_blocks", 
                                    "l_p2_tot_digs", "l_p2_tot_errors", "l_p2_tot_serve_errors", "l_p2_tot_hitpct")]
lp2m_fivb$nr_player <- 'L2'
colnames(lp2m_fivb) <- colnames(wp1m_fivb)


dados_fivb_jogadores_m <- rbind(wp1m_fivb,wp2m_fivb,lp1m_fivb,lp2m_fivb)



### FEMININOS ----
dados_fivb_sem_na_w <- dados_fivb_sem_na[which(dados_fivb_sem_na$gender == 'W'), ]

wp1w_fivb <- dados_fivb_sem_na_w[,c("match_id","match_num", "date", "points_played", "sets_played", "w_player1", "w_p1_age", "w_p1_hgt",
                                    "w_p1_tot_aces", "w_p1_tot_attacks", "w_p1_tot_kills","w_p1_tot_blocks", 
                                    "w_p1_tot_digs", "w_p1_tot_errors", "w_p1_tot_serve_errors", "w_p1_tot_hitpct")]
colnames(wp1w_fivb) <- c('match_id',"match_num", "date", "points_played", "sets_played", 'player', 'age', 'hgt', 'tot_aces', 'tot_attacks', 
                         'tot_kills', 'tot_blocks', 'tot_digs', 'tot_errors', 'tot_serve_errors', "tot_hitpct")

wp1w_fivb$nr_player <- 'W1'

wp2w_fivb <- dados_fivb_sem_na_w[,c("match_id","match_num", "date", "points_played", "sets_played", "w_player2", "w_p2_age", "w_p2_hgt",
                                    "w_p2_tot_aces", "w_p2_tot_attacks", "w_p2_tot_kills","w_p2_tot_blocks", 
                                    "w_p2_tot_digs", "w_p2_tot_errors", "w_p2_tot_serve_errors", "w_p2_tot_hitpct")]
wp2w_fivb$nr_player <- 'W2'
colnames(wp2w_fivb) <- colnames(wp1w_fivb)


lp1w_fivb <- dados_fivb_sem_na_w[,c("match_id","match_num", "date", "points_played", "sets_played", "l_player1", "l_p1_age", "l_p1_hgt",
                                    "l_p1_tot_aces", "l_p1_tot_attacks", "l_p1_tot_kills", "l_p1_tot_blocks", 
                                    "l_p1_tot_digs", "l_p1_tot_errors", "l_p1_tot_serve_errors", "l_p1_tot_hitpct")]
lp1w_fivb$nr_player <- 'L1'
colnames(lp1w_fivb) <- colnames(wp1w_fivb)


lp2w_fivb <- dados_fivb_sem_na_w[,c("match_id","match_num", "date", "points_played", "sets_played", "l_player2", "l_p2_age", "l_p2_hgt",
                                    "l_p2_tot_aces", "l_p2_tot_attacks", "l_p2_tot_kills", "l_p2_tot_blocks", 
                                    "l_p2_tot_digs", "l_p2_tot_errors", "l_p2_tot_serve_errors", "l_p2_tot_hitpct")]
lp2w_fivb$nr_player <- 'L2'
colnames(lp2w_fivb) <- colnames(wp1w_fivb)


dados_fivb_jogadores_w <- rbind(wp1w_fivb,wp2w_fivb,lp1w_fivb,lp2w_fivb)




## Filtrando por jogadores com ao menos 5 jogos ----

### MASCULINO ----
dados_fivb_jogadores_m <- dados_fivb_jogadores_m %>% arrange(player, date, match_num, match_id)

# Criando média geral para colocar nos primeiros jogos, filtrando por jogadores com 
dados_fivb_jogadores_grp_m <- dados_fivb_jogadores_m %>% group_by(player, hgt) %>% summarise(
  tot_games = n()
) %>%filter(tot_games>=5)

for( i in 1:nrow(dados_fivb_sem_na_m)){
  if(dados_fivb_sem_na_m[i,"w_player1"] %in% dados_fivb_jogadores_grp_m$player){
    dados_fivb_sem_na_m[i, 'consid_jog_w1'] <- 'sim'
  }
  if(dados_fivb_sem_na_m[i,"w_player2"] %in% dados_fivb_jogadores_grp_m$player){
    dados_fivb_sem_na_m[i, 'consid_jog_w2'] <- 'sim'
  }
  if(dados_fivb_sem_na_m[i,"l_player1"] %in% dados_fivb_jogadores_grp_m$player){
    dados_fivb_sem_na_m[i, 'consid_jog_l1'] <- 'sim'
  }
  if(dados_fivb_sem_na_m[i,"l_player2"] %in% dados_fivb_jogadores_grp_m$player){
    dados_fivb_sem_na_m[i, 'consid_jog_l2'] <- 'sim'
  }
}


dados_fivb_sem_na_m_n_jogos <- dados_fivb_sem_na_m[which(dados_fivb_sem_na_m$consid_jog_w1 == 'sim'),]
dados_fivb_sem_na_m_n_jogos <- dados_fivb_sem_na_m_n_jogos[which(dados_fivb_sem_na_m_n_jogos$consid_jog_w2 == 'sim'),]
dados_fivb_sem_na_m_n_jogos <- dados_fivb_sem_na_m_n_jogos[which(dados_fivb_sem_na_m_n_jogos$consid_jog_l1 == 'sim'),]
dados_fivb_sem_na_m_n_jogos <- dados_fivb_sem_na_m_n_jogos[which(dados_fivb_sem_na_m_n_jogos$consid_jog_l2 == 'sim'),]


### FEMININO ----
dados_fivb_jogadores_w <- dados_fivb_jogadores_w %>% arrange(player, date,match_num, match_id)

dados_fivb_jogadores_grp_w <- dados_fivb_jogadores_w %>% group_by(player, hgt) %>% summarise(
  tot_games = n()
) %>%filter(tot_games>=5)


for( i in 1:nrow(dados_fivb_sem_na_w)){
  if(dados_fivb_sem_na_w[i,"w_player1"] %in% dados_fivb_jogadores_grp_w$player){
    dados_fivb_sem_na_w[i, 'consid_jog_w1'] <- 'sim'
  }
  if(dados_fivb_sem_na_w[i,"w_player2"] %in% dados_fivb_jogadores_grp_w$player){
    dados_fivb_sem_na_w[i, 'consid_jog_w2'] <- 'sim'
  }
  if(dados_fivb_sem_na_w[i,"l_player1"] %in% dados_fivb_jogadores_grp_w$player){
    dados_fivb_sem_na_w[i, 'consid_jog_l1'] <- 'sim'
  }
  if(dados_fivb_sem_na_w[i,"l_player2"] %in% dados_fivb_jogadores_grp_w$player){
    dados_fivb_sem_na_w[i, 'consid_jog_l2'] <- 'sim'
  }
}

dados_fivb_sem_na_w_n_jogos <- dados_fivb_sem_na_w[which(dados_fivb_sem_na_w$consid_jog_w1 == 'sim'),]
dados_fivb_sem_na_w_n_jogos <- dados_fivb_sem_na_w_n_jogos[which(dados_fivb_sem_na_w_n_jogos$consid_jog_w2 == 'sim'),]
dados_fivb_sem_na_w_n_jogos <- dados_fivb_sem_na_w_n_jogos[which(dados_fivb_sem_na_w_n_jogos$consid_jog_l1 == 'sim'),]
dados_fivb_sem_na_w_n_jogos <- dados_fivb_sem_na_w_n_jogos[which(dados_fivb_sem_na_w_n_jogos$consid_jog_l2 == 'sim'),]


## TRABALHANDO COM MÉDIAS MÓVEIS ----

### MASCULINO ----

#### Criando médias por pontos jogados em cada jogo ----

dados_fivb_jogadores_m$avg_atual_aces_points = dados_fivb_jogadores_m$tot_aces/dados_fivb_jogadores_m$points_played
dados_fivb_jogadores_m$avg_atual_attacks_points = dados_fivb_jogadores_m$tot_attacks/dados_fivb_jogadores_m$points_played
dados_fivb_jogadores_m$avg_atual_kills_points = dados_fivb_jogadores_m$tot_kills/dados_fivb_jogadores_m$points_played
dados_fivb_jogadores_m$avg_atual_blocks_points = dados_fivb_jogadores_m$tot_blocks/dados_fivb_jogadores_m$points_played
dados_fivb_jogadores_m$avg_atual_digs_points = dados_fivb_jogadores_m$tot_digs/dados_fivb_jogadores_m$points_played
dados_fivb_jogadores_m$avg_atual_errors_points = dados_fivb_jogadores_m$tot_errors/dados_fivb_jogadores_m$points_played
dados_fivb_jogadores_m$avg_atual_serve_errors_points = dados_fivb_jogadores_m$tot_serve_errors/dados_fivb_jogadores_m$points_played


# Filtrando somente por jogadores com ao menos 5 jogos na base
dados_fivb_jogadores_m_n_jogos <- dados_fivb_jogadores_m[dados_fivb_jogadores_m$player %in% dados_fivb_jogadores_grp_m$player,]


# Inputando as médias móveis das variáveis
for(i in 1:nrow(dados_fivb_jogadores_m_n_jogos)){
  #Travando para os casos em que é a primeira vez que aparece o jogador
  if(i!=1 && dados_fivb_jogadores_m_n_jogos[i-1, "player"] == dados_fivb_jogadores_m_n_jogos[i,"player"]){
    next
  }
  else{ 
    print(i)
    # j é a quantidade de jogos do jogador i
    j = length(dados_fivb_jogadores_m_n_jogos[which(dados_fivb_jogadores_m_n_jogos$player == dados_fivb_jogadores_m_n_jogos[i,"player"]), 'player'])
    # de i até ((i+j)-1) significa da primeira vez em que aparece o jogador até o último registro dele
    
    # Média móvel da hitting percentage
    dados_fivb_jogadores_m_n_jogos[i:(i+j-1), 'sma_hitpct'] <- (SMA(dados_fivb_jogadores_m_n_jogos[which(dados_fivb_jogadores_m_n_jogos$player == dados_fivb_jogadores_m_n_jogos[i,"player"]), 'tot_hitpct'], n=5)*5-dados_fivb_jogadores_m_n_jogos[i:(i+j-1),"tot_hitpct"])/4
    
    # Média móvel das médias por ponto jogado
    dados_fivb_jogadores_m_n_jogos[i:(i+j-1), 'sma_avg_aces_points'] <- (SMA(dados_fivb_jogadores_m_n_jogos[which(dados_fivb_jogadores_m_n_jogos$player == dados_fivb_jogadores_m_n_jogos[i,"player"]), 'avg_atual_aces_points'], n=5)*5-dados_fivb_jogadores_m_n_jogos[i:(i+j-1),"avg_atual_aces_points"])/4
    dados_fivb_jogadores_m_n_jogos[i:(i+j-1), 'sma_avg_attacks_points'] <- (SMA(dados_fivb_jogadores_m_n_jogos[which(dados_fivb_jogadores_m_n_jogos$player == dados_fivb_jogadores_m_n_jogos[i,"player"]), "avg_atual_attacks_points"], n=5)*5-dados_fivb_jogadores_m_n_jogos[i:(i+j-1),"avg_atual_attacks_points"])/4
    dados_fivb_jogadores_m_n_jogos[i:(i+j-1), 'sma_avg_kills_points'] <- (SMA(dados_fivb_jogadores_m_n_jogos[which(dados_fivb_jogadores_m_n_jogos$player == dados_fivb_jogadores_m_n_jogos[i,"player"]), 'avg_atual_kills_points'], n=5)*5-dados_fivb_jogadores_m_n_jogos[i:(i+j-1),"avg_atual_kills_points"])/4
    dados_fivb_jogadores_m_n_jogos[i:(i+j-1), 'sma_avg_blocks_points'] <- (SMA(dados_fivb_jogadores_m_n_jogos[which(dados_fivb_jogadores_m_n_jogos$player == dados_fivb_jogadores_m_n_jogos[i,"player"]), 'avg_atual_blocks_points'], n=5)*5-dados_fivb_jogadores_m_n_jogos[i:(i+j-1),"avg_atual_blocks_points"])/4
    dados_fivb_jogadores_m_n_jogos[i:(i+j-1), 'sma_avg_digs_points'] <- (SMA(dados_fivb_jogadores_m_n_jogos[which(dados_fivb_jogadores_m_n_jogos$player == dados_fivb_jogadores_m_n_jogos[i,"player"]), 'avg_atual_digs_points'], n=5)*5-dados_fivb_jogadores_m_n_jogos[i:(i+j-1),"avg_atual_digs_points"])/4
    dados_fivb_jogadores_m_n_jogos[i:(i+j-1), 'sma_avg_errors_points'] <- (SMA(dados_fivb_jogadores_m_n_jogos[which(dados_fivb_jogadores_m_n_jogos$player == dados_fivb_jogadores_m_n_jogos[i,"player"]), 'avg_atual_errors_points'], n=5)*5-dados_fivb_jogadores_m_n_jogos[i:(i+j-1),"avg_atual_errors_points"])/4
    dados_fivb_jogadores_m_n_jogos[i:(i+j-1), 'sma_avg_serve_errors_points'] <- (SMA(dados_fivb_jogadores_m_n_jogos[which(dados_fivb_jogadores_m_n_jogos$player == dados_fivb_jogadores_m_n_jogos[i,"player"]), 'avg_atual_serve_errors_points'], n=5)*5-dados_fivb_jogadores_m_n_jogos[i:(i+j-1),"avg_atual_serve_errors_points"])/4
    
    # Média móvel dos totais
    dados_fivb_jogadores_m_n_jogos[i:(i+j-1), 'sma_attacks'] <- (SMA(dados_fivb_jogadores_m_n_jogos[which(dados_fivb_jogadores_m_n_jogos$player == dados_fivb_jogadores_m_n_jogos[i,"player"]), "tot_attacks"], n=5)*5-dados_fivb_jogadores_m_n_jogos[i:(i+j-1),"tot_attacks"])/4
    dados_fivb_jogadores_m_n_jogos[i:(i+j-1), 'sma_kills'] <- (SMA(dados_fivb_jogadores_m_n_jogos[which(dados_fivb_jogadores_m_n_jogos$player == dados_fivb_jogadores_m_n_jogos[i,"player"]), 'tot_kills'], n=5)*5-dados_fivb_jogadores_m_n_jogos[i:(i+j-1),"tot_kills"])/4
    dados_fivb_jogadores_m_n_jogos[i:(i+j-1), 'sma_errors'] <- (SMA(dados_fivb_jogadores_m_n_jogos[which(dados_fivb_jogadores_m_n_jogos$player == dados_fivb_jogadores_m_n_jogos[i,"player"]), 'tot_errors'], n=5)*5-dados_fivb_jogadores_m_n_jogos[i:(i+j-1),"tot_errors"])/4
  }
}



# ID das partidas por jogador
dados_fivb_jogadores_m_n_jogos$match_player_id <- paste(dados_fivb_jogadores_m_n_jogos$match_id,
                                                        dados_fivb_jogadores_m_n_jogos$player,
                                                        dados_fivb_jogadores_m_n_jogos$date,
                                                        dados_fivb_jogadores_m_n_jogos$match_num,
                                                        sep=" - ")

# Removendo partidas com ID-partida-jogador duplicado
dados_fivb_jogadores_m_n_jogos <- dados_fivb_jogadores_m_n_jogos[!duplicated(dados_fivb_jogadores_m_n_jogos$match_player_id),]


#### Juntando as variáveis de médias móveis no dataframe com os jogos masculinos ----

# ID-partida-jogador para este dataframe também
dados_fivb_sem_na_m_n_jogos$match_player_id <- paste(dados_fivb_sem_na_m_n_jogos$match_id,
                                                     dados_fivb_sem_na_m_n_jogos$w_player1,
                                                     dados_fivb_sem_na_m_n_jogos$date,
                                                     dados_fivb_sem_na_m_n_jogos$match_num,
                                                     sep=" - ")

# Também removendo os ID-partida-jogador duplicados
dados_fivb_sem_na_m_n_jogos <- dados_fivb_sem_na_m_n_jogos[!duplicated(dados_fivb_sem_na_m_n_jogos$match_player_id),]



#### Inserindo os dados_fivb para os jogadores identificados como 'Jogador 1 - vencedor' ----
jogadores_fivb_filtrados_w1 <- dados_fivb_jogadores_m_n_jogos %>% filter(nr_player == 'W1')
jogadores_fivb_filtrados_w1 <- jogadores_fivb_filtrados_w1[,-2]

colnames(jogadores_fivb_filtrados_w1)[5] <- 'w_player1'

n = ncol(dados_fivb_sem_na_m_n_jogos)

dados_fivb_sem_na_m_n_jogos <- dados_fivb_sem_na_m_n_jogos %>%
  left_join(jogadores_fivb_filtrados_w1[,c(17:35)], by= 'match_player_id')


# Renomeando as variáveis para constar o 'w1' antes
for(i in (n+1):ncol(dados_fivb_sem_na_m_n_jogos)){
  colnames(dados_fivb_sem_na_m_n_jogos)[i] <- paste('w1_', colnames(dados_fivb_sem_na_m_n_jogos)[i], sep='' )
}


#### Inserindo os dados_fivb para os jogadores identificados como 'Jogador 2 - vencedor' ----

dados_fivb_sem_na_m_n_jogos$match_player_id <- paste(dados_fivb_sem_na_m_n_jogos$match_id,
                                                     dados_fivb_sem_na_m_n_jogos$w_player2,
                                                     dados_fivb_sem_na_m_n_jogos$date,
                                                     dados_fivb_sem_na_m_n_jogos$match_num,
                                                     sep=" - ")

jogadores_fivb_filtrados_w2 <- dados_fivb_jogadores_m_n_jogos %>% filter(nr_player == 'W2')
jogadores_fivb_filtrados_w2 <- jogadores_fivb_filtrados_w2[,-2]

colnames(jogadores_fivb_filtrados_w2)[5] <- 'w_player2'

n = ncol(dados_fivb_sem_na_m_n_jogos)

dados_fivb_sem_na_m_n_jogos <- dados_fivb_sem_na_m_n_jogos %>%
  left_join(jogadores_fivb_filtrados_w2[,c(17:35)], by= 'match_player_id')



# Renomeando as variáveis para constar o 'w2' antes
for(i in (n+1):ncol(dados_fivb_sem_na_m_n_jogos)){
  colnames(dados_fivb_sem_na_m_n_jogos)[i] <- paste('w2_', colnames(dados_fivb_sem_na_m_n_jogos)[i], sep='' )
}




#### Inserindo os dados_fivb para os jogadores identificados como 'Jogador 1 - perdedor' ----

dados_fivb_sem_na_m_n_jogos$match_player_id <- paste(dados_fivb_sem_na_m_n_jogos$match_id,
                                                     dados_fivb_sem_na_m_n_jogos$l_player1,
                                                     dados_fivb_sem_na_m_n_jogos$date,
                                                     dados_fivb_sem_na_m_n_jogos$match_num,
                                                     sep=" - ")

jogadores_fivb_filtrados_l1 <- dados_fivb_jogadores_m_n_jogos %>% filter(nr_player == 'L1')
jogadores_fivb_filtrados_l1 <- jogadores_fivb_filtrados_l1[,-2]

colnames(jogadores_fivb_filtrados_l1)[5] <- 'l_player1'

n = ncol(dados_fivb_sem_na_m_n_jogos)

dados_fivb_sem_na_m_n_jogos <- dados_fivb_sem_na_m_n_jogos %>%
  left_join(jogadores_fivb_filtrados_l1[,c(17:35)], by= 'match_player_id')


# Renomeando as variáveis para constar o 'l1' antes
for(i in (n+1):ncol(dados_fivb_sem_na_m_n_jogos)){
  colnames(dados_fivb_sem_na_m_n_jogos)[i] <- paste('l1_', colnames(dados_fivb_sem_na_m_n_jogos)[i], sep='' )
}




#### Inserindo os dados_fivb para os jogadores identificados como 'Jogador 2 - perdedor' ----

dados_fivb_sem_na_m_n_jogos$match_player_id <- paste(dados_fivb_sem_na_m_n_jogos$match_id,
                                                     dados_fivb_sem_na_m_n_jogos$l_player2,
                                                     dados_fivb_sem_na_m_n_jogos$date,
                                                     dados_fivb_sem_na_m_n_jogos$match_num,
                                                     sep=" - ")

jogadores_fivb_filtrados_l2 <- dados_fivb_jogadores_m_n_jogos %>% filter(nr_player == 'L2')
jogadores_fivb_filtrados_l2 <- jogadores_fivb_filtrados_l2[,-2]

colnames(jogadores_fivb_filtrados_l2)[5] <- 'l_player2'

n = ncol(dados_fivb_sem_na_m_n_jogos)

dados_fivb_sem_na_m_n_jogos <- dados_fivb_sem_na_m_n_jogos %>%
  left_join(jogadores_fivb_filtrados_l2[,c(17:35)], by= 'match_player_id')


# Renomeando as variáveis para constar o 'l2' antes
for(i in (n+1):ncol(dados_fivb_sem_na_m_n_jogos)){
  colnames(dados_fivb_sem_na_m_n_jogos)[i] <- paste('l2_', colnames(dados_fivb_sem_na_m_n_jogos)[i], sep='' )
}


#### Inserindo os dados relativos às duplas ----

duplas_fivb_todas_masc <- dados_fivb_gerais_duplas

duplas_fivb_todas_masc$match_id <- paste(duplas_fivb_todas_masc$match_id, duplas_fivb_todas_masc$date, sep = ' - ')

duplas_fivb_todas_masc <- duplas_fivb_todas_masc %>% arrange(player1, player2, date, match_num, match_id)

duplas_fivb_todas_masc$chave_dupla <- paste(duplas_fivb_todas_masc$player1, duplas_fivb_todas_masc$player2, sep = ' - ')


# Número de jogos juntos da dupla
duplas_fivb_todas_masc$nr_jogos <- 1

for(m in 1:(nrow(duplas_fivb_todas_masc)-1)){
  if(duplas_fivb_todas_masc[m+1, "chave_dupla"] == duplas_fivb_todas_masc[m, "chave_dupla"]){
    duplas_fivb_todas_masc[m+1, "nr_jogos"] <- duplas_fivb_todas_masc[m, "nr_jogos"] +1
  }
}


duplas_fivb_todas_masc$chave_dupla_result <- paste(duplas_fivb_todas_masc$player1, duplas_fivb_todas_masc$player2, duplas_fivb_todas_masc$result, sep = ' - ')


# Sequência de vitórias da duplas
duplas_fivb_todas_masc$streak_aux <- ifelse(duplas_fivb_todas_masc$result == 'L',-1,1)

duplas_fivb_todas_masc$streak <- 0

for(m in 2:nrow(duplas_fivb_todas_masc)){
  if(duplas_fivb_todas_masc[m-1, "chave_dupla"] == duplas_fivb_todas_masc[m, "chave_dupla"]){
    if(duplas_fivb_todas_masc[m-1,"streak_aux"] == -1){
      if(duplas_fivb_todas_masc[m-1,"streak"] >= 0){
        duplas_fivb_todas_masc[m, "streak"] <- -1
      } else{
        duplas_fivb_todas_masc[m, "streak"] <- duplas_fivb_todas_masc[m-1, "streak"] - 1
      }
    } else{
      if(duplas_fivb_todas_masc[m-1,"streak_aux"] == 1){
        if(duplas_fivb_todas_masc[m-1,"streak"] < 0){
          duplas_fivb_todas_masc[m, "streak"] <- 1
        } else{
          duplas_fivb_todas_masc[m, "streak"] <- duplas_fivb_todas_masc[m-1, "streak"] + 1
        }
      }
    }
  }
}


# Tempo entre cada jogo da dupla
duplas_fivb_todas_masc$tempo_entre_jogos <- 0

for(m in 1:(nrow(duplas_fivb_todas_masc)-1)){
  if(duplas_fivb_todas_masc[m+1, "chave_dupla"] == duplas_fivb_todas_masc[m, "chave_dupla"]){
    duplas_fivb_todas_masc[m+1,'tempo_entre_jogos'] <- difftime(duplas_fivb_todas_masc[m+1,"date"],duplas_fivb_todas_masc[m,"date"], units = 'days')
  }
}


# Número de títulos da dupla
dados_fivb_campeoes <- dados_fivb %>% group_by(tourn_id) %>% summarise(
  ult_jogo = max(match_num)
)

dados_fivb_campeoes$tourn_match_id <- paste(dados_fivb_campeoes$tourn_id, dados_fivb_campeoes$ult_jogo, sep = ' - ')

dados_fivb$tourn_match_id <- paste(dados_fivb$tourn_id, dados_fivb$match_num, sep = ' - ')

dados_fivb$jogo_titulo <- ifelse(dados_fivb$tourn_match_id %in% dados_fivb_campeoes$tourn_match_id, 'sim', 'nao')

dados_fivb_ult_jogo <- dados_fivb %>% filter(jogo_titulo == 'sim')

dados_fivb_ult_jogo$dupla_campea <- paste(dados_fivb_ult_jogo$w_player1, dados_fivb_ult_jogo$w_player2, sep = ' - ')

duplas_fivb_todas_masc$titulos <- 0

for(i in 1:nrow(duplas_fivb_todas_masc)){
  duplas_fivb_todas_masc[i,"titulos"] <- nrow(dados_fivb_ult_jogo[which(dados_fivb_ult_jogo$dupla_campea == duplas_fivb_todas_masc[i, 'chave_dupla'] & 
                                                                          dados_fivb_ult_jogo$date<duplas_fivb_todas_masc[i,"date"]),])
}



# Juntando as variáveis criadas ao dataframe com os jogos

duplas_fivb_todas_masc_w <- duplas_fivb_todas_masc %>% filter(result == 'W')
duplas_fivb_todas_masc_l <- duplas_fivb_todas_masc %>% filter(result == 'L')

dados_fivb_sem_na_m_n_jogos$match_id <- paste(dados_fivb_sem_na_m_n_jogos$match_id, dados_fivb_sem_na_m_n_jogos$date, sep=' - ')

dados_fivb_sem_na_m_n_jogos2 <- dados_fivb_sem_na_m_n_jogos %>%
  left_join(duplas_fivb_todas_masc_w[,c(1,8,11,12,13)], by= 'match_id')

colnames(dados_fivb_sem_na_m_n_jogos2)[c((ncol(dados_fivb_sem_na_m_n_jogos2)-3), (ncol(dados_fivb_sem_na_m_n_jogos2)-2), (ncol(dados_fivb_sem_na_m_n_jogos2)-1), ncol(dados_fivb_sem_na_m_n_jogos2))] <- c('nr_jogos_dupla_w', 'streak_dupla_w', 'tempo_entre_jogos_dupla_w', 'titulos_w')

dados_fivb_sem_na_m_n_jogos2 <- dados_fivb_sem_na_m_n_jogos2 %>%
  left_join(duplas_fivb_todas_masc_l[,c(1,8,11,12,13)], by= 'match_id')

colnames(dados_fivb_sem_na_m_n_jogos2)[c((ncol(dados_fivb_sem_na_m_n_jogos2)-3), (ncol(dados_fivb_sem_na_m_n_jogos2)-2), (ncol(dados_fivb_sem_na_m_n_jogos2)-1), ncol(dados_fivb_sem_na_m_n_jogos2))] <- c('nr_jogos_dupla_l', 'streak_dupla_l', 'tempo_entre_jogos_dupla_l', 'titulos_l')


#### Removendo todos os vazios e também os jogos em que houve descontinuidade antes ----

dados_fivb_masc_sexto_mais_jogo <- dados_fivb_sem_na_m_n_jogos2 %>% na.omit() %>% arrange(date, tournament, match_num)

dados_fivb_masc_sexto_mais_jogo <- dados_fivb_masc_sexto_mais_jogo[which(!(dados_fivb_masc_sexto_mais_jogo$w1_match_player_id %in% jogos_fivb_sem_na_desconsiderar$match_player_id) &
                                                                           !(dados_fivb_masc_sexto_mais_jogo$w2_match_player_id %in% jogos_fivb_sem_na_desconsiderar$match_player_id) &
                                                                           !(dados_fivb_masc_sexto_mais_jogo$l1_match_player_id %in% jogos_fivb_sem_na_desconsiderar$match_player_id) &
                                                                           !(dados_fivb_masc_sexto_mais_jogo$l2_match_player_id %in% jogos_fivb_sem_na_desconsiderar$match_player_id)),]


### FEMININO ----

#### Criando médias por pontos jogados em cada jogo ----

dados_fivb_jogadores_w$avg_atual_aces_points = dados_fivb_jogadores_w$tot_aces/dados_fivb_jogadores_w$points_played
dados_fivb_jogadores_w$avg_atual_attacks_points = dados_fivb_jogadores_w$tot_attacks/dados_fivb_jogadores_w$points_played
dados_fivb_jogadores_w$avg_atual_kills_points = dados_fivb_jogadores_w$tot_kills/dados_fivb_jogadores_w$points_played
dados_fivb_jogadores_w$avg_atual_blocks_points = dados_fivb_jogadores_w$tot_blocks/dados_fivb_jogadores_w$points_played
dados_fivb_jogadores_w$avg_atual_digs_points = dados_fivb_jogadores_w$tot_digs/dados_fivb_jogadores_w$points_played
dados_fivb_jogadores_w$avg_atual_errors_points = dados_fivb_jogadores_w$tot_errors/dados_fivb_jogadores_w$points_played
dados_fivb_jogadores_w$avg_atual_serve_errors_points = dados_fivb_jogadores_w$tot_serve_errors/dados_fivb_jogadores_w$points_played


# Filtrando somente por jogadores com ao menos 5 jogos na base
dados_fivb_jogadores_w_n_jogos <- dados_fivb_jogadores_w[dados_fivb_jogadores_w$player %in% dados_fivb_jogadores_grp_w$player,]


# Inputando as médias móveis das variáveis
for(i in 1:nrow(dados_fivb_jogadores_w_n_jogos)){
  #Travando para os casos em que é a primeira vez que aparece o jogador
  if(i!=1 && dados_fivb_jogadores_w_n_jogos[i-1, "player"] == dados_fivb_jogadores_w_n_jogos[i,"player"]){
    next
  }
  else{ 
    print(i)
    # j é a quantidade de jogos do jogador i
    j = length(dados_fivb_jogadores_w_n_jogos[which(dados_fivb_jogadores_w_n_jogos$player == dados_fivb_jogadores_w_n_jogos[i,"player"]), 'player'])
    # de i até ((i+j)-1) significa da primeira vez em que aparece o jogador até o último registro dele
    
    # Média móvel da hitting percentage
    dados_fivb_jogadores_w_n_jogos[i:(i+j-1), 'sma_hitpct'] <- (SMA(dados_fivb_jogadores_w_n_jogos[which(dados_fivb_jogadores_w_n_jogos$player == dados_fivb_jogadores_w_n_jogos[i,"player"]), 'tot_hitpct'], n=5)*5-dados_fivb_jogadores_w_n_jogos[i:(i+j-1),"tot_hitpct"])/4
    
    # Média móvel das médias por ponto jogado
    dados_fivb_jogadores_w_n_jogos[i:(i+j-1), 'sma_avg_aces_points'] <- (SMA(dados_fivb_jogadores_w_n_jogos[which(dados_fivb_jogadores_w_n_jogos$player == dados_fivb_jogadores_w_n_jogos[i,"player"]), 'avg_atual_aces_points'], n=5)*5-dados_fivb_jogadores_w_n_jogos[i:(i+j-1),"avg_atual_aces_points"])/4
    dados_fivb_jogadores_w_n_jogos[i:(i+j-1), 'sma_avg_attacks_points'] <- (SMA(dados_fivb_jogadores_w_n_jogos[which(dados_fivb_jogadores_w_n_jogos$player == dados_fivb_jogadores_w_n_jogos[i,"player"]), "avg_atual_attacks_points"], n=5)*5-dados_fivb_jogadores_w_n_jogos[i:(i+j-1),"avg_atual_attacks_points"])/4
    dados_fivb_jogadores_w_n_jogos[i:(i+j-1), 'sma_avg_kills_points'] <- (SMA(dados_fivb_jogadores_w_n_jogos[which(dados_fivb_jogadores_w_n_jogos$player == dados_fivb_jogadores_w_n_jogos[i,"player"]), 'avg_atual_kills_points'], n=5)*5-dados_fivb_jogadores_w_n_jogos[i:(i+j-1),"avg_atual_kills_points"])/4
    dados_fivb_jogadores_w_n_jogos[i:(i+j-1), 'sma_avg_blocks_points'] <- (SMA(dados_fivb_jogadores_w_n_jogos[which(dados_fivb_jogadores_w_n_jogos$player == dados_fivb_jogadores_w_n_jogos[i,"player"]), 'avg_atual_blocks_points'], n=5)*5-dados_fivb_jogadores_w_n_jogos[i:(i+j-1),"avg_atual_blocks_points"])/4
    dados_fivb_jogadores_w_n_jogos[i:(i+j-1), 'sma_avg_digs_points'] <- (SMA(dados_fivb_jogadores_w_n_jogos[which(dados_fivb_jogadores_w_n_jogos$player == dados_fivb_jogadores_w_n_jogos[i,"player"]), 'avg_atual_digs_points'], n=5)*5-dados_fivb_jogadores_w_n_jogos[i:(i+j-1),"avg_atual_digs_points"])/4
    dados_fivb_jogadores_w_n_jogos[i:(i+j-1), 'sma_avg_errors_points'] <- (SMA(dados_fivb_jogadores_w_n_jogos[which(dados_fivb_jogadores_w_n_jogos$player == dados_fivb_jogadores_w_n_jogos[i,"player"]), 'avg_atual_errors_points'], n=5)*5-dados_fivb_jogadores_w_n_jogos[i:(i+j-1),"avg_atual_errors_points"])/4
    dados_fivb_jogadores_w_n_jogos[i:(i+j-1), 'sma_avg_serve_errors_points'] <- (SMA(dados_fivb_jogadores_w_n_jogos[which(dados_fivb_jogadores_w_n_jogos$player == dados_fivb_jogadores_w_n_jogos[i,"player"]), 'avg_atual_serve_errors_points'], n=5)*5-dados_fivb_jogadores_w_n_jogos[i:(i+j-1),"avg_atual_serve_errors_points"])/4
    
    # Média móvel dos totais
    dados_fivb_jogadores_w_n_jogos[i:(i+j-1), 'sma_attacks'] <- (SMA(dados_fivb_jogadores_w_n_jogos[which(dados_fivb_jogadores_w_n_jogos$player == dados_fivb_jogadores_w_n_jogos[i,"player"]), "tot_attacks"], n=5)*5-dados_fivb_jogadores_w_n_jogos[i:(i+j-1),"tot_attacks"])/4
    dados_fivb_jogadores_w_n_jogos[i:(i+j-1), 'sma_kills'] <- (SMA(dados_fivb_jogadores_w_n_jogos[which(dados_fivb_jogadores_w_n_jogos$player == dados_fivb_jogadores_w_n_jogos[i,"player"]), 'tot_kills'], n=5)*5-dados_fivb_jogadores_w_n_jogos[i:(i+j-1),"tot_kills"])/4
    dados_fivb_jogadores_w_n_jogos[i:(i+j-1), 'sma_errors'] <- (SMA(dados_fivb_jogadores_w_n_jogos[which(dados_fivb_jogadores_w_n_jogos$player == dados_fivb_jogadores_w_n_jogos[i,"player"]), 'tot_errors'], n=5)*5-dados_fivb_jogadores_w_n_jogos[i:(i+j-1),"tot_errors"])/4
  }
}



# ID das partidas por jogador
dados_fivb_jogadores_w_n_jogos$match_player_id <- paste(dados_fivb_jogadores_w_n_jogos$match_id,
                                                        dados_fivb_jogadores_w_n_jogos$player,
                                                        dados_fivb_jogadores_w_n_jogos$date,
                                                        dados_fivb_jogadores_w_n_jogos$match_num,
                                                        sep=" - ")

# Removendo partidas com ID-partida-jogador duplicado
dados_fivb_jogadores_w_n_jogos <- dados_fivb_jogadores_w_n_jogos[!duplicated(dados_fivb_jogadores_w_n_jogos$match_player_id),]


#### Juntando as variáveis de médias móveis no dataframe com os jogos masculinos ----

# ID-partida-jogador para este dataframe também
dados_fivb_sem_na_w_n_jogos$match_player_id <- paste(dados_fivb_sem_na_w_n_jogos$match_id,
                                                     dados_fivb_sem_na_w_n_jogos$w_player1,
                                                     dados_fivb_sem_na_w_n_jogos$date,
                                                     dados_fivb_sem_na_w_n_jogos$match_num,
                                                     sep=" - ")

# Também removendo os ID-partida-jogador duplicados
dados_fivb_sem_na_w_n_jogos <- dados_fivb_sem_na_w_n_jogos[!duplicated(dados_fivb_sem_na_w_n_jogos$match_player_id),]



#### Inserindo os dados_fivb para os jogadores identificados como 'Jogador 1 - vencedor' ----
jogadores_fivb_filtrados_w1 <- dados_fivb_jogadores_w_n_jogos %>% filter(nr_player == 'W1')
jogadores_fivb_filtrados_w1 <- jogadores_fivb_filtrados_w1[,-2]

colnames(jogadores_fivb_filtrados_w1)[5] <- 'w_player1'

n = ncol(dados_fivb_sem_na_w_n_jogos)

dados_fivb_sem_na_w_n_jogos <- dados_fivb_sem_na_w_n_jogos %>%
  left_join(jogadores_fivb_filtrados_w1[,c(17:35)], by= 'match_player_id')


# Renomeando as variáveis para constar o 'w1' antes
for(i in (n+1):ncol(dados_fivb_sem_na_w_n_jogos)){
  colnames(dados_fivb_sem_na_w_n_jogos)[i] <- paste('w1_', colnames(dados_fivb_sem_na_w_n_jogos)[i], sep='' )
}


#### Inserindo os dados_fivb para os jogadores identificados como 'Jogador 2 - vencedor' ----

dados_fivb_sem_na_w_n_jogos$match_player_id <- paste(dados_fivb_sem_na_w_n_jogos$match_id,
                                                     dados_fivb_sem_na_w_n_jogos$w_player2,
                                                     dados_fivb_sem_na_w_n_jogos$date,
                                                     dados_fivb_sem_na_w_n_jogos$match_num,
                                                     sep=" - ")

jogadores_fivb_filtrados_w2 <- dados_fivb_jogadores_w_n_jogos %>% filter(nr_player == 'W2')
jogadores_fivb_filtrados_w2 <- jogadores_fivb_filtrados_w2[,-2]

colnames(jogadores_fivb_filtrados_w2)[5] <- 'w_player2'

n = ncol(dados_fivb_sem_na_w_n_jogos)

dados_fivb_sem_na_w_n_jogos <- dados_fivb_sem_na_w_n_jogos %>%
  left_join(jogadores_fivb_filtrados_w2[,c(17:35)], by= 'match_player_id')



# Renomeando as variáveis para constar o 'w2' antes
for(i in (n+1):ncol(dados_fivb_sem_na_w_n_jogos)){
  colnames(dados_fivb_sem_na_w_n_jogos)[i] <- paste('w2_', colnames(dados_fivb_sem_na_w_n_jogos)[i], sep='' )
}




#### Inserindo os dados_fivb para os jogadores identificados como 'Jogador 1 - perdedor' ----

dados_fivb_sem_na_w_n_jogos$match_player_id <- paste(dados_fivb_sem_na_w_n_jogos$match_id,
                                                     dados_fivb_sem_na_w_n_jogos$l_player1,
                                                     dados_fivb_sem_na_w_n_jogos$date,
                                                     dados_fivb_sem_na_w_n_jogos$match_num,
                                                     sep=" - ")

jogadores_fivb_filtrados_l1 <- dados_fivb_jogadores_w_n_jogos %>% filter(nr_player == 'L1')
jogadores_fivb_filtrados_l1 <- jogadores_fivb_filtrados_l1[,-2]

colnames(jogadores_fivb_filtrados_l1)[5] <- 'l_player1'

n = ncol(dados_fivb_sem_na_w_n_jogos)

dados_fivb_sem_na_w_n_jogos <- dados_fivb_sem_na_w_n_jogos %>%
  left_join(jogadores_fivb_filtrados_l1[,c(17:35)], by= 'match_player_id')


# Renomeando as variáveis para constar o 'l1' antes
for(i in (n+1):ncol(dados_fivb_sem_na_w_n_jogos)){
  colnames(dados_fivb_sem_na_w_n_jogos)[i] <- paste('l1_', colnames(dados_fivb_sem_na_w_n_jogos)[i], sep='' )
}




#### Inserindo os dados_fivb para os jogadores identificados como 'Jogador 2 - perdedor' ----

dados_fivb_sem_na_w_n_jogos$match_player_id <- paste(dados_fivb_sem_na_w_n_jogos$match_id,
                                                     dados_fivb_sem_na_w_n_jogos$l_player2,
                                                     dados_fivb_sem_na_w_n_jogos$date,
                                                     dados_fivb_sem_na_w_n_jogos$match_num,
                                                     sep=" - ")

jogadores_fivb_filtrados_l2 <- dados_fivb_jogadores_w_n_jogos %>% filter(nr_player == 'L2')
jogadores_fivb_filtrados_l2 <- jogadores_fivb_filtrados_l2[,-2]

colnames(jogadores_fivb_filtrados_l2)[5] <- 'l_player2'

n = ncol(dados_fivb_sem_na_w_n_jogos)

dados_fivb_sem_na_w_n_jogos <- dados_fivb_sem_na_w_n_jogos %>%
  left_join(jogadores_fivb_filtrados_l2[,c(17:35)], by= 'match_player_id')


# Renomeando as variáveis para constar o 'l2' antes
for(i in (n+1):ncol(dados_fivb_sem_na_w_n_jogos)){
  colnames(dados_fivb_sem_na_w_n_jogos)[i] <- paste('l2_', colnames(dados_fivb_sem_na_w_n_jogos)[i], sep='' )
}


#### Inserindo os dados relativos às duplas ----

duplas_fivb_todas_fem <- dados_fivb_gerais_duplas

duplas_fivb_todas_fem$match_id <- paste(duplas_fivb_todas_fem$match_id, duplas_fivb_todas_fem$date, sep = ' - ')

duplas_fivb_todas_fem <- duplas_fivb_todas_fem %>% arrange(player1, player2, date, match_num, match_id)

duplas_fivb_todas_fem$chave_dupla <- paste(duplas_fivb_todas_fem$player1, duplas_fivb_todas_fem$player2, sep = ' - ')


# Número de jogos juntos da dupla
duplas_fivb_todas_fem$nr_jogos <- 1

for(m in 1:(nrow(duplas_fivb_todas_fem)-1)){
  if(duplas_fivb_todas_fem[m+1, "chave_dupla"] == duplas_fivb_todas_fem[m, "chave_dupla"]){
    duplas_fivb_todas_fem[m+1, "nr_jogos"] <- duplas_fivb_todas_fem[m, "nr_jogos"] +1
  }
}


duplas_fivb_todas_fem$chave_dupla_result <- paste(duplas_fivb_todas_fem$player1, duplas_fivb_todas_fem$player2, duplas_fivb_todas_fem$result, sep = ' - ')


# Sequência de vitórias da duplas
duplas_fivb_todas_fem$streak_aux <- ifelse(duplas_fivb_todas_fem$result == 'L',-1,1)

duplas_fivb_todas_fem$streak <- 0

for(m in 2:nrow(duplas_fivb_todas_fem)){
  if(duplas_fivb_todas_fem[m-1, "chave_dupla"] == duplas_fivb_todas_fem[m, "chave_dupla"]){
    if(duplas_fivb_todas_fem[m-1,"streak_aux"] == -1){
      if(duplas_fivb_todas_fem[m-1,"streak"] >= 0){
        duplas_fivb_todas_fem[m, "streak"] <- -1
      } else{
        duplas_fivb_todas_fem[m, "streak"] <- duplas_fivb_todas_fem[m-1, "streak"] - 1
      }
    } else{
      if(duplas_fivb_todas_fem[m-1,"streak_aux"] == 1){
        if(duplas_fivb_todas_fem[m-1,"streak"] < 0){
          duplas_fivb_todas_fem[m, "streak"] <- 1
        } else{
          duplas_fivb_todas_fem[m, "streak"] <- duplas_fivb_todas_fem[m-1, "streak"] + 1
        }
      }
    }
  }
}


# Tempo entre cada jogo da dupla
duplas_fivb_todas_fem$tempo_entre_jogos <- 0

for(m in 1:(nrow(duplas_fivb_todas_fem)-1)){
  if(duplas_fivb_todas_fem[m+1, "chave_dupla"] == duplas_fivb_todas_fem[m, "chave_dupla"]){
    duplas_fivb_todas_fem[m+1,'tempo_entre_jogos'] <- difftime(duplas_fivb_todas_fem[m+1,"date"],duplas_fivb_todas_fem[m,"date"], units = 'days')
  }
}


duplas_fivb_todas_fem$titulos <- 0

for(i in 1:nrow(duplas_fivb_todas_fem)){
  duplas_fivb_todas_fem[i,"titulos"] <- nrow(dados_fivb_ult_jogo[which(dados_fivb_ult_jogo$dupla_campea == duplas_fivb_todas_fem[i, 'chave_dupla'] & 
                                                                         dados_fivb_ult_jogo$date<duplas_fivb_todas_fem[i,"date"]),])
}



# Juntando as variáveis criadas ao dataframe com os jogos

duplas_fivb_todas_fem_w <- duplas_fivb_todas_fem %>% filter(result == 'W')
duplas_fivb_todas_fem_l <- duplas_fivb_todas_fem %>% filter(result == 'L')

dados_fivb_sem_na_w_n_jogos$match_id <- paste(dados_fivb_sem_na_w_n_jogos$match_id, dados_fivb_sem_na_w_n_jogos$date, sep=' - ')

dados_fivb_sem_na_w_n_jogos2 <- dados_fivb_sem_na_w_n_jogos %>%
  left_join(duplas_fivb_todas_fem_w[,c(1,8,11,12,13)], by= 'match_id')

colnames(dados_fivb_sem_na_w_n_jogos2)[c((ncol(dados_fivb_sem_na_w_n_jogos2)-3), (ncol(dados_fivb_sem_na_w_n_jogos2)-2), (ncol(dados_fivb_sem_na_w_n_jogos2)-1), ncol(dados_fivb_sem_na_w_n_jogos2))] <- c('nr_jogos_dupla_w', 'streak_dupla_w', 'tempo_entre_jogos_dupla_w', 'titulos_w')

dados_fivb_sem_na_w_n_jogos2 <- dados_fivb_sem_na_w_n_jogos2 %>%
  left_join(duplas_fivb_todas_fem_l[,c(1,8,11,12,13)], by= 'match_id')

colnames(dados_fivb_sem_na_w_n_jogos2)[c((ncol(dados_fivb_sem_na_w_n_jogos2)-3), (ncol(dados_fivb_sem_na_w_n_jogos2)-2), (ncol(dados_fivb_sem_na_w_n_jogos2)-1), ncol(dados_fivb_sem_na_w_n_jogos2))] <- c('nr_jogos_dupla_l', 'streak_dupla_l', 'tempo_entre_jogos_dupla_l', 'titulos_l')




#### Removendo todos os vazios e também os jogos em que houve descontinuidade antes ----

dados_fivb_fem_sexto_mais_jogo <- dados_fivb_sem_na_w_n_jogos2 %>% na.omit() %>% arrange(date, tournament, match_num)

dados_fivb_fem_sexto_mais_jogo <- dados_fivb_fem_sexto_mais_jogo[which(!(dados_fivb_fem_sexto_mais_jogo$w1_match_player_id %in% jogos_fivb_sem_na_desconsiderar$match_player_id) &
                                                                         !(dados_fivb_fem_sexto_mais_jogo$w2_match_player_id %in% jogos_fivb_sem_na_desconsiderar$match_player_id) &
                                                                         !(dados_fivb_fem_sexto_mais_jogo$l1_match_player_id %in% jogos_fivb_sem_na_desconsiderar$match_player_id) &
                                                                         !(dados_fivb_fem_sexto_mais_jogo$l2_match_player_id %in% jogos_fivb_sem_na_desconsiderar$match_player_id)),]
# Agora sim, análises exploratórias ----

# Distribuições das variáveis
variaveis_tentativa1_masc_ambos$genero <- 'M'
variaveis_tentativa1_fem_ambos$genero <- 'F'
variaveis_todos_generos <- rbind(variaveis_tentativa1_masc_ambos,
                                 variaveis_tentativa1_fem_ambos)
variaveis_todos_generos$dupla_ref_rank <- as.integer(variaveis_todos_generos$dupla_ref_rank)
colnames(variaveis_todos_generos)

grf1 <- variaveis_todos_generos %>%
  ggplot( aes(x=variaveis_todos_generos[,2], fill = genero )) +
  geom_density(alpha=0.8) +
  xlab(colnames(variaveis_todos_generos[2]))+
  ylab("Densidade")+
  labs(fill="Gênero")+
  theme(legend.position = "top", axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))

grf2 <- variaveis_todos_generos %>%
  ggplot( aes(x=variaveis_todos_generos[,3], fill = genero )) +
  geom_density(alpha=0.8) +
  ylab("Densidade")+
  xlab(colnames(variaveis_todos_generos[3]))+
  theme(legend.position = "none",axis.title.y=element_blank(), axis.title.x = element_text(size=9))

grf3 <- variaveis_todos_generos %>%
  ggplot( aes(x=variaveis_todos_generos[,4], fill = genero )) +
  geom_density(alpha=0.8) +
  ylab("Densidade")+
  xlab(colnames(variaveis_todos_generos[4]))+
  theme(legend.position = "none",axis.title.y=element_blank(), axis.title.x = element_text(size=9))

grf4 <- variaveis_todos_generos %>%
  ggplot( aes(x=variaveis_todos_generos[,5], fill = genero )) +
  geom_density(alpha=0.8) +
  ylab("Densidade")+
  xlab(colnames(variaveis_todos_generos[5]))+
  theme(legend.position = "none",axis.title.y=element_blank(), axis.title.x = element_text(size=9))


grf6 <- variaveis_todos_generos %>%
  ggplot( aes(x=variaveis_todos_generos[,7], fill = genero )) +
  geom_density(alpha=0.8) +
  ylab("Densidade")+
  xlab(colnames(variaveis_todos_generos[7]))+
  theme(legend.position = "none", axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))

grf7 <- variaveis_todos_generos %>%
  ggplot( aes(x=variaveis_todos_generos[,8], fill = genero )) +
  geom_density(alpha=0.8) +
  ylab("Densidade")+
  xlab(colnames(variaveis_todos_generos[8]))+
  theme(legend.position = "none",axis.title.y=element_blank(), axis.title.x = element_text(size=9))

grf8 <- variaveis_todos_generos %>%
  ggplot( aes(x=variaveis_todos_generos[,9], fill = genero )) +
  geom_density(alpha=0.8) +
  ylab("Densidade")+
  xlab(colnames(variaveis_todos_generos[9]))+
  theme(legend.position = "none",axis.title.y=element_blank(), axis.title.x = element_text(size=9))

grf9 <- variaveis_todos_generos %>%
  ggplot( aes(x=variaveis_todos_generos[,10], fill = genero )) +
  geom_density(alpha=0.8) +
  ylab("Densidade")+
  xlab(colnames(variaveis_todos_generos[10]))+
  theme(legend.position = "none",axis.title.y=element_blank(), axis.title.x = element_text(size=9))

grf10 <- variaveis_todos_generos %>%
  ggplot( aes(x=variaveis_todos_generos[,11], fill = genero )) +
  geom_density(alpha=0.8) +
  ylab("Densidade")+
  xlab(colnames(variaveis_todos_generos[11]))+
  theme(legend.position = "none", axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))

grf11 <- variaveis_todos_generos %>%
  ggplot( aes(x=variaveis_todos_generos[,12], fill = genero )) +
  geom_density(alpha=0.8) +
  ylab("Densidade")+
  xlab(colnames(variaveis_todos_generos[12]))+
  theme(legend.position = "none",axis.title.y=element_blank(), axis.title.x = element_text(size=9))

grf12 <- variaveis_todos_generos %>%
  ggplot( aes(x=variaveis_todos_generos[,13], fill = genero )) +
  geom_density(alpha=0.8) +
  ylab("Densidade")+
  xlab(colnames(variaveis_todos_generos[13]))+
  theme(legend.position = "none",axis.title.y=element_blank(), axis.title.x = element_text(size=9))

grf13 <- variaveis_todos_generos %>%
  ggplot( aes(x=variaveis_todos_generos[,14], fill = genero )) +
  geom_density(alpha=0.8) +
  ylab("Densidade")+
  xlab(colnames(variaveis_todos_generos[14]))+
  theme(legend.position = "none",axis.title.y=element_blank(), axis.title.x = element_text(size=9))

grf14 <- variaveis_todos_generos %>%
  ggplot( aes(x=variaveis_todos_generos[,15], fill = genero )) +
  geom_density(alpha=0.8) +
  ylab("Densidade")+
  xlab(colnames(variaveis_todos_generos[15]))+
  theme(legend.position = "none", axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))

grf15 <- variaveis_todos_generos %>%
  ggplot( aes(x=variaveis_todos_generos[,16], fill = genero )) +
  geom_density(alpha=0.8) +
  ylab("Densidade")+
  xlab(colnames(variaveis_todos_generos[16]))+
  theme(legend.position = "none",axis.title.y=element_blank(), axis.title.x = element_text(size=9))

grf16 <- variaveis_todos_generos %>%
  ggplot( aes(x=variaveis_todos_generos[,17], fill = genero )) +
  geom_density(alpha=0.8) +
  ylab("Densidade")+
  xlab(colnames(variaveis_todos_generos[17]))+
  theme(legend.position = "none",axis.title.y=element_blank(), axis.title.x = element_text(size=9))


grf18 <- variaveis_todos_generos %>%
  ggplot( aes(x=variaveis_todos_generos[,19], fill = genero )) +
  geom_density(alpha=0.8) +
  ylab("Densidade")+
  xlab(colnames(variaveis_todos_generos[19]))+
  theme(legend.position = "none",axis.title.y=element_blank(), axis.title.x = element_text(size=9))

grf19 <- variaveis_todos_generos %>%
  ggplot( aes(x=variaveis_todos_generos[,20], fill = genero )) +
  geom_density(alpha=0.8) +
  ylab("Densidade")+
  xlab(colnames(variaveis_todos_generos[20]))+
  theme(legend.position = "none", axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))

grf20 <- variaveis_todos_generos %>%
  ggplot( aes(x=variaveis_todos_generos[,21], fill = genero )) +
  geom_density(alpha=0.8) +
  ylab("Densidade")+
  xlab(colnames(variaveis_todos_generos[21]))+
  theme(legend.position = "none",axis.title.y=element_blank(), axis.title.x = element_text(size=9))

grf21 <- variaveis_todos_generos %>%
  ggplot( aes(x=variaveis_todos_generos[,22], fill = genero )) +
  geom_density(alpha=0.8) +
  ylab("Densidade")+
  xlab(colnames(variaveis_todos_generos[22]))+
  theme(legend.position = "none",axis.title.y=element_blank(), axis.title.x = element_text(size=9))

grf22 <- variaveis_todos_generos %>%
  ggplot( aes(x=variaveis_todos_generos[,23], fill = genero )) +
  geom_density(alpha=0.8) +
  ylab("Densidade")+
  xlab(colnames(variaveis_todos_generos[23]))+
  theme(legend.position = "none",axis.title.y=element_blank(), axis.title.x = element_text(size=9))

grf23 <- variaveis_todos_generos %>%
  ggplot( aes(x=variaveis_todos_generos[,24], fill = genero )) +
  geom_density(alpha=0.8) +
  ylab("Densidade")+
  xlab(colnames(variaveis_todos_generos[24]))+
  theme(legend.position = "none", axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))

grf24 <- variaveis_todos_generos %>%
  ggplot( aes(x=variaveis_todos_generos[,25], fill = genero )) +
  geom_density(alpha=0.8) +
  ylab("Densidade")+
  xlab(colnames(variaveis_todos_generos[25]))+
  theme(legend.position = "none",axis.title.y=element_blank(), axis.title.x = element_text(size=9))






get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(grf1)

grf1 <- grf1 + theme(legend.position="none")

blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
  cowplot::theme_nothing()

# Top-left legend
grid.arrange(grf1, grf2, grf3,grf4,
             grf6, grf7, grf8, grf9, 
             grf10, grf11, grf12, grf13, 
             grf14, grf15, grf16, grf18, 
             grf19, grf20, grf21, grf22, 
             grf23, grf24, legend, blankPlot,
             ncol=4, nrow = 6, 
             widths = c(1.2, 1.2,1.2, 1.2), 
             heights = c(2.5,2.5,2.5,2.5,2.5,2.5))


# Quantidade


qtds_sem_na <- as.data.frame(rbind(cbind('Masculino', 'AVP', nrow(dados_masc_sexto_mais_jogo),
                                         format(round(mean((dados_masc_sexto_mais_jogo$w_p1_age +
                                                              dados_masc_sexto_mais_jogo$w_p2_age +
                                                              dados_masc_sexto_mais_jogo$l_p1_age + 
                                                              dados_masc_sexto_mais_jogo$l_p2_age)/4), digits=2), nsmall=2),
                                         format(round(2.54*mean((dados_masc_sexto_mais_jogo$w_p1_hgt +
                                                                   dados_masc_sexto_mais_jogo$w_p2_hgt +
                                                                   dados_masc_sexto_mais_jogo$l_p1_hgt + 
                                                                   dados_masc_sexto_mais_jogo$l_p2_hgt)/4), digits=2), nsmall=2),
                                         length(unique(dados_masc_sexto_mais_jogo$tourn_id)),
                                         paste(min(dados_masc_sexto_mais_jogo$year), max(dados_masc_sexto_mais_jogo$year), sep='-')),
                                   cbind('Masculino', 'FIVB', nrow(dados_fivb_masc_sexto_mais_jogo),
                                         format(round(mean((dados_fivb_masc_sexto_mais_jogo$w_p1_age +
                                                              dados_fivb_masc_sexto_mais_jogo$w_p2_age +
                                                              dados_fivb_masc_sexto_mais_jogo$l_p1_age + 
                                                              dados_fivb_masc_sexto_mais_jogo$l_p2_age)/4), digits=2), nsmall=2),
                                         format(round(2.54*mean((dados_fivb_masc_sexto_mais_jogo$w_p1_hgt +
                                                                   dados_fivb_masc_sexto_mais_jogo$w_p2_hgt +
                                                                   dados_fivb_masc_sexto_mais_jogo$l_p1_hgt + 
                                                                   dados_fivb_masc_sexto_mais_jogo$l_p2_hgt)/4), digits=2), nsmall=2),
                                         length(unique(dados_fivb_masc_sexto_mais_jogo$tourn_id)),
                                         paste(min(dados_fivb_masc_sexto_mais_jogo$year), max(dados_fivb_masc_sexto_mais_jogo$year), sep='-')),
                                   cbind('Feminino', 'AVP', nrow(dados_fem_sexto_mais_jogo),
                                         format(round(mean((dados_fem_sexto_mais_jogo$w_p1_age +
                                                              dados_fem_sexto_mais_jogo$w_p2_age +
                                                              dados_fem_sexto_mais_jogo$l_p1_age + 
                                                              dados_fem_sexto_mais_jogo$l_p2_age)/4), digits=2), nsmall=2),
                                         format(round(2.54*mean((dados_fem_sexto_mais_jogo$w_p1_hgt +
                                                                   dados_fem_sexto_mais_jogo$w_p2_hgt +
                                                                   dados_fem_sexto_mais_jogo$l_p1_hgt + 
                                                                   dados_fem_sexto_mais_jogo$l_p2_hgt)/4), digits=2), nsmall=2),
                                         length(unique(dados_fem_sexto_mais_jogo$tourn_id)),
                                         paste(min(dados_fem_sexto_mais_jogo$year), max(dados_fem_sexto_mais_jogo$year), sep='-')),
                                   cbind('Feminino', 'FIVB', nrow(dados_fivb_fem_sexto_mais_jogo),
                                         format(round(mean((dados_fivb_fem_sexto_mais_jogo$w_p1_age +
                                                              dados_fivb_fem_sexto_mais_jogo$w_p2_age +
                                                              dados_fivb_fem_sexto_mais_jogo$l_p1_age + 
                                                              dados_fivb_fem_sexto_mais_jogo$l_p2_age)/4), digits=2), nsmall=2),
                                         format(round(2.54*mean((dados_fivb_fem_sexto_mais_jogo$w_p1_hgt +
                                                                   dados_fivb_fem_sexto_mais_jogo$w_p2_hgt +
                                                                   dados_fivb_fem_sexto_mais_jogo$l_p1_hgt + 
                                                                   dados_fivb_fem_sexto_mais_jogo$l_p2_hgt)/4), digits=2), nsmall=2),
                                         length(unique(dados_fivb_fem_sexto_mais_jogo$tourn_id)),
                                         paste(min(dados_fivb_fem_sexto_mais_jogo$year), max(dados_fivb_fem_sexto_mais_jogo$year), sep='-'))))

colnames(qtds_sem_na) <- c('Gênero', 'Circuito', 'Qtd', 'Idade Media', 'Altura Média (cm)', 'Torneios', 'Período')


## Tabelas da análise exploratória ----
qtds_sem_na %>% gt(groupname_col = 'Circuito', row_group_as_column = TRUE) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_row_groups( groups= everything())) %>%
  tab_style(
    style = list(cell_text(weight = 'bold')),
    locations = cells_column_labels())


paises_jogadores_w1 <- rbind(dados_masc_sexto_mais_jogo[,c("circuit","gender", "w_p1_country", "w_player1")],
                             dados_fem_sexto_mais_jogo[,c("circuit","gender", "w_p1_country", "w_player1")],
                             dados_fivb_masc_sexto_mais_jogo[,c("circuit","gender", "w_p1_country", "w_player1")],
                             dados_fivb_fem_sexto_mais_jogo[,c("circuit","gender", "w_p1_country", "w_player1")]
)

colnames(paises_jogadores_w1) <- c('Circuito', 'Gênero', 'País', 'Jogador')

paises_jogadores_w2 <- rbind(dados_masc_sexto_mais_jogo[,c("circuit","gender", "w_p2_country", "w_player2")],
                             dados_fem_sexto_mais_jogo[,c("circuit","gender", "w_p2_country", "w_player2")],
                             dados_fivb_masc_sexto_mais_jogo[,c("circuit","gender", "w_p2_country", "w_player2")],
                             dados_fivb_fem_sexto_mais_jogo[,c("circuit","gender", "w_p2_country", "w_player2")]
)

colnames(paises_jogadores_w2) <- colnames(paises_jogadores_w1)

paises_jogadores_l1 <- rbind(dados_masc_sexto_mais_jogo[,c("circuit","gender", "l_p1_country", "l_player1")],
                             dados_fem_sexto_mais_jogo[,c("circuit","gender", "l_p1_country", "l_player1")],
                             dados_fivb_masc_sexto_mais_jogo[,c("circuit","gender", "l_p1_country", "l_player1")],
                             dados_fivb_fem_sexto_mais_jogo[,c("circuit","gender", "l_p1_country", "l_player1")]
)

colnames(paises_jogadores_l1) <- colnames(paises_jogadores_w1)

paises_jogadores_l2 <- rbind(dados_masc_sexto_mais_jogo[,c("circuit","gender", "l_p2_country", "l_player2")],
                             dados_fem_sexto_mais_jogo[,c("circuit","gender", "l_p2_country", "l_player2")],
                             dados_fivb_masc_sexto_mais_jogo[,c("circuit","gender", "l_p2_country", "l_player2")],
                             dados_fivb_fem_sexto_mais_jogo[,c("circuit","gender", "l_p2_country", "l_player2")]
)

colnames(paises_jogadores_l2) <- colnames(paises_jogadores_w1)


paises_jogadores <- rbind(paises_jogadores_l1, paises_jogadores_l2, 
                          paises_jogadores_w1, paises_jogadores_w2)

paises_jogadores <- paises_jogadores[!duplicated(paises_jogadores$Jogador),]

paises_jogadores_grp <- paises_jogadores %>% group_by(País, Gênero, Circuito) %>% summarise(
  tot = n()
) %>% arrange(Gênero, Circuito, -tot)

tot_m_avp <- sum(paises_jogadores_grp[which(paises_jogadores_grp$Gênero == 'M' & 
                                              paises_jogadores_grp$Circuito == 'AVP'), "tot"])

tot_f_avp <- sum(paises_jogadores_grp[which(paises_jogadores_grp$Gênero == 'W' & 
                                              paises_jogadores_grp$Circuito == 'AVP'), "tot"])

tot_m_fivb <- sum(paises_jogadores_grp[which(paises_jogadores_grp$Gênero == 'M' & 
                                               paises_jogadores_grp$Circuito == 'FIVB'), "tot"])

tot_f_fivb <- sum(paises_jogadores_grp[which(paises_jogadores_grp$Gênero == 'W' & 
                                               paises_jogadores_grp$Circuito == 'FIVB'), "tot"])


for(i in 1:nrow(paises_jogadores_grp)){
  if(paises_jogadores_grp[i,"Circuito"] == 'AVP' & paises_jogadores_grp[i,"Gênero"] == 'M'){
    paises_jogadores_grp[i,'Percentagem'] <- paises_jogadores_grp[i,"tot"]/tot_m_avp
  } else if(paises_jogadores_grp[i,"Circuito"] == 'AVP' & paises_jogadores_grp[i,"Gênero"] == 'W'){
    paises_jogadores_grp[i,'Percentagem'] <- paises_jogadores_grp[i,"tot"]/tot_f_avp
  } else if(paises_jogadores_grp[i,"Circuito"] == 'FIVB' & paises_jogadores_grp[i,"Gênero"] == 'M'){
    paises_jogadores_grp[i,'Percentagem'] <- paises_jogadores_grp[i,"tot"]/tot_m_fivb
  } else{
    paises_jogadores_grp[i,'Percentagem'] <- paises_jogadores_grp[i,"tot"]/tot_f_fivb
  }
}

paises_jogadores_grp$acum <- paises_jogadores_grp$Percentagem

for(i in 2:nrow(paises_jogadores_grp)){
  if(paises_jogadores_grp[i,"Circuito"] == paises_jogadores_grp[i-1, "Circuito"] &
     paises_jogadores_grp[i, "Gênero"] == paises_jogadores_grp[i-1, "Gênero"]){
    paises_jogadores_grp[i,"acum"] <- paises_jogadores_grp[i-1, "acum"] + paises_jogadores_grp[i, "Percentagem"]
  }
}

for(i in 1:nrow(paises_jogadores_grp)){
  if(paises_jogadores_grp[i, "Percentagem"] >= 0.5){
    paises_jogadores_grp[i,'agrupar'] <- 'não'
  } else if(paises_jogadores_grp[i,"Circuito"] == paises_jogadores_grp[i-1, "Circuito"] &
            paises_jogadores_grp[i, "Gênero"] == paises_jogadores_grp[i-1, "Gênero"] &
            paises_jogadores_grp[i, "acum"] <= 0.5){
    paises_jogadores_grp[i,'agrupar'] <- 'não'
  } else if(paises_jogadores_grp[i,"Circuito"] == paises_jogadores_grp[i-1, "Circuito"] &
            paises_jogadores_grp[i, "Gênero"] == paises_jogadores_grp[i-1, "Gênero"] &
            paises_jogadores_grp[i, "acum"] > 0.5 &
            paises_jogadores_grp[i-1, "acum"] < 0.5){
    paises_jogadores_grp[i,'agrupar'] <- 'não'
  } else{
    paises_jogadores_grp[i,'agrupar'] <- 'sim'
  }
}

paises_jogadores_grp[which(paises_jogadores_grp$País == 'Czech Republic'), "País"] <- 'República Tcheca'
paises_jogadores_grp[which(paises_jogadores_grp$País == 'Germany'), "País"] <- 'Alemanha'
paises_jogadores_grp[which(paises_jogadores_grp$País == 'Latvia'), "País"] <- 'Letônia'
paises_jogadores_grp[which(paises_jogadores_grp$País == 'Netherlands'), "País"] <- 'Países Baixos'
paises_jogadores_grp[which(paises_jogadores_grp$País == 'United States'), "País"] <- 'EUA'



paises_jogadores_grp$pais_ajustado <- ifelse(paises_jogadores_grp$agrupar == 'não',
                                             paises_jogadores_grp$País,
                                             'Outros')

paises_jogadores_grp[which(paises_jogadores_grp$Gênero == 'M'), 'Gênero'] <- 'Masculino' 
paises_jogadores_grp[which(paises_jogadores_grp$Gênero == 'W'), 'Gênero'] <- 'Feminino' 


ggplot(paises_jogadores_grp, aes(fill = fct_reorder(as.factor(pais_ajustado),Percentagem), y = tot, x=Circuito)) + 
  geom_bar(position="fill", stat="identity")+
  facet_grid(~ Gênero)+
  ylab("Percentual")+
  labs(fill = "País")+
  theme_minimal(base_size=14)+
  scale_fill_brewer(palette="Spectral")

dados_masc_sexto_mais_jogo_ano <- dados_masc_sexto_mais_jogo %>% group_by(year) %>% summarise(
  total = n()
)

dados_masc_sexto_mais_jogo_ano$Gênero <- 'Masculino'
dados_masc_sexto_mais_jogo_ano$Circuito <- 'AVP'


dados_fivb_masc_sexto_mais_jogo_ano <- dados_fivb_masc_sexto_mais_jogo %>% group_by(year) %>% summarise(
  total = n()
)

dados_fivb_masc_sexto_mais_jogo_ano$Gênero <- 'Masculino'
dados_fivb_masc_sexto_mais_jogo_ano$Circuito <- 'FIVB'


dados_fem_sexto_mais_jogo_ano <- dados_fem_sexto_mais_jogo %>% group_by(year) %>% summarise(
  total = n()
)

dados_fem_sexto_mais_jogo_ano$Gênero <- 'Feminino'
dados_fem_sexto_mais_jogo_ano$Circuito <- 'AVP'


dados_fivb_fem_sexto_mais_jogo_ano <- dados_fivb_fem_sexto_mais_jogo %>% group_by(year) %>% summarise(
  total = n()
)

dados_fivb_fem_sexto_mais_jogo_ano$Gênero <- 'Feminino'
dados_fivb_fem_sexto_mais_jogo_ano$Circuito <- 'FIVB'



qtd_jogos_ano <- rbind(dados_masc_sexto_mais_jogo_ano,
                       dados_fivb_masc_sexto_mais_jogo_ano,
                       dados_fem_sexto_mais_jogo_ano,
                       dados_fivb_fem_sexto_mais_jogo_ano)


ggplot(qtd_jogos_ano, aes(x=year, y=total, group=Gênero)) +
  geom_line(aes(color=Gênero))+
  geom_point(aes(color=Gênero))+
  facet_wrap(~ Circuito, ncol=1, scales="free_y")+
  ylab("Qtd de Partidas")+
  xlab('Ano')+
  labs(fill = "Gênero")+
  theme_minimal(base_size=14)+
  scale_fill_brewer(palette="Spectral")
