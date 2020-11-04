######################################################################
############ Script de atualização do painel 2020 ####################
###########         Painel de hapatite A                 #############
########### V.1.0 - Desenvolvido por Mikael Lemos ####################
######################################################################

#### Carregando bibliotecas ####

#install.packages('dplyr')
library('dplyr')

#install.packages("tidyr")
library('tidyr')

#install.packages("data.table")
library('data.table')

#install.packages('stringr')
library('stringr')

#install.packages('Amelia')
library('Amelia')

# install.packages("tidyverse")
library(tidyverse)

# install.packages("lubridate")
library(lubridate)

# install.packages("ggplot2")
library(ggplot2)

#library(xlsx)

library(rJava)

#install.packages("read.dbc")

library(read.dbc)

#install.packages("forcats")

#library(forcats)

library("foreign")

#install.packages("foreign")

#install.packages("openxlsx")

library("openxlsx")

library(RColorBrewer)

library(Amelia)

#install.packages("forcats")
library(forcats)

library(readr)

#install.packages("fs")

library(fs)

##############################
#### Carregando banco SINAN - hepatite A

sinan_hepatite_A <- read.dbf("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto3/SINAN_2020_A_B_C/Banco_hepatite_A_2020.dbf")

## Checando preenchimento das datas
sinan_hep_A_datas <- select(sinan_hepatite_A, DT_NOTIFIC, DT_SIN_PRI,DT_DIGITA, DT_TRANSUS, DT_TRANSDM, DT_TRANSSM, DT_TRANSRM, DT_TRANSRS, DT_TRASSE, DT_INVEST, DT_ENCERRA, DTNOT_WIN, DT_ENCER_A, NU_ANO_WIN, NU_ANO_NET )
sinan_hep_A_fonte <- select(sinan_hepatite_A, FONTE_NET, FONTE_WIN )

missmap(sinan_hep_A_fonte)

## Unindo colunas complementares - data notificação
sinan_hepatite_A$NU_ANO_NET <- as.character(sinan_hepatite_A$NU_ANO_NET)
sinan_hepatite_A$NU_ANO_WIN <- as.character(sinan_hepatite_A$NU_ANO_WIN)

sinan_hepatite_A$FONTE_NET <- as.character(sinan_hepatite_A$FONTE_NET)
sinan_hepatite_A$FONTE_WIN <- as.character(sinan_hepatite_A$FONTE_WIN)

sinan_hepatite_A <- sinan_hepatite_A %>% mutate(ano_notificacao = coalesce(NU_ANO_WIN, NU_ANO_NET))

sinan_hepatite_A <- sinan_hepatite_A %>% mutate(dt_notificacao = coalesce(DT_NOTIFIC, DTNOT_WIN))

sinan_hepatite_A <- sinan_hepatite_A %>% mutate(fonte = coalesce(FONTE_NET, FONTE_WIN))

data_notif <- select(sinan_hepatite_A, ano_notificacao)

data_notif2 <- select(sinan_hepatite_A, dt_notificacao)

fonte <- select(sinan_hepatite_A, fonte)

missmap(data_notif)
missmap(data_notif2)
missmap(fonte)

sinan_2019_hepatite_A <- filter(sinan_hepatite_A, ano_notificacao == 2019 )

##### Painel - Informações do SINAN - Causa da infecção, Sexo, Faixa estária

sinan_2019_hepatite_A_sel1 <- select(sinan_2019_hepatite_A, NOME, NOMEMAE, DTNASC, MUNRES, SEXO, ID_MUNICIP, ID_UNIDADE, NM_PACIENT, NU_IDADE_N, CS_SEXO_NE, CS_RACA_NE, CS_CNS_SUS, HIV_NET, OUTRA_DST, SEXUAL_NET, TATU_PIER, MATBIOLOGI, INAL_CRACK, ACUPUNTURA, TRANSFUSAO, INJETAVEIS, CIRURGICO, AGUA_ALIME, DENTARIO_N, TRESMAIS_N, HEMODIALIS, TRANSPLA_N, OUTRAS_NET, CLASSI_FIN,FORMA_NET,CLAS_ETIOL, DSFONTE_NE, SEXO_WIN, IDADE_WIN, RACA_WIN,FORMA_WIN, FONTE_WIN, ANTIHAVI_A, ALCOOLISMO, TRANSFUS_A, ACUPUNTU_A, HEMODIAL_A, EXPOSICAO, TATUAGEM_W, INJETAVE_A, INJETAVE_A, CIRURGIC_A, PIERCING_W, INALAVEIS, DENTARIO_W, TRANSPLA_W, ACIDPERC_W, TRESMAIS_W, PARTO_WIN, PARCEIROS, DST_WIN, SEXUAL_WIN, ANTIHAVIGG, ano_notificacao  )

missmap(sinan_2019_hepatite_A_sel1)

sinan_2019_hepatite_A_sel2 <- select(sinan_2019_hepatite_A, NOME, NOMEMAE, DTNASC, MUNRES, SEXO, ID_MUNICIP, ID_UNIDADE, NM_PACIENT, NU_IDADE_N, CS_SEXO_NE, CS_RACA_NE, HIV_NET, OUTRA_DST, SEXUAL_NET, TATU_PIER, MATBIOLOGI, INAL_CRACK, ACUPUNTURA, TRANSFUSAO, INJETAVEIS, CIRURGICO, AGUA_ALIME, DENTARIO_N, TRESMAIS_N, HEMODIALIS, TRANSPLA_N, OUTRAS_NET, CLASSI_FIN,FORMA_NET,CLAS_ETIOL, ano_notificacao, dt_notificacao, fonte  )

missmap(sinan_2019_hepatite_A_sel2)

colnames(sinan_2019_hepatite_A_sel2)

sinan_2019_hepatite_A_sel2$mes <- as.numeric(format(sinan_2019_hepatite_A_sel2$dt_notificacao,'%m'))

#### Filtros hepatite A - indivíduos vivos

sinan_2019_hepatite_A_sel2_class_fin <- filter(sinan_2019_hepatite_A_sel2, CLASSI_FIN == 1 | CLASSI_FIN == 2 | CLASSI_FIN == 4)

sinan_2019_hepatite_A_sel2_class_fin$CLAS_ETIOL <- as.character(sinan_2019_hepatite_A_sel2_class_fin$CLAS_ETIOL)

sinan_2019_hepatite_A_sel2_class_fin_class_eti <- filter(sinan_2019_hepatite_A_sel2_class_fin, CLAS_ETIOL == "01" | CLAS_ETIOL == "07" | CLAS_ETIOL == "08"  )

#sinan_2019_hepatite_A_sel2_class_fin_class_eti_forma <- filter(sinan_2019_hepatite_A_sel2_class_fin_class_eti, FORMA_NET == 1 | FORMA_NET == 2 | FORMA_NET == 4 |  FORMA_NET == 0 )

sinan_2019_hepatite_A_sel2_class_fin_class_eti_forma <- filter(sinan_2019_hepatite_A_sel2_class_fin_class_eti, FORMA_NET != 3 )

##########################################

##############
### Aba 1 - Causa da infecção, Sexo, Faixa estária
##############

sinan_2019_hepatite_A_aba1 <- select(sinan_2019_hepatite_A_sel2_class_fin_class_eti_forma, NOME, NOMEMAE, MUNRES, SEXO, ID_MUNICIP, CS_RACA_NE, SEXUAL_NET, TATU_PIER, MATBIOLOGI, INAL_CRACK, ACUPUNTURA, TRANSFUSAO, INJETAVEIS, CIRURGICO, AGUA_ALIME, DENTARIO_N,HEMODIALIS, TRANSPLA_N, OUTRAS_NET, CLASSI_FIN, FORMA_NET, CLAS_ETIOL, ano_notificacao, mes, fonte)

sinan_2019_hepatite_A_aba1$CS_RACA_NE  <- fct_explicit_na(sinan_2019_hepatite_A_aba1$CS_RACA_NE )
sinan_2019_hepatite_A_aba1$NOMEMAE  <- fct_explicit_na(sinan_2019_hepatite_A_aba1$NOMEMAE )
sinan_2019_hepatite_A_aba1$fonte  <- fct_explicit_na(sinan_2019_hepatite_A_aba1$fonte )
sinan_2019_hepatite_A_aba1$SEXO  <- fct_explicit_na(sinan_2019_hepatite_A_aba1$SEXO )

sinan_2019_hepatite_A_aba1 <- sinan_2019_hepatite_A_aba1 %>% group_by(NOME, NOMEMAE, MUNRES, SEXO, ID_MUNICIP, CS_RACA_NE, SEXUAL_NET, TATU_PIER, MATBIOLOGI, INAL_CRACK, ACUPUNTURA, TRANSFUSAO, INJETAVEIS, CIRURGICO, AGUA_ALIME, DENTARIO_N,HEMODIALIS, TRANSPLA_N, OUTRAS_NET, CLASSI_FIN, FORMA_NET, CLAS_ETIOL, ano_notificacao, mes, fonte)
sinan_2019_hepatite_A_aba1_n <- sinan_2019_hepatite_A_aba1 %>% summarise(n = n())

### Buscando duplicados

tabela_nome <- table(sinan_2019_hepatite_A_aba1_n$NOME)
tabela_nome <- as.data.frame(tabela_nome)

row_sub = apply(tabela_nome, 1, function(row) all(row !=0 ))

tabela_nome <- tabela_nome[row_sub,]

tabela_nome <- filter(tabela_nome, Freq == 2 )

sinan_2019_hepatite_A_aba1_n_dupli <- filter(sinan_2019_hepatite_A_aba1_n, NOME == "CELSO OLIVEIRA" )

### Removendo duplicados

#sinan_2019_hepatite_A_aba1_n_un <- distinct(sinan_2019_hepatite_A_aba1_n, NOME , .keep_all = TRUE)

##### Substituindo códigos por informações

sinan_2019_hepatite_A_aba1_n$CS_RACA_NE <- as.character(sinan_2019_hepatite_A_aba1_n$CS_RACA_NE)

sinan_2019_hepatite_A_aba1_n$CS_RACA_NE[sinan_2019_hepatite_A_aba1_n$CS_RACA_NE == "1"] <- "Branca"
sinan_2019_hepatite_A_aba1_n$CS_RACA_NE[sinan_2019_hepatite_A_aba1_n$CS_RACA_NE == "2"] <- "Preta"
sinan_2019_hepatite_A_aba1_n$CS_RACA_NE[sinan_2019_hepatite_A_aba1_n$CS_RACA_NE == "3"] <- "Amarela"
sinan_2019_hepatite_A_aba1_n$CS_RACA_NE[sinan_2019_hepatite_A_aba1_n$CS_RACA_NE == "4"] <- "Parda"
sinan_2019_hepatite_A_aba1_n$CS_RACA_NE[sinan_2019_hepatite_A_aba1_n$CS_RACA_NE == "5"] <- "Indígena"
sinan_2019_hepatite_A_aba1_n$CS_RACA_NE[sinan_2019_hepatite_A_aba1_n$CS_RACA_NE == "9"] <- "Ignorado"
sinan_2019_hepatite_A_aba1_n$CS_RACA_NE[sinan_2019_hepatite_A_aba1_n$CS_RACA_NE == "(Missing)" ] <- "Ignorado"

sinan_2019_hepatite_A_aba1_n$fonte <- as.character(sinan_2019_hepatite_A_aba1_n$fonte)

sinan_2019_hepatite_A_aba1_n$fonte[sinan_2019_hepatite_A_aba1_n$fonte == "01"] <- "Sexual"
sinan_2019_hepatite_A_aba1_n$fonte[sinan_2019_hepatite_A_aba1_n$fonte == "02"] <- "Transfusional"
sinan_2019_hepatite_A_aba1_n$fonte[sinan_2019_hepatite_A_aba1_n$fonte == "03"] <- "Uso de Drogas"
sinan_2019_hepatite_A_aba1_n$fonte[sinan_2019_hepatite_A_aba1_n$fonte == "04"] <- "Vertical"
sinan_2019_hepatite_A_aba1_n$fonte[sinan_2019_hepatite_A_aba1_n$fonte == "05"] <- "Acidente de Trabalho"
sinan_2019_hepatite_A_aba1_n$fonte[sinan_2019_hepatite_A_aba1_n$fonte == "06"] <- "Hemodiálise"
sinan_2019_hepatite_A_aba1_n$fonte[sinan_2019_hepatite_A_aba1_n$fonte == "07"] <- "Domiciliar"
sinan_2019_hepatite_A_aba1_n$fonte[sinan_2019_hepatite_A_aba1_n$fonte == "08"] <- "Tratamento cirúrgico"
sinan_2019_hepatite_A_aba1_n$fonte[sinan_2019_hepatite_A_aba1_n$fonte == "09"] <- "Tratamento dentário"
sinan_2019_hepatite_A_aba1_n$fonte[sinan_2019_hepatite_A_aba1_n$fonte == "10"] <- "Pessoa/pessoa"
sinan_2019_hepatite_A_aba1_n$fonte[sinan_2019_hepatite_A_aba1_n$fonte == "11"] <- "Alimento/água"
sinan_2019_hepatite_A_aba1_n$fonte[sinan_2019_hepatite_A_aba1_n$fonte == "12"] <- "Outros"
sinan_2019_hepatite_A_aba1_n$fonte[sinan_2019_hepatite_A_aba1_n$fonte == "99"] <- "Ignorado"
sinan_2019_hepatite_A_aba1_n$fonte[sinan_2019_hepatite_A_aba1_n$fonte == "(Missing)" ] <- "Ignorado"

sinan_2019_hepatite_A_aba1_n <- sinan_2019_hepatite_A_aba1_n %>% group_by( NOME, NOMEMAE, MUNRES, SEXO, CS_RACA_NE, FORMA_NET, CLAS_ETIOL, ano_notificacao, mes, fonte)
sinan_2019_hepatite_A_aba1_n <- sinan_2019_hepatite_A_aba1_n %>% summarise(n = n())

sinan_2019_hepatite_A_aba1_n$FORMA_NET <- as.character(sinan_2019_hepatite_A_aba1_n$FORMA_NET)
sinan_2019_hepatite_A_aba1_n$CLAS_ETIOL <- as.character(sinan_2019_hepatite_A_aba1_n$CLAS_ETIOL)

sinan_2019_hepatite_A_aba1_n$CLAS_ETIOL[sinan_2019_hepatite_A_aba1_n$CLAS_ETIOL == "01"] <- "Vírus A"
sinan_2019_hepatite_A_aba1_n$CLAS_ETIOL[sinan_2019_hepatite_A_aba1_n$CLAS_ETIOL == "07"] <- "Vírus A e B"
sinan_2019_hepatite_A_aba1_n$CLAS_ETIOL[sinan_2019_hepatite_A_aba1_n$CLAS_ETIOL == "08"] <- "Vírus A e C"
sinan_2019_hepatite_A_aba1_n$fonte[sinan_2019_hepatite_A_aba1_n$CLAS_ETIOL == "99"] <- "Ignorado"

sinan_2019_hepatite_A_aba1_n$FORMA_NET[sinan_2019_hepatite_A_aba1_n$FORMA_NET == "1"] <- "Hepatite Aguda"
sinan_2019_hepatite_A_aba1_n$FORMA_NET[sinan_2019_hepatite_A_aba1_n$FORMA_NET == "2"] <- "Hepatite Crônica/Portador Assintomático"
sinan_2019_hepatite_A_aba1_n$FORMA_NET[sinan_2019_hepatite_A_aba1_n$FORMA_NET == "4"] <- "Inconclusivo"

##### Painel - Informações do SINAN - coinfecção HIV - *faixa 20-39 anos

##############
### Aba 2 - oinfecção HIV - *faixa 20-39 anos, Sexo, Faixa estária
##############

sinan_2019_hepatite_A_aba2 <- select(sinan_2019_hepatite_A_sel2_class_fin_class_eti_forma, NOME, NOMEMAE,NU_IDADE_N, MUNRES, SEXO, CS_RACA_NE, SEXUAL_NET, OUTRAS_NET,TRESMAIS_N, HIV_NET, OUTRA_DST, FORMA_NET, CLAS_ETIOL, ano_notificacao, mes, fonte)

sinan_2019_hepatite_A_aba2$CS_RACA_NE  <- fct_explicit_na(sinan_2019_hepatite_A_aba2$CS_RACA_NE )
sinan_2019_hepatite_A_aba2$NOMEMAE  <- fct_explicit_na(sinan_2019_hepatite_A_aba2$NOMEMAE )
sinan_2019_hepatite_A_aba2$fonte  <- fct_explicit_na(sinan_2019_hepatite_A_aba2$fonte )

sinan_2019_hepatite_A_aba2 <- sinan_2019_hepatite_A_aba2 %>% group_by(NOME, NOMEMAE,NU_IDADE_N, MUNRES, SEXO, CS_RACA_NE, SEXUAL_NET, OUTRAS_NET,TRESMAIS_N, HIV_NET, OUTRA_DST, FORMA_NET, CLAS_ETIOL, ano_notificacao, mes, fonte)
sinan_2019_hepatite_A_aba2_n <- sinan_2019_hepatite_A_aba2 %>% summarise(n = n())

##### Substituindo códigos por informações

sinan_2019_hepatite_A_aba2_n$CS_RACA_NE <- as.character(sinan_2019_hepatite_A_aba2_n$CS_RACA_NE)

sinan_2019_hepatite_A_aba2_n$CS_RACA_NE[sinan_2019_hepatite_A_aba2_n$CS_RACA_NE == "1"] <- "Branca"
sinan_2019_hepatite_A_aba2_n$CS_RACA_NE[sinan_2019_hepatite_A_aba2_n$CS_RACA_NE == "2"] <- "Preta"
sinan_2019_hepatite_A_aba2_n$CS_RACA_NE[sinan_2019_hepatite_A_aba2_n$CS_RACA_NE == "3"] <- "Amarela"
sinan_2019_hepatite_A_aba2_n$CS_RACA_NE[sinan_2019_hepatite_A_aba2_n$CS_RACA_NE == "4"] <- "Parda"
sinan_2019_hepatite_A_aba2_n$CS_RACA_NE[sinan_2019_hepatite_A_aba2_n$CS_RACA_NE == "9"] <- "Ignorado"
sinan_2019_hepatite_A_aba2_n$CS_RACA_NE[sinan_2019_hepatite_A_aba2_n$CS_RACA_NE == "(Missing)" ] <- "Ignorado"

sinan_2019_hepatite_A_aba2_n$fonte <- as.character(sinan_2019_hepatite_A_aba2_n$fonte)

sinan_2019_hepatite_A_aba2_n$fonte[sinan_2019_hepatite_A_aba2_n$fonte == "01"] <- "Sexual"
sinan_2019_hepatite_A_aba2_n$fonte[sinan_2019_hepatite_A_aba2_n$fonte == "02"] <- "Transfusional"
sinan_2019_hepatite_A_aba2_n$fonte[sinan_2019_hepatite_A_aba2_n$fonte == "03"] <- "Uso de Drogas"
sinan_2019_hepatite_A_aba2_n$fonte[sinan_2019_hepatite_A_aba2_n$fonte == "04"] <- "Vertical"
sinan_2019_hepatite_A_aba2_n$fonte[sinan_2019_hepatite_A_aba2_n$fonte == "05"] <- "Acidente de Trabalho"
sinan_2019_hepatite_A_aba2_n$fonte[sinan_2019_hepatite_A_aba2_n$fonte == "06"] <- "Hemodiálise"
sinan_2019_hepatite_A_aba2_n$fonte[sinan_2019_hepatite_A_aba2_n$fonte == "07"] <- "Domiciliar"
sinan_2019_hepatite_A_aba2_n$fonte[sinan_2019_hepatite_A_aba2_n$fonte == "08"] <- "Tratamento cirúrgico"
sinan_2019_hepatite_A_aba2_n$fonte[sinan_2019_hepatite_A_aba2_n$fonte == "09"] <- "Tratamento dentário"
sinan_2019_hepatite_A_aba2_n$fonte[sinan_2019_hepatite_A_aba2_n$fonte == "10"] <- "Pessoa/pessoa"
sinan_2019_hepatite_A_aba2_n$fonte[sinan_2019_hepatite_A_aba2_n$fonte == "11"] <- "Alimento/água"
sinan_2019_hepatite_A_aba2_n$fonte[sinan_2019_hepatite_A_aba2_n$fonte == "12"] <- "Outros"
sinan_2019_hepatite_A_aba2_n$fonte[sinan_2019_hepatite_A_aba2_n$fonte == "99"] <- "Ignorado"
sinan_2019_hepatite_A_aba2_n$fonte[sinan_2019_hepatite_A_aba2_n$fonte == "(Missing)" ] <- "Ignorado"

sinan_2019_hepatite_A_aba2_n$FORMA_NET <- as.character(sinan_2019_hepatite_A_aba2_n$FORMA_NET)
sinan_2019_hepatite_A_aba2_n$CLAS_ETIOL <- as.character(sinan_2019_hepatite_A_aba2_n$CLAS_ETIOL)

sinan_2019_hepatite_A_aba2_n$CLAS_ETIOL[sinan_2019_hepatite_A_aba2_n$CLAS_ETIOL == "01"] <- "Vírus A"
sinan_2019_hepatite_A_aba2_n$CLAS_ETIOL[sinan_2019_hepatite_A_aba2_n$CLAS_ETIOL == "07"] <- "Vírus A e B"
sinan_2019_hepatite_A_aba2_n$CLAS_ETIOL[sinan_2019_hepatite_A_aba2_n$CLAS_ETIOL == "08"] <- "Vírus A e C"
sinan_2019_hepatite_A_aba2_n$fonte[sinan_2019_hepatite_A_aba2_n$CLAS_ETIOL == "99"] <- "Ignorado"

sinan_2019_hepatite_A_aba2_n$FORMA_NET[sinan_2019_hepatite_A_aba2_n$FORMA_NET == "1"] <- "Hepatite Aguda"
sinan_2019_hepatite_A_aba2_n$FORMA_NET[sinan_2019_hepatite_A_aba2_n$FORMA_NET == "2"] <- "Hepatite Crônica/Portador Assintomático"
sinan_2019_hepatite_A_aba2_n$FORMA_NET[sinan_2019_hepatite_A_aba2_n$FORMA_NET == "4"] <- "Inconclusivo"

sinan_2019_hepatite_A_aba2_n$SEXUAL_NET <- as.character(sinan_2019_hepatite_A_aba2_n$SEXUAL_NET)
sinan_2019_hepatite_A_aba2_n$OUTRA_DST <- as.character(sinan_2019_hepatite_A_aba2_n$OUTRA_DST)
sinan_2019_hepatite_A_aba2_n$TRESMAIS_N <- as.character(sinan_2019_hepatite_A_aba2_n$TRESMAIS_N)
sinan_2019_hepatite_A_aba2_n$HIV_NET <- as.character(sinan_2019_hepatite_A_aba2_n$HIV_NET)
sinan_2019_hepatite_A_aba2_n$OUTRAS_NET <- as.character(sinan_2019_hepatite_A_aba2_n$OUTRAS_NET)

sinan_2019_hepatite_A_aba2_n$SEXUAL_NET[sinan_2019_hepatite_A_aba2_n$SEXUAL_NET == "1"] <- "Menos de 6 meses"
sinan_2019_hepatite_A_aba2_n$SEXUAL_NET[sinan_2019_hepatite_A_aba2_n$SEXUAL_NET == "2"] <- "Mais de 6 meses"
sinan_2019_hepatite_A_aba2_n$SEXUAL_NET[sinan_2019_hepatite_A_aba2_n$SEXUAL_NET == "3"] <- "Não"
sinan_2019_hepatite_A_aba2_n$SEXUAL_NET[sinan_2019_hepatite_A_aba2_n$SEXUAL_NET == "9"] <- "Ignorado"

sinan_2019_hepatite_A_aba2_n$OUTRA_DST[sinan_2019_hepatite_A_aba2_n$OUTRA_DST == "1"] <- "Sim"
sinan_2019_hepatite_A_aba2_n$OUTRA_DST[sinan_2019_hepatite_A_aba2_n$OUTRA_DST == "2"] <- "Não"
sinan_2019_hepatite_A_aba2_n$OUTRA_DST[sinan_2019_hepatite_A_aba2_n$OUTRA_DST == "9"] <- "Ignorado"

sinan_2019_hepatite_A_aba2_n$TRESMAIS_N[sinan_2019_hepatite_A_aba2_n$TRESMAIS_N == "1"] <- "Menos de 6 meses"
sinan_2019_hepatite_A_aba2_n$TRESMAIS_N[sinan_2019_hepatite_A_aba2_n$TRESMAIS_N == "2"] <- "Mais de 6 meses"
sinan_2019_hepatite_A_aba2_n$TRESMAIS_N[sinan_2019_hepatite_A_aba2_n$TRESMAIS_N == "3"] <- "Não"
sinan_2019_hepatite_A_aba2_n$TRESMAIS_N[sinan_2019_hepatite_A_aba2_n$TRESMAIS_N == "9"] <- "Ignorado"

sinan_2019_hepatite_A_aba2_n$HIV_NET[sinan_2019_hepatite_A_aba2_n$HIV_NET == "1"] <- "Sim"
sinan_2019_hepatite_A_aba2_n$HIV_NET[sinan_2019_hepatite_A_aba2_n$HIV_NET == "2"] <- "Não"
sinan_2019_hepatite_A_aba2_n$HIV_NET[sinan_2019_hepatite_A_aba2_n$HIV_NET == "9"] <- "Ignorado"

sinan_2019_hepatite_A_aba2_n$OUTRAS_NET[sinan_2019_hepatite_A_aba2_n$OUTRAS_NET == "1"] <- "Menos de 6 meses"
sinan_2019_hepatite_A_aba2_n$OUTRAS_NET[sinan_2019_hepatite_A_aba2_n$OUTRAS_NET == "2"] <- "Mais de 6 meses"
sinan_2019_hepatite_A_aba2_n$OUTRAS_NET[sinan_2019_hepatite_A_aba2_n$OUTRAS_NET == "3"] <- "Não"
sinan_2019_hepatite_A_aba2_n$OUTRAS_NET[sinan_2019_hepatite_A_aba2_n$OUTRAS_NET == "9"] <- "Ignorado"


##### Cálculo de faixa etária por idade

sinan_2019_hepatite_A_aba2_n$NU_IDADE_N = substr(sinan_2019_hepatite_A_aba2_n$NU_IDADE_N,3,nchar(sinan_2019_hepatite_A_aba2_n$NU_IDADE_N)-0)

sinan_2019_hepatite_A_aba2_n$NU_IDADE_N <- as.numeric(sinan_2019_hepatite_A_aba2_n$NU_IDADE_N)

labs <- c(paste(seq(0, 95, by = 5), seq(0 + 5 - 1, 100 - 1, by = 5),
                sep = "-"), paste(100, "+", sep = ""))

sinan_2019_hepatite_A_aba2_n$IDADE_faixa <- cut(sinan_2019_hepatite_A_aba2_n$NU_IDADE_N, breaks = c(seq(0, 100, by = 5), Inf), labels = labs, right = FALSE)


##### Substituição código IBGE município por nome do Município

mun_Brasil <- read.xlsx("C:/Users/lemos/Downloads/RELATORIO_DTB_BRASIL_MUNICIPIO.xlsx")

mun_Brasil <- select(mun_Brasil, MUNRES = Código.Município.Completo, Nome_Município)

mun_Brasil$MUNRES = substr(mun_Brasil$MUNRES,1,nchar(mun_Brasil$MUNRES)-1)

sinan_2019_hepatite_A_aba1_n$MUNRES <- as.character(sinan_2019_hepatite_A_aba1_n$MUNRES)
sinan_2019_hepatite_A_aba2_n$MUNRES <- as.character(sinan_2019_hepatite_A_aba2_n$MUNRES)

sinan_2019_hepatite_A_aba1_n <- inner_join(sinan_2019_hepatite_A_aba1_n, mun_Brasil, by = "MUNRES" )
sinan_2019_hepatite_A_aba2_n <- inner_join(sinan_2019_hepatite_A_aba2_n, mun_Brasil, by = "MUNRES" )

### UF a parir do município de residência

sinan_2019_hepatite_A_aba1_n$UF <- substr(sinan_2019_hepatite_A_aba1_n$MUNRES, 0, 2)
sinan_2019_hepatite_A_aba2_n$UF <- substr(sinan_2019_hepatite_A_aba2_n$MUNRES, 0, 2)

sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "11"] <- "Rondônia"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "12"] <- "Acre"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "13"] <- "Amazonas"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "14"] <- "Roraima"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "15"] <- "Pará"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "16"] <- "Amapá"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "17"] <- "Tocantins"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "21"] <- "Maranhão"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "22"] <- "Piauí"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "23"] <- "Ceará"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "24"] <- "Rio Grande do Norte"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "25"] <- "Paraíba"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "26"] <- "Pernambuco"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "27"] <- "Alagoas"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "28"] <- "Sergipe"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "29"] <- "Bahia"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "31"] <- "Minas Gerais"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "32"] <- "Espírito Santo"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "33"] <- "Rio de Janeiro"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "35"] <- "São Paulo"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "41"] <- "Paraná"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "42"] <- "Santa Catarina"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "43"] <- "Rio Grande do Sul"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "50"] <- "Mato Grosso do Sul"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "51"] <- "Mato Grosso"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "52"] <- "Goiás"
sinan_2019_hepatite_A_aba1_n$UF[sinan_2019_hepatite_A_aba1_n$UF == "53"] <- "Distrito Federal"

sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "11"] <- "Rondônia"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "12"] <- "Acre"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "13"] <- "Amazonas"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "14"] <- "Roraima"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "15"] <- "Pará"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "16"] <- "Amapá"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "17"] <- "Tocantins"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "21"] <- "Maranhão"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "22"] <- "Piauí"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "23"] <- "Ceará"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "24"] <- "Rio Grande do Norte"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "25"] <- "Paraíba"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "26"] <- "Pernambuco"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "27"] <- "Alagoas"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "28"] <- "Sergipe"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "29"] <- "Bahia"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "31"] <- "Minas Gerais"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "32"] <- "Espírito Santo"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "33"] <- "Rio de Janeiro"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "35"] <- "São Paulo"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "41"] <- "Paraná"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "42"] <- "Santa Catarina"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "43"] <- "Rio Grande do Sul"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "50"] <- "Mato Grosso do Sul"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "51"] <- "Mato Grosso"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "52"] <- "Goiás"
sinan_2019_hepatite_A_aba2_n$UF[sinan_2019_hepatite_A_aba2_n$UF == "53"] <- "Distrito Federal"

#### Salvando tabelas do painel 

write.xlsx(sinan_2019_hepatite_A_aba1_n, "C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto3/painel_hepatite_A_2020/sinan_2019_hepatite_A_aba1_n.xlsx" )
write.xlsx(sinan_2019_hepatite_A_aba2_n, "C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto3/painel_hepatite_A_2020/sinan_2019_hepatite_A_aba2_n.xlsx" )

##### Painel - Gal - 2019 - Reagentes IgM

### 2019 ### 

IgM_hepA_2019 <- read.xlsx("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto3/GAL/IgM/dados_n0/IgM_2019.xlsx")

IgM_hepA_2019$Resultado <- as.character(IgM_hepA_2019$Resultado)

IgM_hepA_2019_reagente <- filter(IgM_hepA_2019, Resultado == "Reagente")

IgM_hepA_2019_reagente_filt <- select(IgM_hepA_2019_reagente, Estado.da.Requisição,Requisição,Metodologia, Laboratório.de.Cadastro, Unidade.Requisitante,Municipio.do.Requisitante, Idade, Tipo.Idade, Sexo, Municipio.de.Residência,Estado.de.Residência, Laboratório.Responsável,Laboratório.Executor, Data.de.Nascimento, "Reg..Conselho/Matrícula", Data.de.Cadastro, Data.da.Coleta, Data.da.Liberação, Amostra )

missmap(IgM_hepA_2019_reagente_filt)

IgM_hepA_2019_reagente_filt <- IgM_hepA_2019_reagente_filt %>% group_by(Estado.da.Requisição,Requisição,Metodologia, Laboratório.de.Cadastro, Unidade.Requisitante,Municipio.do.Requisitante, Idade, Tipo.Idade, Sexo, Municipio.de.Residência,Estado.de.Residência, Laboratório.Responsável,Laboratório.Executor, Data.de.Nascimento, "Reg..Conselho/Matrícula", Data.de.Cadastro, Data.da.Coleta, Data.da.Liberação, Amostra)
IgM_hepA_2019_reagente_filt_n <- IgM_hepA_2019_reagente_filt %>% summarise(n = n())

IgM_hepA_2019_reagente_filt_n <- read.xlsx( "C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto3/painel_hepatite_A_2020/IgM_hepA_2019_reagente_filt_n.xlsx" )

labs <- c(paste(seq(0, 95, by = 5), seq(0 + 5 - 1, 100 - 1, by = 5),
                sep = "-"), paste(100, "+", sep = ""))

IgM_hepA_2019_reagente_filt_n$IDADE_faixa <- cut(IgM_hepA_2019_reagente_filt_n$Idade, breaks = c(seq(0, 100, by = 5), Inf), labels = labs, right = FALSE)

write.xlsx(IgM_hepA_2019_reagente_filt_n, "C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto3/painel_hepatite_A_2020/IgM_hepA_2019_reagente_filt_n.xlsx" )



