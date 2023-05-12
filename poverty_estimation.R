
###############################################################################
## Estimação de indicadores de pobreza com a PNAD Contínua
###############################################################################

rm(list=ls(all=TRUE))#Libera memória, apagando os objetos abertos no R.
gc() #Libera a memoria RAM que não esta sendo utilizada pelo computador:
options(scipen=999) #Remove notação científica.

# Carregar pacotes
library(PNADcIBGE)
library(survey)
library(srvyr)
library(convey)
library(tidyverse)

# É possível definir variáveis utilizadas na análise em um vetor com o comando abaixo
variaveis <- c('Ano', 'UF', 'Estrato', 'UPA', 'V1008', 'V1014', 'V1032', 'V1022', 'V1023', 
               'V2001', 'V2007', 'V2009', 'V2010', 'VD2002', 'V3002', 
               'VD2003', 'VD4001', 'VD4002', 'VD4009', 'VD3004', 
               'VD4019', 'VD4020', 'VD4048', 'VD5008', 
               'V5001A2', 'V5002A2', 'V5003A2', 'V5004A', 'V5004A2', 'V5005A2',  
               'V5006A2', 'V5007A2', 'V5008A2')
# neste código serão baixadas todas as variáveis

# Carrega dados da PNADC 2022 - dados correspondem a 5ª visita
# Aqui estou carregando os dados sem o desenho amostral
pnadc <- get_pnadc(year = 2022,
                   interview = 5,
                   #vars = variaveis,
                   design = FALSE,
                   labels = FALSE,
                   deflator = TRUE,
                   defyear = 2022)

# Se quiser salvar os dados em um diretório local. Os dados serão salvos em um arquivo '.rds'
# saveRDS(pnadc, pnadc2022)

# Para carregar dados localmente basta usar o seguinte comando:
# Lembre-se de especificar um diretório
# pnadc <- readRDS(file="pnadc2022.rds")

# Como os dados foram carregados com 'design=FALSE', temos um objeto do tipo "data.frame"
class(pnadc)

## Ajustar variáveis
# Criar variaveis individuais
pnadc <- pnadc %>% 
    mutate(
        id_dom = as.numeric(paste(UPA,V1008,V1014, sep = "")),
        membro = ifelse(VD2002=="Pensionista" | 
                            VD2002=="Empregado(a) doméstico(a)" | 
                            VD2002=="Parente do(a) empregado(a) doméstico(a)",
                        0,1),

        ### Dimensões geograficas
        BR = factor("Brasil"),
        regiao = factor(ifelse(substr(UPA, start=1, stop=1)=="1","Norte",
                               ifelse(substr(UPA, start=1, stop=1)=="2","Nordeste",
                                      ifelse(substr(UPA, start=1, stop=1)=="3","Sudeste",
                                             ifelse(substr(UPA, start=1, stop=1)=="4","Sul",
                                                    ifelse(substr(UPA, start=1, stop=1)=="5","Centro-Oeste",NA))))),
                        levels = c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste")),
        
        ### Contruir variaveis de renda
        rend_trabalho = ifelse (is.na (VD4019), 0, VD4019*CO2),
        rend_outrasf = ifelse (is.na (VD4048), 0, VD4048*CO2e),
        renda = rend_trabalho + rend_outrasf
        )

# Criar variaveis agregadas por domicilio
pnadc <- pnadc %>% group_by(id_dom) %>% 
    mutate(
        n_dom = sum(membro), 
        # Construir variavel de renda domiciliar total e rdpc
        rend_dom_outrasf = ifelse(membro==1, sum(rend_outrasf), 0),
        rend_dom_trabalho = ifelse(membro==1, sum(rend_trabalho), 0),
        renda_dom = ifelse(membro==1, sum(renda), 0),
        rdpc = renda_dom/n_dom
        ) %>% 
    ungroup()


##--------------------
## Linhas de pobreza - BM
lp <- 638             # US$ 2,15/dia ~ R$200/mes
lep <- 200            # US$ 6,85/dia ~ R$638/mes

# Criar variáveis dummy para contar pessoas na pobreza
pnadc <- pnadc %>% 
    mutate(
        pobre = factor(ifelse (rdpc< lp & !is.na(rdpc), "pobre", "nao pobre")),
        expobre = factor(ifelse (rdpc< lep & !is.na(rdpc), "pobre ex", "nao pobre ex"))
        )

##----------------------------------------------------------------------------##
## Incorporar desenho amostral da PNADC
svypnadc <- pnadc_design(pnadc)
class(svypnadc)
# Note que temos um objeto do tipo "svyrep.design"

# Aplicar 'srvyr::as_survey'
svypnadc <- srvyr::as_survey(svypnadc)
class(svypnadc)
# Note que agora temos um "tbl_svy", que permite usar funções do dplyr/ tidyverse nos dados de survey

##----------------------------------------------------------------------------##
## ESTIMAÇÕES
##------------ 

## RDPC
# Brasil
svymean(~rdpc, design=svypnadc, na.rm=TRUE)
# Nordeste
svymean(~rdpc, subset(svypnadc, regiao == "Nordeste"), na.rm=TRUE)
# Ceará
svymean(~rdpc, subset(svypnadc, UF == 23), na.rm=TRUE)


## POBREZA
# Brasil - Proporção de Pobres
svymean(~pobre, design=svypnadc, na.rm=TRUE)*100
# Brasil - número de pessoas em situação de pobreza
svytotal(~pobre, design=svypnadc, na.rm=TRUE)

# Nordeste - Proporção de Pobres
svymean(~pobre, subset(svypnadc, regiao == "Nordeste"), na.rm=TRUE)*100
# Nordeste - número de pessoas em situação de pobreza
svytotal(~pobre, subset(svypnadc, regiao == "Nordeste"), na.rm=TRUE)

# Ceará  - Proporção de Pobres
svymean(~pobre, subset(svypnadc, UF == 23), na.rm=TRUE)*100
# Ceará - número de pessoas em situação de pobreza
svytotal(~pobre, subset(svypnadc, UF == 23), na.rm=TRUE)
