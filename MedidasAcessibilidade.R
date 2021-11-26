
library(tidyverse)
library(dplyr)
library(plyr)


 ###### ACESSIBILIDADE EM 15 MINUTOS (escolas - região do ABC - modo caminhada)

 ## Importar a matriz de origens e destinos construída no R

Matrix_GradEst_Escola <- read.csv("MatrixGradEst_Escolas.csv")

 ## 1. Criar uma coluna com o tempo de viagem em MINUTOS do centroide de cada  
 ##    grade estatística (origin_id) para cada escola (destination_id)
 ## Obs.: Multiplica-se por *24*60 porque o QNEAT3 do QGIS retornou o resultado em dias (total_cost)

Matrix_GradEst_Escola <- Matrix_GradEst_Escola %>%
  mutate(TT_minutes = total_cost*24*60)

 ## 2. Criar uma variável binária, em que o resultado será 1 caso a escola possa 
 ##    ser acessada em até 15  minutos, e 0 caso não possa

Matrix_GradEst_Escola <- Matrix_GradEst_Escola %>%
  mutate(Access15 = if_else(TT_minutes<=15,1,0))

 ## 3. Criar um novo df para agrupar as escolas que podem ser acessadas em até 15 min por grade
 ##    Assim, teremos a quantidade de escolas por grade que podem ser acessadas em até 15 min

Access15_GradEst_Escola <- Matrix_GradEst_Escola %>%
  group_by(origin_id) %>%
  summarise(total=sum(Access15))

 ## 4. Salvar e espacializar os dados no QGIS (fazer o join com .gpkg da grade estatística)

write.csv2(Access15_GradEst_Escola, "Access15_GradEst_Escola.csv")



 ###### 3 ESCOLAS MAIS PRÓXIMAS DOS CENTROIDES (região do ABC - modo caminhada)

 ## 1. Ordenar pelo tempo de viagem

Ordered_GradEst_Escola <- Matrix_GradEst_Escola %>% 
  arrange(ordered(origin_id, unique(origin_id)), TT_minutes)

 ## 2. Selecionar as três escolas com menor tempo de viagem para cada grade

TresProx_GradEst_Escola <- ddply(Ordered_GradEst_Escola, .(origin_id), head, n = 3)

 ## 3. Selecionar a escola com menor tempo de viagem para cada grade

PrimProx_GradEst_Escola <- ddply(TresProx_GradEst_Escola, "origin_id", head, 1)

 ## 4. Selecionar a terceira escola com menor tempo de viagem para cada grade

TercProx_GradEst_Escola <- ddply(TresProx_GradEst_Escola, "origin_id", tail, 1) 

 ## 5. Salvar e espacializar os dados no QGIS (fazer o join com .gpkg da grade estatística)

write.csv2(PrimProx_GradEst_Escola, "PrimProx_GradEst_Escola.csv")
write.csv2(TercProx_GradEst_Escola, "TercProx_GradEst_Escola.csv")



