#regresion lineal
#objetivo 2: valorar asociación entre cognición y gravedad de la enfermedad

library(tidyverse)
library(gtsummary)

covid_reg <- BASE_SI_regresion
covid <- BASE_SI

#REGRESIÓN LOGÍSTICA



#Conversión de a #numeric
covid_reg$moca_z <- as.numeric(covid$moca_z)
covid_reg$craft_inm_t <- as.numeric(covid_reg$craft_inm_text_z)
covid_reg$craft_inm_p <- as.numeric(covid_reg$craft_inm_parafraseo_z)
covid_reg$craft_dif_t <- as.numeric(covid_reg$craft_dif_text_z)
covid_reg$craft_dif_p <- as.numeric(covid_reg$craft_dif_parafraseo_z)
covid_reg$ravlt_apren <- as.numeric(covid_reg$ravlt_total_z)
covid_reg$ravlt_evoc <- as.numeric(covid_reg$ravlt_diferido_z)
covid_reg$benson_dif <- as.numeric(covid_reg$benson_diferido_z)
covid_reg$severidad <- as.numeric(covid_reg$severidad)
covid_reg$dig_dir <- as.numeric(covid_reg$digitos_directos_z)
covid_reg$dig_inv <- as.numeric(covid_reg$digitos_inversos_z)
covid_reg$tmt_a <- as.numeric(covid_reg$tmt_a_z)
covid_reg$tmt_b <- as.numeric(covid_reg$tmt_b_z)
covid_reg$stroop_pc <- as.numeric(covid_reg$stroop_pc_z)
covid_reg$stroop_int <- as.numeric(covid_reg$stroop_interferencia_z)
covid_reg$wcst_cat <- as.numeric(covid_reg$wcst_categorias_z)
covid_reg$wcst_pers <- as.numeric(covid_reg$wcst_perseveraciones_z)
covid_reg$mint <- as.numeric(covid_reg$mint_z)
covid_reg$fluencia_sem <- as.numeric(covid_reg$fluencia_sem_animales_pz)
covid_reg$fluencia_fon <- as.numeric(covid_reg$fluencia_fon_p_z)

t.test(covid_reg$atencion~covid_reg$severidad_i)
with(covid,tapply(covid_reg$atencion,covid_reg$severidad_i,sd, na.rm = TRUE))



t.test(covid_reg$lenguaje~covid_reg$severidad_i)
with(covid,tapply(covid_reg$lenguaje,covid_reg$severidad_i,sd, na.rm = TRUE))


t.test(covid_reg$memoria~covid_reg$severidad_i)
with(covid,tapply(covid_reg$memoria,covid_reg$severidad_i,sd, na.rm = TRUE))



t.test(covid_reg$ffee~covid_reg$severidad_i)
with(covid,tapply(covid_reg$ffee,covid_reg$severidad_i,sd, na.rm = TRUE))

covid_reg$memoria<-(covid_reg$ravlt_apren+
                  covid_reg$ravlt_evoc+
                  covid_reg$craft_inm_t+
                  covid_reg$craft_dif_t+
                  covid_reg$benson_dif/5)


covid_reg$atencion<-(covid_reg$tmt_a+
                   covid_reg$dig_dir+
                   covid_reg$dig_inv/3)


covid_reg$ffee<-(covid_reg$tmt_b+
               covid_reg$stroop_pc+
               covid_reg$wcst_cat/3)

covid_reg$lenguaje<-(covid_reg$mint+
                   covid_reg$fluencia_sem+
                   covid_reg$fluencia_fon/3)





