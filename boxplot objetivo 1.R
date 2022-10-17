
covid <- BASE_SI

library(tidyverse)
library(reshape2)
library(ggplot2)
palette_fleni<-c("#fba31b", "#32374c", "#a90123", "#578001")

covid$dx <- as.factor(covid$dx)
covid$moca_z <- as.numeric(covid$moca_z)
covid$craft_inm_t <- as.numeric(covid$craft_inm_text_z)
covid$craft_inm_p <- as.numeric(covid$craft_inm_parafraseo_z)
covid$craft_dif_t <- as.numeric(covid$craft_dif_text_z)
covid$craft_dif_p <- as.numeric(covid$craft_dif_parafraseo_z)
covid$ravlt_apren <- as.numeric(covid$ravlt_total_z)
covid$ravlt_evoc <- as.numeric(covid$ravlt_diferido_z)
covid$benson_dif <- as.numeric(covid$benson_diferido_z)
covid$severidad <- as.numeric(covid$severidad)
covid$dig_dir <- as.numeric(covid$digitos_directos_z)
covid$dig_inv <- as.numeric(covid$digitos_inversos_z)
covid$tmt_a <- as.numeric(covid$tmt_a_z)
covid$tmt_b <- as.numeric(covid$tmt_b_z)
covid$stroop_pc <- as.numeric(covid$stroop_pc_z)
covid$stroop_int <- as.numeric(covid$stroop_interferencia_z)
covid$wcst_cat <- as.numeric(covid$wcst_categorias_z)
covid$wcst_pers <- as.numeric(covid$wcst_perseveraciones_z)
covid$mint <- as.numeric(covid$mint_z)
covid$fluencia_sem <- as.numeric(covid$fluencia_sem_animales_pz)
covid$fluencia_fon <- as.numeric(covid$fluencia_fon_p_z)

covid$memoria<-(covid$ravlt_apren+
                  covid$ravlt_evoc+
                  covid$craft_inm_t+
                  covid$craft_dif_t+
                  covid$benson_dif/5)

covid$lenguaje<-(covid$mint+
                   covid$fluencia_sem+
                   covid$fluencia_fon/3)

covid$atencion<-(covid$tmt_a+
                   covid$dig_dir+
                   covid$dig_inv/3)


covid$ffee<-(covid$tmt_b+
               covid$stroop_pc+
               covid$wcst_cat/3)


dif_composites <- covid %>% select(memoria,
                                   atencion,
                                   lenguaje,
                                   ffee, 
                                   dx, hc)

dif_composites1 <- melt(dif_composites, id=c("dx", "hc"))
View(dif_composites1)

ggplot(dif_composites1, aes(x=variable, y=value, fill=dx))+
  geom_boxplot()+
  scale_fill_manual(values=c("#5b59ac", "#fba31b"))+
  theme_minimal()+
  geom_vline(xintercept = 4.5)+
  geom_point(alpha = .7, position = position_jitter(seed = 1))+
  ggforce::geom_sina(method = "counts", alpha = .5)+
  ggdist::stat_dots(side = "left", dotsize = .3, justification = 1.1, binwidth = .1)+
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = c(0.5, 1)) + 
  ggdist::stat_dots(side = "left", dotsize = .4, justification = 1.05, binwidth = .1,)




install.packages("ggdist")
library(ggdist)

ggplot(dif_composites1, aes(x=variable, y=value, fill=dx))+
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = c(0.5, 1)) + 
  ggdist::stat_dots(side = "left", dotsize = .4, justification = 1.05, binwidth = .1,)


library(ggplot2)
install.packages("ggforce")
library(ggforce)
library(ggdist)
install.packages("gghalves")

library(gghalves)



