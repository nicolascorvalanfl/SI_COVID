
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
covid$severidad <- as.numeric(covid$severidad)



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
covid <- BASE_SI

install.packages("ellipse")
library(ellipse)
library(RColorBrewer)
library(tidyverse)

covid_corr <- covid %>% select (memoria,
                                atencion,
                                lenguaje,
                                ffee,
                                severidad)


cor(covid$severidad,covid$ffee, use = "complete.obs")

m <- cor(covid_corr, use = "complete.obs")
corrplot(m)


plotcorr(m, outline = TRUE, col = 'green', numbers = FALSE, 
         type = c("full","lower","upper"),
         diag = TRUE, bty = "n", axes = FALSE,
         xlab = "", ylab = "", asp = 1,
         cex.lab = par("cex.lab"), cex = 0.75*par("cex"),
         mar = 0.1 + c(2,2,4,2))


install.packages("metan")
library(metan)

corr1 <- corr_coef(m)


