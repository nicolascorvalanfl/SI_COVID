
covid <- BASE_SI
View(covid)

#distribución normal

library(ggplot2)
ggplot(covid$ravlt_apren) +
  geom_histogram(aes(x = moca_z, y = ..density..), binwidth = 0.5, fill = "grey", color = "black")

install.packages("nortest")
library(nortest)


#PRUEBA DE NORMALIDA - shapiro-wilk y kolmorov smirnov

install.packages("nortest")
library(nortest)

shapiro.test(ravlt_apren)
shapiro.test(craft_dif_p)
shapiro.test(mint)
shapiro.test(tmt_a)
shapiro.test(stroop_pc)
#acá ya paré, porque me estaban dando todas significativas. uso MW y ya.


#COMPOSITES :O

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

#T.TEST

#pase a numeric
covid$moca_z <- as.numeric(covid$moca_z)
covid$craft_inm_t <- as.numeric(covid$craft_inm_text_z)
covid$craft_inm_p <- as.numeric(covid$craft_inm_parafraseo_z)
covid$craft_dif_t <- as.numeric(covid$craft_dif_text_z)
covid$craft_dif_p <- as.numeric(covid$craft_dif_parafraseo_z)
covid$ravlt_apren <- as.numeric(covid$ravlt_total_z)
covid$ravlt_evoc <- as.numeric(covid$ravlt_diferido_z)
covid$benson_dif <- as.numeric(covid$benson_diferido_z)
covid$mint <- as.numeric(covid$mint_z)
covid$fluencia_sem <- as.numeric(covid$fluencia_sem_animales_pz)
covid$fluencia_fon <- as.numeric(covid$fluencia_fon_p_z)
covid$dig_dir <- as.numeric(covid$digitos_directos_z)
covid$dig_inv <- as.numeric(covid$digitos_inversos_z)
covid$tmt_a <- as.numeric(covid$tmt_a_z)
covid$tmt_b <- as.numeric(covid$tmt_b_z)
covid$stroop_pc <- as.numeric(covid$stroop_pc_z)
covid$stroop_int <- as.numeric(covid$stroop_interferencia_z)
covid$wcst_cat <- as.numeric(covid$wcst_categorias_z)
covid$wcst_pers <- as.numeric(covid$wcst_perseveraciones_z)
covid$benson_cop <- as.numeric(covid$benson_copia_z)
covid$reloj <- as.numeric(covid$reloj)
covid$hads_ans <- as.numeric(covid$hads_ansiedad)
covid$hads_dep <- as.numeric(covid$hads_depresion)
covid$dx <- as.numeric(covid$dx)
covid$oms_aprender <- as.numeric(covid$oms_aprender)


#MOCA
wilcox.test(covid$moca_z~covid$dx, data = covid)

#MEMORIA
wilcox.test(covid$craft_inm_t~covid$dx, data = covid)
wilcox.test(covid$craft_inm_p~covid$dx, data = covid)
wilcox.test(covid$craft_dif_t~covid$dx, data = covid)
wilcox.test(covid$craft_dif_p~covid$dx, data = covid)
wilcox.test(covid$ravlt_apren~covid$dx, data = covid)
wilcox.test(covid$ravlt_evoc~covid$dx, data = covid)
wilcox.test(covid$benson_dif~covid$dx, data = covid)


#ATENCION
wilcox.test(covid$dig_dir~covid$dx, data = covid)
wilcox.test(covid$dig_inv~covid$dx, data = covid)
wilcox.test(covid$tmt_a~covid$dx, data = covid)
wilcox.test(covid$clave_numeros~covid$dx, data = covid)

#LENGUAJE
wilcox.test(covid$mint~covid$dx, data = covid)
wilcox.test(covid$fluencia_sem~covid$dx, data = covid)
wilcox.test(covid$fluencia_fon~covid$dx, data = covid)


#FFEE
wilcox.test(covid$tmt_b~covid$dx, data = covid)
wilcox.test(covid$stroop_pc~covid$dx, data = covid)
wilcox.test(covid$stroop_int~covid$dx, data = covid)
wilcox.test(covid$wcst_cat~covid$dx, data = covid)
wilcox.test(covid$wcst_pers~covid$dx, data = covid)


#VISUOESPACIALES
wilcox.test(covid$benson_cop~covid$dx, data = covid)
wilcox.test(covid$reloj~covid$dx, data = covid)

#PSICOLÓGICAS
wilcox.test(covid$hads_ans~covid$dx, data = covid)
wilcox.test(covid$hads_dep~covid$dx, data = covid)


#COMPOSITES
wilcox.test(covid$memoria~covid$dx, data = covid)
wilcox.test(covid$atencion~covid$dx, data = covid)
wilcox.test(covid$lenguaje~covid$dx, data = covid)
wilcox.test(covid$ffee~covid$dx, data = covid)



#ahora como la Mann withney no me da los promedios y queda lindo ponerlos
#voy a sacar la M y la DE de cada variable
#screening
with(covid,tapply(sexo,dx,mean))
with(covid,tapply(sexo,dx,sd))

#memoria
with(covid,tapply(covid$craft_dif_t,covid$dx,mean))
with(covid,tapply(covid$craft_dif_t,covid$dx,sd))
with(covid,tapply(covid$craft_inm_t,covid$dx,mean))
with(covid,tapply(covid$craft_inm_t,covid$dx,sd))
with(covid,tapply(covid$craft_dif_p,covid$dx,mean))
with(covid,tapply(covid$craft_dif_p,covid$dx,sd))
with(covid,tapply(covid$craft_inm_p,covid$dx,mean))
with(covid,tapply(covid$craft_inm_p,covid$dx,sd))
with(covid,tapply(covid$ravlt_apren,covid$dx,mean))
with(covid,tapply(covid$ravlt_apren,covid$dx,sd))
with(covid,tapply(covid$ravlt_evoc,covid$dx,mean))
with(covid,tapply(covid$ravlt_evoc,covid$dx,sd))
with(covid,tapply(covid$benson_dif,covid$dx,mean))
with(covid,tapply(covid$benson_dif,covid$dx,sd))


#lenguaje
with(covid,tapply(covid$mint,covid$dx,mean))
with(covid,tapply(covid$mint,covid$dx,sd))
with(covid,tapply(covid$fluencia_fon,covid$dx,mean))
with(covid,tapply(covid$fluencia_fon,covid$dx,sd))
with(covid,tapply(covid$fluencia_sem,covid$dx,mean))
with(covid,tapply(covid$fluencia_sem,covid$dx,sd, na.rm = TRUE))

#atencion
with(covid,tapply(covid$tmt_a,covid$dx,mean))
with(covid,tapply(covid$tmt_a,covid$dx,sd))
with(covid,tapply(covid$dig_dir,covid$dx,mean))
with(covid,tapply(covid$dig_dir,covid$dx,sd))
with(covid,tapply(covid$dig_inv,covid$dx,mean))
with(covid,tapply(covid$dig_inv,covid$dx,sd))


#ffee
with(covid,tapply(covid$tmt_b,covid$dx,mean))
with(covid,tapply(covid$tmt_b,covid$dx,sd))
with(covid,tapply(covid$stroop_pc,covid$dx,mean))
with(covid,tapply(covid$stroop_pc,covid$dx,sd, na.rm = TRUE))
with(covid,tapply(covid$stroop_int,covid$dx,mean))
with(covid,tapply(covid$stroop_int,covid$dx,sd, na.rm =TRUE))
with(covid,tapply(covid$wcst_cat,covid$dx,mean))
with(covid,tapply(covid$wcst_cat,covid$dx,sd, na.rm = TRUE))
with(covid,tapply(covid$wcst_pers,covid$dx,mean))
with(covid,tapply(covid$wcst_pers,covid$dx,sd, na.rm = TRUE))

#visuoesp
with(covid,tapply(covid$benson_cop,covid$dx,mean))
with(covid,tapply(covid$benson_cop,covid$dx,sd))
with(covid,tapply(covid$reloj,covid$dx,mean))
with(covid,tapply(covid$reloj,covid$dx,sd))

#hads
with(covid,tapply(covid$hads_ans,covid$dx,mean))
with(covid,tapply(covid$hads_ans,covid$dx,sd, na.rm = TRUE))
with(covid,tapply(covid$hads_dep,covid$dx,mean))
with(covid,tapply(covid$hads_dep,covid$dx,sd, na.rm = TRUE))

#composites
with(covid,tapply(covid$memoria,covid$dx,mean))
with(covid,tapply(covid$memoria,covid$dx,sd, na.rm = TRUE))
with(covid,tapply(covid$atencion,covid$dx,mean))
with(covid,tapply(covid$atencion,covid$dx,sd, na.rm = TRUE))
with(covid,tapply(covid$lenguaje,covid$dx,mean))
with(covid,tapply(covid$lenguaje,covid$dx,sd, na.rm = TRUE))
with(covid,tapply(covid$ffee,covid$dx,mean))
with(covid,tapply(covid$ffee,covid$dx,sd, na.rm = TRUE))

#d de cohen para estimar tamaño de efecto

install.packages("effsize")
library(effsize)
cohen.d(formula = memoria ~ dx, data = covid, paired = FALSE)

