library(reshape2)
library(readxl)
library(tidyverse)


covid_frec <- BASE_SALUD_INVESTIGA_frecuencia




frecuencia <- covid_frec %>% select(hc,
                               anosmia,
                               perdida_gusto,
                               cansancio,
                               dif_respirar,
                               cefalea,
                               fiebre,
                               dolor_garganta,
                               cutaneas,
                               diarrea,
                               nauseas,
                               dolor_pecho,
                               delirium,
                               tos,
                               debilidad,
                               alucinaciones,
                               insomnio,
                               dif_concentr,
                               mareos,
                               dism_consc)


frecuencia1 <- melt(frecuencia, id.var = "hc")

frecuencia2 <- as.data.frame(table(frecuencia1$variable, frecuencia1$value))





ggplot(frecuencia2, aes(fill = Var2, y = Freq, x = Var1 )) +
  geom_col(position = "fill")+
  theme_minimal()




#grafico de torta


covid_torta<-data.frame(categorias=c("Leve","Moderada","Severa"),
               porcentaje=c(68,22,10))




ggplot(covid_torta, mapping = aes(x="",y=porcentaje, fill=categorias, color = "#fba31b", "#32374c", "#a90123"))+
  geom_bar(stat = "identity",color="white")



install.packages("scales")
library(ggplot2)
library(scales)


ggplot(covid_torta,aes(x="",y=porcentaje, fill=categorias))+
  geom_bar(stat = "identity",
           color="white")+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("#fba31b", "#32374c", "#a90123"))+
  theme_void()+
  labs(title="Gravedad de la infección por SARS-CoV-2")




#gráfico para antecedentes


antecedentes <- covid_frec %>% select(hc,
                                   sin_antec,
                                   enf_autoinmune,
                                   enf_card_cron,
                                   enf_ren_cron,
                                   enf_hig,
                                   enf_pulm_cron,
                                   hipertension,
                                   asma,
                                   cancer,
                                   fumador,
                                   asplenia,
                                   diabetes,
                                   obesidad,
                                   hiv,
                                   epilepsia,
                                   trast_neur)



antecedentes1 <- melt(antecedentes, id.var = "hc")

View(antecedentes1)

antecedentes2 <- as.data.frame(table(antecedentes1$variable, antecedentes1$value))


ggplot(antecedentes2, aes(fill = Var2, y = Freq, x = Var1 )) +
  geom_col(position = "fill")+
  theme_minimal()






