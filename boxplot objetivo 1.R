
covid <- BASE_SI

library(tidyverse)
library(reshape2)
library(ggplot2)
palette_fleni<-c("#fba31b", "#32374c", "#a90123", "#578001")

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
  ggtitle("Rendimiento en tarea de memoria inmediata")



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



