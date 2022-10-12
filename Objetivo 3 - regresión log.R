install.packages("gtsummary")
library(gtsummary)
install.packages("rlang")
library(rlang)
covid_reg <- BASE_SI_regresion
library(tidyverse)

update.packages("rlang")
install.packages("dfidx")
library(rlang)

#REGRESIÓN LOGÍSTICA
install.packages("gtsummary")

covid_reg <- BASE_SI_regresion


modelo1 <- glm(dx_cog~severidad +
                 caide_total+
                 hads_depresion + 
                 hads_ansiedad+
                 obesidad+
                 hipertension+
                 caide_actfis+
                 isi, data=covid_reg, family="binomial")


summary(modelo1)

library(gtsummary)
modelo1 %>%
  tbl_regression()

tbl_regression(modelo1, exponentiate=TRUE)

library(easystats)
report(modelo1)




library(sjPlot)
plot_model(modelo1, 
           colors = c("#32374c","#fba31b"),
           axis.lim=c(0.3,3), 
           show.p = TRUE,
           show.data = T,
           p.val = "profile",
           title = "Factores de riesgo de sintomas cognitivos post-covid (OR)")

check_model(modelo1)
check_collinearity(modelo1)
print(a)
