covid <- BASE_SI
covid_reg <- BASE_SI_regresion

#FUNCIONALIDAD

t.test(covid_reg$total_oms~covid_reg$severidad_i)

t.test(covid_reg$oms_amistades~covid_reg$severidad_i)
t.test(covid_reg$oms_estar_de_pie~covid_reg$severidad_i)
t.test(covid_reg$oms_aprender~covid_reg$severidad_i)
t.test(covid_reg$oms_comunitarias~covid_reg$severidad_i)
t.test(covid_reg$oms_emociones~covid_reg$severidad_i)
t.test(covid_reg$oms_concentracion~covid_reg$severidad_i)
t.test(covid_reg$oms_caminar~covid_reg$severidad_i)
t.test(covid_reg$oms_higiene~covid_reg$severidad_i)
t.test(covid_reg$oms_vestirse~covid_reg$severidad_i)
t.test(covid_reg$oms_desconocidos~covid_reg$severidad_i)
t.test(covid_reg$oms_amistades~covid_reg$severidad_i)
t.test(covid_reg$oms_trabajar~covid_reg$severidad_i)

covid_reg$total_oms <- (covid_reg$oms_aprender+
                          covid_reg$oms_comunitarias+
                          covid_reg$oms_caminar+
                          covid_reg$oms_amistades+
                          covid_reg$oms_concentracion+
                          covid_reg$oms_desconocidos+
                          covid_reg$oms_emociones+
                          covid_reg$oms_estar_de_pie+
                          covid_reg$oms_higiene+
                          covid_reg$oms_tareas_domesticas+
                          covid_reg$oms_vestirse+
                          covid_reg$oms_trabajar)

covid_reg$severidad <- as.numeric(covid_reg$severidad)

kruskal.test(covid_reg$total_oms ~ covid_reg$severidad)
kruskal.test(oms_estar_de_pie ~ severidad, data = covid)
kruskal.test(oms_tareas_domesticas ~ severidad, data = covid)
kruskal.test(oms_aprender ~ severidad, data = covid)
kruskal.test(oms_emociones ~ severidad, data = covid)
kruskal.test(oms_concentracion ~ severidad, data = covid)
kruskal.test(oms_caminar ~ severidad, data = covid)
kruskal.test(oms_higiene ~ severidad, data = covid)
kruskal.test(oms_vestirse ~ severidad, data = covid)
kruskal.test(oms_desconocidos ~ severidad, data = covid)
kruskal.test(oms_trabajar ~ severidad, data = covid)







