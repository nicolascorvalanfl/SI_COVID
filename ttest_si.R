
covid <- BASE_SALUD_INVESTIGA


#T.TEST

#screening
as.numeric(covid$dx)
moca_z <- as.numeric(covid$moca_z)

class(covid$moca_z)
class(covid$dx)

t.test(moca_z~covid$dx)

#memoria
t.test(covid$craft_inm_text_z~covid$dx)
t.test(covid$craft_inm_parafraseo_z~covid$dx)
t.test(covid$craft_dif_text_z~covid$dx)
t.test(covid$craft_dif_parafraseo_z~covid$dx)
t.test(covid$ravlt_total_z~covid$dx)
t.test(covid$ravlt_diferido_z~covid$dx)
t.test(covid$benson_diferido_z~covid$dx)


#atencion
t.test(covid$digitos_directos_z~covid$dx)
t.test(covid$digitos_inversos_z~covid$dx)
t.test(covid$tmt_a_z~covid$dx)
t.test(covid$clave_numeros~covid$dx)

#lenguaje
t.test(covid$mint_z~covid$dx)
t.test(covid$fluencia_fon_p_z~covid$dx)
t.test(covid$fluencia_sem_animales_pz~covid$dx)


#ffee
t.test(covid$tmt_b_z~covid$dx)
t.test(covid$stroop_pc_z~covid$dx)
t.test(covid$stroop_interferencia_z~covid$dx)
t.test(covid$wcst_categorias_z~covid$dx)
t.test(covid$wcst_perseveraciones_z~covid$dx)



#visuoespaciales
t.test(covid$benson_copia_z~covid$dx)
t.test(covid$reloj~covid$dx)



#psicológicas
t.test(covid$hads_ansiedad~covid$dx)
t.test(covid$hads_depresion~covid$dx)



#insomnio
t.test(covid$isi~covid$dx)

