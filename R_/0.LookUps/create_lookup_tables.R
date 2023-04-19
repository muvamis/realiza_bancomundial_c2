#dependencies
source("functions/download_all_reports.R")
source("R_/0.LookUps/utils.R")
library(rio)

exdir <- "data/0look_ups"
create_dir_(exdir)



#read data from zoho 
fac_zoho <- download_realiza("Facilitadoras_Report")
grupos_zoho <- download_realiza("Grupos_Report")
turmas_zoho <- download_realiza("Turmas_fixas_report")
actividades_zoho <- download_realiza("Actividades_Report")
sessoes_fixas <- download_realiza("Sessoes_fixas_Report")
emprendedoras_zoho <- download_realiza('Emprendedoras_Report')



#Look up emprendedoras ========================================================



emprendedoras <- emprendedoras_zoho %>%
  select(ID_BM,
        Emprendedora,
        Grupos_fixos,
        Cidade,
        Agente = Facilitadoras,
        Grupo = Grupos,
        status_realiza)%>%
    distinct() %>%
  #Clean agente and grupos_fixos
  mutate(across(c(Agente, Grupos_fixos), function(x)clean_zoho_lisr(x))
         ) %>%
  left_join(select(grupos_zoho, Grupo = grupo, grupo_accronym), by = "Grupo")





export(emprendedoras, file.path(exdir, "emprendedoras.rds"))



#Look Up agentes ===============================================================


agentes <- fac_zoho %>% 
  dplyr::filter(Roles == "Agente") %>%
  select(ID_agente = ID,
         Agente = Facilitadora,
         Cidade)


export(agentes, file.path(exdir, "agentes.rds"))



# Look Up facilitadoras =======================================================
facilitadoras <- fac_zoho %>% 
  dplyr::filter(Roles == "Facilitadora") %>%
  select(ID_facilitadora = ID,
         Facilitadora = Facilitadora,
         Cidade)




export(facilitadoras, file.path(exdir, "facilitadoras.rds"))



#Look Up Cidade ==============================================================

cidades <- fac_zoho %>%
  select(ends_with("Cidade")) %>%
  group_by(Cidade) %>%
  slice(1) %>%
  ungroup()


export(cidades, file.path(exdir, "cidades.rds"))

#Look Up Grupo ================================================================
grupos <- grupos_zoho %>%
  select(Grupo = grupo,
         ID_Grupo = ID)


names(grupos_zoho)
export(grupos, file.path(exdir, "grupos.rds"))


#Look up Turmas ===============================================================
turmas <- turmas_zoho %>%
  select(Turma = turma_fixa,
         ID_Turma = ID)

export(turmas, file.path(exdir, "turmas.rds"))

#Actividades =================================================================

actividades <- actividades_zoho %>%
  select(actividade,
         ID_actividade = ID, 
         por,
         Grupal_o_individual,
         sessoes = sessoies_obrigatorias)

export(actividades, file.path(exdir, "actividades.rds"))

#Sessoes fixas ==================================================================


sessoes <- sessoes_fixas %>%
  select(Modulo = sessao__ixa)


export(sessoes, file.path(exdir, "sessoes.rds"))
