# Alunos: Ana Luiza Koch, Gabriel Vieira Rabelo, Lucas Crud e Ricardo Augusto

# Importando a base de dados
#Ativando pacote
library(readr)

# Importando os dados do Censo Escolar da Educação Básica 2024
base = read_csv2(file = "microdados_ed_basica_2024.csv")

#Tentando reconhecer o encoding do arquivo
guess_encoding("microdados_ed_basica_2024.csv")

# Reimportando os dados com encoding apropriado
base = read_csv2(file = "microdados_ed_basica_2024.csv",
                 locale = locale(encoding = "ISO-8859-1"),
                 na = c("88888"))

# Carregando o pacote
library(dplyr)

# mostrar apenas os dados do município do RJ
escolas_rj <- base %>%
  filter(SG_UF == "RJ")

# PERCENTUAL DE BIBLIOTECAS nos MUNICÍPIOS DO RIO DE JANEIRO
percentual_bibliotecas <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    total_escolas = n(),
    escolas_com_biblioteca = sum(IN_BIBLIOTECA == 1),
    percentual_com_biblioteca = 100 * escolas_com_biblioteca / total_escolas
  )

# 2) percentual de escolas com banheiro adequado à educação infantil nos MUNICÍPIOs DO RIO DE JANEIRO
percentual_banheiro_infantil <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    total_escolas = n(),
    escolas_com_banheiro_infantil = sum(IN_BANHEIRO_EI == 1),
    Perc_banheiro_infantil = round(100 * escolas_com_banheiro_infantil / total_escolas, 2)
  )

# 3) proporção de escolas com coordenador pedagógico ou orientador educacional
proporcao_funcionarios <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    total_escolas = n(),
    escolas_com_funcionario = sum(QT_PROF_COORDENADOR == 1 | QT_PROF_PEDAGOGIA == 1),
    proporcao_com_funcionario = round(100 * escolas_com_funcionario / total_escolas, 2)
  )


# 4) média de salas de aula acessíveis nas escolas
media_salas_acessiveis <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    media_salas_acessiveis = round(mean(QT_SALAS_UTILIZADAS_ACESSIVEIS, na.rm = TRUE), 2)
  )

# 5) percentual de escolas que oferecem alimentação escolar (PNAE)
percentual_pnae <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    total_escolas = n(),
    escolas_com_pnae = sum(IN_ALIMENTACAO == 1, na.rm = TRUE),
    percentual_pnae = round(100 * escolas_com_pnae / total_escolas, 2)
  )


# 6) percentual de escolas com grêmio estudantil
perc_gremio <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    total_escolas = n(),
    escolas_com_gremio = sum(IN_ORGAO_GREMIO_ESTUDANTIL == 1, na.rm = TRUE),
    percentual_gremio = round(100 * escolas_com_gremio / total_escolas, 2)
  )

# 7) média de computadores para alunos por escola
media_comp_por_municipio <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    media_computadores_por_escola = round(mean(QT_COMP_PORTATIL_ALUNO, na.rm = TRUE), 2)
  )

# 8)  Percentual de escolas com pátio coberto
perc_esc_patio <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    total_escolas = n(),
    escolas_com_patio = sum(IN_PATIO_COBERTO == 1, na.rm = TRUE),
    perc_esc_patio = round(100 * escolas_com_patio / total_escolas, 2)
  )


# 9) Percentual de escolas com laboratório de informática
perc_lab_inf <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    total_escolas = n(),
    esc_com_labinf = sum(IN_LABORATORIO_INFORMATICA == 1, na.rm = TRUE),
    perc_lab_inf = round(100 * esc_com_labinf / total_escolas, 2)
  )

# 10) Percentual de escolas com refeitório
perc_esc_refeitorio <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    total_escolas = n(),
    esc_com_ref = sum(IN_REFEITORIO == 1, na.rm = TRUE),
    perc_esc_refeitorio = round(100 * esc_com_ref / total_escolas, 2)
  )

# 11) Percentual de escolas com água da rede pública
perc_agua_redepublic <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    total_escolas = n(),
    esc_com_redepublic = sum(IN_AGUA_REDE_PUBLICA == 1, na.rm = TRUE),
    perc_agua_redepublic = round(100 * esc_com_redepublic / total_escolas, 2)
  )

# 12) Percentual de Escolas com Sala de Recursos Multifuncionais para Atendimento Educacional Especializado (AEE)
perc_sala_recMultif <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    total_escolas = n(),
    esc_com_salaMultif = sum(IN_SALA_ATENDIMENTO_ESPECIAL == 1, na.rm = TRUE),
    perc_sala_recMultif = round(100 * esc_com_salaMultif / total_escolas, 2)
  )

#13) Percentual de escolas com acessos à banda larga
perc_band_larga <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    total_escolas = n(),
    esc_com_bandlarga = sum(IN_BANDA_LARGA == 1, na.rm = TRUE),
    perc_band_larga = round(100 * esc_com_bandlarga / total_escolas, 2)
  )

# 14) Percentual de escolas com pátio descoberto
perc_patiodesc <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    total_escolas = n(),
    esc_patioDesc = sum(IN_PATIO_DESCOBERTO == 1, na.rm = TRUE),
    perc_patiodesc = round(100 * esc_patioDesc / total_escolas, 2)
  )

# 15)
Perc_abast_agua <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    Escolas_sem_agua = sum(IN_AGUA_INEXISTENTE, na.rm = TRUE),
    Total_escolas = n(),
    Percentual_sem_agua = (Escolas_sem_agua / Total_escolas) * 100
  )

# 16) Percentual de escolas com abastecimento de energia elétrica da rede pública
perc_abast_energEletrica <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    total_escolas = n(),
    esc_abastenerg_eletric = sum(IN_ENERGIA_REDE_PUBLICA == 1, na.rm = TRUE),
    perc_abast_energEletrica = round(100 * esc_abastenerg_eletric / total_escolas, 2)
  )

# 17) Percentual de escolas que não possuem esgotamento sanitário
perc_nao_esgot_sanitario <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    total_escolas = n(),
    esc_nao_esg_sanitario = sum(IN_ESGOTO_INEXISTENTE == 1, na.rm = TRUE),
    perc_nao_esgot_sanitario = round(100 * esc_nao_esg_sanitario / total_escolas, 2)
  )

# 18) Percentual de escolas que possuem serviço de coleta de lixo
perc_colet_lixo <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    total_escolas = n(),
    esc_com_coletLixo = sum(IN_LIXO_SERVICO_COLETA == 1, na.rm = TRUE),
    perc_colet_lixo = round(100 * esc_com_coletLixo / total_escolas, 2)
  )

# 19) Percentual de escolas que fazem o processo de reciclagem
perc_reciclagem <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    total_escolas = n(),
    esc_com_reciclagem = sum(IN_TRATAMENTO_LIXO_RECICLAGEM == 1, na.rm = TRUE),
    perc_reciclagem = round(100 * esc_com_reciclagem / total_escolas, 2)
  )

# 20) Percentual de escolas que reaproveitam/reutilizam o lixo/resíduos
perc_reaprov_lixo <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    total_escolas = n(),
    esc_com_reaprovLixo = sum(IN_TRATAMENTO_LIXO_REUTILIZA == 1, na.rm = TRUE),
    perc_reaprov_lixo = round(100 * esc_com_reaprovLixo / total_escolas, 2)
  )

# 21) Percentual de escolas que possuem área de vegetação
perc_area_verde <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    total_escolas = n(),
    esc_com_area_verde = sum(IN_AREA_VERDE == 1, na.rm = TRUE),
    perc_area_verde = round(100 * esc_com_area_verde / total_escolas, 2)
  )


# 22) Percentual de escolas que possuem refeitório
perc_refeitorio <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    total_escolas = n(),
    esc_com_refeitorio = sum(IN_REFEITORIO == 1, na.rm = TRUE),
    perc_refeitorio = round(100 * esc_com_refeitorio / total_escolas, 2)
  )

# 23) Qual a média de alunos por professor?
media_aluno_prof <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    Total_Matr_Basicas = sum(QT_MAT_BAS, na.rm = TRUE), # Soma as matrículas básicas
    Total_Matr_Especiais = sum(QT_MAT_ESP, na.rm = TRUE), # Soma as matrículas especiais
    Total_Doc_Basicos = sum(QT_DOC_BAS, na.rm = TRUE),       # Soma os docentes básicos
    Total_Doc_Especiais = sum(QT_DOC_ESP, na.rm = TRUE),   # Soma os docentes especiais
    # Calcula a razão aluno/professor
    razao_aluno_prof = round((Total_Matr_Basicas + Total_Matr_Especiais) /
                               (Total_Doc_Basicos + Total_Doc_Especiais), 2)
  )


# 24) Qual a proporção de escolas com acesso à internet?
perc_internet <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    total_escolas = n(),
    esc_com_internet = sum(IN_INTERNET == 1, na.rm = TRUE),
    perc_internet = round(100 * esc_com_internet / total_escolas, 2)
  )

# 25) Qual a relação aluno/sala de aula?

media_sala_aluno <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    Total_Matr_Basicas = sum(QT_MAT_BAS, na.rm = TRUE), # Soma as matrículas básicas
    Total_Matr_Especiais = sum(QT_MAT_ESP, na.rm = TRUE), # Soma as matrículas especiais
    Salas_Existentes = sum(QT_SALAS_UTILIZADAS_DENTRO, na.rm = TRUE),       # Soma os docentes básicos
    # Calcula a razão aluno/professor
    razao_aluno_sala = round((Total_Matr_Basicas + Total_Matr_Especiais) /
                               (Salas_Existentes), 2)
  )

# 26) Qual a proporção de escolas com laboratório de ciências?
perc_lab <- escolas_rj %>%
    group_by(NO_MUNICIPIO) %>%
    summarise(
      total_escolas = n(),
      esc_com_lab = sum(IN_LABORATORIO_CIENCIAS == 1, na.rm = TRUE),
      perc_lab = round(100 * esc_com_lab / total_escolas, 2)
    )


# 27) Qual o percentual de escolas com quadra esportiva?
perc_quadra <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    total_escolas = n(),
    esc_com_quadra = sum(IN_QUADRA_ESPORTES == 1, na.rm = TRUE),
    perc_quadra = round(100 * esc_com_quadra / total_escolas, 2)
  )

# 28) Qual o percentual de escolas com água potável?
perc_agua_potavel <- escolas_rj %>%
    group_by(NO_MUNICIPIO) %>%
    summarise(
      total_escolas = n(),
      esc_com_agua_potavel = sum(IN_AGUA_POTAVEL == 1, na.rm = TRUE),
      perc_agua_potavel = round(100 * esc_com_agua_potavel / total_escolas, 2)
    )

# 29) Qual a taxa de escolas que oferecem ensino médio?
perc_ens_med <- escolas_rj %>%
  group_by(NO_MUNICIPIO) %>%
  summarise(
    total_escolas = n(),
    esc_com_ens_med = sum(IN_MED == 1, na.rm = TRUE),
    perc_ens_med = round(100 * esc_com_ens_med / total_escolas, 2)
  )
