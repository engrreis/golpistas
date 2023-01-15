#### relaciona as  curiosidades na lista os golpitas presos em 11/01/2023
#libs
list.of.packages <- c("crayon", "pdftools", "tm","SnowballC","openxlsx","readxl","tidyr","dplyr","stringr", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
load.libs <- lapply(list.of.packages, require, character.only = TRUE)
# INPUTS


source("function.R", local = TRUE)
path_project <- getwd()
dir_input <- "pdf/lista_presos_sexta_13.pdf"
zumbis_capturados <- lapply(dir_input, pdf_text)
df_zumbi_capturados <- desmembra_pdf(zumbis_capturados)
df_zumbi_capturados <- coleta_signo(df_zumbi_capturados)

linhas_tab <- length(readLines(tab_name))

