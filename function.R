#functions
desmembra_pdf <- function(zumbis_capturados){
  ID <- c()
  NOME <- c()
  NASCIMENTO <- c()
  GENERO <- c()
  df_zumbis_capturados <- data.frame(ID,NOME,NASCIMENTO,GENERO)
  
  #grava paginas
  for(documento in 1:length(zumbis_capturados)){
    for (pagina in 1:length(zumbis_capturados[[1]])) {
      tab_name <- paste("data/tab_",documento,"_",pagina,".dat", sep = '')
      tab <- zumbis_capturados[[documento]][pagina]
      cat(tab, file = tab_name)
    }
    n_paginas <- length(zumbis_capturados[[1]])
    df_zumbis_capturados <- gera_df_zumbis(n_paginas,df_zumbis_capturados)
  }
  #INCONSISTENCIA SUPOSICAO ERRO DO DIGITACAO ALTRADO 1696 PARA 1996 	
  #477   JOSE AUGUSTO DA SILVA  1696-06-22  M
  df_zumbis_capturados$NASCIMENTO[477] <- "1996-06-22"
  return(df_zumbis_capturados)
}

gera_df_zumbis <- function(n_paginas,df_zumbis_capturados){
  for(pagina in 1:n_paginas){
    tab_name <- paste("data/tab_1_",pagina,".dat", sep = '')
    #pagina 1
    if(pagina == 1){
      linhas_tab <- length(readLines(tab_name))
      for(x_linha in 8:linhas_tab){
        gender = "M"
        vt.linha <- coleta_dados(tab_name,x_linha,gender) 
        df_zumbis_capturados <- rbind(df_zumbis_capturados,vt.linha)            
      }
      colnames(df_zumbis_capturados) <- c('ID','NOME','NASCIMENTO','GENERO')  
    }
    #paginas 2:19 "homens"
    zumbi_h <- seq(2,19)
    if(pagina %in% zumbi_h){
      linhas_tab <- length(readLines(tab_name))
      for(x_linha in 0:linhas_tab){
        gender = "M"
        vt.linha <- coleta_dados(tab_name,x_linha,gender) 
        df_zumbis_capturados <- rbind(df_zumbis_capturados,vt.linha)          
      }
    }    
    #pagina 20 h & M
    if(pagina == 20){
      linhas_tab <- length(readLines(tab_name))
      for(x_linha in 0:20){
        gender = "M"
        vt.linha <- coleta_dados(tab_name,x_linha,gender) 
        df_zumbis_capturados <- rbind(df_zumbis_capturados,vt.linha)            
      }
      for(x_linha in 25:linhas_tab){
        gender = "F"
        vt.linha <- coleta_dados(tab_name,x_linha,gender) 
        df_zumbis_capturados <- rbind(df_zumbis_capturados,vt.linha) 
      }
    }
    #pagina 21:final
    zumbi_h <- seq(21,n_paginas)
    if(pagina %in% zumbi_h){
      linhas_tab <- length(readLines(tab_name))
      for(x_linha in 0:linhas_tab){
        gender = "F"
        vt.linha <- coleta_dados(tab_name,x_linha,gender) 
        df_zumbis_capturados <- rbind(df_zumbis_capturados,vt.linha)          
      }
    }  
  }
  return(df_zumbis_capturados)
}

coleta_dados <- function(tab_name,x_linha,gender){
  vt.linha <- c()
  linha <- try(scan(tab_name, skip = x_linha, nlines = 1, what = character(0), quiet = TRUE), silent = TRUE)
  if(length(linha) > 0){  
    ID <- linha[1]
    NOME  <- linha[2]
    NASCIMENTO <- as.character(as.Date(linha[length(linha)], format = "%d.%m.%Y"))
    GENERO <- gender
      for(p in 3:(length(linha)-1)){
        NOME <- paste(NOME, linha[p], sep = ' ')
      }
    vt.linha <- c(ID,NOME,NASCIMENTO,GENERO)
  }
  
return(vt.linha)
}

coleta_signo <- function(df_zumbi_capturados){
  data_ecatombe <- as.Date("2023-01-08")
  #signo
  df_zumbi_capturados$SIGNO <- 0
  for(i in 1:length(df_zumbi_capturados$ID)){
    nascimento <- df_zumbi_capturados$NASCIMENTO[i]
    df_zumbi_capturados$SIGNO[i] <- signos(nascimento)
    df_zumbi_capturados$Dia_Sem_Nas[i] <- wday(nascimento) 
    idade <- round(difftime(data_ecatombe, as.Date(nascimento), units = c("days"))/365)
    df_zumbi_capturados$Idade[i] <- as.numeric(idade)
  }
  #dia da semana que nasceu
  for(i in 1:length(df_zumbi_capturados$ID)){
    if(df_zumbi_capturados$Dia_Sem_Nas[i] == 1){
      df_zumbi_capturados$Dia_Sem_Nas[i] <- 'Domingo'
    }
    if(df_zumbi_capturados$Dia_Sem_Nas[i] == 2){
      df_zumbi_capturados$Dia_Sem_Nas[i] <- 'Segunda'
    }
    if(df_zumbi_capturados$Dia_Sem_Nas[i] == 3){
      df_zumbi_capturados$Dia_Sem_Nas[i] <- 'Terca'
    }
    if(df_zumbi_capturados$Dia_Sem_Nas[i] == 4){
      df_zumbi_capturados$Dia_Sem_Nas[i] <- 'Quarta'
    }
    if(df_zumbi_capturados$Dia_Sem_Nas[i] == 5){
      df_zumbi_capturados$Dia_Sem_Nas[i] <- 'Quinta'
    }
    if(df_zumbi_capturados$Dia_Sem_Nas[i] == 6){
      df_zumbi_capturados$Dia_Sem_Nas[i] <- 'Sexta'
    }
    if(df_zumbi_capturados$Dia_Sem_Nas[i] == 7){
      df_zumbi_capturados$Dia_Sem_Nas[i] <- 'Sabado'
    }
  }
  

  write.xlsx(df_zumbi_capturados,"data/df_zumbi_capturados.xlsx", overwrite = TRUE)
  write.table(df_zumbi_capturados, "data/df_zumbi_capturados.csv", sep = ";")
  return(df_zumbi_capturados)
}

signos <- function(nascimento){
  mes_nas <- as.numeric(month(nascimento))
  dia_nas <- as.numeric(mday(nascimento))
  if(mes_nas == 3){
    seq_teste <- seq(21,31)
    if(dia_nas %in% seq_teste){
      signo <- "Aries"
    }
  }
  if(mes_nas == 4){
    seq_teste <- seq(01,20) 
    if(dia_nas %in% seq_teste){
      signo <- "Aries"
    }
  }
  if(mes_nas == 4){
    seq_teste <- seq(21,31)
    if(dia_nas %in% seq_teste){
      signo <- "Touro"
    }
  }
  if(mes_nas == 5){
    seq_teste <- seq(01,20) 
    if(dia_nas %in% seq_teste){
      signo <- "Touro"
    }
  }
  if(mes_nas == 5){
    seq_teste <- seq(21,31)
    if(dia_nas %in% seq_teste){
      signo <- "Gemeos"
    }
  }
  if(mes_nas == 6){
    seq_teste <- seq(01,20) 
    if(dia_nas %in% seq_teste){
      signo <- "Gemeos"
    }
  }
  if(mes_nas == 6){
    seq_teste <- seq(21,31)
    if(dia_nas %in% seq_teste){
      signo <- "Cancer"
    }
  }
  if(mes_nas == 7){
    seq_teste <- seq(01,22) 
    if(dia_nas %in% seq_teste){
      signo <- "Cancer"
    }
  }
  if(mes_nas == 7){
    seq_teste <- seq(23,31)
    if(dia_nas %in% seq_teste){
      signo <- "Leao"
    }
  }
  if(mes_nas == 8){
    seq_teste <- seq(01,22) 
    if(dia_nas %in% seq_teste){
      signo <- "Leao"
    }
  }
  if(mes_nas == 8){
    seq_teste <- seq(23,31)
    if(dia_nas %in% seq_teste){
      signo <- "Virgem"
    }
  }
  if(mes_nas == 9){
    seq_teste <- seq(01,22) 
    if(dia_nas %in% seq_teste){
      signo <- "Virgem"
    }
  }
  if(mes_nas == 9){
    seq_teste <- seq(23,31)
    if(dia_nas %in% seq_teste){
      signo <- "Libra"
    }
  }
  if(mes_nas == 10){
    seq_teste <- seq(01,22) 
    if(dia_nas %in% seq_teste){
      signo <- "Libra"
    }
  }
  if(mes_nas == 10){
    seq_teste <- seq(23,31)
    if(dia_nas %in% seq_teste){
      signo <- "Escorpiao"
    }
  }
  if(mes_nas == 11){
    seq_teste <- seq(01,21) 
    if(dia_nas %in% seq_teste){
      signo <- "Escorpiao"
    }
  }
  if(mes_nas == 11){
    seq_teste <- seq(22,31)
    if(dia_nas %in% seq_teste){
      signo <- "Sagitario"
    }
  }
  if(mes_nas == 12){
    seq_teste <- seq(01,21) 
    if(dia_nas %in% seq_teste){
      signo <- "Sagitario"
    }
  }
  if(mes_nas == 12){
    seq_teste <- seq(22,31)
    if(dia_nas %in% seq_teste){
      signo <- "Capricornio"
    }
  }
  if(mes_nas == 01){
    seq_teste <- seq(01,20) 
    if(dia_nas %in% seq_teste){
      signo <- "Capricornio"
    }
  }
  if(mes_nas == 01){
    seq_teste <- seq(21,31) 
    if(dia_nas %in% seq_teste){
      signo <- "Aquario"
    }
  }
  if(mes_nas == 02){
    seq_teste <- seq(01,19)
    if(dia_nas %in% seq_teste){
      signo <- "Aquario"
    }
  }
  if(mes_nas == 02){
    seq_teste <- seq(20,29) 
    if(dia_nas %in% seq_teste){
      signo <- "Peixes"
    }
  }
  if(mes_nas == 03){
    seq_teste <- seq(01,20)
    if(dia_nas %in% seq_teste){
      signo <- "Peixes"
    }
  }
return(signo)
}

estatisticas_zumbis <- function(df_zumbi_capturados){
  #inconsistencias nos NASCIMENTOS ID 792, 443 ANO DE NASCIMENTO
  #443 JOÃO PAULO DOS SANTOS 2023-01-01 M Capricornio Domingo 0 
  #792 SAULO SILVA EVANGELISTA 2019-03-20 M Peixes Quarta 4
  df_zumbi_capturados$Idade[792] <- NA
  df_zumbi_capturados$Idade[443] <- NA
  df_zumb <- na.omit(df_zumbi_capturados)
  mulheres <- subset(df_zumb, df_zumb$GENERO == "F", select = c(Idade))
  homens <- subset(df_zumb, df_zumb$GENERO == "M", select = c(Idade))
  m_mais_velha <- max(mulheres$Idade)
  m_mais_Nova <- min(mulheres$Idade)
  h_mais_velho <- max(homens$Idade)
  h_mais_Novo <- min(homens$Idade)
  media_idade <- round(mean(df_zumb$Idade))
  media_m <- round(mean(mulheres$Idade))
  media_H <- round(mean(homens$Idade))
  cat(yellow(" ----------------------------------------------------", "\n",
             "|             | Mais velhos | Mais novos |  Média  | ", "\n",
             "| Mulheres    |    ", m_mais_velha,"     |    ", m_mais_Nova,"    |  ",media_m,"   |\n",
             "| Homens      |    ", h_mais_velho, "     |    ",h_mais_Novo,"    |  ",media_H,"   |\n",
             "----------------------------------------------------","\n",
             "   Média Idade Geral: ", media_idade,"\n",
             "----------------------------------------------------","\n"))
  
}




















