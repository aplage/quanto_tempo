quanto_tempo <-
  function(dma,
           un = "d"){
    
    require(lubridate)
    require(dplyr)
    require(stringr)
    
    
    
    
    if(!is.character(dma)){
      stop("A data deve ser informada como caractere, no formato 'dia mês ano'.")
    }
    
    if(
      ! un %in% c("w", "d", "h", "m", "s")
    ){
      warning("A unidade deve ser informada como 'w'= semana ou 'd' = dia ou 'h' = hora ou 'm' = min ou 's' = segundo.")
    }
    
    dma <- dmy(dma)
    un = un
    
    txt1 <- character()
    txt2 <- character()
    
    txt_un <- 
      if(un == "d"){
        "dias"
      } else if(un == "h"){
        "horas"
      } else if(un == "m"){
        "minutos"
      } else if(un == "s"){
        "segundos"
      } else if(un == "w"){
        "semanas"
      }
    
   
    
     if(length(dma) == 1){
      
       
       if(
         is.na(dma)
       ){
         NA
       }
       else if(
        today() <= dma
      ){
        txt1 <- c("Faltam")
        txt2 <- c("para a data indicada.")
      } else {
        txt1 <- c("Já se passaram")
        txt2 <- c("da data indicada.")
      }
      
      
      dif <- 
        difftime(
          today(),
          dma,
          units = un
        )
      
      dif2 <-
        dif %>% 
        str_extract(.,
                    "[[:digit:]]{1,}") %>% 
        as.numeric()
      
      if(is.na(dif2)){
        NA
      } else {
        
        paste(
          txt1,
          dif2,
          txt_un,
          txt2,
          sep = " "
        )
      }

        
       

    } else if(length(dma) > 1){
      
      sapply(
        dma,
        function(t){
          
          
          
          if(
            is.na(t)
          ){
            NA
          }
          else if(
            today() <= t
          ){
            txt1 <- c("Faltam")
            txt2 <- c("para a data indicada.")
          } else {
            txt1 <- c("Já se passaram")
            txt2 <- c("da data indicada.")
          }
          
          
          dif <- 
            difftime(
              today(),
              t,
              units = un
            )
          
          dif2 <-
            dif %>% 
            str_extract(.,
                        "[[:digit:]]{1,}") %>% 
            as.numeric()
          
          if(is.na(dif2)){
            NA
          } else {
            
            paste(
              txt1,
              dif2,
              txt_un,
              txt2,
              sep = " "
            )  
            
          }
  
        }
      )
          
    }
  }
        
