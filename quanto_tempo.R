quanto_tempo <-
  function(dma,
           un = "d"){
    
    require(lubridate)
    require(dplyr)
    require(stringr)
    
    
    dma <- dmy(dma)
    
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
    
    dif <-
    difftime(
      today(),
      dma,
      units = un)
    
    dif2 <-
      dif %>% 
      str_extract(.,
                  "[[:digit:]]{1,}") %>% 
      as.numeric()


    if(
      today() <= dma
    ){
      txt1 <- c("Faltam")
      txt2 <- c("para a data indicada.")
    } else {
      txt1 <- c("Já se passaram")
      txt2 <- c("da data indicada.")
    }
    
    paste(
      txt1,
      dif2,
      txt_un,
      txt2,
      sep = " "
    )
    
  }