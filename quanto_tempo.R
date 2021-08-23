quanto_tempo <-
  function(dma,
           un = "d"){
    
    require(lubridate)
    require(dplyr)
    require(stringr)
    
    
    dma <- dmy(dma)
    
    dif <-
    difftime(
      today(),
      dma,
      units = un)
    
    # print(dif)
    
    dif2 <-
      dif %>% 
      str_extract(.,
                  "[[:digit:]]{1,}") %>% 
      as.numeric()

    # dif2
    
    paste(
      "Faltam",
      dif2,
      "dia para a data indicada",
      sep = " "
    )
    
  }