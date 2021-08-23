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
    
    dif2 <-
      dif %>% 
      str_extract(.,
                  "[[:digit:]]{1,}") %>% 
      as.numeric()
    
    return(dif)
    dif2
    
  }