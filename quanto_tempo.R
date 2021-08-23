quanto_tempo <-
  function(dma){
    
    library(lubridate)
    
    difftime(
      today(),
      dma)
    
  }