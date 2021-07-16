#@predi = data standardizovaných environmentálních prediktorů v daných bodech, vybrat dané sloupce
#@id = data (sloupec) s hodnotami ID - pořadí musí souhlasit s predi!
#@step = velikost kroku
#@seed = zadat seed pro replikovatelnost
#@save.grids = zadat TRUE, pokud je žádoucí uložit hranice buněk

enviFilter <- function(predi,
                       id,
                       step=0.1,
                       seed=700,
                       save.grid=FALSE) {
  if (step > 0) {
    sequence <- list()
    
    #Vytvoří grid na základě rozsahu hodnot
    #a dle velikosti kroku smerodatne odchylky
    for (i in 1:ncol(predi)) {
      if (min(predi[,i]) < -step) {
        neg.sequence <- sort(seq(0, min(predi[,i]), by = -step))
      }else{
        neg.sequence <- c(-step,0)
      }
      
      if (max(predi[,i]) > step) {
        pos.sequence <- seq(0+step, max(predi[,i]), by = step)
      }else{
        pos.sequence <- c(step)
      }
      interstep.sequence <- c(neg.sequence, pos.sequence)
      sequence[[i]] <- c(interstep.sequence[1]-step, interstep.sequence, interstep.sequence[length(interstep.sequence)]+step)
    }
    
    #Rozřadí záznamy do subskupin pro každý prediktor
    #dle hranic gridu stanovených v předchozím kroku
    groups.data <- as.data.frame(id)
    for (i in 1:ncol(predi)) {
      groups.data[paste0("subgroup_", i)] <- as.numeric(cut(predi[,i], 
                                                            breaks = unlist(sequence[i]),
                                                            include.lowest = TRUE))
    }
    
    #Vytvoří nové skupiny založené na příslušnosti
    #do buňky gridu ve vícedimenzním environmentálním prostoru
    groups.data$group <- c()
    for (i in 1:(ncol(groups.data)-1)) {
      groups.data$group <- paste0(groups.data$group,
                                  groups.data[,1+i])
    }
    
    #Zamíchá záznamy
    set.seed(seed)
    groups.data <- groups.data[sample(nrow(groups.data)),]
    
    #Z každé vícerozměrné buňky environmentálního prostoru
    #vrátí ID právě jednoho záznamu
    if (save.grid == TRUE) {
      return(list(result = sort(groups.data[!duplicated(groups.data$group), 1]),
                  grid = sequence))
    } else {
      return(sort(groups.data[!duplicated(groups.data$group), 1])) 
    }
  }else{
    return(sort(id))
  }
  
}
