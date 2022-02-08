dir()

library(openxlsx)
library(dplyr)

d.askjum <-read.xlsx("2022-02-07_Askjumelva nedre.xlsx", detectDates = T) 
d.grymyr <-read.xlsx("2022-02-07_Grymyrbekken nedre.xlsx", detectDates = T) 
d.slovik <-read.xlsx("2022-02-07_Slovikelva.xls.xlsx" , detectDates = T) 
d.vang <-read.xlsx("2022-02-07_Vangselva.xlsx", detectDates = T) 
d.vigga <-read.xlsx("2022-02-07_Vigga.xlsx", detectDates = T) 

# Bind sammen data 
data <- rbind(d.askjum, d.grymyr, d.slovik, d.vang, d.vigga)
head(data)

data %>% 
  select(Prøvetakingstidspunkt, Vannlokalitetskode, Vannlokalitetsnavn, Oppdragsgiver, Oppdragstaker,
                         ParameterID, Parameternavn, PrøvetakingsmetodeID,  Registreringsverdi,
                         Enhetsnavn, Kommentarer) %>%
  rename(dato = Prøvetakingstidspunkt, verdi = Registreringsverdi) %>%
  filter(Parameternavn %in% c("Totalnitrogen", "Ammonium", "Nitrat + nitritt", "Nitrat",
                              "Totalfosfor", "Orto-fosfat", "Fosfat", 
                              "Termotolerante koliforme bakterier", "E. coli",
                              "Fargetall","Totalt organisk karbon (TOC)", "Kalsium", "pH", "Turbiditet", 
                              "Limnisk bunnfauna nEQR eutrofiering", "Limnisk bunnfauna nEQR organisk belastning",
                              "Average Score per Taxon (ASPT)",  "Begroing nEQR eutrofiering",
                              "Estimert antall fisk (art) per arealenhet")
                            )
  
  

#
