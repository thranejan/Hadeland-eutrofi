dir()

library(openxlsx)

d.askjum <-read.xlsx("2022-02-07_Askjumelva nedre.xlsx", detectDates = T) 
d.grymyr <-read.xlsx("2022-02-07_Grymyrbekken nedre.xlsx", detectDates = T) 
d.slovik <-read.xlsx("2022-02-07_Slovikelva.xls.xlsx" , detectDates = T) 
d.vang <-read.xlsx("2022-02-07_Vangselva.xlsx", detectDates = T) 
d.vigga <-read.xlsx("2022-02-07_Vigga.xlsx", detectDates = T) 


data <- rbind(d.askjum, d.grymyr, d.slovik, d.vang, d.vigga)
head(data)
data2 <- data %>% select(Prøvetakingstidspunkt, Vannlokalitetskode, Vannlokalitetsnavn, Oppdragsgiver, Oppdragstaker,
                         ParameterID, Parameternavn, PrøvetakingsmetodeID, Filtrert_Prove, Registreringsverdi,
                         Enhetsnavn, Øvre.dyp, Kommentarer) 