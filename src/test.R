dir()

library(openxlsx)
library(dplyr)
library(tidyr)
library(lubridate)
############################################################################

d.askjum <-read.xlsx("2022-02-07_Askjumelva nedre.xlsx", detectDates = T) 
d.grymyr <-read.xlsx("2022-02-07_Grymyrbekken nedre.xlsx", detectDates = T) 
d.slovik <-read.xlsx("2022-02-07_Slovikelva.xls.xlsx" , detectDates = T) 
d.vang <-read.xlsx("2022-02-07_Vangselva.xlsx", detectDates = T) 
d.vigga <-read.xlsx("2022-02-07_Vigga.xlsx", detectDates = T) 

unique(d.vigga$ParameterID)


# Bind sammen data 
data <- rbind(d.askjum, d.grymyr, d.slovik, d.vang, d.vigga)
head(data)
names(data)

d <- data %>% 
  select(Prøvetakingstidspunkt, Vannlokalitetskode, Vannlokalitetsnavn, Oppdragsgiver, Oppdragstaker,
                         ParameterID, Parameternavn, PrøvetakingsmetodeID,  Registreringsverdi,
                         Enhetsnavn, Vitenskapelig.navn, Kommentarer) %>%
  rename(dato = Prøvetakingstidspunkt, verdi = Registreringsverdi) %>%
  mutate(dato = ymd(dato)) %>%
  filter(Parameternavn %in% c("Totalnitrogen", "Ammonium", "Nitrat + nitritt", "Nitrat",
                              "Totalfosfor", "Orto-fosfat", "Fosfat", 
                              "Termotolerante koliforme bakterier", "E. coli",
                              "Fargetall","Totalt organisk karbon (TOC)", "Kalsium", "pH", "Turbiditet", 
                              "Limnisk bunnfauna nEQR eutrofiering", "Limnisk bunnfauna nEQR organisk belastning",
                              "Average Score per Taxon (ASPT)",  "Begroing nEQR eutrofiering",
                              "Estimert antall fisk (art) per arealenhet")) %>%
  arrange(dato) %>%
  mutate(prove_ID = paste(Vannlokalitetskode, dato, sep = "_")) %>%
  
  pivot_wider(names_from = ParameterID, values_from = verdi, id_cols = c(Vannlokalitetskode, Vannlokalitetsnavn,
    dato, Oppdragstaker, Oppdragsgiver, Kommentarer)) %>%
  select(-Kommentarer) %>%
  group_by(Vannlokalitetskode, Vannlokalitetsnavn, Oppdragstaker, Oppdragsgiver, dato) %>%
  summarize_all(median, na.rm =T)  %>%
  arrange(Vannlokalitetskode, dato) %>%
  data.frame()

head(d, 5)

### Slå sammen NO3 og (NO3+NO2) samt PO4 og ortofosfat. Av en eller annen grunn er det 
# enkelte år målt med ulik metode. Flest data på P.PO4 og NO3+NO2
d$PO4.comb <- with(d, coalesce(P.PO4, P.ORTO))
d$NO3.comb <- with(d, coalesce(N.NO3, N.SNOX))


### Add år og måned
d$year <- year(d$dato)
d$mnd <- month(d$dato)

# Se kjapt på dataene
library(lattice)
xyplot(log10(P.TOT) ~ dato | Vannlokalitetsnavn, data = filter(d, year > 2009))
bwplot(P.TOT ~ factor(year) | Vannlokalitetsnavn, data = filter(d, year > 2009))
bwplot(P.TOT ~ factor(year) | Vannlokalitetsnavn, data = filter(d, year > 2009))

# fosfat
xyplot(P.PO4 ~ dato | Vannlokalitetsnavn, data = filter(d, year > 2009))
xyplot(P.ORTO ~ dato | Vannlokalitetsnavn, data = filter(d, year > 2009))
xyplot(PO4.comb ~ dato | Vannlokalitetsnavn, data = filter(d, year > 2009))
sum(complete.cases(d$PO4.comb))
sum(complete.cases(d$P.PO4))

# nitrat
xyplot(N.NO3 ~ dato | Vannlokalitetsnavn, data = filter(d, year > 2009))
xyplot(N.SNOX ~ dato | Vannlokalitetsnavn, data = filter(d, year > 2009))
xyplot(NO3.comb ~ dato | Vannlokalitetsnavn, data = filter(d, year > 2009))
sum(complete.cases(d$NO3.comb))
sum(complete.cases(d$N.NO3))
sum(complete.cases(d$N.SNOX))


### Sjekk vanntype-parametere ###
bwplot(FARGE ~ factor(year) | Vannlokalitetsnavn, data = filter(d, year > 2009))
boxplot(FARGE ~  Vannlokalitetsnavn, data = filter(d, year > 2009), log = "y")
abline(h = c(10, 30)) # Alle er innenfir "Klar" vanntype 

d %>% group_by(Vannlokalitetsnavn) %>%
  filter(year > 2010) %>%
  summarize(mean(FARGE, na.rm = T), median(FARGE, na.rm = T))


boxplot(TOC ~  Vannlokalitetsnavn, data = filter(d, year > 2009), log = "y")
boxplot(CA~  Vannlokalitetsnavn, data = filter(d, year > 2009), log = "y")

### Klassegrenser ###
# TOTP i moderat kalkrike og kalkrike, klare elver i lavland
tp1 = data.frame(vanntype = "mod_kalkrik_klar_lavland", referanse = 9, SGG = 15, GM = 25, MD = 38, DSD = 65)
# TOTP i moderat kalkrik, skog (kalkrik, skog finnes ikke - går under denne)
tp2 = data.frame(vanntype = "mod_kalkrik_klar_skog", referanse = 6, SGG = 11, GM = 17, MD = 30, DSD = 60)
# TOTP humøs, moderat kalkrik, lavland
tp3 = data.frame(vanntype = "mod_kalkrik_humøs_lavland", referanse =11, SGG = 20, GM = 29, MD = 58, DSD = 98)
# TOTP humøs, moderat kalkrik, skog
tp4 = data.frame(vanntype = "mod_kalkrik_humøs_skog", referanse =9, SGG = 17, GM = 24, MD = 45, DSD = 83)

tp.klassegrenser <- rbind(tp1, tp2, tp3, tp4)







