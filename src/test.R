dir()

library(openxlsx)
library(dplyr)
library(tidyr)
library(lubridate)

SG <- rgb(0,92,230, max = 255)
G <- rgb(0,144,54, max = 255)
M <- rgb(255, 236, 0, max = 255)
D <- rgb(242, 148,0, max = 255)
SD <- rgb(236,28,36, max = 255)
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

#####################
### Klassegrenser ###
#####################

# TOTP i moderat kalkrike og kalkrike, klare elver i lavland
tp1 = data.frame(vanntype = "mod_kalkrik_klar_lavland", referanse = 9, SGG = 15, GM = 25, MD = 38, DSD = 65)
# TOTP i moderat kalkrik, skog (kalkrik, skog finnes ikke - går under denne)
tp2 = data.frame(vanntype = "mod_kalkrik_klar_skog", referanse = 6, SGG = 11, GM = 17, MD = 30, DSD = 60)
# TOTP humøs, moderat kalkrik, lavland
tp3 = data.frame(vanntype = "mod_kalkrik_humøs_lavland", referanse =11, SGG = 20, GM = 29, MD = 58, DSD = 98)
# TOTP humøs, moderat kalkrik, skog
tp4 = data.frame(vanntype = "mod_kalkrik_humøs_skog", referanse =9, SGG = 17, GM = 24, MD = 45, DSD = 83)
tp.klassegrenser <- rbind(tp1, tp2, tp3, tp4)

#############################################
### Plott for hver elv og TOTP ##############
#############################################
unique(d$Vannlokalitetsnavn)

#### Vigga ###
# Antall observasjoner pr. år av TotP
with(filter(d, Vannlokalitetsnavn == "Røykenvik" & complete.cases(P.TOT)), table(year))

# Definerer year som faktor med alle år for plott
d.vig <- filter(d, Vannlokalitetsnavn == "Røykenvik")
d.vig$year <- factor(d.vig$year, levels = 2012:2021)

d.vig.mean <- d.vig %>% 
  group_by(year) %>%
  summarize_all(mean, na.rm = T) %>% 
  data.frame()


### TOTP og TOTN ###
png("totP og totN Vigga.png", res = 1000, height = 7.2, width = 7.5, units = "in")
par(mfrow = c(2,1), mar = c(2,5,2,2), oma = c(4,2,2,2))
# Boxplot: om man ønsker å ta med de gamle dataene (94-96) så bruk d.vig
boxplot(P.TOT ~ year, data = d.vig, log = "y", axes = F, border = NA,
        ylab = "")
rect(xleft = -1, xright = 50, ybottom = 0.1, ytop = 15, col = SG, border = NA)
rect(xleft = -1, xright = 50, ybottom = 15, ytop = 25, col = G, border = NA)
rect(xleft = -1, xright = 50, ybottom = 25, ytop = 38, col = M, border = NA)
rect(xleft = -1, xright = 50, ybottom = 38, ytop = 65, col = D, border = NA)
rect(xleft = -1, xright = 50, ybottom = 65, ytop = 200, col = SD, border = NA)
boxplot(P.TOT ~ year, data = d.vig, log = "y", add = T, las = 2)
mtext("Total fosfor (µg P/L)", side = 2, line = 3.5, cex = 1.2)

# Add gjennomsnitt som punkt
points(P.TOT ~ year, data = d.vig.mean, col = 1, pch = "*", cex = 1.5, log = "y")

# TOTN
with(filter(d, Vannlokalitetsnavn == "Røykenvik" & complete.cases(N.TOT)), table(year))
boxplot(N.TOT ~ year, data = d.vig, log = "y", axes = F, ylim  = c(250, 8000), border = "grey31",
        ylab = "")
rect(xleft = -1, xright = 50, ybottom = 0.1, ytop = 425, col = SG, border = NA)
rect(xleft = -1, xright = 50, ybottom = 425, ytop = 675, col = G, border = NA)
rect(xleft = -1, xright = 50, ybottom = 675, ytop = 950, col = M, border = NA)
rect(xleft = -1, xright = 50, ybottom = 950, ytop = 1425, col = D, border = NA)
rect(xleft = -1, xright = 50, ybottom = 1425, ytop = 10000, col = SD, border = NA)
boxplot(N.TOT ~ year, data = d.vig, log = "y",add = T, las = 2)
points(N.TOT  ~ year, data = d.vig.mean, col = 1, pch = "*", cex = 1.5)
mtext("Total nitrogen (µg N/L)", side = 2, line = 3.5, cex = 1.2)
dev.off()

### Fosfat og nitrat ###
d.vig2 <- filter(d, Vannlokalitetsnavn == "Røykenvik" & year >= 2012)
d.vig2year <- factor(d.vig$year, levels = 2014:2021)

png("PO4 og NO3 Vigga.png", res = 1000, height = 7.2, width = 7.5, units = "in")
par(mfrow = c(2,1), mar = c(2,5,2,2), oma = c(4,2,2,2))
# Fosfat

with(filter(d, Vannlokalitetsnavn == "Røykenvik" & complete.cases(PO4.comb)), table(year))
boxplot(PO4.comb ~ year, data = d.vig2, log = "", ylab = "", las = 2)
mtext("Fosfat (µg PO4-P/L)", side = 2, line = 3.5, cex = 1.2)
abline(h = c(1,2), lty = 2) # Q1 og Q3 basert på total fosfat fra Kjaglielva, Lomma og Kjørstadelva (2017-2021) (<1 satt til 1)


# Nitrat
with(filter(d, Vannlokalitetsnavn == "Røykenvik" & complete.cases(NO3.comb)), table(year))
boxplot(NO3.comb ~ year, data = d.vig2, log = "", ylab = "", las = 2, ylim = c(20, 5000))
mtext("Nitrat (µg NO3+NO2-N/L)", side = 2, line = 3.5, cex = 1.2)
abline(h = c(90, 215), lty = 2) # Basert på q1 = 90, median = 150 og q3 = 215 an NO3-N fra Kjaglielva, Lomma og Kjørstadelva (2017-2021)

#text(expression(italic("Referansenivå av NO"[3])), x = 2.2, y = 150, font = )
dev.off()



fos <- read.table("clipboard")
summary(fos)
