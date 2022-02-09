dir()

library(openxlsx)
library(dplyr)
library(tidyr)
library(lubridate)
library(lattice)

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

# Vurdere å gå for kalkrik,klar, lavland for alle for å få sammenliknbare data
# Vigga er på grensa til humøs, men "strengest" klassegrenser (dvs. klar) bør da brukes
# Alle renner ut i Randsfjorden (135 moh) og kan sånn sett vurderes som lavland,
# selv om medianhøyden i nedbørfeltet varierer og noen i Vann-nett er satt til "skog"

# TOTP i moderat kalkrike og kalkrike, klare elver i lavland
tp1 = data.frame(vanntype = "mod_kalkrik_klar_lavland", referanse = 9, SGG = 15, GM = 25, MD = 38, DSD = 65)
tn1 = data.frame(vanntype = "mod_kalkrik_klar_lavland", referanse = 275, SGG = 425, GM = 675, MD = 950, DSD = 1425)


# TOTP i moderat kalkrik, skog (kalkrik, skog finnes ikke - går under denne)
tp2 = data.frame(vanntype = "mod_kalkrik_klar_skog", referanse = 6, SGG = 11, GM = 17, MD = 30, DSD = 60)
# TOTP humøs, moderat kalkrik, lavland
tp3 = data.frame(vanntype = "mod_kalkrik_humøs_lavland", referanse =11, SGG = 20, GM = 29, MD = 58, DSD = 98)
# TOTP humøs, moderat kalkrik, skog
tp4 = data.frame(vanntype = "mod_kalkrik_humøs_skog", referanse =9, SGG = 17, GM = 24, MD = 45, DSD = 83)


klassegrenser <- rbind(tp1, tn1, tp2, tp3, tp4)

###############################################################
### Plott for hver elv av TOTP, TOTN, NO3 og PO4 ##############
###############################################################
unique(d$Vannlokalitetsnavn)

##############
#### Vigga ###
##############
# Vanntype kalkrik, klar, lavland

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
points(P.TOT ~ year, data = d.vig.mean, col = "blueviolet", pch = 8, cex = 1.5, log = "y")

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
points(N.TOT  ~ year, data = d.vig.mean, col = "blueviolet", pch = 8, cex = 1.5)
mtext("Total nitrogen (µg N/L)", side = 2, line = 3.5, cex = 1.2)
dev.off()

### Fosfat og nitrat ###
d.vig2 <- filter(d, Vannlokalitetsnavn == "Røykenvik" & year >= 2014)
d.vig2$year <- factor(d.vig2$year, levels = 2014:2021)

d.vig2.mean <- d.vig2 %>% 
  group_by(year) %>%
  summarize_all(mean, na.rm = T) %>% 
  data.frame()

png("PO4 og NO3 Vigga.png", res = 1000, height = 7.2, width = 7.5, units = "in")
par(mfrow = c(2,1), mar = c(2,5,2,2), oma = c(4,2,2,2))
# Fosfat

with(filter(d, Vannlokalitetsnavn == "Røykenvik" & complete.cases(PO4.comb)), table(year))
boxplot(PO4.comb ~ year, data = d.vig2, log = "", ylab = "", las = 2, axes = F)
grid()
mtext("Fosfat (µg P/L)", side = 2, line = 3.5, cex = 1.2)
#abline(h = c(1,2), lty = 2) # Q1 og Q3 basert på total fosfat fra Kjaglielva, Lomma og Kjørstadelva (2017-2021) (<1 satt til 1)
rect(xleft = -1, xright = 100, ybottom = 1, ytop = 2, col = "lightpink", border = NA)
boxplot(PO4.comb ~ year, data = d.vig2, log = "", ylab = "", las = 2, add = T)
points(PO4.comb ~ year, data = d.vig2.mean, col = "blueviolet", pch = 8, cex = 1.5, log = "y")


# Nitrat
with(filter(d, Vannlokalitetsnavn == "Røykenvik" & complete.cases(NO3.comb)), table(year))
boxplot(NO3.comb ~ year, data = d.vig2, log = "", ylab = "", las = 2, ylim = c(20, 4500), axes = F)
grid()
mtext("Nitrat + nitritt (µg N/L)", side = 2, line = 3.5, cex = 1.2)
#abline(h = c(90, 215), lty = 2) # Basert på q1 = 90, median = 150 og q3 = 215 an NO3-N fra Kjaglielva, Lomma og Kjørstadelva (2017-2021)
rect(xleft = -1, xright = 100, ybottom = 90, ytop = 215, col = "lightpink", border = NA)
boxplot(NO3.comb ~ year, data = d.vig2, log = "", ylab = "", las = 2, ylim = c(20, 5000), add = T)
points(NO3.comb ~ year, data = d.vig2.mean, col = "blueviolet", pch = 8, cex = 1.5, log = "y")
#text(expression(italic("Referansenivå av NO"[3])), x = 2.2, y = 150, font = )
dev.off()

###################
#### Askjumelva ###
###################
unique(d$Vannlokalitetsnavn)

# Vanntype kalkrik, klar, lavland

# Antall observasjoner pr. år av TotP
with(filter(d, Vannlokalitetsnavn == "Askjumelva, nedre" & complete.cases(P.TOT)), table(year))
with(filter(d, Vannlokalitetsnavn == "Askjumelva, nedre" & complete.cases(N.TOT)), table(year))
with(filter(d, Vannlokalitetsnavn == "Askjumelva, nedre" & complete.cases(NO3.comb)), table(year))
with(filter(d, Vannlokalitetsnavn == "Askjumelva, nedre" & complete.cases(PO4.comb)), table(year))

# Definerer year som faktor med alle år for plott
d.ask <- filter(d, Vannlokalitetsnavn == "Askjumelva, nedre")
d.ask$year <- factor(d.ask$year, levels = 2018:2021)

d.ask.mean <- d.ask %>% 
  group_by(year) %>%
  summarize_all(mean, na.rm = T) %>% 
  data.frame()

### TOTP og TOTN ###
# NB: usikker på log eller ikke log på y-aksen

png("totP og totN Askjumelva.png", res = 1000, height = 7.2, width = 7.5, units = "in")
par(mfrow = c(2,1), mar = c(2,5,2,2), oma = c(4,2,2,2))
# Boxplot: om man ønsker å ta med de gamle dataene (94-96) så bruk d.vig
boxplot(P.TOT ~ year, data = d.ask, log = "", axes = F, border = NA,
        ylab = "", ylim = c(10, 70))
rect(xleft = -1, xright = 50, ybottom = 0.1, ytop = 15, col = SG, border = NA)
rect(xleft = -1, xright = 50, ybottom = 15, ytop = 25, col = G, border = NA)
rect(xleft = -1, xright = 50, ybottom = 25, ytop = 38, col = M, border = NA)
rect(xleft = -1, xright = 50, ybottom = 38, ytop = 65, col = D, border = NA)
rect(xleft = -1, xright = 50, ybottom = 65, ytop = 200, col = SD, border = NA)
boxplot(P.TOT ~ year, data = d.ask, log = "y", add = T, las = 2, ylim = c(2, 80))
mtext("Total fosfor (µg P/L)", side = 2, line = 3.5, cex = 1.2)

# Add gjennomsnitt som punkt
points(P.TOT ~ year, data = d.ask.mean, pch = 8, col = "blueviolet", cex = 1.5, log = "y")

# TOTN
boxplot(N.TOT ~ year, data = d.ask, log = "", axes = F, ylim  = c(250, 10000), border = "grey31",
        ylab = "")
rect(xleft = -1, xright = 50, ybottom = -10, ytop = 425, col = SG, border = NA)
rect(xleft = -1, xright = 50, ybottom = 425, ytop = 675, col = G, border = NA)
rect(xleft = -1, xright = 50, ybottom = 675, ytop = 950, col = M, border = NA)
rect(xleft = -1, xright = 50, ybottom = 950, ytop = 1425, col = D, border = NA)
rect(xleft = -1, xright = 50, ybottom = 1425, ytop = 15000, col = SD, border = NA)
boxplot(N.TOT ~ year, data = d.ask, log = "y",add = T, las = 2)
points(N.TOT  ~ year, data = d.ask.mean, col = "blueviolet", pch = 8, cex = 1.5)
mtext("Total nitrogen (µg N/L)", side = 2, line = 3.5, cex = 1.2)
dev.off()

### Fosfat og nitrat ###
d.ask2 <- filter(d, Vannlokalitetsnavn == "Askjumelva, nedre" & year >= 2014)
d.ask2$year <- factor(d.ask2$year, levels = 2018:2021)

d.ask2.mean <- d.ask2 %>% 
  group_by(year) %>%
  summarize_all(mean, na.rm = T) %>% 
  data.frame()

png("PO4 og NO3 Askjumelva.png", res = 1000, height = 7.2, width = 7.5, units = "in")
par(mfrow = c(2,1), mar = c(2,5,2,2), oma = c(4,2,2,2))
# Fosfat

with(filter(d, Vannlokalitetsnavn == "Askjumelva, nedre" & complete.cases(PO4.comb)), table(year))
boxplot(PO4.comb ~ year, data = d.ask2, log = "", ylab = "", las = 2, axes = F, ylim = c(1, 50))
grid()
mtext("Fosfat (µg P/L)", side = 2, line = 3.5, cex = 1.2)
#abline(h = c(1,2), lty = 2) # Q1 og Q3 basert på total fosfat fra Kjaglielva, Lomma og Kjørstadelva (2017-2021) (<1 satt til 1)
rect(xleft = -1, xright = 100, ybottom = 1, ytop = 2, col = "lightpink", border = NA)
boxplot(PO4.comb ~ year, data = d.ask2, log = "", ylab = "", las = 2, add = T)
points(PO4.comb ~ year, data = d.ask2.mean, col = "blueviolet", pch = 8, cex = 1.5, log = "y")


# Nitrat
with(filter(d, Vannlokalitetsnavn == "Askjumelva, nedre" & complete.cases(NO3.comb)), table(year))
boxplot(NO3.comb ~ year, data = d.ask2, log = "", ylab = "", las = 2, ylim = c(20, 10000), axes = F)
grid()
mtext("Nitrat + nitritt (µg N/L)", side = 2, line = 3.5, cex = 1.2)
#abline(h = c(90, 215), lty = 2) # Basert på q1 = 90, median = 150 og q3 = 215 an NO3-N fra Kjaglielva, Lomma og Kjørstadelva (2017-2021)
rect(xleft = -1, xright = 100, ybottom = 90, ytop = 215, col = "lightpink", border = NA)
boxplot(NO3.comb ~ year, data = d.ask2, log = "", ylab = "", las = 2, ylim = c(20, 5000), add = T)
points(NO3.comb ~ year, data = d.ask2.mean, col = "blueviolet", pch = 8, cex = 1.5, log = "y")
#text(expression(italic("Referansenivå av NO"[3])), x = 2.2, y = 150, font = )
dev.off()


###################
#### Sløvikelva ###
###################
unique(d$Vannlokalitetsnavn)

# Vanntype kalkrik, klar, lavland

# Antall observasjoner pr. år av TotP
with(filter(d, Vannlokalitetsnavn == "Sløvikelva SLØ1" & complete.cases(P.TOT)), table(year))
with(filter(d, Vannlokalitetsnavn == "Sløvikelva SLØ1" & complete.cases(N.TOT)), table(year))
with(filter(d, Vannlokalitetsnavn == "Sløvikelva SLØ1" & complete.cases(NO3.comb)), table(year))
with(filter(d, Vannlokalitetsnavn == "Sløvikelva SLØ1" & complete.cases(PO4.comb)), table(year))

# Definerer year som faktor med alle år for plott
d.slo <- filter(d, Vannlokalitetsnavn == "Sløvikelva SLØ1")
d.slo$year <- factor(d.slo$year, levels = 2018:2021)

d.slo.mean <- d.slo %>% 
  group_by(year) %>%
  summarize_all(mean, na.rm = T) %>% 
  data.frame()

### TOTP og TOTN ###
# NB: usikker på log eller ikke log på y-aksen

png("totP og totN Sløvikelva.png", res = 1000, height = 7.2, width = 7.5, units = "in")
par(mfrow = c(2,1), mar = c(2,5,2,2), oma = c(4,2,2,2))
# Boxplot: om man ønsker å ta med de gamle dataene (94-96) så bruk d.vig
boxplot(P.TOT ~ year, data = d.slo, log = "", axes = F, border = NA,
        ylab = "", ylim = c(1, 50))
rect(xleft = -1, xright = 50, ybottom = -10, ytop = 15, col = SG, border = NA)
rect(xleft = -1, xright = 50, ybottom = 15, ytop = 25, col = G, border = NA)
rect(xleft = -1, xright = 50, ybottom = 25, ytop = 38, col = M, border = NA)
rect(xleft = -1, xright = 50, ybottom = 38, ytop = 65, col = D, border = NA)
rect(xleft = -1, xright = 50, ybottom = 65, ytop = 200, col = SD, border = NA)
boxplot(P.TOT ~ year, data = d.slo, log = "y", add = T, las = 2, ylim = c(2, 80))
mtext("Total fosfor (µg P/L)", side = 2, line = 3.5, cex = 1.2)

# Add gjennomsnitt som punkt
points(P.TOT ~ year, data = d.slo.mean, pch = 8, col = "blueviolet", cex = 1.5, log = "y")

# TOTN
boxplot(N.TOT ~ year, data = d.slo, log = "", axes = F, ylim  = c(250, 5000), border = "grey31",
        ylab = "")
rect(xleft = -1, xright = 50, ybottom = -10, ytop = 425, col = SG, border = NA)
rect(xleft = -1, xright = 50, ybottom = 425, ytop = 675, col = G, border = NA)
rect(xleft = -1, xright = 50, ybottom = 675, ytop = 950, col = M, border = NA)
rect(xleft = -1, xright = 50, ybottom = 950, ytop = 1425, col = D, border = NA)
rect(xleft = -1, xright = 50, ybottom = 1425, ytop = 15000, col = SD, border = NA)
boxplot(N.TOT ~ year, data = d.slo, log = "y",add = T, las = 2)
points(N.TOT  ~ year, data = d.slo.mean, col = "blueviolet", pch = 8, cex = 1.5)
mtext("Total nitrogen (µg N/L)", side = 2, line = 3.5, cex = 1.2)
dev.off()

### Fosfat og nitrat ###
d.slo2 <- filter(d, Vannlokalitetsnavn == "Sløvikelva SLØ1" & year >= 2014)
d.slo2$year <- factor(d.slo2$year, levels = 2018:2021)

d.slo2.mean <- d.slo2 %>% 
  group_by(year) %>%
  summarize_all(mean, na.rm = T) %>% 
  data.frame()

png("PO4 og NO3 Slovikelva.png", res = 1000, height = 7.2, width = 7.5, units = "in")
par(mfrow = c(2,1), mar = c(2,5,2,2), oma = c(4,2,2,2))
# Fosfat

with(filter(d, Vannlokalitetsnavn == "Sløvikelva SLØ1" & complete.cases(PO4.comb)), table(year))
boxplot(PO4.comb ~ year, data = d.slo2, log = "", ylab = "", las = 2, axes = F, ylim = c(1, 50))
grid()
mtext("Fosfat (µg P/L)", side = 2, line = 3.5, cex = 1.2)
#abline(h = c(1,2), lty = 2) # Q1 og Q3 basert på total fosfat fra Kjaglielva, Lomma og Kjørstadelva (2017-2021) (<1 satt til 1)
rect(xleft = -1, xright = 100, ybottom = 1, ytop = 2, col = "lightpink", border = NA)
boxplot(PO4.comb ~ year, data = d.slo2, log = "", ylab = "", las = 2, add = T)
points(PO4.comb ~ year, data = d.slo2.mean, col = "blueviolet", pch = 8, cex = 1.5, log = "y")


# Nitrat
with(filter(d, Vannlokalitetsnavn == "Sløvikelva SLØ1" & complete.cases(NO3.comb)), table(year))
boxplot(NO3.comb ~ year, data = d.slo2, log = "", ylab = "", las = 2, ylim = c(20, 5000), axes = F)
grid()
mtext("Nitrat + nitritt (µg N/L)", side = 2, line = 3.5, cex = 1.2)
#abline(h = c(90, 215), lty = 2) # Basert på q1 = 90, median = 150 og q3 = 215 an NO3-N fra Kjaglielva, Lomma og Kjørstadelva (2017-2021)
rect(xleft = -1, xright = 100, ybottom = 90, ytop = 215, col = "lightpink", border = NA)
boxplot(NO3.comb ~ year, data = d.slo2, log = "", ylab = "", las = 2, ylim = c(20, 5000), add = T)
points(NO3.comb ~ year, data = d.slo2.mean, col = "blueviolet", pch = 8, cex = 1.5, log = "y")
#text(expression(italic("Referansenivå av NO"[3])), x = 2.2, y = 150, font = )
dev.off()

###################
#### Vangselva  ###
###################
unique(d$Vannlokalitetsnavn)

# Vanntype kalkrik, klar, lavland

# Antall observasjoner pr. år av TotP
with(filter(d, Vannlokalitetsnavn == "VAN 1 Vangselva" & complete.cases(P.TOT)), table(year))
with(filter(d, Vannlokalitetsnavn == "VAN 1 Vangselva" & complete.cases(N.TOT)), table(year))
with(filter(d, Vannlokalitetsnavn == "VAN 1 Vangselva" & complete.cases(NO3.comb)), table(year))
with(filter(d, Vannlokalitetsnavn == "VAN 1 Vangselva" & complete.cases(PO4.comb)), table(year))

# Definerer year som faktor med alle år for plott
d.van <- filter(d, Vannlokalitetsnavn == "VAN 1 Vangselva")
d.van$year <- factor(d.van$year, levels = 2018:2021)

d.van.mean <- d.van %>% 
  group_by(year) %>%
  summarize_all(mean, na.rm = T) %>% 
  data.frame()

### TOTP og TOTN ###
# NB: usikker på log eller ikke log på y-aksen

png("totP og totN Vangselva.png", res = 1000, height = 7.2, width = 7.5, units = "in")
par(mfrow = c(2,1), mar = c(2,5,2,2), oma = c(4,2,2,2))
# Boxplot: om man ønsker å ta med de gamle dataene (94-96) så bruk d.vig
boxplot(P.TOT ~ year, data = d.van, log = "", axes = F, border = NA,
        ylab = "", ylim = c(1, 30)) # en utligge på 60 µg/L ligger utenfor plott
rect(xleft = -1, xright = 50, ybottom = -10, ytop = 15, col = SG, border = NA)
rect(xleft = -1, xright = 50, ybottom = 15, ytop = 25, col = G, border = NA)
rect(xleft = -1, xright = 50, ybottom = 25, ytop = 38, col = M, border = NA)
rect(xleft = -1, xright = 50, ybottom = 38, ytop = 65, col = D, border = NA)
rect(xleft = -1, xright = 50, ybottom = 65, ytop = 200, col = SD, border = NA)
boxplot(P.TOT ~ year, data = d.van, log = "y", add = T, las = 2, ylim = c(2, 80))
mtext("Total fosfor (µg P/L)", side = 2, line = 3.5, cex = 1.2)

# Add gjennomsnitt som punkt
points(P.TOT ~ year, data = d.van.mean, pch = 8, col = "blueviolet", cex = 1.5, log = "y")

# TOTN
boxplot(N.TOT ~ year, data = d.van, log = "", axes = F, ylim  = c(250, 3500), border = "grey31",
        ylab = "")
rect(xleft = -1, xright = 50, ybottom = -10, ytop = 425, col = SG, border = NA)
rect(xleft = -1, xright = 50, ybottom = 425, ytop = 675, col = G, border = NA)
rect(xleft = -1, xright = 50, ybottom = 675, ytop = 950, col = M, border = NA)
rect(xleft = -1, xright = 50, ybottom = 950, ytop = 1425, col = D, border = NA)
rect(xleft = -1, xright = 50, ybottom = 1425, ytop = 15000, col = SD, border = NA)
boxplot(N.TOT ~ year, data = d.van, log = "y",add = T, las = 2)
points(N.TOT  ~ year, data = d.van.mean, col = "blueviolet", pch = 8, cex = 1.5)
mtext("Total nitrogen (µg N/L)", side = 2, line = 3.5, cex = 1.2)
dev.off()

### Fosfat og nitrat ###
d.van2 <- filter(d, Vannlokalitetsnavn == "VAN 1 Vangselva" & year >= 2014)
d.van2$year <- factor(d.van2$year, levels = 2018:2021)

d.van2.mean <- d.van2 %>% 
  group_by(year) %>%
  summarize_all(mean, na.rm = T) %>% 
  data.frame()

png("PO4 og NO3 Vangselva.png", res = 1000, height = 7.2, width = 7.5, units = "in")
par(mfrow = c(2,1), mar = c(2,5,2,2), oma = c(4,2,2,2))
# Fosfat

with(filter(d, Vannlokalitetsnavn == "VAN 1 Vangselva" & complete.cases(PO4.comb)), table(year))
boxplot(PO4.comb ~ year, data = d.van2, log = "", ylab = "", las = 2, axes = F, ylim = c(0, 15))
grid()
mtext("Fosfat (µg P/L)", side = 2, line = 3.5, cex = 1.2)
#abline(h = c(1,2), lty = 2) # Q1 og Q3 basert på total fosfat fra Kjaglielva, Lomma og Kjørstadelva (2017-2021) (<1 satt til 1)
rect(xleft = -1, xright = 100, ybottom = 1, ytop = 2, col = "lightpink", border = NA)
boxplot(PO4.comb ~ year, data = d.van2, log = "", ylab = "", las = 2, add = T)
points(PO4.comb ~ year, data = d.van2.mean, col = "blueviolet", pch = 8, cex = 1.5, log = "y")


# Nitrat
with(filter(d, Vannlokalitetsnavn == "VAN 1 Vangselva" & complete.cases(NO3.comb)), table(year))
boxplot(NO3.comb ~ year, data = d.van2, log = "", ylab = "", las = 2, ylim = c(20, 3000), axes = F)
grid()
mtext("Nitrat + nitritt (µg N/L)", side = 2, line = 3.5, cex = 1.2)
#abline(h = c(90, 215), lty = 2) # Basert på q1 = 90, median = 150 og q3 = 215 an NO3-N fra Kjaglielva, Lomma og Kjørstadelva (2017-2021)
rect(xleft = -1, xright = 100, ybottom = 90, ytop = 215, col = "lightpink", border = NA)
boxplot(NO3.comb ~ year, data = d.van2, log = "", ylab = "", las = 2, ylim = c(20, 5000), add = T)
points(NO3.comb ~ year, data = d.van2.mean, col = "blueviolet", pch = 8, cex = 1.5, log = "y")
#text(expression(italic("Referansenivå av NO"[3])), x = 2.2, y = 150, font = )
dev.off()

#####################
#### Grymyrbekken ###
#####################
unique(d$Vannlokalitetsnavn)

# Vanntype kalkrik, klar, lavland

# Antall observasjoner pr. år av TotP
with(filter(d, Vannlokalitetsnavn == "GO1. Grymyrb. ned" & complete.cases(P.TOT)), table(year))
with(filter(d, Vannlokalitetsnavn == "GO1. Grymyrb. ned" & complete.cases(N.TOT)), table(year))
with(filter(d, Vannlokalitetsnavn == "GO1. Grymyrb. ned" & complete.cases(NO3.comb)), table(year))
with(filter(d, Vannlokalitetsnavn == "GO1. Grymyrb. ned" & complete.cases(PO4.comb)), table(year))

# Definerer year som faktor med alle år for plott
d.gry <- filter(d, Vannlokalitetsnavn == "GO1. Grymyrb. ned")
d.gry$year <- factor(d.gry$year, levels = 2018:2021)

d.gry.mean <- d.gry %>% 
  group_by(year) %>%
  summarize_all(mean, na.rm = T) %>% 
  data.frame()

### TOTP og TOTN ###
# NB: usikker på log eller ikke log på y-aksen

png("totP og totN Grymyrbekken.png", res = 1000, height = 7.2, width = 7.5, units = "in")
par(mfrow = c(2,1), mar = c(2,5,2,2), oma = c(4,2,2,2))
# Boxplot: om man ønsker å ta med de gamle dataene (94-96) så bruk d.vig
boxplot(P.TOT ~ year, data = d.gry, log = "", axes = F, border = NA,
        ylab = "", ylim = c(1, 50)) # en utligge på 60 µg/L ligger utenfor plott
rect(xleft = -1, xright = 50, ybottom = -10, ytop = 15, col = SG, border = NA)
rect(xleft = -1, xright = 50, ybottom = 15, ytop = 25, col = G, border = NA)
rect(xleft = -1, xright = 50, ybottom = 25, ytop = 38, col = M, border = NA)
rect(xleft = -1, xright = 50, ybottom = 38, ytop = 65, col = D, border = NA)
rect(xleft = -1, xright = 50, ybottom = 65, ytop = 200, col = SD, border = NA)
boxplot(P.TOT ~ year, data = d.gry, log = "y", add = T, las = 2, ylim = c(2, 80))
mtext("Total fosfor (µg P/L)", side = 2, line = 3.5, cex = 1.2)

# Add gjennomsnitt som punkt
points(P.TOT ~ year, data = d.gry.mean, pch = 8, col = "blueviolet", cex = 1.5, log = "y")

# TOTN
boxplot(N.TOT ~ year, data = d.gry, log = "", axes = F, ylim  = c(250, 5000), border = "grey31",
        ylab = "")
rect(xleft = -1, xright = 50, ybottom = -10, ytop = 425, col = SG, border = NA)
rect(xleft = -1, xright = 50, ybottom = 425, ytop = 675, col = G, border = NA)
rect(xleft = -1, xright = 50, ybottom = 675, ytop = 950, col = M, border = NA)
rect(xleft = -1, xright = 50, ybottom = 950, ytop = 1425, col = D, border = NA)
rect(xleft = -1, xright = 50, ybottom = 1425, ytop = 15000, col = SD, border = NA)
boxplot(N.TOT ~ year, data = d.gry, log = "y",add = T, las = 2)
points(N.TOT  ~ year, data = d.gry.mean, col = "blueviolet", pch = 8, cex = 1.5)
mtext("Total nitrogen (µg N/L)", side = 2, line = 3.5, cex = 1.2)
dev.off()

### Fosfat og nitrat ###
d.gry2 <- filter(d, Vannlokalitetsnavn == "GO1. Grymyrb. ned" & year >= 2014)
d.gry2$year <- factor(d.gry2$year, levels = 2018:2021)

d.gry2.mean <- d.gry2 %>% 
  group_by(year) %>%
  summarize_all(mean, na.rm = T) %>% 
  data.frame()

png("PO4 og NO3 Grymyrbekken.png", res = 1000, height = 7.2, width = 7.5, units = "in")
par(mfrow = c(2,1), mar = c(2,5,2,2), oma = c(4,2,2,2))
# Fosfat

with(filter(d, Vannlokalitetsnavn == "GO1. Grymyrb. ned" & complete.cases(PO4.comb)), table(year))
boxplot(PO4.comb ~ year, data = d.gry2, log = "", ylab = "", las = 2, axes = F, ylim = c(0, 50))
grid()
mtext("Fosfat (µg P/L)", side = 2, line = 3.5, cex = 1.2)
#abline(h = c(1,2), lty = 2) # Q1 og Q3 basert på total fosfat fra Kjaglielva, Lomma og Kjørstadelva (2017-2021) (<1 satt til 1)
rect(xleft = -1, xright = 100, ybottom = 1, ytop = 2, col = "lightpink", border = NA)
boxplot(PO4.comb ~ year, data = d.gry2, log = "", ylab = "", las = 2, add = T)
points(PO4.comb ~ year, data = d.gry2.mean, col = "blueviolet", pch = 8, cex = 1.5, log = "y")


# Nitrat
with(filter(d, Vannlokalitetsnavn == "GO1. Grymyrb. ned" & complete.cases(NO3.comb)), table(year))
boxplot(NO3.comb ~ year, data = d.gry2, log = "", ylab = "", las = 2, ylim = c(20, 4200), axes = F)
grid()
mtext("Nitrat + nitritt (µg N/L)", side = 2, line = 3.5, cex = 1.2)
#abline(h = c(90, 215), lty = 2) # Basert på q1 = 90, median = 150 og q3 = 215 an NO3-N fra Kjaglielva, Lomma og Kjørstadelva (2017-2021)
rect(xleft = -1, xright = 100, ybottom = 90, ytop = 215, col = "lightpink", border = NA)
boxplot(NO3.comb ~ year, data = d.gry2, log = "", ylab = "", las = 2, ylim = c(20, 5000), add = T)
points(NO3.comb ~ year, data = d.gry2.mean, col = "blueviolet", pch = 8, cex = 1.5, log = "y")
#text(expression(italic("Referansenivå av NO"[3])), x = 2.2, y = 150, font = )
dev.off()

