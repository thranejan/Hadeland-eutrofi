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
#####################################

# Les kompilert rådatafil
d <- read.xlsx("raw data fra nedre stasjon i bekkene.xlsx", detectDates = T)
head(d)

# Hent biologisk data
# MN: LBNEQR_E = LBNEQR_G (nEQR for ASPT)
b <- d %>%
  select(Vannlokalitetsnavn, dato, year, mnd, ASPT, LBNEQR_E,  BENEQR_E) %>%
  filter(year >= 2014)
head(b)

b.comp <- b[apply(is.na(b[ , c("ASPT", "LBNEQR_E", "BENEQR_E")]),1, sum) != 3, ]
b.comp

# Ser at det mangler mye data på PIT, men de ligger her:
# https://vannomrade-randsfjorden.no/wp-content/uploads/Tilstandsrapport-2020-Randsfjorden-med-tillopselver.pdf