f <- dir()
f

# Les inn rådatafiler for innsjøene fra Vannmiljø og bind dem sammen til en fil
data = NULL

for(i in 1:length(f)) {
d.i <- read.xlsx(f[i], detectDates = T)
  d.i2 <- d.i %>% select(Vannlokalitetskode, Vannlokalitetsnavn, ParameterID, Parameternavn,
                         Prøvetakingstidspunkt, Registreringsverdi, Enhetsnavn)
  data <- rbind(data, d.i2)
}
data
str(data)
head(data)

# Filtrer ut aktuelle parametere
data.frame(unique(data$Parameternavn), unique(data$ParameterID))

d <- filter(data, ParameterID %in% c("P-TOT", "N-TOT", "N-NH4", "N-NO3", "N-SNOX", "FARGE", "CA", "PPNEQR_E", "KLFA", "PPTI", "PPBIOMTOVO"))
head(d)
names(d)[6] <- "verdi"


boxplot(verdi ~ Vannlokalitetsnavn, data = filter(d, ParameterID == "P-TOT"), horizontal = T, log = "x",
        las = 2, cex.axis = 0.5)
