library(tidyverse)
library(readxl)

damme <- read_excel("damme.xlsx")

zendingen <- damme %>% select(Verlaaddatum, LOTNR, Producent, Merk, Colli) %>%
  na.omit()
zendingen$Verlaaddatum <- as.Date(zendingen$Verlaaddatum)
write_excel_csv(zendingen, "zendingen.txt")
