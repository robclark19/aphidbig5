# big five supplemental analyses

# Supp tables
choice.pcr <- read.csv("../Data/test choice pcr.csv", header = TRUE)
repro.pcr <-read.csv("../Data/test reproduction pcr.csv", header=TRUE)
bflc <- read.csv("../Data/big five Logan check.csv", header = TRUE) %>%
  mutate(
    Genotype <- as.factor(Genotype)
  ) %>%
  rename(
    "PEMV (PCR)" = PEMV.PCR
  )