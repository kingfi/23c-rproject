by_country <- use("./R/by_country.R")
import(dplyr)

# If this were a real package, we'd want dependency injection here
load("./data/Global Party Survey by Expert 29 Dec 2019.RData")

## ----------------------------------------------------------------
## Accessors by Region (World Bank, 9 cat.)
## ----------------------------------------------------------------

# ---- Region IDs ----
# 1 E. Europe & C. Asia
# 2 Latin Am. & Carib
# 3 MENA
# 4 Sub-Saharan Africa
# 5 W. Europe, N. America, Australia/NZ
# 6 Asia-Pacific (not Aus/NZ)

# ------------------------------

export("iso_by_region")
iso_by_region <- function(region_id) {
  responses <- table %>% filter(table$Region == region_id)
  unique(responses$ISO)
}

# ------------------------------

export("mean_responses_for_all_parties")
mean_responses_for_all_parties <- function(region_id) {
  df <- data.frame()
  isos <- iso_by_region(region_id)

  for (iso in isos) {
    df <- rbind(df, by_country$mean_responses_for_parties(iso))
  }

  df
}

# ------------------------------
