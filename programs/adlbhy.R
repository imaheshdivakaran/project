# Name: ADLBHY
#
# Label: Analysis Dataset Lab Hy's Law
#
# Author: Bimal Thomas
#
# Input: adsl, ae

# Adding required libraries
library(haven)
library(admiral)
library(dplyr)
library(tidyr)
library(stringr)
library(xportr)

# Loading Datsets
lb <- haven::read_xpt(file.path("sdtm", "lb.xpt"))
adsl <- haven::read_xpt(file.path("adam", "adsl.xpt"))

# placeholder for origin=predecessor, use metatool::build_from_derived()
metacore <- spec_to_metacore("metadata/specs.xlsx",where_sep_sheet = FALSE)

# Iterate spec for ADAE
adae_spec <- metacore %>%
  select_dataset("ADLBHY")

# Convert blanks to NA
lb <- convert_blanks_to_na(lb)

unique(lb$LBTESTCD)

"ALT"     "AST"     "BILI"    "BILIHY"  "TRANSHY" "HYLAW"


lbtest <- lb %>%
  filter(LBTESTCD==c("ALT","AST","BILI","BILIHY","TRANSHY","HYLAW" ))
