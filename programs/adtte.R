###########################################################################
#' developers           :Mahesh Divakaran/
#' date                 : 20FEB2023
#' modification History :
#' program              : ADTTE
###########################################################################

## setup
library(dplyr)
library(tidyr)
library(admiral)
library(metacore)
library(metatools)
library(stringr)
library(xportr)

# placeholder for origin=predecessor, use metatool::build_from_derived()
metacore <- spec_to_metacore("metadata/specs.xlsx", where_sep_sheet = FALSE)

# Iterate spec for ADLBC
adtte_spec <- metacore %>%
  select_dataset("ADTTE")


adsl <- haven::read_xpt(file.path("adam", "adsl.xpt"))
ae <- haven::read_xpt(file.path("sdtm", "ae.xpt"))

# Convert blanks to NA
adae <- convert_blanks_to_na(adae)


