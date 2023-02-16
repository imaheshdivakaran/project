###########################################################################
#' developers           :Mahesh Divakaran/
#' date                 : 10FEB2023
#' modification History :
#' program              : ADLBC
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

# Iterate spec for ADVS
adlbc_spec <- metacore %>%
  select_dataset("ADLBC")

lb <- haven::read_xpt(file.path("sdtm", "lb.xpt"))
adsl <- haven::read_xpt(file.path("adam", "adsl.xpt"))

# Convert blanks to NA
lb <- convert_blanks_to_na(lb)

# Merge with ADSL and creating variables

adlb <-derive_vars_merged(dataset = lb,
                            dataset_add = adsl,
                            by_vars = vars(STUDYID, USUBJID))

