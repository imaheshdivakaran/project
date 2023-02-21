# Name: ADADAS
#
# Label: ADAS-Cog Analysis
#
# Author: Bimal Thomas
#
# Input: adsl, qs
#
# Date: 21-02-2023

# Adding required libraries
library(dplyr)
library(tidyr)
library(admiral)
library(metacore)
library(metatools)
library(stringr)
library(xportr)

# Loading Datsets
dm <- haven::read_xpt(file.path("sdtm", "dm.xpt"))
qs <- haven::read_xpt(file.path("sdtm", "qs.xpt"))
adsl <- haven::read_xpt(file.path("adam", "adsl.xpt"))

# placeholder for origin=predecessor, use metatool::build_from_derived()
metacore <- spec_to_metacore("metadata/specs.xlsx",where_sep_sheet = FALSE)

# Iterate spec for ADAE
adadas_spec <- metacore %>%
  select_dataset("ADADAS")

print(adadas_spec)

# Convert blanks to NA
qs <- convert_blanks_to_na(qs)
dm <- convert_blanks_to_na(dm)


# Merge with ADSL and creating variables
adad_1 <-derive_vars_merged(dataset = qs,
                            dataset_add = adsl,
                            by_vars = vars(STUDYID, USUBJID)) %>%
  filter(grepl('ACITM', QSTESTCD,ignore.case = TRUE)|QSTESTCD=="ACTOT") %>%
  # ADT
  derive_vars_dt(new_vars_prefix = "A",
                 dtc = QSDTC) %>%
  # Creating Day variables
  derive_vars_dy(reference_date = TRTSDT,
                 source_vars = vars(ADT)) %>%
  mutate(PARAMCD=trimws(QSTESTCD),
         PARAM=trimws(QSTEST) %>% str_to_title(),
         PARAMN=case_match(PARAMCD,"ACITM01"~1,"ACITM02"~2,"ACITM03"~3,"ACITM04"~4,
                           "ACITM05"~5,"ACITM06"~6,"ACITM07"~7,"ACITM08"~8,"ACITM09"~9,
                           "ACITM10"~10,"ACITM11"~11,"ACITM12"~12,"ACITM13"~13,"ACITM14"~14,
                           "ACTOT"~15,.default = NA),
         AVISIT = case_when(ADY <= 1 ~ "Baseline",ADY >= 2 & ADY <= 84 ~ "Week 8",
                            ADY >= 85 & ADY <= 140 ~ "Week 16",ADY > 140 ~ "Week 24",
                            TRUE ~ NA_character_),
         AVISITN=case_match(AVISIT,"Baseline"~0,"Week 8"~8,"Week 16"~16,"Week 24"~24,
                            .default = NA),
         AVAL=QSSTRESN,
         ABLFL=QSBLFL) %>%
  derive_var_base(by_vars = vars(STUDYID, USUBJID, PARAMCD),
                  source_var = AVAL,
                  new_var = BASE) %>%
  derive_var_chg() %>%
  derive_var_pchg()

# creating all dataset with paramcd "ACTOT" with all combination of AVISIT
adadas_expected_obsv <- tibble::tribble(
  ~PARAMCD, ~AVISITN, ~AVISIT,
  "ACTOT",0, "Baseline",
  "ACTOT",8,"Week 8",
  "ACTOT",16,"Week 16",
  "ACTOT",24,"Week 24")

# Deriving LOCF records
adad_2 <- adad_1 %>%
  derive_locf_records(dataset_expected_obs = adadas_expected_obsv,
                      by_vars = vars(STUDYID, USUBJID, PARAMCD),
                      order = vars(AVISITN, AVISIT))

## derive AWRANGE/AWTARGET/AWTDIFF/AWLO/AWHI/AWU
aw_vars <- tribble(
  ~AVISIT, ~AWRANGE, ~AWTARGET, ~AWLO, ~AWHI,
  "Baseline", "<=1", 1, NA_integer_, 1,
  "Week 8", "2-84", 56, 2, 84,
  "Week 16", "85-140", 112, 85, 140,
  "Week 24", ">140", 168, 141, NA_integer_
)

# merging
adad_3 <- derive_vars_merged(dataset=adad_2,
                             dataset_add = aw_vars,
                             by_vars = vars(AVISIT)) %>%
  mutate(AWTDIFF = abs(AWTARGET - ADY),AWU = "DAYS")

## ANL01FL
adad_4 <- adad_3 %>%
  mutate(diff = AWTARGET - ADY) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID, PARAMCD, AVISIT),
      order = vars(AWTDIFF, diff),
      new_var = ANL01FL,
      mode = "first"
    ),
    filter = !is.na(AVISIT)
  )

# Adding labels and selecting required variables from metadata
adadas<-adad_4 %>%
  # only keep vars from define
  drop_unspec_vars(adadas_spec) %>%
  # order columns based on define
  order_cols(adadas_spec) %>%
  # apply variable labels based on define
  set_variable_labels(adadas_spec) %>%
  # Creating .XPT and adding dataset Label
  xportr_write("adam/adadas.xpt",label = "ADAS-Cog Analysis")



