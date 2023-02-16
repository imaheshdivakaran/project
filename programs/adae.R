# Name: ADAE
#
# Label: Adverse Events Analysis Dataset
#
# Input: adsl, ae

# Adding required libraries
library(haven)
library(admiral)
library(dplyr)
library(tidyr)
library(stringr)
library(xportr)

ae <- haven::read_xpt(file.path("sdtm", "ae.xpt"))
adsl <- haven::read_xpt(file.path("adam", "adsl.xpt"))

# placeholder for origin=predecessor, use metatool::build_from_derived()
metacore <- spec_to_metacore("metadata/specs.xlsx", where_sep_sheet = FALSE)

# Iterate spec for ADVS
adae_spec <- metacore %>%
  select_dataset("ADAE")

# Convert blanks to NA
ae <- convert_blanks_to_na(ae)

# Merge with ADSL and creating variables

adae_1 <-derive_vars_merged(dataset = ae,
                          dataset_add = adsl,
                          by_vars = vars(STUDYID, USUBJID)) %>%
  filter(ARM!="Screen Failure") %>%
# Deriving AESTDTC
  derive_vars_dt(new_vars_prefix = "AEN",
                 dtc = AEENDTC) %>%
# Deriving AEENDTC
  derive_vars_dt(dtc = AESTDTC,
                 new_vars_prefix = "AST",
                 highest_imputation = "D",
                 min_dates = vars(TRTSDT)) %>%
# Creating Day variables
  derive_vars_dy(reference_date = TRTSDT,
                 source_vars = vars(ASTDT, AENDT)) %>%
  mutate(AESTDT=as.Date(AESTDTC),
         AEENDT=as.Date(AEENDTC)) %>%
# Deriving Duration variable
  derive_vars_duration(new_var = ADURN,
                       new_var_unit = ADURU,
                       start_date = AESTDT,
                       end_date = AEENDT,
                       out_unit = "days") %>%
# Deriving Treatment variables
  mutate(TRTA=TRT01A,TRTAN=TRT01AN,
         # RACEN=case_when(RACE=="AMERICAN INDIAN OR ALASKA NATIVE"~1,
         #                 RACE=="ASIAN"~2,
         #                 RACE=="BLACK OR AFRICAN AMERICAN"~3,
         #                 RACE=="NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER"~5,
         #                 RACE=="WHITE"~6,
         #                 TRUE~NA),
         ADURU=ifelse(ADURU=="DAYS","DAY","")) %>%
# Deriving Treatment Emergent Flag
  derive_var_trtemfl(
    new_var = TRTEMFL,
    start_date = ASTDT,
    end_date = AENDT,
    trt_start_date = TRTSDT,
    trt_end_date = TRTEDT) %>%
  mutate(TRTEMFL=ifelse(is.na(ASTDT),NA,TRTEMFL)) %>%
# Derive 1st Occurrence of Any AE Flag
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID),
      order = vars(USUBJID, ASTDT, AESEQ),
      new_var = AOCCFL,
      mode = "first"),
    filter = TRTEMFL == "Y") %>%
# Derive 1st Occurrence of SOC Flag
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID,AESOC),
      order = vars(USUBJID,AESOC, ASTDT, AESEQ),
      new_var = AOCCSFL,
      mode = "first"),
    filter = TRTEMFL == "Y") %>%
# Derive 1st Occurrence of Preferred Term Flag
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID,AEBODSYS,AEDECOD),
      order = vars(USUBJID,AEBODSYS,AEDECOD, ASTDT, AESEQ),
      new_var = AOCCPFL,
      mode = "first"),
    filter = TRTEMFL == "Y") %>%
# Derive 1st Occurrence 02 Flag for Serious
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID),
      order = vars(USUBJID,desc(AESEV), ASTDT, AESEQ),
      new_var = AOCC02FL,
      mode = "first"),
    filter = TRTEMFL == "Y") %>%

# Derive 1st Occurrence 03 Flag for Serious SOC
#   restrict_derivation(
#     derivation = derive_var_extreme_flag,
#     args = params(
#       by_vars = vars(USUBJID,AEBODSYS),
#       order = vars(USUBJID,AEBODSYS,desc(AESEV), ASTDT, AESEQ),
#       new_var = AOCC03FL,
#       mode = "first"),
#     filter = TRTEMFL == "Y") %>%
# # Derive 1st Occurrence 04 Flag for Serious PT
#   restrict_derivation(
#     derivation = derive_var_extreme_flag,
#     args = params(
#       by_vars = vars(USUBJID,AEBODSYS,AEDECOD),
#       order = vars(USUBJID,AEBODSYS,AEDECOD,desc(AESEV), ASTDT, AESEQ),
#       new_var = AOCC04FL,
#       mode = "first"),
#     filter = TRTEMFL == "Y") %>%
# mutate(AOCC02FL="",
#        AOCC03FL="",
#        AOCC04FL="") %>%
# Deriving CQ01NAM
  mutate(CQ01NAM=ifelse((str_detect(AEDECOD,'APPLICATION')|
                         str_detect(AEDECOD,'DERMATITIS')|
                         str_detect(AEDECOD,'ERYTHEMA')|
                         str_detect(AEDECOD,'BLISTER')|
                         str_detect(AEBODSYS,"SKIN AND SUBCUTANEOUS TISSUE DISORDERS"))&!(
                         str_detect(AEDECOD,'COLD SWEAT')|
                         str_detect(AEDECOD,'HYPERHIDROSIS')|
                         str_detect(AEDECOD,'ALOPECIA')),"DERMATOLOGIC EVENTS",NA)) %>%
# Derive 1st Occurrence 04 Flag for CQ01
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID,CQ01NAM),
      order = vars(USUBJID,CQ01NAM,ASTDT, AESEQ),
      new_var = AOCC01FL,
      mode = "first"),
    filter = !is.na(CQ01NAM)&TRTEMFL == "Y") %>%
  arrange(USUBJID, AETERM, ASTDT, AESEQ)

# Adding labels and selecting required variables from metadata
adae<-adae_1 %>%
  drop_unspec_vars(adae_spec) %>% # only keep vars from define
  order_cols(adae_spec) %>% # order columns based on define
  set_variable_labels(adae_spec) %>% # apply variable labels based on define
  xportr_format(adae_spec$var_spec %>%
                  mutate_at(c("format"), ~ replace_na(., "")), "ADAE") %>%
  xportr_write("adam/adae.xpt",
               label = "Adverse Events Analysis Dataset"
  )

