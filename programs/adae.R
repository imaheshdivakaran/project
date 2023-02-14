# Name: ADAE
#
# Label: Adverse Events Analysis Dataset
#
# Input: adsl, ae

library(haven)
library(admiral)
library(dplyr)
library(tidyr)
library(stringr)
library(metacore)
library(metatools)
library(xportr)
library(readxl)


# Loading requred datasets

ae <- read_xpt("sdtm/ae.xpt")
suppae <- read_xpt("sdtm/ae.xpt")
adsl <- read_xpt("adam/adsl.xpt")

# Convert blanks to NA

ae <- convert_blanks_to_na(ae)

# Merge with ADSL and creating variables

adae_1 <-derive_vars_merged(dataset = ae,
                          dataset_add = adsl,
                          by_vars = vars(STUDYID, USUBJID)) %>%
# Deriving AESTDTC
  derive_vars_dt(new_vars_prefix = "AEN",
                 dtc = AEENDTC) %>%
# Deriving AEENDTC
  derive_vars_dt(dtc = AESTDTC,
                 new_vars_prefix = "AST",
                 highest_imputation = "M",
                 min_dates = vars(TRTSDT)
  ) %>%
# Creating Day variables
  derive_vars_dy(reference_date = TRTSDT,
                 source_vars = vars(ASTDT, AENDT)) %>%
# Deriving Duration variable
  derive_vars_duration(new_var = ADURN,
                       new_var_unit = ADURU,
                       start_date = ASTDT,
                       end_date = AENDT,
                       out_unit = "days") %>%
# Deriving Treatment variables
  mutate(TRTA=TRT01A,
         TRTP=TRT01P,
         TRTAN=TRT01AN,
         TRTPN=TRT01PN) %>%
# Deriving Treatment Emergent Flag
  derive_var_trtemfl(
    new_var = TRTEMFL,
    start_date = ASTDT,
    end_date = AENDT,
    trt_start_date = TRTSDT,
    trt_end_date = TRTEDT,
    end_window = NULL) %>%
# Derive 1st Occurrence of Any AE Flag
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID),
      order = vars(USUBJID, ASTDT, AESEQ),
      new_var = AOCCFL,
      mode = "first"),
    filter = !is.na(AETERM)) %>%
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
    filter = !is.na(AETERM)) %>%
# Derive 1st Occurrence 03 Flag for Serious SOC
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID,AEBODSYS),
      order = vars(USUBJID,AEBODSYS,desc(AESEV), ASTDT, AESEQ),
      new_var = AOCC03FL,
      mode = "first"),
    filter = TRTEMFL == "Y") %>%
# Derive 1st Occurrence 04 Flag for Serious PT
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID,AEBODSYS,AEDECOD),
      order = vars(USUBJID,AEBODSYS,AEDECOD,desc(AESEV), ASTDT, AESEQ),
      new_var = AOCC04FL,
      mode = "first"),
    filter = TRTEMFL == "Y") %>%
# Deriving CQ01NAM
  mutate(CQ01NAM=ifelse((str_detect(AEDECOD,'APPLICATION')|
                         str_detect(AEDECOD,'DERMATITIS')|
                         str_detect(AEDECOD,'ERYTHEMA')|
                         str_detect(AEDECOD,'BLISTER')|
                         str_detect(AEBODSYS,"SKIN AND SUBC UTANEOUS TISSUE DISORDERS"))&!(
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
    filter = !is.na(CQ01NAM))

#filtering variable tab of adae of spec
spec <- read_excel(file.path("./metadata","specs.xlsx"),"Variables") %>%
  filter(Dataset == "ADAE")

#===============label for pc============
adae <- adae_1 %>%
  select(spec$Variable) %>%
  arrange(USUBJID, AETERM, ASTDT, AESEQ)

#applying labels
Labels <- spec[match(names(adae), spec$Variable),]$Label

attr(adae, "variable.labels") <- Labels

xportr_write(adae, "./adam/adae.xpt")
