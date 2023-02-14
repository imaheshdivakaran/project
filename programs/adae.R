# Name: ADAE
#
# Label: Adverse Events Analysis Dataset
#
# Input: adsl, ae

# Adding required libraries
library(haven)
library(admiral)
library(admiral.test)
library(dplyr)
library(tidyr)
library(stringr)
library(xportr)
library(readxl)
library(fmtr)

formats
armn <- value(condition(x=="Placebo",0),
                 condition(x=="Xanomeline High Dose",81),
                 condition(x=="Xanomeline Low Dose",54))

racen <- value(condition(x=="AMERICAN INDIAN OR ALASKA NATIVE",1),
               condition(x=="ASIAN",2),
               condition(x=="BLACK OR AFRICAN AMERICAN",3),
               condition(x=="NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",5),
               condition(x=="WHITE",6))

agen <- value(condition(x=="18-64",2),
              condition(x==">=65",3),
              condition(x=="<18",1))

# Loading requred datasets
data("admiral_ae")
data("admiral_adsl")

adsl <- admiral_adsl
ae <- admiral_ae

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
                 highest_imputation = "M",
                 min_dates = vars(TRTSDT)) %>%
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
         TRTAN=fapply(TRTA,armn),
         RACEN=fapply(RACE,racen),
         AGEGR1N=fapply(AGEGR1,agen)) %>%
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
    filter = TRTEMFL == "Y") %>%
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

# Reading Spec for ADAE and extract variables and their corresponding labels
spec <- read_excel(file.path("./metadata","specs.xlsx"),"Variables") %>%
  filter(Dataset == "ADAE")

# Selecting required variables and arranging
adae <- adae_1 %>%
  select(spec$Variable) %>%
  arrange(USUBJID, AETERM, ASTDT, AESEQ)

# Applying labels
Labels <- spec[match(names(adae), spec$Variable),]$Label

# Applying labels
attr(adae, "variable.labels") <- Labels

# Converting to XPT
xportr_write(adae, "./adam/adae.xpt")

# dir <- tempdir() # Change to whichever directory you want to save the dataset in
# saveRDS(adae, file = file.path("./adam", "adae.rds"), compress = "bzip2")
