# ADVS

###############################
# developers : Arya Vijayan   #
# date: 09Feb2023             #
# modification History: Nil   #
# ADVS program                #
###############################

#Setups
library(admiral)
library(dplyr)
library(metacore)
library(metatools)
library(xportr)

# Calling datasets
vs <- haven::read_xpt(file.path("sdtm", "vs.xpt"))
adsl <- haven::read_xpt(file.path("adam", "adsl.xpt"))

# convert blanks to NA
vs <- convert_blanks_to_na(vs)

# placeholder for origin=predecessor, use metatool::build_from_derived()
metacore <- spec_to_metacore("metadata/specs.xlsx", where_sep_sheet = FALSE)

# Iterate spec for ADVS
advs_spec <- metacore %>%
  select_dataset("ADVS")

# # Pull together all the predecessor variables
# advs_pred <- build_from_derived(advs_spec,
#                                   vs_list = list("ADSL" = adsl, "VS" = vs))

# ADVS
advs <- derive_vars_merged(
  dataset = vs,
  dataset_add = adsl,
  by_vars = vars(STUDYID, USUBJID)) %>%
  # Derive Treatment variables
  mutate(TRTA = TRT01A,
         TRTP = TRT01P,
         TRTAN = TRT01AN,
         TRTPN = TRT01PN,
         ADY = VSDY,
         ATPT = VSTPT,
         AVAL = VSSTRESN,
         # AVISIT and AVISITN
         AVISIT = case_when(
           str_detect(VISIT, "SCREEN") ~ NA_character_,
           str_detect(VISIT, "UNSCHED") ~ NA_character_,
           #str_detect(VISIT, "RETRIEVAL") ~ NA_character_,
           str_detect(VISIT, "AMBUL") ~ NA_character_,
           !is.na(VISIT) ~ str_to_title(VISIT)),
         AVISITN = as.numeric(case_when(
           VISIT == "BASELINE" ~ "0",
           str_detect(VISIT, "WEEK") ~ str_trim(str_replace(VISIT, "WEEK", "")))),
         PARAMN = as.numeric(case_when(VSTEST == "Systolic Blood Pressure" ~ "1",
                                       VSTEST == "Diastolic Blood Pressure" ~ "2",
                                       VSTEST == "Pulse Rate" ~ "3",
                                       VSTEST == "Weight" ~ "4",
                                       VSTEST == "Height" ~ "5",
                                       VSTEST == "Temperature" ~ "6")) ,
         ATPTN = VSTPTNUM,
         PARAM = VSTEST,
         PARAMCD = VSTESTCD,
         ABLFL = VSBLFL,
         AVAL = VSSTRESN,
         BASETYPE = VSTPT) %>%
  # ADT
  derive_vars_dt(new_vars_prefix = "A", dtc = VSDTC) %>%
  # BASE
  derive_var_base(
    by_vars = vars(STUDYID, USUBJID, PARAMCD, BASETYPE),
    source_var = AVAL,
    new_var = BASE) %>%
  derive_var_chg() %>%
  derive_var_pchg() %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(STUDYID, USUBJID, BASETYPE, PARAMCD, AVISIT),
      order = vars(ADT, ATPTN, AVAL),
      new_var = ANL01FL,
      mode = "last"
    ),
    filter = !is.na(AVISITN)) %>%
  select(STUDYID,SITEID,USUBJID,AGE,AGEGR1,AGEGR1N,RACE,RACEN,SEX,SAFFL,TRTSDT,
         TRTEDT,TRTP,TRTPN,TRTA,TRTAN,PARAMCD,PARAM,PARAMN,ADT,ADY,ATPTN,ATPT,
         AVISIT,AVISITN,AVAL,BASE,BASETYPE,CHG,PCHG,VISITNUM,VISIT,VSSEQ,ANL01FL,ABLFL)

A  <- advs %>% select(ANL01FL, AVISIT) %>%filter(is.na(AVISIT))

advs %>%
  drop_unspec_vars(advs_spec) %>% # only keep vars from define
  order_cols(advs_spec) %>% # order columns based on define
  set_variable_labels(advs_spec) %>% # apply variable labels based on define
  #xportr_type(advs_spec, "ADVS") %>%
  #xportr_length(advs_spec, "ADVS") %>%
  # unresolved issue in xportr_length due to:
  # https://github.com/tidyverse/haven/issues/699
  # no difference found by diffdf after commenting out xportr_length()
  xportr_format(advs_spec$var_spec %>%
                  mutate_at(c("format"), ~ replace_na(., "")), "ADVS") %>%
  xportr_write("adam/advs.xpt",
               label = "Vital Signs Analysis Dataset"
  )


