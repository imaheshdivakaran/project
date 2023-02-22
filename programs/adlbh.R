# ADLBH

###############################
# developers : Arya Vijayan   #
# date: 22Feb2023             #
# modification History: Nil   #
# ADLBH program               #
###############################

#Setups
library(admiral)
library(dplyr)
library(metacore)
library(metatools)
library(xportr)

# placeholder for origin=predecessor, use metatool::build_from_derived()
metacore <- spec_to_metacore("metadata/specs.xlsx", where_sep_sheet = FALSE)

# Iterate spec for ADVS
adlbh_spec <- metacore %>%
  select_dataset("ADLBH")

# Calling datasets
lb <- haven::read_xpt(file.path("sdtm", "lb.xpt"))
adsl <- haven::read_xpt(file.path("adam", "adsl.xpt"))

# convert blanks to NA
lb <- convert_blanks_to_na(lb)

#ADLBH
adlbh <- derive_vars_merged(
  dataset = lb,
  dataset_add = adsl,
  by_vars = vars(STUDYID, USUBJID)) %>%
  # Derive Treatment variables
  mutate(TRTA = TRT01A,
         TRTP = TRT01P,
         TRTAN = TRT01AN,
         TRTPN = TRT01PN,
         ADY = LBDY,
         AVAL = LBSTRESN,
         A1HI =LBSTNRHI,
         A1LO = VISITNUM,
         ABLFL = LBBLFL,
         ANRIND = VISITNUM,
         BNRIND = VISITNUM,
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
         PARAMN = as.numeric(case_when(LBTEST == "Anisocytes"~"1",
                                       LBTEST == "Basophils (GI/L)	"~"2",
                                       LBTEST == "Eosinophils (GI/L)"~"3",
                                       LBTEST == "Hematocrit"~"4",
                                       LBTEST == "Hemoglobin (mmol/L)"~"5",
                                       LBTEST == "Lymphocytes (GI/L)"~"6",
                                       LBTEST == "Macrocytes"~"7",
                                       LBTEST == "Ery. Mean Corpuscular Hemoglobin (fmol(Fe))"~"8",
                                       LBTEST == "Ery. Mean Corpuscular HGB Concentration (mmol/L)"~"9",
                                       LBTEST == "Ery. Mean Corpuscular Volume (fL)"~"10",
                                       LBTEST == "Microcytes"~"11",
                                       LBTEST == "Monocytes (GI/L)"~"12",
                                       LBTEST == "Platelet (GI/L)"~"13",
                                       LBTEST == "Poikilocytes"~"14",
                                       LBTEST == "Polychromasia"~"15",
                                       LBTEST == "Erythrocytes (TI/L)"~"16",
                                       LBTEST == "Leukocytes (GI/L)"~"17")) ,
         PARAM = LBTEST,
         PARAMCD = LBTESTCD) %>%
  # ADT
  derive_vars_dt(new_vars_prefix = "A", dtc = LBDTC) %>%
  #R2A1HI and R2A1LO
  derive_var_analysis_ratio(
    numer_var = AVAL,
    denom_var = A1LO,
    new_var = R2A1LO) %>%
  derive_var_analysis_ratio(
    numer_var = AVAL,
    denom_var = A1HI,
    new_var = R2A1HI) %>%
  derive_var_analysis_ratio(
    numer_var = AVAL,
    denom_var = A1LO,
    new_var = a) %>%
  derive_var_analysis_ratio(
    numer_var = AVAL,
    denom_var = A1HI,
    new_var = b) %>%
  #BR2A1HI and BR2A1LO
  BR2A1LO = ifelse(ABLFL == "Y",a,NA) %>%
  BR2A1HI = ifelse(ABLFL == "Y",b,NA) %>%
  # BASE
  derive_var_base(
    by_vars = vars(STUDYID, USUBJID, PARAMCD),
    source_var = AVAL,
    new_var = BASE) %>%
  #CHG
  derive_var_chg() %>%
  #PCHG
  derive_var_pchg() %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(STUDYID, USUBJID, PARAMCD, AVISIT),
      order = vars(ADT, AVAL),
      new_var = ANL01FL,
      mode = "last"
    ),
    filter = !is.na(AVISITN)) %>%
  select(STUDYID,SUBJID,USUBJID,TRTP,TRTPN,TRTA,TRTAN,TRTSDT,TRTEDT,AGE,AGEGR1,AGEGR1N,RACE,
         RACEN,SEX,COMP24FL,DSRAEFL,SAFFL,AVISIT,AVISITN,ADY,ADT,VISIT,VISITNUM,
         PARAM,PARAMCD,PARAMN,
         #PARCAT1,
         AVAL,BASE,CHG,A1LO,A1HI,
         #R2A1LO,R2A1HI,BR2A1LO,BR2A1HI,
         ANL01FL,
         #ALBTRVAL,
         ANRIND,BNRIND,
         ABLFL,
         #AENTMTFL,
         LBSEQ,LBNRIND,LBSTRESN)

A  <- lb %>% select(LBTEST) %>% filter(LBTEST == "Anisocytes change from previous visit, relative to normal range")

B  <- adlbh %>% select(ABLFL) %>% filter(ABLFL == "Y")

adlbh %>%
  drop_unspec_vars(adlbh_spec) %>% # only keep vars from define
  order_cols(adlbh_spec) %>% # order columns based on define
  set_variable_labels(adlbh_spec) %>% # apply variable labels based on define
  #xportr_type(advs_spec, "ADLBH") %>%
  #xportr_length(advs_spec, "ADLBH") %>%
  xportr_format(adlbh_spec$var_spec %>%
                  mutate_at(c("format"), ~ replace_na(., "")), "ADLBH") %>%
  xportr_write("adam/adlbh.xpt",
               label = "Analysis Dataset Lab Hematology"
  )
