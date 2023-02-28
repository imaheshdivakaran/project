###########################################################################
#' developers : Bimal Thomas
#' date: 22FEB2023
#' modification History:
#' program ADSL
###########################################################################

# Adding required libraries
library(dplyr)
library(tidyr)
library(admiral)
library(metacore)
library(metatools)
library(stringr)
library(xportr)

dm <- read_xpt("sdtm/dm.xpt")
ds <- read_xpt("sdtm/ds.xpt")
ex <- read_xpt("sdtm/ex.xpt")
ae <- read_xpt("sdtm/ae.xpt")
lb <- read_xpt("sdtm/lb.xpt")
sc <- read_xpt("sdtm/sc.xpt")
sv <- read_xpt("sdtm/sv.xpt")
mh <- read_xpt("sdtm/mh.xpt")
vs <- read_xpt("sdtm/vs.xpt")
qs <- read_xpt("sdtm/qs.xpt")

metacore <- spec_to_metacore("metadata/specs.xlsx", where_sep_sheet = FALSE)

adsl_spec <- metacore %>%
  select_dataset("ADSL")

dm <- convert_blanks_to_na(dm)

# Creating TRT01P/TRT01A/AGEGR1
adsl <- dm %>%
  filter(ARMCD!="Scrnfail") %>%
  mutate(TRT01P=ARM,
         TRT01A=TRT01P) %>%
  derive_vars_dtm(dtc = RFSTDTC,
                  new_vars_prefix = "TRTS") %>%
  derive_vars_dtm(dtc = RFENDTC,
                  new_vars_prefix = "TRTE") %>%
  derive_vars_dtm_to_dt(source_vars = vars(TRTSDTM, TRTEDTM)) %>%
  group_by(SITEID, ARM) %>%
  mutate(size=n(),
         SITEGR1=ifelse(size<3|SITEID=="715"|SITEID=="717","900",SITEID)) %>%
  ungroup() %>%
  mutate(ITTFL=ifelse(ARMCD!='',"Y","N"),
         SAFFL=ifelse(ITTFL=="Y"&!is.na(TRTSDT),"Y","N"))

# HEIGHTBL
adsl <- adsl %>%  derive_vars_merged( dataset_add = vs,
                                    by_vars = vars(STUDYID,USUBJID),
                                    filter_add = (VSTESTCD=='HEIGHT' & VISITNUM == 1),
                                    new_vars = vars(HEIGHTBL = VSSTRESN))

# WEIGHTBL
adsl <- adsl %>%  derive_vars_merged( dataset_add = vs,
                                    by_vars = vars(STUDYID,USUBJID),
                                    filter_add = (VSTESTCD=='WEIGHT' & VISITNUM == 3),
                                    new_vars = vars(WEIGHTBL = VSSTRESN))

# BMIBL
adsl <- adsl %>%  mutate(BMIBL = compute_bmi(HEIGHTBL, WEIGHTBL))

# BMIBLGR1/AGEGR1
adsl <- adsl %>%  create_cat_var(adsl_spec, BMIBL, BMIBLGR1) %>%
  create_cat_var(adsl_spec, AGE, AGEGR1)

# for COMP16FL
sv_16 <- sv %>%
  filter(VISITNUM==10) %>%
  select(STUDYID,USUBJID,SVSTDTC) %>%
  rename(COMP16D=SVSTDTC)

# for COMP24FL
sv_24 <- sv %>%
  filter(VISITNUM==12) %>%
  select(STUDYID,USUBJID,SVSTDTC) %>%
  rename(COMP24D=SVSTDTC)

# for COMP8FL
sv_8 <- sv %>%
  filter(VISITNUM==8) %>%
  select(STUDYID,USUBJID,SVSTDTC) %>%
  rename(COMP8D=SVSTDTC)

# COMP16FL/COMP24FL/COMP8FL
adsl <- adsl %>% mutate(DATE1=as.POSIXct(RFSTDTC,format="%Y-%m-%dT%H:%M",tz="UTC")) %>%
  derive_vars_merged(dataset_add =sv_16,
                     by_vars = vars(STUDYID, USUBJID)) %>%
  mutate(DATE2=as.POSIXct(COMP16D,format="%Y-%m-%dT%H:%M",tz="UTC"),
         COMP16FL=ifelse(DATE1>=DATE2),"Y","N") %>%
  derive_vars_merged(dataset_add =sv_24,
                     by_vars = vars(STUDYID, USUBJID)) %>%
  mutate(DATE2=as.POSIXct(COMP24D,format="%Y-%m-%dT%H:%M",tz="UTC"),
         COMP24FL=ifelse(DATE1>=DATE2),"Y","N") %>%
  derive_vars_merged(dataset_add =sv_8,
                     by_vars = vars(STUDYID, USUBJID)) %>%
  mutate(DATE2=as.POSIXct(COMP8D,format="%Y-%m-%dT%H:%M",tz="UTC"),
         COMP8FL=ifelse(DATE1>=DATE2),"Y","N")

# VISIT1DT
adsl <- adsl %>%  derive_vars_merged(dataset_add = sv,
                                     by_vars = vars(STUDYID, USUBJID),
                                     filter_add = VISITNUM == 1,
                                     new_vars = vars(VISIT1DT = SVSTDTC))

# Deriving VISNUMEN from ds dataframe
ds1 <- ds %>%
  mutate(VISNUMEN = ifelse(VISITNUM == 13 & (DSTERM == "PROTOCOL COMPLETED" | DSTERM == "ADVERSE EVENT"),12,VISITNUM))

# Deriving DCSREAS
ds2 <- ds %>%
  mutate(DCSREAS=ifelse(DSTERM!="PROTOCOL ENTRY CRITERIA NOT MET",ifelse(DSDECOD=="ADVERSE EVENT","Adverse Event",
                                                                  ifelse(DSDECOD=="STUDY TERMINATED BY SPONSOR","Sponsor Decision",
                                                                  ifelse(DSDECOD=="DEATH","Death",
                                                                  ifelse(DSDECOD=="WITHDRAWAL BY SUBJECT","Withdrew Consent",
                                                                  ifelse(DSDECOD=="PHYSICIAN DECISION","Physician Decision",
                                                                  ifelse(DSDECOD=="PROTOCOL VIOLATION","Protocol Violation",
                                                                  ifelse(DSDECOD=="LOST TO FOLLOW-UP","Lost to Follow-up",
                                                                  ifelse(DSDECOD=="LACK OF EFFICACY","Lack of Efficacy",NA)))))))),
                        ifelse(DSTERM=="PROTOCOL ENTRY CRITERIA NOT MET" & DSDECOD=="PROTOCOL VIOLATION","I/E Not Met",NA))) %>%
  filter(!is.na(DCSREAS))

# DCDECOD/VISNUMEN/DCSREAS
adsl <- adsl %>%derive_vars_merged(dataset_add = ds1,
                                  filter_add = (DSCAT == "DISPOSITION EVENT"),
                                  new_vars = vars(DCDECOD = DSDECOD, VISNUMEN),
                                  order = vars(DSDECOD, VISNUMEN, DSTERM),
                                  mode = "first",
                                  by_vars = vars(STUDYID, USUBJID))

# EDUCLVL
adsl <- adsl %>%  derive_vars_merged(dataset_add = sc,
                                   by_vars = vars(STUDYID, USUBJID),
                                   filter_add = SCTESTCD == 'EDLEVEL',
                                   new_vars = vars(EDUCLVL = SCSTRESN))

# RACEN/AGEGR1N/TRT01AN/TRT01PN
adsl <- adsl %>% create_var_from_codelist(adsl_spec,RACE, RACEN) %>%
  create_var_from_codelist(adsl_spec,AGEGR1, AGEGR1N) %>%
  create_var_from_codelist(adsl_spec,TRT01P, TRT01PN) %>%
  create_var_from_codelist(adsl_spec,TRT01A, TRT01AN)


# EOSSTT
format_eosstt <- function(DSDECOD) {
  case_when(DSDECOD %in% c("COMPLETED") ~ "COMPLETED",
            DSDECOD %in% c("SCREEN FAILURE") ~ NA_character_,
            !is.na(DSDECOD) ~ "DISCONTINUED",
            TRUE ~ "ONGOING")
}

# deriving EOSSTT
adsl <- adsl %>%
  derive_var_disposition_status(dataset_ds = ds,
                                new_var = EOSSTT,
                                status_var = DSDECOD,
                                format_new_var = format_eosstt,
                                filter_ds = DSCAT == "DISPOSITION EVENT")

# DISONSDT
adsl <- adsl %>%
  derive_vars_merged(dataset_add = mh,
                     filter_add = (MHCAT == "PRIMARY DIAGNOSIS"),
                     new_vars = vars(DISONSDT = MHSTDTC),
                     order = vars(MHSTDTC),
                     mode = 'first',
                     by_vars = vars(STUDYID, USUBJID))

# MSSETOT
qs <- qs %>%
  select(USUBJID, QSORRES, QSCAT, VISITNUM)

USUBJID = vector("character", 0)
MMSETOT = vector("integer", 0)
flag_ADASCog = vector("character", 0)
flag_CIBIC = vector("character", 0)

n_MMSETOT = 0

is_ADASCog = FALSE
is_CIBIC = FALSE

for(i in 1:nrow(qs))
{
  if(i == 1)
  {
    subject = qs[1, 1]
    USUBJID = c(subject)
  }

  if(qs[i, 3] == "MINI-MENTAL STATE")
  {
    n_MMSETOT = n_MMSETOT + as.integer(qs[i, 2])
  }

  if(qs[i, 3] == "ALZHEIMER'S DISEASE ASSESSMENT SCALE" & qs[i, 4] > 3 & is_ADASCog == FALSE)
  {
    flag_ADASCog = c(flag_ADASCog, 'Y')
    is_ADASCog = TRUE
  }

  if(qs[i, 3] == "CLINICIAN'S INTERVIEW-BASED IMPRESSION OF CHANGE (CIBIC+)" & qs[i, 4] > 3 & is_CIBIC == FALSE)
  {
    flag_CIBIC = c(flag_CIBIC, 'Y')
    is_CIBIC = TRUE
  }

  if(qs[i, 1] != subject)
  {
    MMSETOT = c(MMSETOT, n_MMSETOT)
    subject = qs[i, 1]
    USUBJID = c(USUBJID, subject)
    n_MMSETOT = 0

    if(is_ADASCog == TRUE)
    {
      is_ADASCog = FALSE
    }
    else
    {
      flag_ADASCog = c(flag_ADASCog, 'N')
    }

    if(is_CIBIC == TRUE)
    {
      is_CBIC = FALSE
    }
    else
    {
      flag_CIBIC = c(flag_CIBIC, 'N')
    }
  }
}

MMSETOT = c(MMSETOT, n_MMSETOT)

if(is_ADASCog == TRUE)
{
  is_ADASCog = FALSE
} else
{
  flag_ADASCog = c(flag_ADASCog, 'N')
}

if(is_CIBIC == TRUE)
{
  is_CBIC = FALSE
} else
{
  flag_CIBIC = c(flag_CIBIC, 'N')
}

USUBJID <- paste(USUBJID)
MMSETOT <- paste(MMSETOT)

qs_adsl <- cbind(USUBJID, MMSETOT, flag_ADASCog, flag_CIBIC)

# MMSETOT
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = as.data.frame(qs_adsl),
    new_vars = vars(MMSETOT, flag_ADASCog, flag_CIBIC),
    order = vars(MMSETOT),
    mode = 'first',
    by_vars = vars(USUBJID))%>%

# Deriving VISIT4DT from sv to derive CUMDOSE
  derive_vars_merged(
    dataset_add = sv,
    filter_add = (VISITNUM == 4),
    new_vars = vars(VISIT4DT = SVSTDTC),
    order = vars(SVSTDTC),
    mode = 'first',
    by_vars = vars(STUDYID, USUBJID)) %>%

# Deriving VISIT12DT from sv to derive CUMDOSE
  derive_vars_merged(
    dataset_add = sv,
    filter_add = (VISITNUM == 12),
    new_vars = vars(VISIT12DT = SVSTDTC),
    order = vars(SVSTDTC),
    mode = 'first',
    by_vars = vars(STUDYID, USUBJID)) %>%

# Deriving DURDIS
  mutate(VISIT1DT = as.Date(VISIT1DT),
         VISIT4DT = as.Date(VISIT4DT),
         VISIT12DT = as.Date(VISIT12DT),
         DISONSDT = as.Date(DISONSDT),
         TRTSDT = as.Date(TRTSDT),
         TRTEDT = as.Date(TRTEDT))

# DURDIS
adsl$DURDIS <- compute_duration(adsl$DISONSDT,
                                adsl$VISIT1DT,
                                in_unit = "days",
                                out_unit = "months",
                                floor_in = TRUE,
                                add_one = TRUE,
                                trunc_out = FALSE)

# TRTDURD
adsl$TRTDURD <- compute_duration(adsl$TRTSDT,
                                 adsl$TRTEDT,
                                 in_unit = "days",
                                 out_unit = "days",
                                 floor_in = TRUE,
                                 add_one = TRUE,
                                 trunc_out = FALSE)

# Deriving variables used for caluculating CUMDOSE

# Duration of b/w TRTSDT & VISIT4DT
adsl$Interval_1 <- compute_duration(adsl$TRTSDT,
                                    adsl$VISIT4DT,
                                    in_unit = "days",
                                    out_unit = "days",
                                    floor_in = TRUE,
                                    add_one = TRUE,
                                    trunc_out = FALSE)

# Duration of b/w TRTSDT & TRTEDT
adsl$Interval_1_Discontinued <- compute_duration(adsl$TRTSDT,
                                                 adsl$TRTEDT,
                                                 in_unit = "days",
                                                 out_unit = "days",
                                                 floor_in = TRUE,
                                                 add_one = TRUE,
                                                 trunc_out = FALSE)

# Duration of b/w VISIT4DT & VISIT12DT
adsl$Interval_2 <- compute_duration(adsl$VISIT4DT,
                                    adsl$VISIT12DT,
                                    in_unit = "days",
                                    out_unit = "days",
                                    floor_in = TRUE,
                                    add_one = FALSE,
                                    trunc_out = FALSE)

# Duration of b/w VISIT4DT & VISIT12DT
adsl$Interval_3 <- compute_duration(adsl$VISIT12DT,
                                    adsl$TRTEDT,
                                    in_unit = "days",
                                    out_unit = "days",
                                    floor_in = TRUE,
                                    add_one = FALSE,
                                    trunc_out = FALSE)

# Duration of b/w VISIT4DT & TRTEDT
adsl$Interval_2_Discontinued <- compute_duration(adsl$VISIT4DT,
                                                 adsl$TRTEDT,
                                                 in_unit = "days",
                                                 out_unit = "days",
                                                 floor_in = TRUE,
                                                 add_one = FALSE,
                                                 trunc_out = FALSE)

# # DCSREAS
# adsl <- adsl %>%
#   derive_vars_disposition_reason(dataset_ds = ds,
#                                  new_var = DCSREAS,
#                                  reason_var = DSDECOD,
#                                  new_var_spe = DCSREASP,
#                                  reason_var_spe = DSTERM,
#                                  filter_ds = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE")

adsl <- adsl %>%
  mutate( ARMN = ifelse(ARM == "Placebo",0,
                 ifelse(ARM == "Xanomeline Low Dose",1,
                 ifelse(ARM == "Xanomeline High Dose",2,NA))),
          CUMDOSE = case_when(ARMN == 0 ~ 0,
                             ARMN == 1 ~ TRT01PN * TRTDURD,
                             ARMN == 2 ~ case_when(VISNUMEN > 3 & VISNUMEN <= 4 ~ case_when(EOSSTT == "COMPLETED" ~ 54 * Interval_1,
                                                                                            EOSSTT == "DISCONTINUED" ~ 54 * Interval_1_Discontinued),
                                                   VISNUMEN > 4 & VISNUMEN <= 12 ~ case_when(EOSSTT == "COMPLETED" ~ 54 * Interval_1 + 81 * Interval_2 + 54 * Interval_3,
                                                                                             EOSSTT == "DISCONTINUED" ~ 54 * Interval_1 + 81 * Interval_2_Discontinued),
                                                   VISNUMEN > 12 ~ 54 * Interval_1 + 81 * Interval_2 + 54 * Interval_3)),
         AVGDD = CUMDOSE / TRTDURD,
         DISCONFL = ifelse(DCSREAS == "PROTOCOL COMPLETED",'Y',NA),
         DSRAEFL = ifelse(DCSREAS == "ADVERSE EVENT",'Y',NA),
         DURDSGR1 = ifelse(DURDIS < 12,"<12",
                    ifelse(DURDIS >= 12,">=12",NA)),
         EFFFL = ifelse(SAFFL == 'Y' & flag_ADASCog == 'Y' & flag_CIBIC == 'Y','Y','N'),
         RFENDT = format(as.Date(RFENDTC), "%d-%b-%Y"),
         RFSTDTC = format(as.Date(RFSTDTC), "%d-%b-%Y"),
         RFENDTC = format(as.Date(RFENDTC), "%d-%b-%Y"),
         TRTSDT = format(as.Date(TRTSDT), "%d-%b-%Y"),
         TRTEDT = format(as.Date(TRTEDT), "%d-%b-%Y"),
         VISIT1DT = format(as.Date(VISIT1DT), "%d-%b-%Y"),
         VISIT4DT = format(as.Date(VISIT4DT), "%d-%b-%Y"),
         VISIT12DT = format(as.Date(VISIT12DT), "%d-%b-%Y"),
         DISONSDT = format(as.Date(DISONSDT), "%d-%b-%Y"))


# Adding labels and selecting required variables from metadata
adsl<-adsl %>%
  # only keep vars from define
  drop_unspec_vars(adsl_spec) %>%
  # order columns based on define
  order_cols(adsl_spec) %>%
  # apply variable labels based on define
  set_variable_labels(adsl_spec) %>%
  # Creating .XPT and adding dataset Label
  xportr_write("adam/adsldev.xpt",label = "Subject-Level Analysis Dataset")








