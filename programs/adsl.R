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
  derive_vars_dtm_to_dt(source_vars = vars(TRTSDTM)) %>%
  group_by(SITEID, ARM) %>%
  mutate(size=n(),
         SITEGR1=ifelse(size<3|SITEID=="715"|SITEID=="717","900",SITEID)) %>%
  ungroup() %>%
  mutate(ITTFL=ifelse(ARMCD!='',"Y","N"),
         SAFFL=ifelse(ITTFL=="Y"&!is.na(TRTSDT),"Y","N"))

# Deriving TRTEDT from ex

ex_dt <- ex %>%
  derive_vars_dt(
    new_vars_prefix = "EXEN",
    dtc = EXENDTC
  )

ds_dt <- ds %>%
  derive_vars_dt(
    new_vars_prefix = "DS",
    dtc = DSDTC
  )


adsl <-  adsl %>%
  derive_vars_merged(
    dataset_add = ex_dt,
    by_vars = vars(STUDYID, USUBJID),
    order = vars(EXENDT, EXSEQ),
    new_vars = vars(TRTEDT = EXENDT),
    mode = "last",
    filter_add = EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))
  )

adsl <-  adsl %>%
  derive_vars_merged(
    dataset_add = ds_dt %>% select(-DOMAIN),
    by_vars = vars(STUDYID, USUBJID),
    filter_add = (DSCAT=="DISPOSITION EVENT")
  ) %>%
  mutate(TRTEDT=ifelse(is.na(TRTEDT)&VISITNUM>3,as.character(DSDT),as.character(TRTEDT)),
         TRTEDT=as.POSIXct(TRTEDT,format="%Y-%m-%d",tz="UTC")) %>% select(-DSDECOD)

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

# # for COMP16FL
# sv_16 <- sv %>%
#   filter(VISITNUM==10) %>%
#   select(STUDYID,USUBJID,SVSTDTC) %>%
#   rename(COMP16D=SVSTDTC)
#
# # for COMP24FL
# sv_24 <- sv %>%
#   filter(VISITNUM==12) %>%
#   select(STUDYID,USUBJID,SVSTDTC) %>%
#   rename(COMP24D=SVSTDTC)
#
# # for COMP8FL
# sv_8 <- sv %>%
#   filter(VISITNUM==8) %>%
#   select(STUDYID,USUBJID,SVSTDTC) %>%
#   rename(COMP8D=SVSTDTC)
#
# # COMP16FL/COMP24FL/COMP8FL
# adsl <- adsl %>% mutate(DATE1=as.POSIXct(RFSTDTC,format="%Y-%m-%d",tz="UTC")) %>%
#   derive_vars_merged(dataset_add =sv_16,
#                      by_vars = vars(STUDYID, USUBJID)) %>%
#   mutate(DATE2=as.POSIXct(COMP16D,format="%Y-%m-%d",tz="UTC"),
#          COMP16FL=as.character(ifelse(DATE1>=DATE2,"Y","N"))) %>%
#   derive_vars_merged(dataset_add =sv_24,
#                      by_vars = vars(STUDYID, USUBJID)) %>%
#   mutate(DATE2=as.POSIXct(COMP24D,format="%Y-%m-%d",tz="UTC"),
#          COMP24FL=as.character(ifelse(DATE1>=DATE2,"Y","N"))) %>%
#   derive_vars_merged(dataset_add =sv_8,
#                      by_vars = vars(STUDYID, USUBJID)) %>%
#   mutate(DATE2=as.POSIXct(COMP8D,format="%Y-%m-%d",tz="UTC"),
#          COMP8FL=as.character(ifelse(DATE1>=DATE2,"Y","N")))

# Creating sv variables to add to the adsl

sv <- sv %>%
  mutate(COMP16FL = case_when(VISITNUM == 10 & VISITDY >= 70 ~ 'Y',
                              TRUE ~ 'N'),
         COMP24FL = case_when(VISITNUM == 12 & VISITDY >= 84 ~ 'Y',
                              TRUE ~ 'N'),
         COMP8FL = case_when(VISITNUM == 8 & VISITDY >= 56 ~ 'Y',
                             TRUE ~ 'N'),
         TRTSDT = case_when(VISITNUM == 3 ~ SVSTDTC),
         VISIT1DT = case_when(VISITNUM == 1 ~ SVSTDTC))

# Extracting relevant sv variables

sv_extract <- sv %>%
  select(USUBJID, COMP16FL, COMP24FL, COMP8FL, TRTSDT, VISIT1DT)

# Initializing columns to add to the adsl

USUBJID = vector("character", 0)
COMP16FL = vector("character", 0)
COMP24FL = vector("character", 0)
COMP8FL = vector("character", 0)
TRTSDT = vector("character", 0)
VISIT1DT = vector("character", 0)
# The Boolean flags ensure that no more than 1 record is added for a subject

COMP16FL_b = FALSE
COMP24FL_b = FALSE
COMP8FL_b = FALSE
TRTSDT_b = FALSE
VISIT1DT_b = FALSE

# This nested loop goes through every element in the sv data set and extract relevant data
# i is row and j is column

for(i in 1:nrow(sv_extract))
{
  for(j in 1:ncol(sv_extract))
  {
    if(i == 1 & j == 1)
    {
      subject = sv_extract[i, j]
      USUBJID <- c(subject)

      COMP16FL_b = FALSE
      COMP24FL_b = FALSE
      COMP8FL_b = FALSE
      TRTSDT_b = FALSE
      VISIT1DT_b = FALSE
    }

    if(j == 1 & sv_extract[i, j] != subject)
    {
      if(COMP16FL_b == FALSE)
      {
        COMP16FL <- c(COMP16FL, 'N')
      }

      if(COMP24FL_b == FALSE)
      {
        COMP24FL <- c(COMP24FL, 'N')
      }

      if(COMP8FL_b == FALSE)
      {
        COMP8FL <- c(COMP8FL, 'N')
      }

      if(TRTSDT_b == FALSE)
      {
        TRTSDT <- c(TRTSDT, NA)
      }

      if(VISIT1DT_b == FALSE)
      {
        VISIT1DT <- c(VISIT1DT, NA)
      }

      subject = sv_extract[i, j]
      USUBJID = c(USUBJID, subject)

      COMP16FL_b = FALSE
      COMP24FL_b = FALSE
      COMP8FL_b = FALSE
      TRTSDT_b = FALSE
      VISIT1DT_b = FALSE
    }

    if(sv_extract[i, 2] == 'Y' & COMP16FL_b == FALSE)
    {
      COMP16FL <- c(COMP16FL, 'Y')
      COMP16FL_b = TRUE
    }

    if(sv_extract[i, 3] == 'Y' & COMP24FL_b == FALSE)
    {
      COMP24FL <- c(COMP24FL, 'Y')
      COMP24FL_b = TRUE
    }

    if(sv_extract[i, 4] == 'Y' & COMP8FL_b == FALSE)
    {
      COMP8FL <- c(COMP8FL, 'Y')
      COMP8FL_b = TRUE
    }

    if(is.na(sv_extract[i, 5]) == FALSE & TRTSDT_b == FALSE)
    {
      TRTSDT <- c(TRTSDT, sv_extract[i, 5])
      TRTSDT_b = TRUE
    }

    if(is.na(sv_extract[i, 6]) == FALSE & VISIT1DT_b == FALSE)
    {
      VISIT1DT = c(VISIT1DT, sv_extract[i, 6])
      VISIT1DT_b = TRUE
    }
  }
}

if(COMP16FL_b == FALSE)
{
  COMP16FL <- c(COMP16FL, 'N')
}

if(COMP24FL_b == FALSE)
{
  COMP24FL <- c(COMP24FL, 'N')
}

if(COMP8FL_b == FALSE)
{
  COMP8FL <- c(COMP8FL, 'N')
}

if(TRTSDT_b == FALSE)
{
  TRTSDT <- c(TRTSDT, NA)
}

if(VISIT1DT_b == FALSE)
{
  VISIT1DT <- c(VISIT1DT, NA)
}

# Converting lists in to characters because the adsl columns are characters

USUBJID <- paste(USUBJID)
TRTSDT <- paste(TRTSDT)
VISIT1DT <-paste(VISIT1DT)

# Creating the data set to merge with the adsl

sv_adsl <- cbind(USUBJID, COMP16FL, COMP24FL, COMP8FL, TRTSDT, VISIT1DT)

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = as.data.frame(sv_adsl),
    new_vars = vars(COMP16FL, COMP24FL, COMP8FL),
    order = vars(COMP16FL, COMP24FL, COMP8FL, TRTSDT),
    mode = 'first',
    by_vars = vars(USUBJID)
  )

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

# DCDECOD/VISNUMEN
adsl <- adsl %>%derive_vars_merged(dataset_add = ds1,
                                  filter_add = (DSCAT == "DISPOSITION EVENT"),
                                  new_vars = vars(DCDECOD = DSDECOD, VISNUMEN),
                                  order = vars(DSDECOD, VISNUMEN, DSTERM),
                                  mode = "first",
                                  by_vars = vars(STUDYID, USUBJID))

# DCSREAS
adsl <- adsl %>%derive_vars_merged(dataset_add = ds2,
                                   filter_add = (DSCAT == "DISPOSITION EVENT"),
                                   new_vars = vars(DCSREAS=DCSREAS),
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
  mutate(MMSETOT=as.numeric(MMSETOT)) %>%

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


adsl <- adsl %>%
  mutate(ARMN = ifelse(ARM == "Placebo",0,
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
         DISCONFL = ifelse(DCSREAS == "Completed",'Y',NA),
         DISCONFL=as.character(DISCONFL),
         DSRAEFL = ifelse(DCSREAS == "Adverse Event",'Y',NA),
         DSRAEFL=as.character(DSRAEFL),
         DURDSGR1 = ifelse(DURDIS < 12,"<12",
                    ifelse(DURDIS >= 12,">=12",NA)),
         EFFFL = ifelse(SAFFL == 'Y' & flag_ADASCog == 'Y' & flag_CIBIC == 'Y','Y','N'),
         RFENDT = as.Date(RFENDTC),
         RFSTDTC = as.character(RFSTDTC),
         RFENDTC =as.character(RFENDTC),
         AVGDD=round(AVGDD,digits = 1),
         BMIBL=round(BMIBL,digits = 1),
         HEIGHTBL=round(HEIGHTBL,digits = 1),
         WEIGHTBL=round(WEIGHTBL,digits = 1),
         DURDIS=round(DURDIS,digits = 1))


# Adding labels and selecting required variables from metadata
adsl<-adsl %>%
  # only keep vars from define
  drop_unspec_vars(adsl_spec) %>%
  # order columns based on define
  order_cols(adsl_spec) %>%
  # apply variable labels based on define
  set_variable_labels(adsl_spec) %>%
  # Creating .XPT and adding dataset Label
  xportr_write("adam/adsl.xpt",label = "Subject-Level Analysis Dataset")








