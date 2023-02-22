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

metacore <- spec_to_metacore("metadata/specs.xlsx", where_sep_sheet = FALSE)

adsl_spec <- metacore %>%
  select_dataset("ADSL")

# Creating TRT01P/TRT01A/AGEGR1
adsl_1 <- dm %>%
  filter(ARMCD!="Scrnfail") %>%
  mutate(TRT01P=ARM,
         TRT01A=TRT01P,
         AGEGR1N=ifelse(AGE<65,1,ifelse(AGE>=65&AGE<=80,2,ifelse(AGE>80,3,NA))),
         AGEGR1=ifelse(AGEGR1N==1,"<65",ifelse(AGEGR1N==2,"65-80",ifelse(AGEGR1N==3,">80",NA_character_)))) %>%
  derive_vars_dtm(dtc = RFSTDTC,
                  new_vars_prefix = "TRTS") %>%
  derive_vars_dtm(dtc = RFENDTC,
                  new_vars_prefix = "TRTE") %>%
  derive_vars_dtm_to_dt(source_vars = vars(TRTSDTM, TRTEDTM)) %>%


  pooled_sites <- dm %>%
  filter(ARMCD != "Scrnfail") %>%
  group_by(SITEID, ARM) %>%
  summarise(grp_size = n(),.groups =NULL ) %>%
  filter (grp_size < 3) %>%
  select (SITEID,grp_size,ARM) %>%
  distinct()



