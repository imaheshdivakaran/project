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

dm <- haven::read_xpt(file.path("sdtm", "dm.xpt"))
lb <- haven::read_xpt(file.path("sdtm", "lb.xpt"))
adsl <- haven::read_xpt(file.path("adam", "adsl.xpt"))


