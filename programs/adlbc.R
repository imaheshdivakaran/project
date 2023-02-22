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

# placeholder for origin=predecessor, use metatool::build_from_derived()
metacore <- spec_to_metacore("metadata/specs.xlsx", where_sep_sheet = FALSE)

# Iterate spec for ADLBC
adlbc_spec <- metacore %>%
  select_dataset("ADLBC")

lb <- haven::read_xpt(file.path("sdtm", "lb.xpt"))
adsl <- haven::read_xpt(file.path("adam", "adsl.xpt"))

# Convert blanks to NA
lb <- convert_blanks_to_na(lb)

# Merge with ADSL and creating variables

adlb <-derive_vars_merged(dataset = lb,
                            dataset_add = adsl,
                            by_vars = vars(STUDYID, USUBJID))

# Look-up tables ----

# Assign PARAMCD, PARAM, and PARAMN
param_lookup <- tibble::tribble(
  ~LBTESTCD, ~PARAMCD,  ~PARAM,                                             ~PARAMN,
  "ALB",     "ALB",     "Albumin (g/L)",                                    1,
  "ALP",     "ALKPH",   "Alkaline Phosphatase (U/L)",                       2,
  "ALT",     "ALT",     "Alanine Aminotransferase (U/L)",                   3,
  "ANISO",   "ANISO",   "Anisocytes",                                       4,
  "AST",     "AST",     "Aspartate Aminotransferase (U/L)",                 5,
  "BASO",    "BASO",    "Basophils (10^9/L)",                               6,
  "BILI",    "BILI",    "Bilirubin (umol/L)",                               8,
  "BUN",     "BUN",     "Blood Urea Nitrogen (mmol/L)",                     9,
  "CA",      "CA",      "Calcium (mmol/L)",                                 10,
  "CHOL",    "CHOLES",  "Cholesterol (mmol/L)",                             11,
  "CK",      "CK",      "Creatinine Kinase (U/L)",                          12,
  "CL",      "CL",      "Chloride (mmol/L)",                                13,
  "COLOR",   "COLOR",   "Color",                                            14,
  "CREAT",   "CREAT",   "Creatinine (umol/L)",                              15,
  "EOS",     "EOS",     "Eosinophils (10^9/L)",                             16,
  "GGT",     "GGT",     "Gamma Glutamyl Transferase (U/L)",                 17,
  "GLUC",    "GLUC",    "Glucose (mmol/L)",                                 18,
  "HBA1C",   "HBA1C",   "Hemoglobin A1C (1)",                               19,
  "HCT",     "HCT",     "Hematocrit (1)",                                   20,
  "HGB",     "HGB",     "Hemoglobin (mmol/L)",                              21,
  "K",       "POTAS",   "Potassium (mmol/L)",                               22,
  "KETONES", "KETON",   "Ketones",                                          23,
  "LYM",     "LYMPH",   "Lymphocytes (10^9/L)",                             24,
  "MACROCY", "MACROC",  "Macrocytes",                                       25,
  "MCH",     "MCH",     "Ery. Mean Corpuscular Hemoglobin (fmol(Fe))",      26,
  "MCHC",    "MCHC",    "Ery. Mean Corpuscular HGB Concentration (mmol/L)", 27,
  "MCV",     "MCV",     "Ery. Mean Corpuscular Volume (f/L)",               28,
  "MICROCY", "MICROC",  "Microcytes",                                       29,
  "MONO",    "MONO",    "Monocytes (10^9/L)",                               30,
  "PH",      "PH",      "pH",                                               31,
  "PHOS",    "PHOS",    "Phosphate (mmol/L)",                               32,
  "PLAT",    "PLAT",    "Platelet (10^9/L)",                                33,
  "POIKILO", "POIKIL",  "Poikilocytes",                                     34,
  "POLYCHR", "POLYCH",  "Polychromasia",                                    35,
  "PROT",    "PROT",    "Protein (g/L)",                                    36,
  "RBC",     "RBC",     "Erythrocytes (TI/L)",                              37,
  "SODIUM",  "SODIUM",  "Sodium (mmol/L)",                                  38,
  "SPGRAV",  "SPGRAV",  "Specific Gravity",                                 39,
  "TSH",     "TSH",     "Thyrotropin (mU/L)",                               40,
  "URATE",   "URATE",   "Urate (umol/L)",                                   41,
  "UROBIL",  "UROBIL",  "Urobilinogen",                                     42,
  "VITB12",  "VITB12",  "Vitamin B12 (pmol/L)",                             43,
  "WBC",     "WBC",     "Leukocytes (10^9/L)",                              44
)

adlb <- lb %>%
  ## Add PARAMCD PARAM and PARAMN - from LOOK-UP table
  derive_vars_merged_lookup(
    dataset_add = param_lookup,
    new_vars = vars(PARAMCD, PARAM, PARAMN),
    by_vars = vars(LBTESTCD)
  ) %>%
  ## Calculate PARCAT1 AVAL AVALC ANRLO ANRHI
  ## Dummy the values for BASE
  mutate(
    PARCAT1 = LBCAT,
    AVAL = LBSTRESN,
    AVALC = LBSTRESC,
    ANRLO = LBSTNRLO,
    ANRHI = LBSTNRHI,
    BASE = AVAL - 10
  )

# Assign ATOXDSCL and ATOXDSCH to hold lab grading terms
# ATOXDSCL and ATOXDSCH hold terms defined by NCI-CTCAEv4.
grade_lookup <- tibble::tribble(
  ~PARAMCD, ~ATOXDSCL,                    ~ATOXDSCH,
  "ALB",    "Hypoalbuminemia",            NA_character_,
  "ALKPH",  NA_character_,                "Alkaline phosphatase increased",
  "ALT",    NA_character_,                "Alanine aminotransferase increased",
  "AST",    NA_character_,                "Aspartate aminotransferase increased",
  "BILI",   NA_character_,                "Blood bilirubin increased",
  "CA",     "Hypocalcemia",               "Hypercalcemia",
  "CHOLES", NA_character_,                "Cholesterol high",
  "CK",     NA_character_,                "CPK increased",
  "CREAT",  NA_character_,                "Creatinine increased",
  "GGT",    NA_character_,                "GGT increased",
  "GLUC",   "Hypoglycemia",               "Hyperglycemia",
  "HGB",    "Anemia",                     "Hemoglobin increased",
  "POTAS",  "Hypokalemia",                "Hyperkalemia",
  "LYMPH",  "CD4 lymphocytes decreased",  NA_character_,
  "PHOS",   "Hypophosphatemia",           NA_character_,
  "PLAT",   "Platelet count decreased",   NA_character_,
  "SODIUM", "Hyponatremia",               "Hypernatremia",
  "WBC",    "White blood cell decreased", "Leukocytosis",
)

adlb <- adlb %>%
  derive_vars_merged(
    dataset_add = grade_lookup,
    by_vars = vars(PARAMCD),
  )

adlb <- adlb %>%
  derive_var_atoxgr_dir(
    new_var = ATOXGRL,
    tox_description_var = ATOXDSCL,
    meta_criteria = atoxgr_criteria_ctcv4,
    criteria_direction = "L",
    get_unit_expr = extract_unit(PARAM)
  ) %>%
  derive_var_atoxgr_dir(
    new_var = ATOXGRH,
    tox_description_var = ATOXDSCH,
    meta_criteria = atoxgr_criteria_ctcv4,
    criteria_direction = "H",
    get_unit_expr = extract_unit(PARAM)
  )
adlb <- adlb %>%
  derive_var_atoxgr()
