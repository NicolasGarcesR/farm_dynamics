################################################################################
#  DATA CLEANING — BASELINE (2021) AND ENDLINE (2023)
#  FNC-EFI Coffee Producer Survey
#
#  This script produces a single clean analytical dataset by processing each
#  wave separately and then stacking them.  Every step is written out in full
#  so the logic is transparent.
#
#  IMPORTANT VARIABLE MAPPING NOTES (baseline vs endline):
#    - Age        baseline: M1_106  /  endline: M1_105  (question renumbered)
#    - Sex        baseline: M1_105  /  endline: M1_108  (question renumbered)
#    - Education  baseline: M1_109A1, M1_109B1  /  endline: M1_112A1, M1_112A2
#    - Cooperative baseline: SEC1 M1_115  /  endline: SEC0 M1_120
#    - Machines   baseline: M1_601A-N (1/2 binary)  /  endline: M1_603A-M (binary)
#    - Labour     baseline: M1_602A, M1_603, M1_601C*-F*  /  endline: M1_601A-E*
#    - Land       baseline: M1_414A + unit code SEC4  /  endline: M1_403A ha SEC2
#    - Credit     baseline: SEC3 M1_301A-E, M1_311A-E  /  endline: SEC1 M1_310N
#
#  OUTPUT: a single data frame called `data_clean` (one row per farm x wave)
################################################################################

library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

setwd("C:/Users/wb631291/OneDrive - WBG/Desktop/Thesis")
# ==============================================================================
# PART 1 — BASELINE (2021, survey code ARD607)
# ==============================================================================

# --- B1. Load all baseline files ----------------------------------------------

b_sec0 <- read_excel("Data/Baseline/ARD607_SEC0_DATOS_LINEALES.xlsx")
b_sec1 <- read_excel("Data/Baseline/ARD607_SEC1_MATRIZ_IDENTIFICACION.xlsx")
b_sec3 <- read_excel("Data/Baseline/ARD607_SEC3_MATRIZ_FINANCIERA.xlsx")  # avoids type-guessing errors in wide file
b_sec4 <- read_excel("Data/Baseline/ARD607_SEC4_MATRIZ_ACT_PRODUC_INV_AGROPE_FAC_PRODUC.xlsx")
b_sec5 <- read_excel("Data/Baseline/ARD607_SEC5_MATRIZ_INV_AGROPECUARIO.xlsx")


# --- B2. Labour variables (from SEC0 datos lineales) -------------------------
# Each variable represents annual labour hours for a specific worker category.
# Conversion: 2496 h/year for permanent (52 weeks * 48 h/week).
# For temporary: if time unit == 1 (days), 8 h/day; else 192 h/month.

b_labour <- b_sec0 %>%
  dplyr::select(NIM, DEPARTAMENTO, MUNICIPIO,
         M1_602A, M1_603, M1_604A, M1_604B,
         M1_601C0, M1_601C1, M1_601C2, M1_601C3,
         M1_601D0, M1_601D1,
         M1_601E0, M1_601E1, M1_601E2, M1_601E3,
         M1_601F0, M1_601F1, M1_601F2, M1_601F3) %>%
  mutate(
    # Permanent paid workers: count * annual hours
    permanente = replace_na(M1_602A, 0) * 2496,

    # Temporary paid harvest workers
    temp_time  = if_else(M1_604A == 1, 8, 192),
    temporal   = replace_na(M1_603, 0) * replace_na(temp_time, 8) * replace_na(M1_604B, 0),

    # Secondary contracted workers (contratistas) — non-harvest tasks
    secu_time  = if_else(M1_601C2 == 1, 8, 192),
    secu       = replace_na(M1_601C1, 0) * replace_na(secu_time, 8) * replace_na(M1_601C3, 0),

    # Unpaid family workers — permanent (full-year)
    fam        = replace_na(M1_601D1, 0) * 2496,

    # Unpaid family workers — seasonal/occasional
    fam_secu_time = if_else(M1_601E2 == 1, 8, 192),
    fam_secu   = replace_na(M1_601E1, 0) * replace_na(fam_secu_time, 8) * replace_na(M1_601E3, 0),

    # Unpaid family workers — other occasional
    fam_otra_time = if_else(M1_601F2 == 1, 8, 192),
    fam_otra   = replace_na(M1_601F1, 0) * replace_na(fam_otra_time, 8) * replace_na(M1_601F3, 0),

    # Aggregated labour totals
    labor      = permanente + temporal + secu + fam + fam_secu + fam_otra,
    non_harvest = permanente + fam,               # permanent workers (non-seasonal)
    harvest    = temporal + secu + fam_secu + fam_otra  # seasonal/occasional workers
  ) %>%
  filter(labor > 0) %>%
  dplyr::select(NIM, DEPARTAMENTO, MUNICIPIO, permanente, temporal, secu,
         fam, fam_secu, fam_otra, labor, non_harvest, harvest)


# --- B3. Machine variables (from SEC0 datos lineales) ------------------------
# Variables M1_601A through M1_601N are binary indicators of machine ownership
# where 1 = owns at least one unit, 2 = does not own.
# Recode to 0/1 binary so they can be summed into an index.
#
# Machine order: A=germinador, B=almácigo, C=arvenses, D=motosierra,
#   E=guadañadora, F=ecomill, G=becolsub, H=despulpadora, I=desmucilaginador,
#   J=tanque, K=secador, L=elva, M=camioneta, N=moto

b_machines <- b_sec0 %>%
  dplyr::select(NIM,
         M1_601A, M1_601B, M1_601C, M1_601D, M1_601E,
         M1_601F, M1_601G, M1_601H, M1_601I, M1_601J,
         M1_601K, M1_601L, M1_601M, M1_601N) %>%
  # Recode: 1 (has machine) → 1, anything else (2 = no, NA = no data) → 0
  mutate(
    germinador       = replace_na(as.integer(M1_601A == 1), 0L),
    almacigo         = replace_na(as.integer(M1_601B == 1), 0L),
    arvenses         = replace_na(as.integer(M1_601C == 1), 0L),
    motosierra       = replace_na(as.integer(M1_601D == 1), 0L),
    Guadanadora      = replace_na(as.integer(M1_601E == 1), 0L),
    Ecomill          = replace_na(as.integer(M1_601F == 1), 0L),
    Becolsub         = replace_na(as.integer(M1_601G == 1), 0L),
    Despulpadora     = replace_na(as.integer(M1_601H == 1), 0L),
    Desmucilaginador = replace_na(as.integer(M1_601I == 1), 0L),
    Tanque           = replace_na(as.integer(M1_601J == 1), 0L),
    Secador          = replace_na(as.integer(M1_601K == 1), 0L),
    Elva             = replace_na(as.integer(M1_601L == 1), 0L),
    Camioneta        = replace_na(as.integer(M1_601M == 1), 0L),
    Moto             = replace_na(as.integer(M1_601N == 1), 0L)
  ) %>%
  mutate(
    # Automation machines used in the recursion test (labour-saving, post-harvest)
    machines  = motosierra + Desmucilaginador + Elva + Guadanadora + Despulpadora,
    # All machines for broader capital index
    maquinaria = machines + Becolsub + Ecomill + Secador
  ) %>%
  dplyr::select(NIM, germinador, almacigo, arvenses, motosierra, Guadanadora,
         Ecomill, Becolsub, Despulpadora, Desmucilaginador, Tanque,
         Secador, Elva, Camioneta, Moto, machines, maquinaria)


# --- B4. Household demographics (from SEC1 identification matrix) ------------
# One row per household member. We need household-level summaries.
# Head = person with M1_ID_DE_PERSONA == 0 (first household member = respondent).

# 4a. Per-person age, sex, education
b_persons <- b_sec1 %>%
  dplyr::select(NIM, M1_ID_DE_PERSONA, M1_105, M1_106, M1_109A1, M1_109B1, M1_115) %>%
  rename(
    person_id  = M1_ID_DE_PERSONA,
    sexo_code  = M1_105,     # 1 = male, 2 = female
    edad       = M1_106,     # age in years
    edu_nivel  = M1_109A1,   # education level code
    edu_anios  = M1_109B1,   # years completed at that level
    cooperativa = M1_115     # 1 = member of local cooperative
  ) %>%
  mutate(
    # Recode sex to binary (1 = male)
    sexo = as.integer(sexo_code == 1),

    # Compute total years of education from level + years-in-level
    # Level codes: 2/3 = primary/secondary (years stand alone),
    # 5/8 = technical/specialisation (add 11 years of bachillerato),
    # 9 = university graduate (add 15 years), 7 = no schooling (0)
    educacion = case_when(
      edu_nivel %in% c(2, 3) ~ replace_na(edu_anios, 0),
      edu_nivel %in% c(5, 8) ~ replace_na(edu_anios, 0) + 11L,
      edu_nivel == 9         ~ replace_na(edu_anios, 0) + 15L,
      edu_nivel == 7         ~ 0L,
      TRUE                   ~ 0L
    )
  )

# 4b. Household head sex (person 0)
b_head_sex <- b_persons %>%
  filter(person_id == 0) %>%
  dplyr::select(NIM, sexo)

# 4c. Household head cooperative membership (person 0)
b_coop <- b_persons %>%
  filter(person_id == 0) %>%
  transmute(NIM, coop = as.integer(cooperativa == 1))

# 4d. Household size: adults (age >= 10) and children (age < 10)
b_hh_size <- b_persons %>%
  group_by(NIM) %>%
  summarise(
    Total_HH      = sum(edad >= 10, na.rm = TRUE),
    Total_HH_Kids = sum(edad <  10, na.rm = TRUE),
    .groups = "drop"
  )

# 4e. Demographic shares: prime-age and elderly, by sex
# Prime-age adults: 18-59 years (full weight) or 10-17 years (half weight as partial)
# Elderly: 60+ years
b_demog <- b_persons %>%
  mutate(
    Prime_Male    = if_else(edad > 18 & edad < 60 & sexo_code == 1, 1,
                    0.5 * as.numeric(edad >= 10 & edad < 60 & sexo_code == 1)),
    Elderly_Male  = as.numeric(edad >= 60 & sexo_code == 1),
    Prime_Female  = if_else(edad > 18 & edad < 60 & sexo_code == 2, 1,
                    0.5 * as.numeric(edad >= 10 & edad < 60 & sexo_code == 2)),
    Elderly_Female = as.numeric(edad >= 60 & sexo_code == 2)
  ) %>%
  group_by(NIM) %>%
  summarise(
    Prime_Male     = sum(Prime_Male,     na.rm = TRUE),
    Elderly_Male   = sum(Elderly_Male,   na.rm = TRUE),
    Prime_Female   = sum(Prime_Female,   na.rm = TRUE),
    Elderly_Female = sum(Elderly_Female, na.rm = TRUE),
    .groups = "drop"
  )

# 4f. Average education in household
b_edu_avg <- b_persons %>%
  group_by(NIM) %>%
  summarise(Educacion_Promedio = mean(educacion, na.rm = TRUE), .groups = "drop")


# --- B5. Land area (from SEC4 productive activities) ------------------------
# M1_414A = area, M1_414B = unit code (1=m², 2=ha, 3=cuadras, 4=plazas)
# Convert everything to square metres so baseline and endline are comparable.

b_land <- b_sec4 %>%
  dplyr::select(NIM, M1_414A, M1_414B) %>%
  mutate(
    unit_factor = case_when(
      M1_414B == 1 ~ 1,        # already m²
      M1_414B == 2 ~ 10000,    # 1 ha = 10 000 m²
      M1_414B == 3 ~ 6400,     # 1 cuadra ≈ 6 400 m²
      M1_414B == 4 ~ 3930,     # 1 plaza ≈ 3 930 m²
      TRUE         ~ NA_real_
    ),
    Tierra_m2 = M1_414A * unit_factor
  ) %>%
  group_by(NIM) %>%
  summarise(Tierra = sum(Tierra_m2, na.rm = TRUE), .groups = "drop")


# --- B6. Certified coffee (from SEC5 agricultural inventory) ----------------
# M1_508D == 1 means the farm produces certified coffee.
# One row per crop per farm; keep indicator at farm level (any crop certified).

b_cert <- b_sec5 %>%
  group_by(NIM) %>%
  summarise(Certificado = as.integer(any(M1_508D == 1, na.rm = TRUE)),
            .groups = "drop")


# --- B6b. Coffee production (from SEC5 agricultural inventory) --------------
# Unit codes (same instrument across both waves):
#   1 = carga (125 kg pergamino seco)
#   2 = arroba (12.5 kg)
#   3 = kg (already in kg)
#   4 = bulto/saco (60 kg)
#   5 = other / unknown — only 7 baseline rows, set to NA
# Revenue: M1_204 is not collected in the baseline survey → NA.
# M1_501 == 1 identifies coffee; sum across rows in case a farm has
# multiple coffee plots in the inventory matrix.

coffee_unit_to_kg <- function(unit_code) {  dplyr::case_when(
  unit_code == 1 ~ 125,    
  unit_code == 2 ~ 12.5,    
  unit_code == 3 ~ 1,    
  unit_code == 4 ~ 60,    
  TRUE           ~ NA_real_ )
                                                      }

b_production <- b_sec5 %>%
  filter(!is.na(M1_501) & M1_501 == 1) %>%
  transmute(
    NIM,
    prod_kg_row = as.numeric(M1_508A) * coffee_unit_to_kg(as.integer(M1_508C))
  ) %>%
  group_by(NIM) %>%
  summarise(
    production_kg  = sum(prod_kg_row, na.rm = TRUE),
    revenue_cop    = NA_real_,   # not collected in baseline survey
    .groups = "drop"
  ) %>%
  mutate(production_kg = if_else(production_kg == 0, NA_real_, production_kg))

message("Baseline production: ",
        sum(!is.na(b_production$production_kg)), " farms with kg data")


# --- B7. Credit access (from SEC3 financial matrix) -------------------------
# The SEC3 financial matrix is person-level. We restrict to the HEAD (person 0)
# to match the original analysis which used a farm-level credit indicator.
#HELPERS

any_yes <- function(...) {
  m <- cbind(...)
  has_yes <- rowSums(m == 1L, na.rm = TRUE) > 0
  all_na  <- rowSums(!is.na(m)) == 0
  dplyr::if_else(has_yes, 1L, dplyr::if_else(all_na, NA_integer_, 0L))
}

# Household collapse: max over members, NA only if every member is NA.
max_hh <- function(x) {
  if (all(is.na(x))) NA_integer_ else as.integer(max(x, na.rm = TRUE))
}

# Household collapse for value variables: sum over members, NA if all NA.
sum_hh <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

credit_status <- function(formal, informal) {
  dplyr::case_when(
    is.na(formal) & is.na(informal)      ~ NA_character_,
    formal == 1 & informal == 1          ~ "Mixto",
    formal == 1                          ~ "Formal",
    informal == 1                        ~ "Informal",
    TRUE                                 ~ "Sin credito"
  )
}
# Informal sources:
#   M1_312A  family / friend
#   M1_312B   moneylender / gota-a-gota
#   M1_312C   pawnshop
#   M1_312D   agricultural-input supplier
#   M1_312E   chain store / supermarket
#
# Formal sources:
#   M1_312F   cooperative or coffee-producer association
#   M1_312G   financial cooperative (Huila / Tolima)
#   M1_312H   microfinance institution
#   M1_312I   NGO
#   M1_312J   Banco Agrario
#   M1_312K   commercial bank (Davivienda, Bancolombia, etc.)
#
# NOTE: ~10% of households have no financial-module respondent (M1_3103);  
# those will retain NA for credit variables after the left_join.
 
b_credit <- b_sec3 %>%
   transmute(
     NIM,
     inf_person = any_yes(as.integer(M1_313A == 1), as.integer(M1_313B == 1),
                          as.integer(M1_313C == 1), as.integer(M1_313D == 1),
                          as.integer(M1_313E == 1)),
     for_person = any_yes(as.integer(M1_313F == 1), as.integer(M1_313G == 1),
                          as.integer(M1_313H == 1), as.integer(M1_313I == 1),
                          as.integer(M1_313J == 1), as.integer(M1_313K == 1))
   ) %>%
   group_by(NIM) %>%
   summarise(
     tiene_credito_informal = max_hh(inf_person),
     tiene_credito_formal   = max_hh(for_person),
     .groups = "drop"
   ) %>%
   mutate(
     # [FIX-17] Flag BEFORE filling, so the imputation stays visible.
     credit_module_missing  = as.integer(is.na(tiene_credito_informal) &
                                           is.na(tiene_credito_formal)),
     tiene_credito_informal = replace_na(tiene_credito_informal, 0L),
     tiene_credito_formal   = replace_na(tiene_credito_formal,   0L),
     tiene_credito          = as.integer(tiene_credito_informal == 1 |
                                           tiene_credito_formal   == 1),
     status                 = credit_status(tiene_credito_formal,
                                            tiene_credito_informal)
   )


# --- B8. Farm age (from SEC4 productive activities) -------------------------
# M1_408 = year the farm/plot was established; farm age = 2021 - mean year.

b_farm_age <- b_sec4 %>%
  filter(!is.na(M1_408)) %>%
  group_by(NIM) %>%
  summarise(
    farms_antiquity = 2021 - round(mean(M1_408, na.rm = TRUE)),
    .groups = "drop"
  )

# M1_415 = received technical assistance (1=yes, 2=no)
b_tech_assist <- b_sec4 %>%
  group_by(NIM) %>%
  summarise(
    Asistencia_tecnica = as.integer(any(M1_415 == 1, na.rm = TRUE)),
    .groups = "drop"
  )


# --- B9. Merge all baseline pieces into one farm-level dataset ---------------

baseline_clean <- b_labour %>%
  left_join(b_machines,    by = "NIM") %>%
  left_join(b_hh_size,     by = "NIM") %>%
  left_join(b_demog,       by = "NIM") %>%
  left_join(b_edu_avg,     by = "NIM") %>%
  left_join(b_head_sex,    by = "NIM") %>%
  left_join(b_coop,        by = "NIM") %>%
  left_join(b_land,        by = "NIM") %>%
  left_join(b_cert,        by = "NIM") %>%
  left_join(b_production,  by = "NIM") %>%
  left_join(b_credit, by = "NIM") %>%
  left_join(b_farm_age,    by = "NIM") %>%
  left_join(b_tech_assist, by = "NIM") %>%
  # Replace NA demographic counts with 0
  mutate(
    across(c(Prime_Male, Elderly_Male, Prime_Female, Elderly_Female), ~ replace_na(.x, 0)),
    # Household shares: each group as fraction of adult household members
    prime_male_share     = if_else(Total_HH > 0, Prime_Male     / Total_HH, 0),
    prime_female_share   = if_else(Total_HH > 0, Prime_Female   / Total_HH, 0),
    elderly_male_share   = if_else(Total_HH > 0, Elderly_Male   / Total_HH, 0),
    elderly_female_share = if_else(Total_HH > 0, Elderly_Female / Total_HH, 0),
    # Interactions needed for the recursion test
    no_tiene_credito     = 1L - replace_na(tiene_credito, 0L),
    # Certifications: replace NA with 0 (assume uncertified if missing)
    Certificado          = replace_na(Certificado, 0L),
    # Wave identifier
    wave = 1L,
    DPTO       = DEPARTAMENTO,
    MUNICIPIO  = MUNICIPIO
  ) %>%
  dplyr::select(-DEPARTAMENTO)

message("Baseline: ", nrow(baseline_clean), " farm-level observations")


# ==============================================================================
# PART 2 — ENDLINE (2023, survey code ARD634)
# ==============================================================================

# --- E1. Load all endline files -----------------------------------------------

e_sec0 <- read_excel("Data/Endline/A634_SEC_0_DATOS_LINEALES.xlsx")
e_sec1 <- read_excel("Data/Endline/A634_SEC_1_MATRIZ_IDENTIFICACION.xlsx",
                     guess_max = 3000)
e_sec2 <- read_excel("Data/Endline/A634_SEC_2_MATRIZ_ACT_PRODUCTIVA.xlsx")
e_sec3 <- read_excel("Data/Endline/A634_SEC_3_MATRIZ_INV_AGROPECUARIO.xlsx")


# --- E2. Labour variables (from SEC0 datos lineales) -------------------------
# The endline labour module was redesigned from the baseline. Variable mapping:
#   M1_601A  (yes/no hired permanent)  → M1_601A1 count → like baseline M1_602A
#   M1_601B  (yes/no hired temporary)  → M1_601B1 count, M1_601B2 time type, M1_601B3 periods
#   M1_601D  (yes/no family permanent) → M1_601D1 count → like baseline M1_601D1
#   M1_601E  (yes/no family temporary) → M1_601E1 count, M1_601E2 time type, M1_601E3 periods
# No equivalent of baseline "secu" (secondary contracted) or "fam_otra" categories.

e_labour <- e_sec0 %>%
  dplyr::select(NIM, DPTO, MPIO,
         M1_601A, M1_601A1,
         M1_601B, M1_601B1, M1_601B2, M1_601B3,
         M1_601D, M1_601D1,
         M1_601E, M1_601E1, M1_601E2, M1_601E3) %>%
  mutate(
    # Permanent hired workers: 0 if none or question unanswered
    permanente = replace_na(
      if_else(M1_601A == 1, replace_na(M1_601A1, 0), 0) * 2496,
      0),

    # Temporary hired workers: 0 if none or question unanswered
    temp_time  = replace_na(if_else(M1_601B2 == 1, 8, 192), 192),
    temporal   = replace_na(
      if_else(M1_601B == 1,
              replace_na(M1_601B1, 0) * temp_time * replace_na(M1_601B3, 0),
              0),
      0),

    # Unpaid permanent family workers
    fam        = replace_na(
      if_else(M1_601D == 1, replace_na(M1_601D1, 0), 0) * 2496,
      0),

    # Unpaid temporary family workers
    fam_time   = replace_na(if_else(M1_601E2 == 1, 8, 192), 192),
    fam_secu   = replace_na(
      if_else(M1_601E == 1,
              replace_na(M1_601E1, 0) * fam_time * replace_na(M1_601E3, 0),
              0),
      0),

    secu     = 0,
    fam_otra = 0,

    labor       = permanente + temporal + fam + fam_secu,
    non_harvest = permanente + fam,
    harvest     = temporal + fam_secu
  ) %>%
  dplyr::select(NIM, DPTO, MPIO, permanente, temporal, secu,
         fam, fam_secu, fam_otra, labor, non_harvest, harvest)


# --- E3. Machine variables (from SEC0 datos lineales) ------------------------
# Machine presence coded as M1_603X (1=has it, 2=doesn't have it).
# Machine ordering in endline (A-M matching baseline A-M, minus Moto):
#   A=germinador, B=almácigo, C=arvenses, D=motosierra, E=guadañadora,
#   F=ecomill, G=becolsub, H=despulpadora, I=desmucilaginador, J=tanque,
#   K=secador, L=elva/elda, M=camioneta.
# NOTE: "Moto" (M1_601N in baseline) has no equivalent in the endline survey.

e_machines <- e_sec0 %>%
  dplyr::select(NIM,
         M1_603A, M1_603B, M1_603C, M1_603D, M1_603E,
         M1_603F, M1_603G, M1_603H, M1_603I, M1_603J,
         M1_603K, M1_603L, M1_603M) %>%
  mutate(
    # 1=owns at least one unit, 2=doesn't own; NA treated as 0 (no machine)
    germinador       = replace_na(as.integer(M1_603A == 1), 0L),
    almacigo         = replace_na(as.integer(M1_603B == 1), 0L),
    arvenses         = replace_na(as.integer(M1_603C == 1), 0L),
    motosierra       = replace_na(as.integer(M1_603D == 1), 0L),
    Guadanadora      = replace_na(as.integer(M1_603E == 1), 0L),
    Ecomill          = replace_na(as.integer(M1_603F == 1), 0L),
    Becolsub         = replace_na(as.integer(M1_603G == 1), 0L),
    Despulpadora     = replace_na(as.integer(M1_603H == 1), 0L),
    Desmucilaginador = replace_na(as.integer(M1_603I == 1), 0L),
    Tanque           = replace_na(as.integer(M1_603J == 1), 0L),
    Secador          = replace_na(as.integer(M1_603K == 1), 0L),
    Elva             = replace_na(as.integer(M1_603L == 1), 0L),
    Camioneta        = replace_na(as.integer(M1_603M == 1), 0L),
    Moto             = NA_integer_          # not collected in endline
  ) %>%
  mutate(
    machines  = motosierra + Desmucilaginador + Elva + Guadanadora + Despulpadora,
    maquinaria = machines + Becolsub + Ecomill + Secador
  ) %>%
  dplyr::select(NIM, germinador, almacigo, arvenses, motosierra, Guadanadora,
         Ecomill, Becolsub, Despulpadora, Desmucilaginador, Tanque,
         Secador, Elva, Camioneta, Moto, machines, maquinaria)


# --- E4. Household demographics (from SEC1 identification matrix) ------------
# IMPORTANT SHIFT: in endline, M1_105 = age, M1_108 = sex (baseline was reversed).
# Head = person with MATRIZ_IDENTIFICACION__ID == 0.
# NOTE: M1_108 (sex) has ~86% NA because the question was only asked for
# non-respondent members; the respondent's sex needs external confirmation.
# Here we take sex from the first non-respondent adult with non-NA M1_108.

e_persons <- e_sec1 %>%
  dplyr::select(NIM, MATRIZ_IDENTIFICACION__ID, M1_105, M1_108, M1_112A1, M1_112A2) %>%
  rename(
    person_id = MATRIZ_IDENTIFICACION__ID,
    edad      = M1_105,     # age in years (ENDLINE: shifted from M1_106)
    sexo_code = M1_108,     # sex: 1=male, 2=female (ENDLINE: shifted from M1_105)
    edu_nivel = M1_112A1,   # education level (ENDLINE: shifted from M1_109A1)
    edu_anios = M1_112A2    # education years  (ENDLINE: shifted from M1_109B1)
  ) %>%
  mutate(
    sexo = as.integer(sexo_code == 1),
    # Same education coding as baseline; verify level labels against endline dictionary
    educacion = case_when(
      edu_nivel %in% c(2, 3) ~ replace_na(edu_anios, 0),
      edu_nivel %in% c(5, 8) ~ replace_na(edu_anios, 0) + 11L,
      edu_nivel == 9         ~ replace_na(edu_anios, 0) + 15L,
      edu_nivel == 7         ~ 0L,
      TRUE                   ~ 0L
    )
  )

# Sex was not re-collected in endline for the respondent (person_id == 0) and
# is largely missing for other members too.  Fill sex from the baseline
# identification matrix by matching on NIM (household) and person_id (member).
# This is valid for a panel survey where the same household members are tracked.

b_person_sex_bl <- b_sec1 %>%
  dplyr::select(NIM, M1_ID_DE_PERSONA, M1_105) %>%
  rename(person_id       = M1_ID_DE_PERSONA,
         sexo_code_bl    = M1_105)   # baseline sex code: 1=male, 2=female

e_persons <- e_persons %>%
  left_join(b_person_sex_bl, by = c("NIM", "person_id")) %>%
  mutate(
    sexo_code = coalesce(sexo_code, sexo_code_bl),   # fill NAs from baseline
    sexo      = as.integer(sexo_code == 1)           # recompute binary after fill
  ) %>%
  dplyr::select(-sexo_code_bl)

# Head sex: person_id == 0 (now populated from baseline via the merge above)
e_head_sex <- e_persons %>%
  filter(person_id == 0) %>%
  dplyr::select(NIM, sexo)

# Household size
e_hh_size <- e_persons %>%
  group_by(NIM) %>%
  summarise(
    Total_HH      = sum(edad >= 10, na.rm = TRUE),
    Total_HH_Kids = sum(edad <  10, na.rm = TRUE),
    .groups = "drop"
  )

# Demographic shares (same logic as baseline)
e_demog <- e_persons %>%
  mutate(
    Prime_Male    = if_else(edad > 18 & edad < 60 & sexo_code == 1, 1,
                    0.5 * as.numeric(edad >= 10 & edad < 60 & sexo_code == 1)),
    Elderly_Male  = as.numeric(edad >= 60 & sexo_code == 1),
    Prime_Female  = if_else(edad > 18 & edad < 60 & sexo_code == 2, 1,
                    0.5 * as.numeric(edad >= 10 & edad < 60 & sexo_code == 2)),
    Elderly_Female = as.numeric(edad >= 60 & sexo_code == 2)
  ) %>%
  group_by(NIM) %>%
  summarise(
    Prime_Male     = sum(Prime_Male,     na.rm = TRUE),
    Elderly_Male   = sum(Elderly_Male,   na.rm = TRUE),
    Prime_Female   = sum(Prime_Female,   na.rm = TRUE),
    Elderly_Female = sum(Elderly_Female, na.rm = TRUE),
    .groups = "drop"
  )

# Average household education
e_edu_avg <- e_persons %>%
  group_by(NIM) %>%
  summarise(Educacion_Promedio = mean(educacion, na.rm = TRUE), .groups = "drop")

# Cooperative membership (from SEC0, one value per farm — M1_120 in endline)
# M1_120: 1=member, 2=not member
e_coop <- e_sec0 %>%
  transmute(NIM, coop = as.integer(M1_120 == 1))


# --- E5. Land area (from SEC2 productive activities) -------------------------
# M1_403A = total farm area in HECTARES (endline already in ha, no unit code).
# Convert to m² (multiply by 10 000) to match the baseline metric.
# M1_403B = agricultural area in ha (use as cross-check or alternative measure).

e_land <- e_sec2 %>%
  dplyr::select(NIM, M1_403A) %>%
  mutate(Tierra_m2 = M1_403A * 10000) %>%   # ha → m²
  group_by(NIM) %>%
  summarise(Tierra = sum(Tierra_m2, na.rm = TRUE), .groups = "drop")


# --- E6. Certified coffee (from SEC3 agricultural inventory) -----------------
# M1_501 == 1 identifies the coffee crop row.
# M1_508D == 1 means certified coffee production.

e_cert <- e_sec3 %>%
  filter(M1_501 == 1) %>%    # keep only coffee rows
  group_by(NIM) %>%
  summarise(Certificado = as.integer(any(M1_508D == 1, na.rm = TRUE)),
            .groups = "drop")


# --- E6b. Coffee production (from SEC3 agricultural inventory) ---------------
# Unit codes: 1=125 kg, 2=12.5 kg, 3=kg, 4=60 kg (user-specified for endline).
# Revenue: M1_204 in e_sec0 is the total value of coffee production in COP,
# reported at farm level (one row per NIM in e_sec0 — the household-head answer).

e_production <- e_sec3 %>%
  filter(!is.na(M1_501) & M1_501 == 1) %>%
  transmute(
    NIM,
    prod_kg_row = as.numeric(M1_508A) * coffee_unit_to_kg(as.integer(M1_508C))
  ) %>%
  group_by(NIM) %>%
  summarise(
    production_kg = sum(prod_kg_row, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(production_kg = if_else(production_kg == 0, NA_real_, production_kg)) %>%
  left_join(
    e_sec0 %>% transmute(NIM, revenue_cop = as.numeric(M1_204)),
    by = "NIM"
  )

message("Endline production: ",
        sum(!is.na(e_production$production_kg)), " farms with kg data; ",
        sum(!is.na(e_production$revenue_cop)), " farms with revenue data")


# --- E7. Credit access (from SEC1 financial module) --------------------------
# M1_3103 == 1 identifies the financial-module respondent (one per household).
# Credit sources: each variable asks whether a loan from that source was active
# in the last 12 months.  Coding: 1 = Sí, 2 = No.
#
# Informal sources:
#   M1_329A  family / friend
#   M1_334   moneylender / gota-a-gota
#   M1_339   pawnshop
#   M1_344   agricultural-input supplier
#   M1_349   chain store / supermarket
#
# Formal sources:
#   M1_354   cooperative or coffee-producer association
#   M1_359   financial cooperative (Huila / Tolima)
#   M1_364   microfinance institution
#   M1_369   Banco Agrario
#   M1_374   commercial bank (Davivienda, Bancolombia, etc.)
#
# NOTE: ~10% of households have no financial-module respondent (M1_3103);  
# those will retain NA for credit variables after the left_join.

e_credit <- e_sec1_active %>%
  transmute(
    NIM,
    inf_person = any_yes(as.integer(M1_329A == 1), as.integer(M1_334 == 1),
                         as.integer(M1_339  == 1), as.integer(M1_344 == 1),
                         as.integer(M1_349  == 1)),
    for_person = any_yes(as.integer(M1_354  == 1), as.integer(M1_359 == 1),
                         as.integer(M1_364  == 1), as.integer(M1_369 == 1),
                         as.integer(M1_374  == 1),as.integer(M1_379  == 1))
  ) %>%
  group_by(NIM) %>%
  summarise(
    tiene_credito_informal = max_hh(inf_person),
    tiene_credito_formal   = max_hh(for_person),
    .groups = "drop"
  ) %>%
  mutate(
    # [FIX-17] Identical treatment to the baseline. See the long note in B7.
    credit_module_missing  = as.integer(is.na(tiene_credito_informal) &
                                          is.na(tiene_credito_formal)),
    tiene_credito_informal = replace_na(tiene_credito_informal, 0L),
    tiene_credito_formal   = replace_na(tiene_credito_formal,   0L),
    tiene_credito          = as.integer(tiene_credito_informal == 1 |
                                          tiene_credito_formal   == 1),
    status                 = credit_status(tiene_credito_formal,
                                           tiene_credito_informal)
  )

# --- E8. Farm age and technical assistance -----------------------------------
# Farm age: no establishment-year variable exists in the endline SEC2;
# this variable cannot be constructed for the endline.
# Technical assistance: M1_701 in SEC0.
#   "¿En los últimos 12 meses usted recibió asistencia o asesoría para el
#    desarrollo de las actividades agropecuarias?"  1=Sí, 2=No.
# NOTE: M1_700A–M1_700E are government-support programme questions (FINAGRO,
# FAG, etc.) and are NOT equivalent to technical assistance.

e_tech_assist <- e_sec0 %>%
  transmute(NIM, Asistencia_tecnica = as.integer(M1_701 == 1))


# --- E9. Merge all endline pieces -------------------------------------------

endline_clean <- e_labour %>%
  left_join(e_machines,    by = "NIM") %>%
  left_join(e_hh_size,     by = "NIM") %>%
  left_join(e_demog,       by = "NIM") %>%
  left_join(e_edu_avg,     by = "NIM") %>%
  left_join(e_head_sex,    by = "NIM") %>%
  left_join(e_coop,        by = "NIM") %>%
  left_join(e_land,        by = "NIM") %>%
  left_join(e_cert,        by = "NIM") %>%
  left_join(e_production,  by = "NIM") %>%
  left_join(e_credit,      by = "NIM") %>%
  left_join(e_tech_assist, by = "NIM") %>%
  mutate(
    across(c(Prime_Male, Elderly_Male, Prime_Female, Elderly_Female), ~ replace_na(.x, 0)),
    prime_male_share     = if_else(Total_HH > 0, Prime_Male     / Total_HH, 0),
    prime_female_share   = if_else(Total_HH > 0, Prime_Female   / Total_HH, 0),
    elderly_male_share   = if_else(Total_HH > 0, Elderly_Male   / Total_HH, 0),
    elderly_female_share = if_else(Total_HH > 0, Elderly_Female / Total_HH, 0),
    no_tiene_credito     = 1L - replace_na(tiene_credito, 0L),
    Certificado          = replace_na(Certificado, 0L),
    farms_antiquity      = NA_real_,   # not available in endline
    wave                 = 2L,
    MUNICIPIO            = MPIO
  ) %>%
  dplyr::select(-MPIO)

message("Endline: ", nrow(endline_clean), " farm-level observations")


# ==============================================================================
# PART 3 — STACK BASELINE AND ENDLINE
# ==============================================================================
# Align columns: baseline has DEPARTAMENTO→DPTO; endline has DPTO directly.
# All columns are renamed to a common schema before binding.


# Column union: both data frames share the same column schema after cleaning
data_clean <- bind_rows(baseline_clean, endline_clean) %>%
  # Final NA-filling for variables that are structurally absent in one wave
  mutate(
    # Derived interactions (used in regressions)
    tiene_credito    = replace_na(tiene_credito, 0L),
    no_tiene_credito = 1L - tiene_credito
  ) %>%
  arrange(NIM, wave)

message("Combined dataset: ", nrow(data_clean), " observations")
message("Baseline: ", sum(data_clean$wave == 1), " | Endline: ", sum(data_clean$wave == 2))

# ==============================================================================
# PART 4 — ADD TREATMENT TAG
# ==============================================================================

key <- read_xlsx("Data/Endline/key.xlsx")

data_clean <- data_clean %>% left_join(key, by="NIM")

# ==============================================================================
# PART 5 — SAVE THE DATASET
# ==============================================================================

write_xlsx(data_clean, "Data/data.xlsx")


# ==============================================================================
# PART 6 — INDIVIDUAL × WAVE LABOR PANEL  (for Act 2b sector-choice models)
#
#  Produces Data/labor_panel.xlsx — one row per household member × month
#  (baseline) or per household member (endline, no monthly grid available).
#
#  BASELINE (ARD607_SEC2_MATRIZ_INGRESOS.xlsx):
#    - M1_200 : person identifier within household
#    - M1_201 : primary sector (1=agriculture, 2=non-ag, 99=inactive)
#    - M1_203 : employment type (1=seasonal, 2=full-time, 3=occasional, 5=out)
#    - M1_204B[A-L] : monthly flag for seasonal primary activity
#    - M1_207B[A-L] : monthly flag for full-time/hired primary activity
#    - M1_209B[A-L] : monthly flag for occasional primary activity
#    - Same schema with ".2." infix for secondary activity
#    - Expanded to 12 months per member row
#
#  ENDLINE (A634_SEC_1_MATRIZ_IDENTIFICACION.xlsx):
#    KEY LIMITATION: the monthly work grid was removed from the endline.
#    Available: M1_201 (sector flag), M1_215A1 (months in non-ag activity).
#    → We create one member-level row per wave (no month dimension).
#    Act 2b in econometrics.R handles this asymmetry explicitly.
# ==============================================================================

# --- P6a. Baseline: member × month panel -------------------------------------

b_sec2_ingresos <- read_excel("Data/Baseline/ARD607_SEC2_MATRIZ_INGRESOS.xlsx")

# Helper: classify each member-month as "Agro", "Non Agro", or "Sin Trabajo"
# by reading the appropriate monthly dummy column for the given employment type.
classify_month_sector <- function(df, month_vec, sector_col, type_col,
                                  prefix_seas, prefix_occ, prefix_hired) {
  suffixes <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")

  make_flag <- function(prefix) {
    cols <- paste0(prefix, suffixes)
    cols_ok <- intersect(cols, names(df))
    if (length(cols_ok) == 0) return(rep(0L, nrow(df)))
    mat <- as.matrix(df[, cols_ok, drop = FALSE])
    if (ncol(mat) < 12) {
      mat <- cbind(mat, matrix(0L, nrow(df), 12 - ncol(mat)))
    }
    idx <- cbind(seq_len(nrow(df)), month_vec)
    replace_na(as.integer(mat[idx] == 1), 0L)
  }

  sector <- df[[sector_col]]
  ptype  <- replace_na(df[[type_col]], 0L)
  seas   <- make_flag(prefix_seas)
  occ    <- make_flag(prefix_occ)
  hired  <- make_flag(prefix_hired)

  label   <- rep("Sin Trabajo", nrow(df))
  is_ag   <- !is.na(sector) & sector == 1
  is_nag  <- !is.na(sector) & sector == 2

  # Agricultural sector
  label[is_ag & ptype %in% c(2, 3)] <- "Agro"
  label[is_ag & ptype == 1] <- if_else(
    seas[is_ag & ptype == 1] == 1, "Agro", "Sin Trabajo")
  label[is_ag & ptype == 4] <- if_else(
    occ[is_ag & ptype == 4] == 1, "Agro", "Sin Trabajo")

  # Non-agricultural sector
  label[is_nag & ptype %in% c(2, 3)] <- if_else(
    hired[is_nag & ptype %in% c(2, 3)] == 1, "Non Agro", "Sin Trabajo")
  label[is_nag & ptype == 1] <- if_else(
    seas[is_nag & ptype == 1] == 1, "Non Agro", "Sin Trabajo")
  label[is_nag & ptype == 4] <- if_else(
    occ[is_nag & ptype == 4] == 1, "Non Agro", "Sin Trabajo")

  label
}

# Demographics (age, sex, education) are in SEC1, not SEC2.
# Join by NIM + person_id (M1_200 in SEC2 matches M1_ID_DE_PERSONA in SEC1).
b_sec1_demog <- b_sec1 %>%
  transmute(
    NIM,
    person_id = M1_ID_DE_PERSONA,
    edad      = M1_106,
    sexo_code = M1_105,
    educacion = case_when(
      M1_109A1 %in% c(2, 3) ~ replace_na(as.numeric(M1_109B1), 0),
      M1_109A1 %in% c(5, 8) ~ replace_na(as.numeric(M1_109B1), 0) + 11,
      M1_109A1 == 9         ~ replace_na(as.numeric(M1_109B1), 0) + 15,
      TRUE                  ~ 0
    )
  )

b_labor_wide <- b_sec2_ingresos %>%
  # NOTE: M1_200 in SEC2 is the NUMBER OF ACTIVITIES (1, 2, or NA) — NOT the
  # person identifier.  The real person ID is M1_ID_DE_PERSONAS_SEC02 (0–9).
  # Keep all 2,239 person rows; deduplicate by actual person ID in case a
  # person appears twice (one row with data, one blank).
  group_by(NIM, M1_ID_DE_PERSONAS_SEC02) %>%
  arrange(is.na(M1_201), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    person_id = M1_ID_DE_PERSONAS_SEC02,
    M1_201    = replace_na(M1_201, 99L),
    M1_201.2  = replace_na(M1_201.2, 99L),
    M1_203    = replace_na(M1_203, 0L),
    M1_203.2  = replace_na(M1_203.2, 0L)
  ) %>%
  left_join(b_sec1_demog, by = c("NIM", "person_id"))

# Expand each person row to 12 month rows via a merge with a month table.
# merge(..., by = character()) is a SQL-style CROSS JOIN in base R.
months_df   <- data.frame(mes = 1L:12L)
b_labor_long <- merge(b_labor_wide, months_df, by = character()) %>%
  as_tibble() %>%
  mutate(
    sector_principal = classify_month_sector(
      pick(everything()), mes, "M1_201", "M1_203",
      "M1_204B", "M1_209B", "M1_207B"
    ),
    sector_secundario = classify_month_sector(
      pick(everything()), mes, "M1_201.2", "M1_203.2",
      "M1_204.2.B", "M1_209.2.B", "M1_207.2.B"
    ),
    # Off-farm = 1 if non-ag in either primary or secondary activity this month
    offfarm        = as.integer(
      sector_principal == "Non Agro" | sector_secundario == "Non Agro"
    ),
    months_offfarm = NA_real_,
    wave           = 1L
  ) %>%
  dplyr::select(NIM, person_id, mes, edad, sexo_code, educacion,
         sector_principal, sector_secundario, offfarm, months_offfarm, wave)

message("Baseline labor panel: ", nrow(b_labor_long), " member-month rows")


# --- P6b. Endline: two-source labour panel ------------------------------------
#
# The endline survey split labour questions between two files:
#
#  SOURCE 1 — FARM HEAD (person_id = 0)  →  e_sec0
#    M1_200   : number of jobs the respondent holds (1 or 2)
#    M1_201   : primary job sector  (1 = Agro, else = Non Agro)
#    M1_217   : secondary job sector (1 = Agro, else = Non Agro; only if M1_200 == 2)
#    M1_215A1 : months worked in last year  (if available; mostly sparse)
#    M1_215A2 : time unit — 1 = days, 2 = months
#    Demographics pulled from e_sec1 for MATRIZ_IDENTIFICACION__ID == 0.
#    Monthly grid: 12 rows per head using harvest-season sector-switching rules.
#
#  SOURCE 2 — OTHER HOUSEHOLD MEMBERS (person_id > 0)  →  e_sec1
#    M1_113   : worked/contributed to household income last year (1 = yes, 2 = no)
#    M1_114   : activity sector (1 = Agro, else = Non Agro)
#    No time data available → same sector assigned to all 12 months.
#    Demographics already in e_sec1.

# ── Source 1: Head (person_id = 0) ──────────────────────────────────────────

# Sector info for the head from SEC0
e_head_sector <- e_sec0 %>%
  transmute(
    NIM,
    DPTO,
    person_id = 0L,
    n_jobs    = M1_200,
    sector_j1 = case_when(
      is.na(M1_201) ~ NA_character_,
      M1_201 == 1   ~ "Agro",
      TRUE          ~ "Non Agro"
    ),
    sector_j2 = case_when(
      replace_na(as.integer(n_jobs), 0L) != 2L ~ NA_character_,
      is.na(M1_217)                            ~ NA_character_,
      M1_217 == 1                              ~ "Agro",
      TRUE                                     ~ "Non Agro"
    ),
    # Months worked off-farm: M1_215A2 = 1 (days → ÷30), 2 (months direct)
    months_nag_raw = case_when(
      is.na(M1_215A1)  ~ NA_real_,
      M1_215A2 == 2    ~ as.numeric(M1_215A1),
      M1_215A2 == 1    ~ round(as.numeric(M1_215A1) / 30, 1),
      TRUE             ~ NA_real_
    )
  )

# Demographics for head from SEC1
e_head_demog <- e_sec1 %>%
  filter(MATRIZ_IDENTIFICACION__ID == 0) %>%
  transmute(
    NIM,
    edad      = M1_105,
    sexo_code = M1_108,
    educacion = case_when(
      M1_112A1 %in% c(2, 3) ~ replace_na(as.numeric(M1_112A2), 0),
      M1_112A1 %in% c(5, 8) ~ replace_na(as.numeric(M1_112A2), 0) + 11,
      M1_112A1 == 9         ~ replace_na(as.numeric(M1_112A2), 0) + 15,
      TRUE                  ~ 0
    )
  )

e_labor_head <- e_head_sector %>%
  left_join(e_head_demog, by = "NIM") %>%
  # Fill sex from baseline roster where endline M1_108 is missing
  left_join(
    b_sec1 %>% transmute(NIM, person_id = M1_ID_DE_PERSONA, sexo_bl = M1_105),
    by = c("NIM", "person_id")
  ) %>%
  mutate(sexo_code = coalesce(sexo_code, sexo_bl)) %>%
  dplyr::select(-sexo_bl) %>%
  # Expand to 12-month grid
  merge(months_df, by = character()) %>%
  as_tibble() %>%
  mutate(
    harvest_e = case_when(
      DPTO == "HUILA"  & mes %in% c(4, 5, 9, 10, 11, 12)  ~ 1L,
      DPTO == "TOLIMA" & mes %in% c(1, 3, 4, 5, 6, 10, 11) ~ 1L,
      TRUE ~ 0L
    ),
    sector_principal = case_when(
      is.na(n_jobs)                                                                     ~ NA_character_,
      n_jobs == 1                                                                       ~ sector_j1,
      n_jobs == 2 & sector_j1 == "Agro"                                                ~ "Agro",
      n_jobs == 2 & sector_j1 == "Non Agro" & sector_j2 == "Agro" & harvest_e == 1L   ~ "Agro",
      n_jobs == 2 & sector_j1 == "Non Agro" & sector_j2 == "Agro" & harvest_e == 0L   ~ "Non Agro",
      n_jobs == 2 & sector_j1 == "Non Agro"                                            ~ "Non Agro",
      TRUE                                                                              ~ NA_character_
    ),
    sector_secundario = if_else(is.na(sector_principal), NA_character_, "Sin Trabajo"),
    offfarm           = as.integer(sector_principal == "Non Agro"),
    wave              = 2L
  ) %>%
  group_by(NIM, person_id) %>%
  mutate(
    # Use M1_215A1 directly when recorded; otherwise count Non Agro months from grid
    months_offfarm = if_else(
      !is.na(months_nag_raw),
      months_nag_raw,
      as.numeric(sum(offfarm, na.rm = TRUE))
    )
  ) %>%
  ungroup() %>%
  dplyr::select(NIM, person_id, mes, edad, sexo_code, educacion,
                sector_principal, sector_secundario, offfarm, months_offfarm, wave)

# ── Source 2: Other household members (person_id > 0) ───────────────────────
# M1_113: 1 = worked/contributed to household income, 2 = did not.
# M1_114: sector code — 1 = Agropecuario (Agro), any other non-NA = Non Agro.
# No time granularity available → if they worked, apply that sector for all 12 months.

e_labor_members <- e_sec1 %>%
  filter(!is.na(MATRIZ_IDENTIFICACION__ID),
         MATRIZ_IDENTIFICACION__ID != 0) %>%
  transmute(
    NIM,
    person_id = MATRIZ_IDENTIFICACION__ID,
    edad      = M1_105,
    sexo_code = M1_108,
    educacion = case_when(
      M1_112A1 %in% c(2, 3) ~ replace_na(as.numeric(M1_112A2), 0),
      M1_112A1 %in% c(5, 8) ~ replace_na(as.numeric(M1_112A2), 0) + 11,
      M1_112A1 == 9         ~ replace_na(as.numeric(M1_112A2), 0) + 15,
      TRUE                  ~ 0
    ),
    worked = M1_113,   # 1 = yes, 2 = no
    sector = M1_114,   # 1 = Agro, other = Non Agro
    wave   = 2L
  ) %>%
  # Fill sex from baseline roster where endline M1_108 is missing
  left_join(
    b_sec1 %>% transmute(NIM, person_id = M1_ID_DE_PERSONA, sexo_bl = M1_105),
    by = c("NIM", "person_id")
  ) %>%
  mutate(sexo_code = coalesce(sexo_code, sexo_bl)) %>%
  dplyr::select(-sexo_bl) %>%
  # Expand to 12-month grid (same sector assigned to all 12 months)
  merge(months_df, by = character()) %>%
  as_tibble() %>%
  mutate(
    sector_principal = case_when(
      replace_na(worked, 2L) == 2L ~ "Sin Trabajo",  # did not work
      sector == 1                   ~ "Agro",
      !is.na(sector)               ~ "Non Agro",
      TRUE                          ~ NA_character_
    ),
    sector_secundario = if_else(is.na(sector_principal), NA_character_, "Sin Trabajo"),
    offfarm           = as.integer(sector_principal == "Non Agro"),
    # All 12 months assumed in the same sector → months_offfarm is 12 or 0
    months_offfarm    = if_else(offfarm == 1L, 12, 0)
  ) %>%
  dplyr::select(NIM, person_id, mes, edad, sexo_code, educacion,
                sector_principal, sector_secundario, offfarm, months_offfarm, wave)

# ── Combine both sources ─────────────────────────────────────────────────────
e_labor_long <- bind_rows(e_labor_head, e_labor_members)

message("Endline labor panel: ", nrow(e_labor_long), " member-month rows (",
        n_distinct(paste(e_labor_long$NIM, e_labor_long$person_id)),
        " members x 12 months)")
message("  Off-farm member-months: ",
        sum(e_labor_long$offfarm == 1L, na.rm = TRUE))


# --- P6c. Attach farm-level controls and stack --------------------------------
# Use wave-matched farm controls so that capital, credit, and treatment
# status correspond to the same period as the labor observation.

farm_controls_lp <- data_clean %>%
  dplyr::select(NIM, wave, machines, Tierra, Total_HH,
         tiene_credito, no_tiene_credito, Grupo_Tratamiento,
         MUNICIPIO, DPTO,
         prime_male_share, prime_female_share,
         elderly_male_share, elderly_female_share,
         Educacion_Promedio, Certificado, Asistencia_tecnica)

labor_panel <- bind_rows(b_labor_long, e_labor_long) %>%
  left_join(farm_controls_lp, by = c("NIM", "wave")) %>%
  mutate(
    # Harvest-season dummy (Huila: Apr-May, Sep-Dec; Tolima: Jan, Mar-Jun, Oct-Nov)
    harvest_season = case_when(
      DPTO == "HUILA"  & mes %in% c(4, 5, 9, 10, 11, 12)   ~ 1L,
      DPTO == "TOLIMA" & mes %in% c(1, 3, 4, 5, 6, 10, 11)  ~ 1L,
      is.na(mes)                                             ~ NA_integer_,
      TRUE                                                   ~ 0L
    ),
    post              = as.integer(wave == 2L),
    treated           = as.integer(Grupo_Tratamiento == "Tratamiento"),
    # Unique member identifier for panel FE
    member_uid        = paste0(NIM, "_", person_id)
  ) %>%
  arrange(NIM, person_id, wave, mes)

message("Combined labor panel:")
message("  Baseline member-months : ", sum(labor_panel$wave == 1, na.rm = TRUE))
message("  Endline members        : ", sum(labor_panel$wave == 2, na.rm = TRUE))
message("  Total rows             : ", nrow(labor_panel))


# --- P6d. Save ----------------------------------------------------------------

write_xlsx(labor_panel, "Data/labor_panel.xlsx")
message("Saved: Data/labor_panel.xlsx")
