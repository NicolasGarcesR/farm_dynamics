####################################################################################################
#                 MARKET FAILURES, TASK-BASED PRODUCTION MODEL AND THE                               #
#                         AGRICULTURAL PRODUCTIVITY GAP                                              #
#                                                                                                    #
#  Author  : Nicolás Garcés Rodríguez                                                                #
#  Contact : nicolas.garces@urosario.edu.co                                                          #
#  Affil.  : Universidad del Rosario — Facultad de Economía                                          #                                                                    #
#  Last update: 13/06/2023                                                                              #
#                                                                                                    #
#  Description:                                                                                      #
#    Master script for the empirical analysis of factor-market failures in                            #
#    Colombian coffee production under a task-based production framework.                             #
#    Uses FNC-EFI coffee-producer survey data to estimate recursion tests,                            #
#    double-hurdle models for machinery adoption, and sectoral labor-                                 #
#    allocation logit/probit models.                                                                  #
#                                                                                                    #
#  Data:                                                                                             #
#    Seven CSV/XLSX files from the FNC-EFI Coffee Producers Survey.                                  #
#    Access upon request to Alianza EFI principal investigators.                                      #
#                                                                                                    #
#  Replication:                                                                                      #
#    Fully replicable with raw data files placed in the working directory.                            #
####################################################################################################

# ╔════════════════════════════════════════════════════════════════════════════╗
# ║                     0.  ENVIRONMENT & CONFIGURATION                      ║
# ╚════════════════════════════════════════════════════════════════════════════╝

## 0.1  Clean slate -----------------------------------------------------------
rm(list = ls())
gc()
options(
  scipen   = 999,
  digits   = 4,
  warn     = 1,
  dplyr.summarise.inform = FALSE
)

## 0.2  Package management via pacman -----------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  # Core data wrangling
  tidyverse, readxl, data.table,
  # DEA & productivity
  rDEA, productivity, Benchmarking,
  # Econometric models
  sampleSelection, nnet, mhurdle, systemfit,
  # Inference & diagnostics
  lmtest, sandwich, miceadds, car, robustbase,
  # Presentation & output
  stargazer, modelsummary, kableExtra,
  # Correlation & partial correlation
  GGally, ppcor, corpcor,
  # Visualisation
  ggthemes, scales, patchwork,
  # Reproducibility
  here, tictoc
)

## 0.3  Custom ggplot2 theme for publication-quality figures -------------------
theme_thesis <- function(base_size = 11, base_family = "serif") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.title       = element_text(face = "bold", size = rel(1.2),
                                      hjust = 0, margin = margin(b = 8)),
      plot.subtitle    = element_text(colour = "grey40", size = rel(0.9),
                                      hjust = 0, margin = margin(b = 10)),
      plot.caption     = element_text(colour = "grey50", size = rel(0.7),
                                      hjust = 1),
      axis.title       = element_text(face = "bold", size = rel(0.85)),
      axis.text        = element_text(colour = "grey30"),
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", size = rel(0.8)),
      strip.text       = element_text(face = "bold")
    )
}
theme_set(theme_thesis())

## 0.4  Custom colour palette -------------------------------------------------
palette_thesis <- c(
  primary   = "#2C3E50",
  accent    = "#E74C3C",
  secondary = "#3498DB",
  muted     = "#95A5A6",
  highlight = "#F39C12"
)

## 0.5  Working directory (project root) --------------------------------------
setwd("~/Desktop/DATOS_FINAL")

# ╔════════════════════════════════════════════════════════════════════════════╗
# ║               1.  REUSABLE UTILITY FUNCTIONS                             ║
# ╚════════════════════════════════════════════════════════════════════════════╝

#' Inverse Hyperbolic Sine transformation (IHS)
#' Approximates log(x) for large x but is defined at zero and negative values.
#' @param x Numeric vector
#' @return  Numeric vector: log(x + sqrt(x^2 + 1))
ihs <- function(x) log(x + sqrt(x^2 + 1))

#' Safe division: returns 0 instead of NaN/Inf when denominator is zero
#' @param num Numerator
#' @param den Denominator
#' @return   num / den, with 0 where den == 0
safe_divide <- function(num, den) {
  ifelse(den == 0 | is.na(den), 0, num / den)
}

#' Unit conversion factor for land area → square metres
#' Codebook: 1 = m², 2 = ha (10 000 m²), 3 = cuadras (6 400 m²),
#'           4 = plazas (3 930 m²)
#' @param code Integer unit code from the survey
#' @return     Numeric conversion factor
land_unit_to_m2 <- function(code) {
  dplyr::recode(code, `1` = 1, `2` = 10000, `3` = 6400, `4` = 3930, .default = NA_real_)
}

#' Education years from survey codes
#' Maps (level, years) pairs to a scalar for total schooling.
#' @param level Survey code for education level (M1_109A1)
#' @param years Reported years completed   (M1_109B1)
#' @return      Integer: total years of education
compute_education <- function(level, years) {
  case_when(
    level %in% c(2, 3) ~ years,
    level %in% c(5, 8) ~ years + 11L,
    level == 9          ~ years + 15L,
    level == 7          ~ 0L,
    TRUE                ~ 0L
  )
}

#' Robust coefficient test wrapper
#' @param model  lm / glm object
#' @param hc     HC type (default "HC3")
#' @param cl     Cluster formula (e.g. ~MUNICIPIO)
#' @return       coeftest object
robust_coeftest <- function(model, hc = "HC3", cl = NULL) {
  v <- vcovHC(model, type = hc)
  lmtest::coeftest(model, vcov = v, cluster = cl)
}

#' Run F-test (linear hypothesis) with White-robust variance
#' @param model  Model object
#' @param hyp    Character vector of hypotheses
#' @param adjust HC adjustment type (default "hc1")
#' @return       linearHypothesis result
robust_ftest <- function(model, hyp, adjust = "hc1") {
  car::linearHypothesis(model, hyp, white.adjust = adjust)
}

#' Build a standard formula for the recursion-test regressions
#' @param dep      Character: dependent variable expression
#' @param controls Character vector of additional control terms
#' @return         formula object
build_recursion_formula <- function(dep, controls = NULL) {
  hh_vars <- c(
    "tiene_credito:ihs(Total_HH)",
    "tiene_credito:ihs(prime_male_share)",
    "tiene_credito:ihs(prime_female_share)",
    "tiene_credito:ihs(elderly_female_share)",
    "tiene_credito:ihs(elderly_male_share)",
    "no_tiene_credito:ihs(Total_HH)",
    "no_tiene_credito:ihs(prime_male_share)",
    "no_tiene_credito:ihs(prime_female_share)",
    "no_tiene_credito:ihs(elderly_female_share)",
    "no_tiene_credito:ihs(elderly_male_share)"
  )
  base_rhs <- c("ihs(Tierra)", "ihs(machines)", hh_vars,
                 "sexo", "dist_2_bank", "Certificado",
                 "Educacion_Promedio", "farms_antiquity",
                 "factor(MUNICIPIO)")
  rhs <- paste(c(base_rhs, controls), collapse = " + ")
  as.formula(paste(dep, "~", rhs))
}

#' Vectorised monthly-sector lookup
#' Given a month index and a set of 12 monthly indicator columns (A..L),
#' return the value for the correct month without any row-wise loop.
#' @param df      Data frame
#' @param month   Integer vector (1–12) indicating the calendar month
#' @param prefix  Column prefix (e.g. "M1_204B")
#' @return        Integer vector with the looked-up values
vectorised_month_lookup <- function(df, month, prefix) {
  suffixes <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")
  col_names <- paste0(prefix, suffixes)
  # Build a matrix of the 12 columns, then index by month
  mat <- as.matrix(df[, col_names, drop = FALSE])
  idx <- cbind(seq_len(nrow(df)), month)
  mat[idx]
}

#' Determine labour sector for a given activity (vectorised)
#' Replaces the original ~300-line row-by-row for-loop.
#' @param df            Data frame with survey variables
#' @param month_col     Name of month column (integer 1–12)
#' @param sector_col    M1_201 / M1_201.2  (1 = agro)
#' @param type_col      M1_203 / M1_203.2  (1–5 periodicity type)
#' @param prefix_seas   Prefix for seasonal columns (e.g. "M1_204B")
#' @param prefix_occ    Prefix for occasional columns (e.g. "M1_209B")
#' @param prefix_hired  Prefix for hired columns      (e.g. "M1_207B")
#' @return              Character vector: "Agro", "Non Agro", "Sin Trabajo", "out"
classify_sector_vectorised <- function(df, month_col,
                                       sector_col, type_col,
                                       prefix_seas, prefix_occ,
                                       prefix_hired) {
  month  <- df[[month_col]]
  sector <- df[[sector_col]]
  ptype  <- df[[type_col]]

  # Precompute monthly lookups for each column family
  lookup_seas  <- vectorised_month_lookup(df, month, prefix_seas)
  lookup_occ   <- vectorised_month_lookup(df, month, prefix_occ)
  lookup_hired <- vectorised_month_lookup(df, month, prefix_hired)

  result <- rep(NA_character_, nrow(df))

  # --- Agro sector (sector == 1) ---
  is_agro <- (sector == 1)

  # Full-time / permanent (type 2 or 3): always "Agro"
  result[is_agro & ptype %in% c(2, 3)] <- "Agro"

  # Seasonal (type 1): check monthly indicator
  idx <- is_agro & ptype == 1
  result[idx] <- ifelse(lookup_seas[idx] == 1, "Agro", "Sin Trabajo")

  # Occasional (type 4): check monthly indicator
  idx <- is_agro & ptype == 4
  result[idx] <- ifelse(lookup_occ[idx] == 1, "Agro", "Sin Trabajo")

  # Retired / other (type 5)
  result[is_agro & ptype == 5] <- "out"

  # --- Non-Agro sector (sector != 1) ---
  is_non <- (!is_agro & !is.na(sector) & sector != 0)

  # Seasonal (type 1)
  idx <- is_non & ptype == 1
  result[idx] <- ifelse(lookup_seas[idx] == 1, "Non Agro", "Sin Trabajo")

  # Full-time / permanent (type 2 or 3)
  idx <- is_non & ptype %in% c(2, 3)
  result[idx] <- ifelse(lookup_hired[idx] == 1, "Non Agro", "Sin Trabajo")

  # Occasional (type 4)
  idx <- is_non & ptype == 4
  result[idx] <- ifelse(lookup_occ[idx] == 1, "Non Agro", "Sin Trabajo")

  # Remaining non-agro
  result[is_non & is.na(result)] <- "out"

  # sector == 0 or NA → Sin Trabajo

  result[is.na(sector) | sector == 0] <- "Sin Trabajo"
  result[is.na(result)] <- "Sin Trabajo"

  result
}

#' Combine primary and secondary activity into a single monthly label
#' @param primary   Character vector of primary activity
#' @param secondary Character vector of secondary activity
#' @return          Character: "Agro", "Non Agro", "Hybrid", "Sin Trabajo", "out"
combine_sector_labels <- function(primary, secondary) {
  case_when(
    primary == "Agro"        & secondary == "Agro"        ~ "Agro",
    primary == "Non Agro"    & secondary == "Non Agro"    ~ "Non Agro",
    primary == "Agro"        & secondary == "Non Agro"    ~ "Hybrid",
    primary == "Non Agro"    & secondary == "Agro"        ~ "Hybrid",
    primary == "Agro"        & secondary == "Sin Trabajo" ~ "Agro",
    primary == "Sin Trabajo" & secondary == "Agro"        ~ "Agro",
    primary == "Non Agro"    & secondary == "Sin Trabajo" ~ "Non Agro",
    primary == "Sin Trabajo" & secondary == "Non Agro"    ~ "Non Agro",
    primary == "Agro"        & secondary == "out"         ~ "Agro",
    primary == "out"         & secondary == "Agro"        ~ "Agro",
    primary == "Non Agro"    & secondary == "out"         ~ "Non Agro",
    primary == "out"         & secondary == "Non Agro"    ~ "Non Agro",
    primary == "Sin Trabajo" & secondary == "Sin Trabajo" ~ "Sin Trabajo",
    primary == "Sin Trabajo" & secondary == "out"         ~ "Sin Trabajo",
    primary == "out"         & secondary == "Sin Trabajo" ~ "Sin Trabajo",
    primary == "out"         & secondary == "out"         ~ "out",
    TRUE                                                  ~ "danger"
  )
}

#' Determine harvest season by department & month (vectorised)
#' @param dept  Character vector of department names
#' @param month Integer vector (1–12)
#' @return      Integer 0/1
is_crop_season <- function(dept, month) {
  huila_months  <- c(4, 5, 9, 10, 11, 12)
  tolima_months <- c(1, 3, 4, 5, 6, 10, 11)
  case_when(
    dept == "HUILA" & month %in% huila_months  ~ 1L,
    dept == "TOLIM" & month %in% tolima_months ~ 1L,
    TRUE                                       ~ 0L
  )
}

# ╔════════════════════════════════════════════════════════════════════════════╗
# ║                      2.  DATA LOADING                                      ║
# ╚════════════════════════════════════════════════════════════════════════════╝

message("── Loading raw data ────────────────────────────────────────────────")
tic("Data loading")

personas   <- read_excel("ARD607_SEC1_MATRIZ_IDENTIFICACION.xlsx")
d_lineal   <- read_excel("ARD607_SEC0_DATOS_LINEALES.xlsx")
fin        <- read_excel("ARD607_SEC3_MATRIZ_FINANCIERA.xlsx")
inf        <- read_excel("ARD607_SEC3_MATRIZ_FINANCIERA.xlsx")
credit     <- read_excel("credit.xlsx")
inver_agro <- read_excel("ARD607_SEC4_MATRIZ_ACT_PRODUC_INV_AGROPE_FAC_PRODUC.xlsx")
inv_agro   <- read_excel("ARD607_SEC5_MATRIZ_INV_AGROPECUARIO.xlsx")
id         <- read_excel("ARD607_SEC1_MATRIZ_IDENTIFICACION.xlsx")

toc()

# ╔════════════════════════════════════════════════════════════════════════════╗
# ║                      3.  DATA MANAGEMENT                                 ║
# ╚════════════════════════════════════════════════════════════════════════════╝

# ── 3.1  Labour variables ────────────────────────────────────────────────────

l <- d_lineal %>%
  select(NIM, starts_with("M1_")) %>%
  mutate(
    permanente         = M1_602A * 2496,
    temporal_tiempo    = if_else(M1_604A == 1, 8, 192),
    temporal           = M1_603 * temporal_tiempo * M1_604B,
    temporal_secu_tiempo = if_else(M1_601C2 == 1, 8, 192),
    secu               = M1_601C1 * temporal_secu_tiempo * M1_601C3,
    fam                = M1_601D1 * 2496,
    fam_secu_tiempo    = if_else(M1_601E2 == 1, 8, 192),
    fam_secu           = M1_601E1 * fam_secu_tiempo * M1_601E3,
    fam_tiempo         = if_else(M1_601F2 == 1, 8, 192),
    fam_otra           = M1_601F1 * fam_tiempo * M1_601F3,
    trabajadores       = rowSums(across(starts_with("M1_")), na.rm = TRUE),
    labor              = permanente + temporal + secu + fam + fam_secu + fam_otra,
    non_harvest        = permanente + fam,
    harvest            = temporal + secu + fam_secu + fam_otra
  ) %>%
  replace_na(list(labor = 0)) %>%
  filter(labor != 0) %>%
  select(NIM, permanente, temporal, secu, fam, fam_secu, fam_otra,
         trabajadores, labor, non_harvest, harvest)

# ── 3.2  Household demographics ─────────────────────────────────────────────

ide <- id %>%
  select(NIM, M1_ID_DE_PERSONA, M1_105, M1_106, M1_100A, M1_109A1, M1_109B1, M1_115) %>%
  mutate(
    sexo               = if_else(M1_105 == 1, "Hombre", "Mujer"),
    edad               = M1_106,
    contribuye_ingresos = if_else(M1_100A == 1, "Si", "No"),
    educacion          = compute_education(M1_109A1, M1_109B1),
    cooperativa_local  = if_else(M1_115 == 1, "Si", "No")
  )

# Adults (≥ 10) and children (< 10) counts
hh_counts <- ide %>%
  group_by(NIM) %>%
  summarise(
    Total_HH      = sum(edad >= 10, na.rm = TRUE),
    Total_HH_Kids = sum(edad < 10,  na.rm = TRUE),
    .groups = "drop"
  )

# Demographic composition shares
demographics <- ide %>%
  mutate(
    Prime_Male     = if_else(edad > 18 & edad < 60 & sexo == "Hombre", 1, 0.5 * (edad > 10 & edad < 60 & sexo == "Hombre")),
    Elderly_Male   = as.numeric(edad >= 60 & sexo == "Hombre"),
    Prime_Female   = if_else(edad > 18 & edad < 60 & sexo == "Mujer",  1, 0.5 * (edad > 10 & edad < 60 & sexo == "Mujer")),
    Elderly_Female = as.numeric(edad >= 60 & sexo == "Mujer")
  ) %>%
  group_by(NIM) %>%
  summarise(across(c(Prime_Male, Elderly_Male, Prime_Female, Elderly_Female),
                   \(x) sum(x, na.rm = TRUE)),
            .groups = "drop")

# Average household education
edu <- ide %>%
  group_by(NIM) %>%
  summarise(Educacion_Promedio = mean(educacion, na.rm = TRUE), .groups = "drop")

# Merge all identity / demographic data
identy <- hh_counts %>%
  left_join(demographics, by = "NIM") %>%
  replace_na(list(Prime_Male = 0, Elderly_Male = 0,
                  Prime_Female = 0, Elderly_Female = 0))

# ── 3.3  Certified coffee ───────────────────────────────────────────────────

cafe_c <- inv_agro %>%
  transmute(NIM, Certificado = as.integer(M1_508D == 1))

# ── 3.4  Land area (convert to m²) ──────────────────────────────────────────

land_data <- inver_agro %>%
  transmute(
    NIM,
    Tierra_total = M1_414A * land_unit_to_m2(M1_414B)
  ) %>%
  group_by(NIM) %>%
  summarise(Tierra_total = sum(Tierra_total, na.rm = TRUE), .groups = "drop")

# ── 3.5  Master merge ───────────────────────────────────────────────────────

data <- l %>%
  left_join(identy, by = "NIM") %>%
  left_join(edu,    by = "NIM") %>%
  left_join(cafe_c, by = "NIM") %>%
  left_join(land_data, by = "NIM") %>%
  left_join(credit %>% select(NIM, Ahorro), by = "NIM") %>%
  left_join(soil   %>% select(NIM, everything()),  by = "NIM")

# ── 3.6  Derived household share variables ──────────────────────────────────

data <- data %>%
  mutate(
    prime_male_share    = safe_divide(`Prime Male`,    Total_HH),
    prime_female_share  = safe_divide(`Prime Female`,  Total_HH),
    elderly_female_share = safe_divide(`Elderly Female`, Total_HH),
    elderly_male_share  = safe_divide(`Elderly Male`,  Total_HH),
    no_tiene_credito    = 1 - tiene_credito,
    tiene_credito_formal   = as.integer(status == "Formal"),
    tiene_credito_informal = as.integer(status == "Informal"),
    tiene_credito_mixto    = as.integer(status == "Mixto")
  )

# ── 3.7  Capital (machines) for coffee production ───────────────────────────

data <- data %>%
  rename(Guadanadora = Guadañadora) %>%
  select(-any_of("Guadañadora"))

# ── 3.8  Labour-intensive machine regressions (auxiliary) ───────────────────

machine_vars <- c("germinador", "almacigo", "arvenses", "motosierra",
                  "Guadanadora", "Ecomill", "Becolsub", "Despulpadora",
                  "Desmucilaginador", "Tanque", "Secador", "Elva",
                  "Camioneta", "Moto")

fml_harvest    <- reformulate(machine_vars, response = "harvest")
fml_nonharvest <- reformulate(c(machine_vars, "Tierra"), response = "non_harvest")

intensive_lm_harvest    <- lm(fml_harvest,    data = data)
intensive_lm_nonharvest <- lm(fml_nonharvest, data = data)

# ── 3.9  Aggregate machine indices ──────────────────────────────────────────

automation_machines <- c("motosierra", "Desmucilaginador", "Elva",
                         "Guadanadora", "Despulpadora")
all_machines        <- c(automation_machines, "Becolsub", "Ecomill", "Secador")

data <- data %>%
  mutate(
    machines            = rowSums(across(all_of(automation_machines)), na.rm = TRUE),
    maquinaria          = rowSums(across(all_of(all_machines)),        na.rm = TRUE),
    non_harvest_capital = Tot_K - Moto - Camioneta
  )

# ── 3.10  Farm age & ownership ──────────────────────────────────────────────

farms_age <- inver_agro %>%
  filter(!is.na(M1_408)) %>%
  group_by(NIM) %>%
  summarise(farms_antiquity = 2021 - round(mean(M1_408, na.rm = TRUE)),
            .groups = "drop")

farms_property <- inver_agro %>%
  group_by(NIM) %>%
  summarise(propia = as.integer(any(M1_408 > 0, na.rm = TRUE)),
            .groups = "drop")

data <- data %>%
  left_join(farms_age,      by = "NIM") %>%
  left_join(farms_property, by = "NIM")

# ── 3.11  Capital investment (machines bought ≤ 2 years ago) ────────────────

k <- d_lineal %>%
  select(NIM, starts_with("M1_601"), starts_with("M1_606")) %>%
  mutate(across(everything(), \(x) replace_na(x, 0))) %>%
  filter(rowSums(across(starts_with("M1_601"))) > 0,
         rowSums(across(starts_with("M1_606"))) <= 2)

# Machine-specific investment values
machine_codes <- setNames(
  paste0(LETTERS[1:14]),
  c("germinador", "almacigo", "arvenses", "motosierra", "Guadanadora",
    "Ecomill", "Becolsub", "Despulpadora", "Desmucilaginador",
    "Tanque", "Secador", "Elva", "Camioneta", "Moto")
)

for (nm in names(machine_codes)) {
  ltr <- machine_codes[[nm]]
  qty_col  <- paste0("M1_601", ltr)
  yr_col   <- paste0("M1_606", ltr, "2")
  val_col  <- paste0("M1_606", ltr, "1")
  k[[nm]]  <- ifelse(k[[yr_col]] <= 2, k[[qty_col]] * k[[val_col]], 0)
}

k <- k %>%
  mutate(tot_k = rowSums(across(all_of(names(machine_codes))), na.rm = TRUE))

k_hoy      <- filter(k, tot_k > 0) %>% select(NIM, all_of(names(machine_codes)), tot_k)
k_anterior <- filter(k, tot_k > 0) %>% select(NIM, all_of(names(machine_codes)), tot_k)

k_analysis <- left_join(k_hoy, k_anterior, by = "NIM")
data       <- left_join(data, k_analysis,  by = "NIM")

# Machine new/old aggregates
data <- data %>%
  mutate(
    machines_new = motosierra_nuevo + Desmucilaginador_nuevo + Elva_nuevo +
                   Guadanadora_nuevo + Despulpadora_nuevo,
    machines_share = replace_na(machines_share, 0),
    machines_old = motosierra_viejo + Desmucilaginador_viejo + Elva_viejo +
                   Guadanadora_viejo + Despulpadora_viejo
  )

# ── 3.12  Price-adjusted investment values ──────────────────────────────────

precios_k <- read_excel("precios_capital.xlsx") %>%
  rename(Producto = `Producto`, Precio_promedio_anual = `Precio promedio anual`,
         Presentacion = `Presentacion`)

price_index <- c(
  germinador = 1, almacigo = 3, arvenses = 5, motosierra = 6,
  Guadanadora = 7, Ecomill = 9, Becolsub = 10, Despulpadora = 8,
  Desmucilaginador = 11, Tanque = 12, Secador = 13, Elva = 14,
  Camioneta = 15, Moto = 16
)

for (nm in names(price_index)) {
  col_new    <- paste0(nm, "_nuevo")
  price_val  <- as.integer(precios_k[[price_index[[nm]], 2]])
  data[[col_new]] <- data[[col_new]] * price_val
}

toc()

# ╔════════════════════════════════════════════════════════════════════════════╗
# ║                   4.  KERNEL REGRESSIONS (Non-Parametric)                ║
# ╚════════════════════════════════════════════════════════════════════════════╝

## Helper: publication-quality kernel plot
plot_kernel <- function(x, y, x_lab, y_lab, title, subtitle = NULL) {
  ks <- ksmooth(x, y, kernel = "normal", bandwidth = "SJ")
  tibble(x = x, y = y) %>%
    ggplot(aes(x, y)) +
    geom_point(alpha = 0.35, size = 1.4, colour = palette_thesis["primary"]) +
    geom_line(data = tibble(x = ks$x, y = ks$y),
              aes(x, y), colour = palette_thesis["accent"],
              linewidth = 1.1) +
    labs(x = x_lab, y = y_lab, title = title, subtitle = subtitle,
         caption = "Gaussian kernel with Sheather–Jones bandwidth") +
    theme_thesis()
}

# 4.1  Machines ∼ Land
p1 <- plot_kernel(
  ihs(data$Tierra), ihs(data$machines),
  "Land (IHS)", "Machines (IHS)",
  "Kernel Regression: Machines and Cultivated Land",
  "Non-parametric relationship between capital stock and farm size"
)

# 4.2  Capital ∼ Non-Harvest Labour
p2 <- plot_kernel(
  ihs(data$non_harvest), ihs(data$Tot_K),
  "Non-Harvest Labour (IHS)", "Total Capital (IHS)",
  "Kernel Regression: Capital and Non-Harvest Labour",
  "Factor substitutability in the post-harvest stage"
)

# 4.3  Non-Harvest Capital ∼ Non-Harvest Labour (inefficiency proof)
valid_idx <- data$non_harvest > 0 & data$non_harvest_capital > 0
p3 <- plot_kernel(
  ihs(data$non_harvest[valid_idx]),
  ihs(data$non_harvest_capital[valid_idx]),
  "Non-Harvest Labour (IHS)", "Non-Harvest Capital (IHS)",
  "Inefficiency Proof: Non-Harvest Capital vs Labour",
  "Restricted to observations with positive labour and capital"
)

# Combine with patchwork
kernel_panel <- p1 / p2 / p3 +
  plot_annotation(
    title = "Figure 1 — Non-Parametric Factor Relationships",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )
print(kernel_panel)

# ╔════════════════════════════════════════════════════════════════════════════╗
# ║              5.  PRE-ESTIMATION DIAGNOSTICS                              ║
# ╚════════════════════════════════════════════════════════════════════════════╝


# 5.1  Partial correlations among controls
control_vars <- c("Educacion_Promedio", "land_index", "sexo", "dist_2_bank",
                  "tiene_credito", "Certificado", "Tierra", "farms_antiquity")

controls_df <- data %>%
  select(all_of(control_vars)) %>%
  mutate(sexo = as.integer(factor(sexo)))

cor_matrix <- cor(controls_df, use = "pairwise.complete.obs")
partial_cor <- pcor(controls_df, method = "pearson")

# 5.2  IHS-transformed control correlations
data <- data %>%
  mutate(
    land = ihs(Tierra),
    K    = ihs(machines),
    HH   = ihs(Total_HH),
    sex  = as.integer(sexo == "Mujer")
  )

controls_ihs <- data %>%
  select(Educacion_Promedio, dist_2_bank, tiene_credito, land, K, HH, sex, Certificado)
pcor(controls_ihs, method = "pearson")

# ╔════════════════════════════════════════════════════════════════════════════╗
# ║              6.  SUMMARY STATISTICS                                      ║
# ╚════════════════════════════════════════════════════════════════════════════╝

summary_vars <- c("Tierra", "machines", "Tierra_Predio", "labor", "coop",
                  "prime_male_share", "prime_female_share",
                  "elderly_male_share", "elderly_female_share",
                  "Total_HH", "Educacion_Promedio", "sexo", "dist_2_bank")

summary_df <- data %>% select(any_of(summary_vars))
stargazer(as.data.frame(summary_df), type = "text",
          title = "Summary Statistics — Coffee Producer Survey",
          digits = 3, out = "summary_stats.tex")

# ╔════════════════════════════════════════════════════════════════════════════╗
# ║     7.  MAIN REGRESSIONS — RECURSION TEST (Separability Hypothesis)      ║
# ╚════════════════════════════════════════════════════════════════════════════╝


## Hypothesis set for no-credit F-tests
h0_no_credit <- c(
  "ihs(Total_HH):no_tiene_credito = 0",
  "ihs(prime_male_share):no_tiene_credito = 0",
  "ihs(prime_female_share):no_tiene_credito = 0",
  "ihs(elderly_female_share):no_tiene_credito = 0",
  "ihs(elderly_male_share):no_tiene_credito = 0"
)

## 7.1  Aggregated labour demand  ─────────────────────────────────────────────

fml_agg <- build_recursion_formula("ihs(labor)")
mod_agg <- lm(fml_agg, data = data)

results_agg <- list(
  robust  = robust_coeftest(mod_agg, cl = ~MUNICIPIO),
  f_test  = robust_ftest(mod_agg, h0_no_credit),
  vif     = car::vif(mod_agg, type = "marginal")
)

## 7.2  Non-Harvest labour demand  ────────────────────────────────────────────

fml_nh <- build_recursion_formula("ihs(non_harvest)")
mod_nh <- lm(fml_nh, data = data)

results_nh <- list(
  robust  = robust_coeftest(mod_nh, cl = ~MUNICIPIO),
  f_test  = robust_ftest(mod_nh, h0_no_credit),
  vif     = car::vif(mod_nh, type = "marginal")
)

## 7.3  Harvest labour demand  ────────────────────────────────────────────────

fml_h <- build_recursion_formula("ihs(harvest)")
mod_h  <- lm(fml_h, data = data)

results_h <- list(
  robust  = robust_coeftest(mod_h, cl = ~MUNICIPIO),
  bp_test = bptest(mod_h),
  vif     = car::vif(mod_h, type = "marginal")
)

## 7.4  Model comparison (AIC / BIC) ─────────────────────────────────────────

model_comparison <- tibble(
  Model = c("Aggregated", "Non-Harvest", "Harvest"),
  AIC   = c(AIC(mod_agg), AIC(mod_nh), AIC(mod_h)),
  BIC   = c(BIC(mod_agg), BIC(mod_nh), BIC(mod_h)),
  Adj_R2 = c(summary(mod_agg)$adj.r.squared,
             summary(mod_nh)$adj.r.squared,
             summary(mod_h)$adj.r.squared)
)
print(model_comparison)

## 7.5  Publication table (all three models side by side) ─────────────────────

modelsummary(
  list("Total Labour" = mod_agg, "Non-Harvest" = mod_nh, "Harvest" = mod_h),
  vcov    = list(\(m) vcovHC(m, "HC3"), \(m) vcovHC(m, "HC3"), \(m) vcovHC(m, "HC3")),
  stars   = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
  title   = "Table 3 — Recursion Test Results",
  output  = "recursion_test.tex"
)

# ╔════════════════════════════════════════════════════════════════════════════╗
# ║             8.  CAPITAL MARKET — MACHINERY RECURSION                     ║
# ╚════════════════════════════════════════════════════════════════════════════╝


fml_k <- ihs(machines) ~ ihs(Tierra) + non_harvest +
  tiene_credito:ihs(Total_HH) + tiene_credito:ihs(prime_male_share) +
  tiene_credito:ihs(prime_female_share) + tiene_credito:ihs(elderly_female_share) +
  tiene_credito:ihs(elderly_male_share) +
  no_tiene_credito:ihs(Total_HH) + no_tiene_credito:ihs(prime_male_share) +
  no_tiene_credito:ihs(prime_female_share) + no_tiene_credito:ihs(elderly_female_share) +
  no_tiene_credito:ihs(elderly_male_share) +
  Certificado + dist_2_bank + Educacion_Promedio + Asistencia_tecnica +
  sexo + factor(MUNICIPIO)

mod_k <- lm(fml_k, data = data)
robust_coeftest(mod_k, cl = ~MUNICIPIO)

# ╔════════════════════════════════════════════════════════════════════════════╗
# ║         9.  DOUBLE-HURDLE MODEL FOR MACHINERY ADOPTION                   ║
# ╚════════════════════════════════════════════════════════════════════════════╝

## 9.1  Productivity data merge & normalization ───────────────────────────────

tfp <- read_excel("TFP_RESULTS.xlsx")

tfp_cols <- c("DEA", "FARE_PRIMONT_TFP", "SFA", "COBB_DOUGLAS_TFP")
tfp <- tfp %>% mutate(across(all_of(tfp_cols), scale))

data <- data %>%
  left_join(tfp, by = "NIM") %>%
  distinct(NIM, .keep_all = TRUE) %>%
  mutate(inef = as.integer(non_harvest != 0))

## 9.2  Hurdle model specification ────────────────────────────────────────────

dh_formula <- ihs(machines_new) ~ factor(MUNICIPIO) +
  ihs(farms_antiquity) + tiene_credito +
  ihs(prime_male_share) + ihs(Total_HH) +
  ihs(prime_female_share) + ihs(elderly_male_share) +
  ihs(machines_old) + Asistencia_tecnica

dh_rhs <- ~ factor(MUNICIPIO) + ihs(farms_antiquity) +
  tiene_credito + ihs(prime_male_share) +
  ihs(Total_HH) + ihs(prime_female_share) +
  ihs(elderly_male_share) + ihs(machines_old) +
  Asistencia_tecnica

dh_model <- mhurdle(
  dh_formula | dh_rhs | 0,
  data   = data,
  corr   = TRUE,
  h2     = TRUE,
  dist   = "n",
  method = "bfgs"
)

summary(dh_model)
robust_coeftest(dh_model, hc = "HC1", cl = ~MUNICIPIO)

## 9.3  F-tests for the hurdle model ──────────────────────────────────────────

hurdle_hypotheses <- list(
  h2_no_credit = c(
    "h2.ihs(prime_male_share):no_tiene_credito = 0",
    "h2.ihs(Total_HH):no_tiene_credito = 0",
    "h2.ihs(prime_female_share):no_tiene_credito = 0",
    "h2.ihs(elderly_female_share):no_tiene_credito = 0"
  ),
  h2_credit = c(
    "h2.tiene_credito:ihs(prime_male_share) = 0",
    "h2.tiene_credito:ihs(Total_HH) = 0",
    "h2.tiene_credito:ihs(prime_female_share) = 0",
    "h2.tiene_credito:ihs(elderly_female_share) = 0"
  ),
  h1_no_credit = c(
    "h1.ihs(prime_male_share):no_tiene_credito = 0",
    "h1.ihs(Total_HH):no_tiene_credito = 0",
    "h1.ihs(prime_female_share):no_tiene_credito = 0",
    "h1.ihs(elderly_female_share):no_tiene_credito = 0"
  ),
  joint = c(
    "h1.ihs(prime_male_share):no_tiene_credito = 0",
    "h1.ihs(Total_HH):no_tiene_credito = 0",
    "h1.ihs(prime_female_share):no_tiene_credito = 0",
    "h1.ihs(elderly_female_share):no_tiene_credito = 0",
    "h2.ihs(prime_male_share):no_tiene_credito = 0",
    "h2.ihs(Total_HH):no_tiene_credito = 0",
    "h2.ihs(prime_female_share):no_tiene_credito = 0",
    "h2.ihs(elderly_female_share):no_tiene_credito = 0"
  )
)

hurdle_ftests <- purrr::map(hurdle_hypotheses, \(h) {
  robust_ftest(dh_model, h, adjust = "hc3")
})

## 9.4  Interaction hurdle model ──────────────────────────────────────────────

dh_2 <- mhurdle(
  ihs(machines_new) ~ factor(MUNICIPIO) + Asistencia_tecnica +
    tienes_credito:ihs(prime_male_share) + tienes_credito:ihs(Total_HH) +
    tienes_credito:ihs(prime_female_share) + tienes_credito:ihs(elderly_male_share) +
    tienes_credito:ihs(elderly_female_share) +
    no_tienes_credito:ihs(prime_male_share) + no_tienes_credito:ihs(Total_HH) +
    no_tienes_credito:ihs(prime_female_share) + no_tienes_credito:ihs(elderly_male_share) +
    no_tienes_credito:ihs(elderly_female_share) +
    ihs(machines_old),
  data   = data,
  corr   = TRUE,
  h2     = TRUE,
  dist   = "n",
  method = "bfgs"
)
summary(dh_2)

# ╔════════════════════════════════════════════════════════════════════════════╗
# ║        10.  PRODUCTIVITY ALLOCATION REGRESSIONS                          ║
# ╚════════════════════════════════════════════════════════════════════════════╝

## 10.1  Labour allocation data ───────────────────────────────────────────────

trabajo <- read_excel("ARD607_SEC2_MATRIZ_INGRESOS.xlsx") %>%
  mutate(
    sector_agro = as.integer(M1_201 == 1),
    sector_agro = replace_na(sector_agro, 0),
    count       = 1L
  )

force <- trabajo %>%
  group_by(NIM) %>%
  summarise(
    SHARE = sum(sector_agro),
    num   = n(),
    .groups = "drop"
  ) %>%
  mutate(labor_distribution = SHARE / num)

data <- left_join(data, force, by = "NIM")

## 10.2  Scale normalisation ──────────────────────────────────────────────────

data <- data %>%
  mutate(
    labor        = labor / 12,
    Tot_K        = Tot_K / 1e6,
    Tierra_Predio = Tierra / 100
  )

## 10.3  Productivity allocation models ───────────────────────────────────────

productivity_controls <- "tiene_credito + dist_2_bank + sexo + Tot_K +
                          Tierra_Predio + land_share + propia + factor(MUNICIPIO)"

prod_models <- list(
  DEA      = lm(as.formula(paste("DEA ~",      "harvest + non_harvest +", productivity_controls)), data = data),
  FP_TFP   = lm(as.formula(paste("FARE_PRIMONT_TFP ~", "harvest + non_harvest +", productivity_controls)), data = data),
  CD_TFP   = lm(as.formula(paste("COBB_DOUGLAS_TFP ~", "harvest + non_harvest +", productivity_controls)), data = data),
  SFA      = lm(as.formula(paste("SFA ~",      "harvest + non_harvest +", productivity_controls)), data = data)
)

prod_robust <- purrr::map(prod_models, \(m) robust_coeftest(m, hc = "HC1", cl = ~MUNICIPIO))

## 10.4  Market failure analysis ──────────────────────────────────────────────

data <- data %>%
  mutate(
    nh_share = safe_divide(non_harvest, non_harvest + harvest),
    model    = as.integer(non_harvest > 0),
    model2   = as.integer(machines > 0)
  )

mod_mf_share <- lm(
  nh_share ~ ihs(Tierra) + ihs(machines) + tiene_credito +
    ihs(Total_HH) + factor(MUNICIPIO) + dist_2_bank + Certificado,
  data = data
)
robust_coeftest(mod_mf_share, cl = ~MUNICIPIO)

## 10.5  Logistic models for market failures ──────────────────────────────────

logit_base_rhs <- paste(
  "ihs(Tierra) + ihs(machines)",
  "tiene_credito:ihs(Total_HH)", "tiene_credito:ihs(prime_male_share)",
  "tiene_credito:ihs(prime_female_share)", "tiene_credito:ihs(elderly_male_share)",
  "tiene_credito:ihs(elderly_female_share)",
  "no_tiene_credito:ihs(Total_HH)", "no_tiene_credito:ihs(prime_male_share)",
  "no_tiene_credito:ihs(prime_female_share)", "no_tiene_credito:ihs(elderly_male_share)",
  "no_tiene_credito:ihs(elderly_female_share)",
  "factor(MUNICIPIO) + dist_2_bank + Certificado",
  sep = " + "
)

logit_nh <- glm(as.formula(paste("model ~", logit_base_rhs)),
                data = data, family = binomial("logit"))
logit_k  <- glm(as.formula(paste("model2 ~", logit_base_rhs)),
                data = data, family = binomial("logit"))

# ╔════════════════════════════════════════════════════════════════════════════╗
# ║    11.  NON-AGRICULTURAL LABOUR MARKET (Worker-Month Panel)              ║
# ╚════════════════════════════════════════════════════════════════════════════╝

## 11.1  Load and prepare off-farm labour data ────────────────────────────────

outside_labor <- read_excel("ARD607_SEC2_MATRIZ_INGRESOS.xlsx") %>%
  filter(!is.na(M1_200)) %>%
  mutate(counter = 1L)

workers <- outside_labor %>%
  group_by(NIM) %>%
  summarise(`#_WORKERS` = sum(counter), .groups = "drop")

outside_labor <- outside_labor %>%
  mutate(
    sector_principal  = if_else(M1_201 == 1, "Agro", "No Agro"),
    sector_secundario = if_else(M1_201.2 == 1, "Agro", "No Agro"),
    Tiempo_principal  = recode(as.character(M1_202),
                               "1" = "Completo", "2" = "Estacional",
                               "3" = "Ocasional", .default = ""),
    Tiempo_secundario = recode(as.character(M1_202.2),
                               "1" = "Completo", "2" = "Estacional",
                               "3" = "Ocasional", .default = ""),
    Actividad_principal  = paste(sector_principal, Tiempo_principal),
    Actividad_secundaria = paste(sector_secundario, Tiempo_secundario)
  )

# Worker time totals
time_prefixes <- c("M1_204BA", "M1_204.2.BA", "M1_207BA", "M1_207.2.BA",
                   "M1_209BA", "M1_209.2.BA", "M1_211BA", "M1_211.2.BA",
                   "M1_212BA", "M1_212.2.BA", "M1_213BA", "M1_213.2.BA")

for (i in seq_along(time_prefixes)) {
  col_nm <- paste0("time", ifelse(i == 1, "", i))
  outside_labor[[col_nm]] <- rowSums(
    select(outside_labor, starts_with(time_prefixes[i])), na.rm = TRUE
  )
}

outside_labor <- outside_labor %>%
  mutate(
    agro_month_prin = if_else(M1_201 == 1, time + time5 + time7, 0),
    agro_month_sec  = if_else(M1_201.2 == 1, time2 + time6 + time8, 0),
    non_agro_prin   = if_else(M1_201 != 1, time + time3 + time5, 0),
    non_agro_sec    = if_else(M1_201.2 != 1, time2 + time4 + time6, 0),
    agro_month      = agro_month_prin + agro_month_sec,
    non_agro_month  = non_agro_prin + non_agro_sec,
    tot_months      = agro_month + non_agro_month,
    edad_id         = M1_106,
    sexo_id         = if_else(M1_105 == 1, "Hombre", "Mujer"),
    educacion_id    = compute_education(M1_109A1, M1_109B1)
  )

data <- left_join(data, workers, by = "NIM")

## 11.2  Municipality department merge ────────────────────────────────────────

depo <- d_lineal %>% select(NIM, DEPARTAMENTO)
data      <- left_join(data,      depo, by = "NIM")
off_farm  <- left_join(off_farm,  depo, by = "NIM")

## 11.3  Expand to worker × month panel (VECTORISED) ─────────────────────────
## The original code used a row-by-row for-loop over ~12 × N rows.
## This version uses vectorised operations for ~100x speedup.

tic("Panel expansion")

# Create the 12-month panel via cross-join
final <- off_farm %>%
  tidyr::crossing(mes = 1L:12L)

# Replace NAs in key variables
final <- final %>%
  mutate(
    M1_203   = replace_na(M1_203, 0),
    M1_201.2 = replace_na(M1_201.2, 0),
    M1_203.2 = replace_na(M1_203.2, 0)
  )

## 11.4  Classify primary sector per month (VECTORISED) ───────────────────────
## This replaces the ~300-line for-loop in the original code.

final$month_sector_principal <- classify_sector_vectorised(
  df          = final,
  month_col   = "mes",
  sector_col  = "M1_201",
  type_col    = "M1_203",
  prefix_seas = "M1_204B",
  prefix_occ  = "M1_209B",
  prefix_hired = "M1_207B"
)

## 11.5  Classify secondary sector per month (VECTORISED) ─────────────────────

final$month_sector_secundario <- classify_sector_vectorised(
  df          = final,
  month_col   = "mes",
  sector_col  = "M1_201.2",
  type_col    = "M1_203.2",
  prefix_seas = "M1_204.2.B",
  prefix_occ  = "M1_209.2.B",
  prefix_hired = "M1_207.2.B"
)

## 11.6  Combine primary + secondary into unified monthly label (VECTORISED) ──

final$month_labor <- combine_sector_labels(
  final$month_sector_principal,
  final$month_sector_secundario
)

## 11.7  Harvest season indicator (VECTORISED) ────────────────────────────────

final$crop <- is_crop_season(final$DEPARTAMENTO.y, final$mes)

toc()

## 11.8  Filter to active workers and create analysis variables ───────────────

finales <- final %>%
  filter(month_labor %in% c("Agro", "Non Agro")) %>%
  mutate(
    sector = as.integer(month_labor == "Non Agro"),
    ider   = paste0(NIM, M1_ID_DE_PERSONA),
    un     = paste0(ider, mes)
  ) %>%
  distinct(un, .keep_all = TRUE)

# ╔════════════════════════════════════════════════════════════════════════════╗
# ║          12.  SECTOR CHOICE REGRESSIONS (Logit / Probit)                 ║
# ╚════════════════════════════════════════════════════════════════════════════╝

## 12.1  Common formula for sector choice models ─────────────────────────────

sector_rhs <- paste(
  "crop", "educacion_id", "sexo_id",
  "ihs(Total_HH)", "ihs(machines)",
  "ihs(prime_male_share)", "ihs(prime_female_share)", "ihs(elderly_male_share)",
  "dist_2_bank", "Certificado", "ihs(Tierra)",
  "factor(MUNICIPIO)", "factor(mes)",
  sep = " + "
)

## 12.2  Pooled regression ────────────────────────────────────────────────────

fml_pooled <- as.formula(paste(
  "sector ~ crop + crop:ihs(Total_HH) + edad_id + edad_id_sq +",
  sector_rhs, "+ no_tiene_credito"
))

mod_pooled <- glm(fml_pooled, data = finales, family = binomial("logit"))

## 12.3  Split by credit status ──────────────────────────────────────────────

con  <- filter(finales, tiene_credito == 1)
sinc <- filter(finales, tiene_credito == 0)

fml_sector <- as.formula(paste("sector ~ crop +", sector_rhs))

# With credit — Logit & Probit
mod_con_logit  <- glm(fml_sector, data = con,  family = binomial("logit"))
mod_con_probit <- glm(fml_sector, data = con,  family = binomial("probit"))

# Without credit — Logit & Probit
mod_sin_logit  <- glm(fml_sector, data = sinc, family = binomial("logit"))
mod_sin_probit <- glm(fml_sector, data = sinc, family = binomial("probit"))

## 12.4  Multi-level cluster-robust inference ────────────────────────────────

cluster_levels <- list(mes = ~mes, municipio = ~MUNICIPIO, nim = ~NIM)

robust_results <- list(
  with_credit = purrr::map(cluster_levels, \(cl) robust_coeftest(mod_con_probit, cl = cl)),
  no_credit   = purrr::map(cluster_levels, \(cl) robust_coeftest(mod_sin_probit, cl = cl))
)

## 12.5  Publication table ────────────────────────────────────────────────────

modelsummary(
  list(
    "With Credit (Logit)"    = mod_con_logit,
    "With Credit (Probit)"   = mod_con_probit,
    "Without Credit (Logit)" = mod_sin_logit,
    "Without Credit (Probit)" = mod_sin_probit
  ),
  vcov  = \(m) vcovHC(m, "HC3"),
  stars = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
  title = "Table 4 — Sector Choice by Credit Status",
  output = "sector_choice.tex"
)

# ╔════════════════════════════════════════════════════════════════════════════╗
# ║              13.  DIAGNOSTICS & ROBUSTNESS CHECKS                        ║
# ╚════════════════════════════════════════════════════════════════════════════╝


## 13.1  VIF analysis
vif_results <- list(
  no_credit   = car::vif(mod_sin_logit, type = "marginal"),
  with_credit = car::vif(mod_con_logit, type = "marginal")
)

## 13.2  Cook's distance outlier detection (functional approach) ──────────────

detect_outliers <- function(model, data, threshold_mult = 3) {
  cd <- cooks.distance(model)
  cd <- cd[!is.na(cd)]
  threshold <- threshold_mult * mean(cd, na.rm = TRUE)
  influential <- cd[cd > threshold]
  outlier_rows <- data[names(influential), ]
  clean_data <- anti_join(data, outlier_rows)
  list(
    n_influential = length(influential),
    threshold     = threshold,
    clean_data    = clean_data
  )
}

outlier_con_logit  <- detect_outliers(mod_con_logit,  con)
outlier_sin_logit  <- detect_outliers(mod_sin_logit,  sinc)
outlier_con_probit <- detect_outliers(mod_con_probit, con)
outlier_sin_probit <- detect_outliers(mod_sin_probit, sinc)

## 13.3  Additional binary choice models ──────────────────────────────────────

# Overall working decision
final <- final %>% mutate(work = as.integer(month_labor != "Sin Trabajo"))

mod_work <- glm(
  work ~ crop + edad_id + edad_id_sq + educacion_id + sexo_id +
    ihs(Total_HH) + ihs(machines) +
    ihs(prime_male_share) + ihs(prime_female_share) + ihs(elderly_male_share) +
    dist_2_bank + tiene_credito + Certificado + ihs(Tierra) +
    factor(MUNICIPIO) + factor(mes),
  data = final, family = binomial("logit")
)

# Dual-job decision
finalin <- final %>%
  filter(month_labor %in% c("Non Agro", "Agro", "Hybrid")) %>%
  mutate(dual = as.integer(month_labor == "Hybrid"))

mod_dual <- glm(
  dual ~ crop + edad_id + edad_id_sq + educacion_id + sexo_id +
    ihs(Total_HH) + ihs(non_harvest) + ihs(machines) +
    ihs(prime_male_share) + ihs(prime_female_share) + ihs(elderly_male_share) +
    dist_2_bank + tiene_credito + Certificado + ihs(Tierra) +
    factor(MUNICIPIO) + factor(mes),
  data = finalin, family = binomial("logit")
)

# ╔════════════════════════════════════════════════════════════════════════════╗
# ║    14.  HOUSEHOLD LABOUR SHARE IN EACH TASK (Endogeneity Analysis)       ║
# ╚════════════════════════════════════════════════════════════════════════════╝

data <- data %>%
  mutate(
    fam_share_harvest = safe_divide(fam_secu + fam_otra, harvest),
    fam_share_nh      = safe_divide(fam, non_harvest)
  )

## 14.1  Non-harvest share model (with credit-type interactions) ──────────────

credit_interaction_rhs <- function(credit_var) {
  hh_terms <- c("ihs(Total_HH)", "ihs(prime_male_share)",
                "ihs(prime_female_share)", "ihs(elderly_female_share)",
                "ihs(elderly_male_share)")
  paste(paste0(credit_var, ":", hh_terms), collapse = " + ")
}

credit_types <- c("tiene_credito_formal", "tiene_credito_informal",
                  "tiene_credito_mixto", "no_tiene_credito")

share_rhs_full <- paste(
  "ihs(Tierra) + ihs(machines)",
  paste(purrr::map_chr(credit_types, credit_interaction_rhs), collapse = " + "),
  "factor(MUNICIPIO) + Educacion_Promedio + land_index + sexo + dist_2_bank + Certificado",
  sep = " + "
)

shares_nh <- lm(as.formula(paste("ihs(fam_share_nh) ~", share_rhs_full)), data = data)
robust_coeftest(shares_nh, cl = ~MUNICIPIO)

## 14.2  Harvest share model ──────────────────────────────────────────────────

shares_h <- lm(as.formula(paste("ihs(fam_share_harvest) ~", share_rhs_full)), data = data)
robust_coeftest(shares_h, cl = ~MUNICIPIO)

