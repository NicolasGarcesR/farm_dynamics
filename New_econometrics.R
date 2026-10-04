################################################################################
#  SMALLHOLDER FARMS AS FINANCIALLY CONSTRAINED FIRMS
#  Task-level factor demand, household composition, and credit
#
#  FNC-EFI Coffee Producer Survey — Baseline 2021 / Endline 2023
#
#  DESIGN NOTE (read first):
#   The financial-consumer-protection RCT does NOT move credit take-up
#   (first stage: 0.008, s.e. 0.036). No IV strategy is pursued anywhere in
#   this script. The null first stage is REPORTED as a data fact, not used.
#
#   Randomization was at CLUSTER level (groups of nearby farms within
#   municipalities). All treatment-related inference clusters on `cluster`.
#   Non-treatment regressions cluster at municipality.
#
#  STRUCTURE
#   0  Packages, options, helpers
#   1  Load data + diagnostics (treatment integrity, credit comparability)
#   2  Variable construction
#   3  Act 1 — the misallocation wedge      (levels + first difference)
#   4  Act 2 — separation test              *** HEADLINE ***
#   5  Act 3 — credit and investment
#   6  Robustness (lead/lag, harvest contrast, MDEs)
#   7  Tables
################################################################################


# ==============================================================================
# 0. PACKAGES, OPTIONS, HELPERS
# ==============================================================================
options(scipen = 999, digits = 4, dplyr.summarise.inform = FALSE)

pkgs <- c("readxl", "dplyr", "tidyr", "fixest", "modelsummary", "car", "knitr")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}

## >>> EDIT PATH <<<
setwd("C:/Users/wb631291/OneDrive - WBG/Desktop/Thesis")
dir.create("Output", showWarnings = FALSE)

STARS <- c("*" = 0.1, "**" = 0.05, "***" = 0.01)
GOF   <- c("nobs", "r.squared", "adj.r.squared")
NOTE  <- "Standard errors clustered at municipality in parentheses."

# center a variable at its mean (so interaction main effects are interpretable)
ctr <- function(x) x - mean(x, na.rm = TRUE)

# minimum detectable effect at 80% power, 5% size
mde <- function(model, param, alpha = 0.05, power = 0.80) {
  se <- tryCatch(sqrt(diag(vcov(model))[param]), error = function(e) NA_real_)
  if (is.na(se)) return(NA_real_)
  (qnorm(1 - alpha/2) + qnorm(power)) * se
}


# ==============================================================================
# 1. LOAD DATA + DIAGNOSTICS
# ==============================================================================
d  <- read_excel("Data/data.xlsx")
lp <- read_excel("Data/labor_panel.xlsx")

# ---- treatment variable -------------------------------------------------
# NOTE: replace `cluster` with the actual cluster column name in data.xlsx
d <- d %>%
  mutate(
    D_i      = as.integer(Grupo_Tratamiento == "Tratamiento"),
    mun_wave = interaction(MUNICIPIO, wave, sep = "_")
  )

cat("\n=== DIAGNOSTIC 1: treatment integrity ===\n")
print(table(d$Grupo_Tratamiento, useNA = "ifany"))
cat("Treated share:", round(mean(d$D_i, na.rm = TRUE), 3), "\n")

# Treatment must be constant within cluster. Uncomment once `cluster` exists.
# chk <- d %>% group_by(cluster) %>% summarise(k = n_distinct(D_i))
# cat("Clusters with mixed treatment (should be 0):", sum(chk$k > 1), "\n")
# cat("Number of randomization clusters:", n_distinct(d$cluster), "\n")

# ---- credit comparability across waves -----------------------------------
# Baseline and endline credit come from DIFFERENT question batteries.
# A large jump signals a definitional break, which invalidates switcher designs.
cat("\n=== DIAGNOSTIC 2: credit comparability ===\n")
cred_by_wave <- d %>% group_by(wave) %>%
  summarise(share_credit = mean(tiene_credito, na.rm = TRUE), n = n())
print(cred_by_wave)
cat("If the endline share is far above baseline, the two waves are NOT\n",
    "measuring the same construct. Switcher-based designs are then unusable.\n")

# ---- credit transitions ---------------------------------------------------
credit_wide <- d %>%
  dplyr::select(NIM, wave, tiene_credito) %>%
  tidyr::pivot_wider(names_from = wave, values_from = tiene_credito,
                     names_prefix = "Q_wave") %>%
  mutate(
    Gained_i = as.integer(Q_wave1 == 0 & Q_wave2 == 1),
    Lost_i   = as.integer(Q_wave1 == 1 & Q_wave2 == 0),
    Stay1_i  = as.integer(Q_wave1 == 1 & Q_wave2 == 1),
    Stay0_i  = as.integer(Q_wave1 == 0 & Q_wave2 == 0)
  )
cat("\nCredit transitions: gained =", sum(credit_wide$Gained_i, na.rm = TRUE),
    "| lost =", sum(credit_wide$Lost_i, na.rm = TRUE),
    "| stay(1,1) =", sum(credit_wide$Stay1_i, na.rm = TRUE),
    "| stay(0,0) =", sum(credit_wide$Stay0_i, na.rm = TRUE), "\n")

# ---- the null first stage, reported as a fact ------------------------------
# Kept ONLY to document that no IV is available. Never used downstream.
cat("\n=== DIAGNOSTIC 3: treatment effect on credit take-up ===\n")


# ==============================================================================
# 2. VARIABLE CONSTRUCTION
# ==============================================================================

# ---- 2a. output -----------------------------------------------------------
if ("production_kg" %in% names(d)) {
  d <- d %>% mutate(ln_Y = log(pmax(production_kg, 1)))
} else {
  d$ln_Y <- NA_real_
  cat("\n[!] production_kg missing -> Act 1 skipped.\n")
}
has_Y <- any(is.finite(d$ln_Y))

# ---- 2b. pooled (long) variables ------------------------------------------
d <- d %>%
  mutate(
    ln_labor    = log(pmax(labor, 1)),
    ln_machines = log(pmax(machines, 1)),
    ln_Tierra   = log(pmax(Tierra, 1)),
    s_it        = if_else(labor > 0, non_harvest / labor, NA_real_),
    D_NH_it     = as.integer(non_harvest > 0)
  )

# ---- 2c. wide (farm-level) frame ------------------------------------------
d_wide <- d %>%
  dplyr::select(NIM, wave, D_i, MUNICIPIO,
                labor, non_harvest, harvest, machines, Tierra,
                Total_HH, Educacion_Promedio, sexo, tiene_credito,
                Certificado, Asistencia_tecnica, farms_antiquity,
                Prime_Male, Prime_Female, Elderly_Male, Elderly_Female,
                any_of("ln_Y"))

d_wide_pivot <- d_wide %>%
  tidyr::pivot_wider(
    id_cols     = c(NIM, MUNICIPIO, D_i),
    names_from  = wave,
    values_from = c(labor, non_harvest, harvest, machines, Tierra,
                    Total_HH, Educacion_Promedio, sexo, tiene_credito,
                    Certificado, Asistencia_tecnica,
                    Prime_Male, Prime_Female, Elderly_Male, Elderly_Female,
                    any_of("ln_Y")),
    names_sep   = "_w"
  ) %>%
  mutate(
    Q_w1 = tiene_credito_w1,
    Q_w2 = tiene_credito_w2,
    
    # first differences
    D_labor       = log(pmax(labor_w2, 1))       - log(pmax(labor_w1, 1)),
    D_non_harvest = log(pmax(non_harvest_w2, 1)) - log(pmax(non_harvest_w1, 1)),
    D_machines    = log(pmax(machines_w2, 1))    - log(pmax(machines_w1, 1)),
    D_educ        = Educacion_Promedio_w2 - Educacion_Promedio_w1,
    D_cert        = Certificado_w2 - Certificado_w1,
    D_lnY         = if ("ln_Y_w2" %in% names(.)) ln_Y_w2 - ln_Y_w1 else NA_real_,
    
    # misallocation indices
    s_w1       = if_else(labor_w1 > 0, non_harvest_w1 / labor_w1, NA_real_),
    s_w2       = if_else(labor_w2 > 0, non_harvest_w2 / labor_w2, NA_real_),
    D_s        = s_w2 - s_w1,
    D_NH_w1    = as.integer(non_harvest_w1 > 0),
    D_NH_w2    = as.integer(non_harvest_w2 > 0),
    Delta_D_NH = D_NH_w2 - D_NH_w1,
    
    # predetermined inputs
    ln_K_w1 = log(pmax(machines_w1, 1)),
    ln_T_w1 = log(pmax(Tierra_w1, 1))
  )

# Why does the FD sample shrink? Diagnose before writing Act 1.
cat("\n=== DIAGNOSTIC 4: first-difference sample ===\n")
cat("Farms in wide frame:", nrow(d_wide_pivot), "\n")
cat("  non-missing D_lnY :", sum(is.finite(d_wide_pivot$D_lnY)), "\n")
cat("  non-missing D_s   :", sum(is.finite(d_wide_pivot$D_s)), "\n")
cat("  both              :", sum(is.finite(d_wide_pivot$D_lnY) &
                                   is.finite(d_wide_pivot$D_s)), "\n")


# ==============================================================================
# 3. ACT 1 — THE MISALLOCATION WEDGE
#    Descriptive. Total labor is controlled, so the NH terms compare
#    identically-scaled bundles that are differently allocated across tasks.
# ==============================================================================
cat("\n=== ACT 1: MISALLOCATION WEDGE ===\n")

if (has_Y) {
  eq1 <- feols(
    ln_Y ~ ln_labor + ln_machines + ln_Tierra + D_NH_it + s_it +
      Educacion_Promedio + sexo + Certificado + Asistencia_tecnica | mun_wave,
    data = d, cluster = ~MUNICIPIO)
  
  eq2 <- feols(
    D_lnY ~ D_labor + D_machines + Delta_D_NH + D_s | MUNICIPIO,
    data = d_wide_pivot, cluster = ~MUNICIPIO)
  
  print(etable(eq1, eq2))
} else cat("Skipped (no output data).\n")


# ==============================================================================
# 4. ACT 2 — SEPARATION TEST  *** HEADLINE ***
#
#  Wave-2 cross-section. Immune to cross-wave credit comparability problems.
#
#  g(L_j,i2) = b0 + b1 lnT_i1 + b2 lnK_i1 + b3'Z_i2 + b4'(Z_i2 x Q_i2)
#              + b5 Q_i2 + X_i1'g + delta_m + e
#
#  Z counts are CENTERED so that b5 is the credit effect at a household of
#  average composition (uncentered, it is the effect at a household of zero
#  members, which is meaningless extrapolation).
#
#  T1: b3 = 0        separation among farms WITHOUT credit
#  T2: b3 + b4 = 0   separation among farms WITH credit
#  T3: b4 = 0        *** does credit change separation? THE PAPER'S TEST ***
# ==============================================================================
cat("\n=== ACT 2: SEPARATION ===\n")

act2 <- d_wide_pivot %>%
  filter(!is.na(non_harvest_w2), !is.na(Total_HH_w1)) %>%
  mutate(
    L_NH_w2 = non_harvest_w2,
    L_H_w2  = harvest_w2,
    D_NH_w2 = as.integer(non_harvest_w2 > 0),
    D_H_w2  = as.integer(harvest_w2 > 0),
    # CENTERED demographic counts
    Z_pm = ctr(Prime_Male_w2),
    Z_pf = ctr(Prime_Female_w2),
    Z_em = ctr(Elderly_Male_w2),
    Z_ef = ctr(Elderly_Female_w2),
    Q    = tiene_credito_w2,
    educ_w1 = Educacion_Promedio_w1,
    cert_w1 = Certificado_w1,
    sexo_w1 = sexo_w1
  )

Z_str   <- "Z_pm + Z_pf + Z_em + Z_ef"
ZxQ_str <- "Z_pm:Q + Z_pf:Q + Z_em:Q + Z_ef:Q"
X_str   <- "ln_K_w1 + ln_T_w1 + educ_w1 + cert_w1 + sexo_w1"
rhs     <- paste(Z_str, ZxQ_str, "Q", X_str, sep = " + ")

eq_nh_lpm  <- feols(as.formula(paste("D_NH_w2 ~", rhs, "| MUNICIPIO")),
                    data = act2, cluster = ~MUNICIPIO)
eq_h_lpm   <- feols(as.formula(paste("D_H_w2 ~",  rhs, "| MUNICIPIO")),
                    data = act2, cluster = ~MUNICIPIO)
eq_nh_ppml <- fepois(as.formula(paste("L_NH_w2 ~", rhs, "| MUNICIPIO")),
                     data = act2, cluster = ~MUNICIPIO)
eq_h_ppml  <- fepois(as.formula(paste("L_H_w2 ~",  rhs, "| MUNICIPIO")),
                     data = act2, cluster = ~MUNICIPIO)

# ---- T1 / T2 / T3 ---------------------------------------------------------
Zc  <- c("Z_pm", "Z_pf", "Z_em", "Z_ef")
ZQc <- paste0(Zc, ":Q")

t_tests <- function(model, label) {
  cn <- names(coef(model))
  Zp <- intersect(Zc, cn); ZQp <- intersect(ZQc, cn); V <- vcov(model)
  gp <- function(h) tryCatch(
    car::linearHypothesis(model, h, vcov = V)$`Pr(>Chisq)`[2],
    error = function(e) NA_real_)
  c(T1 = gp(paste(Zp, "= 0")),
    T2 = gp(paste(Zp, "+", ZQp, "= 0")),
    T3 = gp(paste(ZQp, "= 0")))
}

tests <- rbind(
  "NH extensive (LPM)"  = t_tests(eq_nh_lpm,  "NH ext"),
  "NH intensive (PPML)" = t_tests(eq_nh_ppml, "NH int"),
  "H extensive (LPM)"   = t_tests(eq_h_lpm,   "H ext"),
  "H intensive (PPML)"  = t_tests(eq_h_ppml,  "H int")
)
cat("\nSeparation tests (p-values):\n")
cat("  T1 rejects -> separation FAILS without credit (friction exists)\n")
cat("  T2 fails to reject -> separation HOLDS with credit\n")
cat("  T3 rejects -> credit CHANGES separation  [THE TEST]\n\n")
print(round(tests, 4))

# ---- marginal effects by credit regime (for the write-up) ------------------
slopes <- function(model, label) {
  b <- coef(model); V <- vcov(model)
  out <- lapply(Zc, function(z) {
    zq <- paste0(z, ":Q")
    if (!z %in% names(b)) return(NULL)
    e0 <- b[z]; s0 <- sqrt(V[z, z])
    e1 <- b[z] + b[zq]
    s1 <- sqrt(V[z, z] + V[zq, zq] + 2 * V[z, zq])
    data.frame(model = label, demo = z,
               no_credit = round(e0, 3), se_nc = round(s0, 3),
               with_credit = round(e1, 3), se_wc = round(s1, 3))
  })
  do.call(rbind, out)
}
cat("\nMarginal effects by credit regime:\n")
print(rbind(slopes(eq_nh_ppml, "NH int"), slopes(eq_h_ppml, "H int")))


# ==============================================================================
# 5. ACT 3 — CREDIT AND INVESTMENT
#    No IV. Descriptive financing-sensitivity test with an opportunity proxy.
# ==============================================================================
cat("\n=== ACT 3: CREDIT AND INVESTMENT ===\n")

act3 <- d_wide_pivot %>%
  filter(!is.na(machines_w1), !is.na(Tierra_w1), !is.na(machines_w2)) %>%
  mutate(
    delta_lnK      = ln_K_w2_helper <- log(pmax(machines_w2, 1)) - ln_K_w1,
    land_cap_ratio = ln_T_w1 - ln_K_w1,
    lcr_c          = ctr(land_cap_ratio),     # centered for the interaction
    Q_i1           = Q_w1,
    educ_w1        = Educacion_Promedio_w1,
    cert_w1        = Certificado_w1
  )

cat("N =", nrow(act3),
    "| share with positive capital growth:",
    round(mean(act3$delta_lnK > 0, na.rm = TRUE), 3), "\n")

actA <- feols(delta_lnK ~ Q_i1 + ln_T_w1 + ln_K_w1 + educ_w1 + cert_w1 | MUNICIPIO,
              data = act3, cluster = ~MUNICIPIO)
actB <- feols(delta_lnK ~ Q_i1 + land_cap_ratio + educ_w1 + cert_w1 | MUNICIPIO,
              data = act3, cluster = ~MUNICIPIO)
actC <- feols(delta_lnK ~ Q_i1 * lcr_c + ln_K_w1 + educ_w1 + cert_w1 | MUNICIPIO,
              data = act3, cluster = ~MUNICIPIO)

print(etable(actA, actB, actC))

# marginal effect of credit across under-capitalization (for the write-up)
b <- coef(actC); V <- vcov(actC)
qn <- "Q_i1"; iq <- "Q_i1:lcr_c"
if (all(c(qn, iq) %in% names(b))) {
  grid <- quantile(act3$lcr_c, c(.1, .25, .5, .75, .9), na.rm = TRUE)
  me   <- b[qn] + b[iq] * grid
  se   <- sqrt(V[qn,qn] + grid^2 * V[iq,iq] + 2 * grid * V[qn,iq])
  cat("\nMarginal effect of credit on investment, by under-capitalization:\n")
  print(data.frame(pctile = names(grid), lcr = round(grid,2),
                   effect = round(me,3), se = round(se,3),
                   t = round(me/se,2)))
}


# ==============================================================================
# 6. ROBUSTNESS
# ==============================================================================
cat("\n=== ROBUSTNESS ===\n")

# ---- lead/lag: does predetermined composition predict current demand? ------
rhs_lag  <- paste(Z_str, "Q", X_str, sep = " + ")
rhs_lead <- paste("Prime_Male_w1 + Prime_Female_w1 + Elderly_Male_w1 + Elderly_Female_w1",
                  "Q", X_str, sep = " + ")
eq_lag  <- feols(as.formula(paste("D_NH_w2 ~", rhs_lag,  "| MUNICIPIO")),
                 data = act2, cluster = ~MUNICIPIO)
eq_lead <- feols(as.formula(paste("D_NH_w2 ~", rhs_lead, "| MUNICIPIO")),
                 data = act2, cluster = ~MUNICIPIO)

# ---- MDEs for every non-rejection -----------------------------------------
cat("\nMDEs (80% power, 5% size):\n")
mde_tab <- data.frame(
  Coefficient = c("NH ext: prime male x credit",
                  "NH int: prime male x credit",
                  "H  ext: prime male x credit",
                  "H  int: prime male x credit"),
  MDE = round(c(mde(eq_nh_lpm,  "Z_pm:Q"),
                mde(eq_nh_ppml, "Z_pm:Q"),
                mde(eq_h_lpm,   "Z_pm:Q"),
                mde(eq_h_ppml,  "Z_pm:Q")), 4))
print(mde_tab)


# ==============================================================================
# 7. TABLES
# ==============================================================================
cat("\n=== WRITING TABLES ===\n")

# ---- Table 1: summary statistics ------------------------------------------
sum_vars <- intersect(
  c("labor","non_harvest","harvest","machines","Tierra","Total_HH",
    "Educacion_Promedio","tiene_credito","Certificado","Asistencia_tecnica",
    "Prime_Male","Prime_Female","Elderly_Male","Elderly_Female"),
  names(d))
datasummary(
  formula = as.formula(paste(paste(sum_vars, collapse = " + "),
                             "~ factor(wave) * (Mean + SD)")),
  data = d, output = "Output/table1_summary.tex",
  title = "Summary statistics by wave",
  notes = "Wave 1 = baseline (2021); wave 2 = endline (2023). Labor in annual person-hours; land in m2.")

# ---- Table 2: Act 1 wedge --------------------------------------------------
if (has_Y) {
  modelsummary(
    list("(1) Levels" = eq1, "(2) First difference" = eq2),
    coef_map = c("s_it"="NH share (intensive)", "D_NH_it"="NH dummy (extensive)",
                 "D_s"="$\\Delta$ NH share", "Delta_D_NH"="$\\Delta$ NH dummy",
                 "ln_labor"="ln L", "D_labor"="$\\Delta$ ln L",
                 "ln_machines"="ln K", "D_machines"="$\\Delta$ ln K",
                 "ln_Tierra"="ln T"),
    gof_map = GOF, stars = STARS,
    title = "Output and the allocation of labor across tasks",
    notes = paste("Dependent variable: log output (col. 1), change in log output",
                  "(col. 2). Total labor is controlled throughout, so the",
                  "non-harvest terms compare identically-scaled input bundles",
                  "allocated differently across tasks.", NOTE),
    output = "Output/table2_wedge.tex")
}

# ---- Table 3: separation (HEADLINE) ----------------------------------------
modelsummary(
  list("(1) NH extensive" = eq_nh_lpm, "(2) NH intensive" = eq_nh_ppml,
       "(3) H extensive"  = eq_h_lpm,  "(4) H intensive"  = eq_h_ppml),
  coef_map = c("Z_pm"="Prime male", "Z_pf"="Prime female",
               "Z_em"="Elderly male", "Z_ef"="Elderly female", "Q"="Credit",
               "Z_pm:Q"="  Prime male $\\times$ Credit",
               "Z_pf:Q"="  Prime female $\\times$ Credit",
               "Z_em:Q"="  Elderly male $\\times$ Credit",
               "Z_ef:Q"="  Elderly female $\\times$ Credit"),
  gof_map = GOF, stars = STARS,
  title = "Separation test: household composition and task-level factor demand",
  notes = paste("Wave-2 factor demand on wave-2 demographic counts, conditioning",
                "on wave-1 (predetermined) capital and land. Demographic counts",
                "are centered, so the credit coefficient is evaluated at average",
                "household composition. Cols. 1 and 3 are linear probability",
                "models on the extensive margin; cols. 2 and 4 are PPML on the",
                "intensive margin, retaining zeros. Joint tests in Table 4.", NOTE),
  output = "Output/table3_separation.tex")

# ---- Table 4: T1/T2/T3 joint tests -----------------------------------------
tt <- as.data.frame(round(tests, 3))
tt$Specification <- rownames(tt)
tt <- tt[, c("Specification", "T1", "T2", "T3")]
writeLines(
  kable(tt, format = "latex", booktabs = TRUE, row.names = FALSE,
        caption = "Joint tests of the separation hypothesis ($p$-values)"),
  "Output/table4_tests.tex")

# ---- Table 5: Act 3 credit and investment ----------------------------------
modelsummary(
  list("(1) Land proxy" = actA, "(2) Ratio proxy" = actB,
       "(3) Credit $\\times$ under-cap." = actC),
  coef_map = c("Q_i1"="Credit (t-1)", "ln_T_w1"="ln T (t-1)",
               "ln_K_w1"="ln K (t-1)", "land_cap_ratio"="Under-capitalization",
               "lcr_c"="Under-capitalization (centered)",
               "Q_i1:lcr_c"="Credit $\\times$ Under-cap."),
  gof_map = GOF, stars = STARS,
  title = "Credit access and capital investment",
  notes = paste("Dependent variable: change in log machine capital. Investment",
                "opportunity is proxied by land and existing capitalization.",
                "Col. 3 centers the under-capitalization measure, so the credit",
                "coefficient is the effect at average under-capitalization.", NOTE),
  output = "Output/table5_investment.tex")

# ---- Appendix: lead/lag ----------------------------------------------------
modelsummary(
  list("Contemporaneous Z" = eq_lag, "Predetermined Z (wave 1)" = eq_lead),
  gof_map = GOF, stars = STARS,
  title = "Predetermined household composition and non-harvest labor",
  notes = paste("Wave-1 composition predicting wave-2 demand cannot reflect",
                "contemporaneous shocks. Persistence of household composition",
                "makes this a weak check.", NOTE),
  output = "Output/tableA1_leadlag.tex")

# ---- Appendix: MDEs --------------------------------------------------------
writeLines(
  kable(mde_tab, format = "latex", booktabs = TRUE, row.names = FALSE,
        caption = "Minimum detectable effects (80\\% power, 5\\% size)"),
  "Output/tableA2_mde.tex")

cat("\n=== DONE ===\n")
cat("MAIN:     table1_summary, table2_wedge, table3_separation,\n",
    "          table4_tests, table5_investment\n")
cat("APPENDIX: tableA1_leadlag, tableA2_mde\n")

################################################################################
#  OFF-FARM LABOR ALLOCATION
#  Companion module to analysis_no_iv.R — run AFTER it (needs `d`, `act2`).
#
#  DESIGN
#   Cross-sectional, parallel to the separation test. No switchers, no DiD,
#   no IV. Wave-2 outcome, wave-1 predetermined capital, wave-2 credit.
#
#   Pr(offfarm_ki) = f( g1 lnK_i1 + g2 (lnK_i1 x Q_i2) + g3 Q_i2
#                       + g4' Z_i2 + W_ki' g5 + delta_m )
#
#   g1        : capital-labor relationship among farms WITHOUT credit
#   g1 + g2   : same among farms WITH credit
#   g2        : the difference (THE TEST — same logic as T3)
#
#  DATA CAVEAT (must be stated in the paper)
#   Endline sector was reconstructed from the farm head's responses and applied
#   to all household members, so within-farm variation in wave 2 is zero.
#   Wave 1 has a genuine member-level monthly grid. We therefore report
#   wave 1 as the headline and wave 2 as a robustness check, NOT the reverse.
#
#  OUTPUT
#   Output/table6_offfarm.tex     main
#   Output/tableA3_offfarm_rob.tex appendix (logit + wave-2)
################################################################################

cat("\n=== OFF-FARM LABOR ALLOCATION ===\n")

# ==============================================================================
# 1. BUILD THE ESTIMATION SAMPLE
# ==============================================================================

lp <- read_excel("Data/labor_panel.xlsx")

# --- farm-level predetermined variables (wave 1) ------------------------------
farm_w1 <- d %>%
  filter(wave == 1) %>%
  transmute(
    NIM,
    machines_w1 = machines,
    Tierra_w1   = Tierra,
    ln_K_w1     = log(pmax(machines, 1)),
    ln_T_w1     = log(pmax(Tierra, 1)),
    educ_w1     = Educacion_Promedio,
    cert_w1     = Certificado
  )

# --- farm-level contemporaneous credit + composition, by wave ----------------
farm_wave <- d %>%
  transmute(
    NIM, wave, MUNICIPIO,
    Q          = tiene_credito,
    Z_pm       = Prime_Male,
    Z_pf       = Prime_Female,
    Z_em       = Elderly_Male,
    Z_ef       = Elderly_Female,
    Total_HH, Certificado
  )

# --- assemble ----------------------------------------------------------------
off <- lp %>%
  filter(!is.na(offfarm)) %>%
  left_join(farm_w1,   by = "NIM") %>%
  left_join(farm_wave, by = c("NIM", "wave")) %>%
  filter(!is.na(ln_K_w1), !is.na(Q)) %>%
  mutate(
    # CENTER capital and composition so main effects are interpretable at the
    # sample mean rather than at ln K = 0 (a farm with no machinery).
    lnK_c   = ctr(ln_K_w1),
    Zc_pm   = ctr(Z_pm),
    Zc_pf   = ctr(Z_pf),
    Zc_em   = ctr(Z_em),
    Zc_ef   = ctr(Z_ef),
    lnK_x_Q = lnK_c * Q,
    # individual controls
    edad_sq   = edad^2,
    sexo_male = as.integer(sexo_code == 1),
    member_uid = paste0(NIM, "_", person_id)
  )

cat("Estimation sample:\n")
cat("  wave 1 member-months:", sum(off$wave == 1, na.rm = TRUE), "\n")
cat("  wave 2 member-months:", sum(off$wave == 2, na.rm = TRUE), "\n")
cat("  distinct members     :", n_distinct(off$member_uid), "\n")
cat("  distinct farms       :", n_distinct(off$NIM), "\n")
cat("  off-farm rate        :", round(mean(off$offfarm, na.rm = TRUE), 3), "\n")

off <- off %>% filter(sector_principal %in% c("Agro", "Non Agro"))

# Within-farm variation check — documents the wave-2 limitation
wf <- off %>%
  group_by(NIM, wave) %>%
  summarise(varies = n_distinct(offfarm) > 1, .groups = "drop") %>%
  group_by(wave) %>%
  summarise(share_farms_with_variation = round(mean(varies), 3))
cat("\nShare of farms with within-farm variation in off-farm status:\n")
print(wf)
cat("(If wave 2 is ~0, the endline reconstruction is farm-level. Say so in text.)\n")


# ==============================================================================
# 2. ESTIMATION
# ==============================================================================

RHS_ind  <- "edad + edad_sq + sexo_male + educacion"
RHS_farm <- "Zc_pm + Zc_pf + Zc_em + Zc_ef + ln_T_w1 + educ_w1 + cert_w1"

# --- (1) Wave 1, capital only -------------------------------------------------
off1 <- feols(
  as.formula(paste("offfarm ~ lnK_c + lnK_x_Q + Q +", RHS_ind,
                   "+ harvest_season | MUNICIPIO.y + mes")),
  data = off %>% filter(wave == 1), cluster = ~NIM)

# --- (2) Wave 1, adding household composition --------------------------------
#     Ties this section to the separation test: does the composition that
#     predicts ON-farm factor demand also predict OFF-farm labor supply?
off2 <- feols(
  as.formula(paste("offfarm ~ lnK_c + lnK_x_Q + Q +", RHS_ind, "+", RHS_farm,
                   "+ harvest_season | MUNICIPIO.y + mes")),
  data = off %>% filter(wave == 1), cluster = ~NIM)

# --- (3) Wave 1, harvest-season interaction ----------------------------------
#     Does the capital-labor relationship differ in the season when labor holds
#     the comparative advantage? Direct analog of the task asymmetry.
off3 <- feols(
  as.formula(paste("offfarm ~ lnK_c * harvest_season + lnK_x_Q + Q +",
                   RHS_ind, "+", RHS_farm, "| MUNICIPIO.y + mes")),
  data = off %>% filter(wave == 1), cluster = ~NIM)

# --- (4) Pooled both waves (robustness) --------------------------------------
off4 <- feols(
  as.formula(paste("offfarm ~ lnK_c + lnK_x_Q + Q +", RHS_ind, "+", RHS_farm,
                   "+ harvest_season | MUNICIPIO.y^wave + mes")),
  data = off, cluster = ~NIM)

# --- (5) Logit, wave 1 (robustness; no municipality dummies) -----------------
#     Municipality dummies in a logit invite quasi-separation, so they are
#     dropped here rather than producing divergent coefficients.
off5 <- glm(
  as.formula(paste("offfarm ~ lnK_c + lnK_x_Q + Q +", RHS_ind, "+", RHS_farm,
                   "+ harvest_season + factor(mes)")),
  data = off %>% filter(wave == 1), family = binomial("logit"))

print(etable(off1, off2, off3, off4))


# ==============================================================================
# 3. THE REGIME TEST  (same logic as T3 in the separation test)
# ==============================================================================
cat("\n--- Capital-labor relationship by credit regime ---\n")

regime <- function(model, label) {
  b <- coef(model); V <- vcov(model)
  if (!all(c("lnK_c", "lnK_x_Q") %in% names(b))) return(NULL)
  g1  <- b["lnK_c"]
  g12 <- b["lnK_c"] + b["lnK_x_Q"]
  se1 <- sqrt(V["lnK_c","lnK_c"])
  se12<- sqrt(V["lnK_c","lnK_c"] + V["lnK_x_Q","lnK_x_Q"] +
                2*V["lnK_c","lnK_x_Q"])
  p_diff <- tryCatch(
    car::linearHypothesis(model, "lnK_x_Q = 0", vcov = V)$`Pr(>Chisq)`[2],
    error = function(e) NA_real_)
  data.frame(
    model = label,
    no_credit   = round(g1, 4),  se_nc = round(se1, 4),
    with_credit = round(g12, 4), se_wc = round(se12, 4),
    diff        = round(b["lnK_x_Q"], 4),
    p_diff      = round(p_diff, 4),
    MDE_diff    = round(mde(model, "lnK_x_Q"), 4)
  )
}

reg_tab <- rbind(
  regime(off1, "(1) W1 capital only"),
  regime(off2, "(2) W1 + composition"),
  regime(off4, "(4) Pooled")
)
print(reg_tab, row.names = FALSE)
cat("\nInterpretation:\n")
cat("  no_credit  < 0 -> capital associated with RETENTION on farm\n")
cat("  with_credit> 0 -> capital associated with SUBSTITUTION off farm\n")
cat("  p_diff     < .05 -> the two regimes differ (the test)\n")
cat("  If p_diff is not significant, report MDE_diff alongside the null.\n")


# ==============================================================================
# 4. TABLES
# ==============================================================================

CM_OFF <- c(
  "lnK_c"                    = "ln K (t-1), centered",
  "lnK_x_Q"                  = "  ln K $\\times$ Credit",
  "Q"                        = "Credit",
  "harvest_season"           = "Harvest month",
  "lnK_c:harvest_season"     = "  ln K $\\times$ Harvest month",
  "Zc_pm"                    = "Prime male",
  "Zc_pf"                    = "Prime female",
  "Zc_em"                    = "Elderly male",
  "Zc_ef"                    = "Elderly female",
  "sexo_male"                = "Male",
  "edad"                     = "Age",
  "educacion"                = "Education (years)",
  "ln_T_w1"                  = "ln T (t-1)"
)

modelsummary(
  list("(1) Capital"      = off1,
       "(2) + Composition"= off2,
       "(3) + Season"     = off3,
       "(4) Pooled waves" = off4),
  coef_map = CM_OFF, gof_map = GOF, stars = STARS,
  title = "Off-farm labor allocation of household members",
  notes = paste(
    "Dependent variable: indicator for the member working off-farm in a given",
    "month. Linear probability models. Capital is predetermined at wave 1 and",
    "centered, so the credit coefficient is evaluated at average capitalization.",
    "Demographic counts are centered. Cols. 1-3 use the baseline member-month",
    "panel; col. 4 pools both waves. Municipality and month fixed effects",
    "throughout; col. 4 uses municipality $\\times$ wave. Standard errors",
    "clustered at the farm."),
  output = "Output/table6_offfarm.tex")

modelsummary(
  list("Logit (wave 1)" = off5),
  coef_map = CM_OFF, gof_map = c("nobs"), stars = STARS,
  title = "Off-farm labor allocation: logit specification",
  notes = paste(
    "Logit estimates of the wave-1 specification, reported as a robustness",
    "check. Municipality dummies are omitted because they induce",
    "quasi-separation; month dummies are retained. Coefficients are log-odds",
    "and are not directly comparable to the linear probability estimates in",
    "the main table."),
  output = "Output/tableA3_offfarm_rob.tex")

writeLines(
  kable(reg_tab, format = "latex", booktabs = TRUE, row.names = FALSE,
        caption = "Capital and off-farm labor supply, by credit regime"),
  "Output/table7_offfarm_regime.tex")

cat("\nWritten: table6_offfarm, table7_offfarm_regime, tableA3_offfarm_rob\n")