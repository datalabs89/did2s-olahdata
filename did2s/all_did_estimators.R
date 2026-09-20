# ==============================================================================
# Olah Data Semarang
# WhatsApp : +6285227746673
# IG : @olahdatasemarang_
# ------------------------------------------------------------------------------
# Script Lengkap: Seluruh Estimator Modern Difference-in-Differences (DiD)
# 1. Two-Way Fixed Effects (TWFE Konvensional)
# 2. Gardner (2021) - Two-Stage DiD (did2s)
# 3. Callaway and Sant'Anna (2020) - did
# 4. Sun and Abraham (2020) - sunab (fixest)
# 5. Borusyak, Jaravel, and Spiess (2021) - didimputation
# 6. Roth and Sant'Anna (2021) - staggered
# 7. Multi-Estimator Unified Comparison & Plot (did2s::event_study)
# ==============================================================================

# ------------------------------------------------------------------------------
# LANGKAH 1: INSTALASI & PEMANGGILAN PAKET (PACKAGES)
# ------------------------------------------------------------------------------
packages <- c("did2s", "fixest", "did", "didimputation", "staggered", "ggplot2")

# Install package jika belum terpasang
for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}

library(did2s)
library(fixest)
library(did)
library(didimputation)
library(staggered)
library(ggplot2)

# ------------------------------------------------------------------------------
# LANGKAH 2: LOAD DATASET
# ------------------------------------------------------------------------------
# Data dapat dibaca dari file lokal maupun langsung dari URL GitHub
data_path <- "C:/Users/User/did2s/did2s/did2s.csv"

if (file.exists(data_path)) {
  data <- read.csv(data_path, sep = ";")
} else {
  data <- read.csv("https://raw.githubusercontent.com/timbulwidodostp/did2s/main/did2s/did2s.csv", sep = ";")
}

cat("=== DATA SUMMARY ===\n")
cat("Total Observasi :", nrow(data), "\n")
cat("Variabel        :", paste(names(data), collapse = ", "), "\n\n")


# ==============================================================================
# ESTIMATOR 1: TWO-WAY FIXED EFFECTS (TWFE KONVENSIONAL)
# Catatan: Rentan terhadap bias pembobotan negatif jika terdapat staggered timing
# ==============================================================================
cat("\n======================================================\n")
cat("1. TWO-WAY FIXED EFFECTS (TWFE KONVENSIONAL)\n")
cat("======================================================\n")

# Model Statis TWFE
twfe_static <- feols(dep_var ~ treat | unit + year, data = data, cluster = ~unit)
print(summary(twfe_static))

# Model Dinamis TWFE (Event Study)
twfe_dynamic <- feols(
  dep_var ~ i(rel_year_binned, ref = c(-1, Inf)) | unit + year,
  data = data,
  cluster = ~unit
)
print(summary(twfe_dynamic))


# ==============================================================================
# ESTIMATOR 2: GARDNER (2021) - TWO-STAGE DiD (did2s)
# Tahap 1: Estimasi efek tetap unit & tahun hanya pada observasi untreated
# Tahap 2: Regresikan residual hasil tahap 1 terhadap indikator treat/event
# ==============================================================================
cat("\n======================================================\n")
cat("2. GARDNER (2021) - TWO-STAGE DiD (did2s)\n")
cat("======================================================\n")

# Model Statis did2s
did2s_static <- did2s(
  data = data,
  yname = "dep_var",
  treatment = "treat",
  first_stage = ~ 0 | unit + year,
  second_stage = ~ i(treat, ref = FALSE),
  cluster_var = "unit",
  verbose = FALSE
)
print(summary(did2s_static))

# Model Dinamis did2s (Event-Study)
did2s_dynamic <- did2s(
  data = data,
  yname = "dep_var",
  treatment = "treat",
  first_stage = ~ 0 | unit + year,
  second_stage = ~ i(rel_year_binned, ref = c(-1, Inf)),
  cluster_var = "unit",
  verbose = FALSE
)
print(summary(did2s_dynamic))


# ==============================================================================
# ESTIMATOR 3: CALLAWAY AND SANT'ANNA (2020) - did
# Mengestimasi Group-Time Average Treatment Effects ATT(g,t)
# ==============================================================================
cat("\n======================================================\n")
cat("3. CALLAWAY AND SANT'ANNA (2020)\n")
cat("======================================================\n")

cs_mod <- att_gt(
  yname = "dep_var",
  tname = "year",
  idname = "unit",
  gname = "g",
  data = data,
  control_group = "nevertreated",
  base_period = "universal"
)

# Agregasi Efek Rata-Rata Sederhana (Simple ATT)
cs_simple <- aggte(cs_mod, type = "simple")
cat("\n--- Simple Aggregate ATT (Callaway & Sant'Anna) ---\n")
summary(cs_simple)

# Agregasi Dinamis (Event-Study)
cs_dynamic <- aggte(cs_mod, type = "dynamic")
cat("\n--- Dynamic Event-Study ATT (Callaway & Sant'Anna) ---\n")
summary(cs_dynamic)


# ==============================================================================
# ESTIMATOR 4: SUN AND ABRAHAM (2020) - INTERACTION-WEIGHTED (fixest::sunab)
# Menghindari bias TWFE dengan memisahkan efek per kelompok (cohort) dan waktu
# ==============================================================================
cat("\n======================================================\n")
cat("4. SUN AND ABRAHAM (2020) - sunab\n")
cat("======================================================\n")

sunab_mod <- feols(
  dep_var ~ sunab(g, year) | unit + year,
  data = data,
  cluster = ~unit
)

# Event-Study Estimates
print(summary(sunab_mod))

# Agregasi ATT Keseluruhan
cat("\n--- Aggregate ATT (Sun & Abraham) ---\n")
print(summary(sunab_mod, agg = "ATT"))


# ==============================================================================
# ESTIMATOR 5: BORUSYAK, JARAVEL, AND SPIESS (2021) - IMPUTATION (didimputation)
# Pendekatan imputasi nilai counterfactual dari kelompok untreated
# ==============================================================================
cat("\n======================================================\n")
cat("5. BORUSYAK, JARAVEL, AND SPIESS (2021) - didimputation\n")
cat("======================================================\n")

# Model Statis Imputasi
bjs_static <- did_imputation(
  data = data,
  yname = "dep_var",
  gname = "g",
  tname = "year",
  idname = "unit"
)
cat("\n--- Static Imputation ATT ---\n")
print(bjs_static)

# Model Dinamis Imputasi (Event-Study: pre-trends -6 s/d -1, horizon 0 s/d 6)
bjs_dynamic <- did_imputation(
  data = data,
  yname = "dep_var",
  gname = "g",
  tname = "year",
  idname = "unit",
  horizon = 0:6,
  pretrends = -6:-1
)
cat("\n--- Dynamic Event-Study Imputation ---\n")
print(bjs_dynamic)


# ==============================================================================
# ESTIMATOR 6: ROTH AND SANT'ANNA (2021) - staggered
# Estimator efisien berbasis pembobotan dan inferensi exact Neyman
# ==============================================================================
cat("\n======================================================\n")
cat("6. ROTH AND SANT'ANNA (2021) - staggered\n")
cat("======================================================\n")

# Simple ATT
rs_simple <- staggered(
  df = data,
  i = "unit",
  t = "year",
  g = "g",
  y = "dep_var",
  estimand = "simple"
)
cat("\n--- Simple ATT (Roth & Sant'Anna) ---\n")
print(rs_simple)

# Event Study (-6 s/d 6)
rs_dynamic <- staggered(
  df = data,
  i = "unit",
  t = "year",
  g = "g",
  y = "dep_var",
  estimand = "eventstudy",
  eventTime = -6:6
)
cat("\n--- Dynamic Event-Study (Roth & Sant'Anna) ---\n")
print(rs_dynamic)


# ==============================================================================
# 7. PERBANDINGAN MULTI-ESTIMATOR TERPADU (did2s::event_study & plot_event_study)
# ==============================================================================
cat("\n======================================================\n")
cat("7. MENJALANKAN MULTI-ESTIMATOR SEKALIGUS (did2s::event_study)\n")
cat("======================================================\n")

# Menjalankan seluruh estimator sekaligus
es_all <- event_study(
  data = data,
  yname = "dep_var",
  idname = "unit",
  tname = "year",
  gname = "g",
  estimator = "all"
)

# Plot perbandingan dengan jendela -6 s/d 6
plot_comparison <- plot_event_study(es_all, horizon = c(-6, 6)) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Perbandingan 6 Estimator Event-Study DiD",
    subtitle = "Dataset: did2s.csv | Olah Data Semarang",
    x = "Waktu Relatif terhadap Perlakuan (Event Time)",
    y = "Estimasi Treatment Effect"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

# Menyimpan visualisasi perbandingan
output_plot_path <- "C:/Users/User/did2s/did2s/event_study_all_estimators.png"
ggsave(output_plot_path, plot = plot_comparison, width = 11, height = 7, dpi = 300)
cat("\nGrafik perbandingan berhasil disimpan ke:", output_plot_path, "\n")

cat("\n======================================================\n")
cat("SELESAI / FINISHED\n")
cat("Olah Data Semarang | WhatsApp : +6285227746673 | IG : @olahdatasemarang_\n")
cat("======================================================\n")
