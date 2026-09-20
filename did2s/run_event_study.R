# ==============================================================================
# Olah Data Semarang
# WhatsApp : +6285227746673 | IG : @olahdatasemarang_
# Advanced DID & Event-Study Analysis using did2s (Gardner, 2021)
# ==============================================================================

library(did2s)
library(ggplot2)
library(fixest)

# 1. Load Data
cat("Loading data...\n")
data <- read.csv("C:/Users/User/did2s/did2s/did2s.csv", sep = ";")

# 2. Static Two-Stage DID (Gardner, 2021)
cat("\n=== 1. STATIC DID (Gardner, 2021) ===\n")
static_model <- did2s(
  data = data,
  yname = "dep_var",
  treatment = "treat",
  first_stage = ~ 0 | unit + year,
  second_stage = ~ i(treat, ref = FALSE),
  cluster_var = "unit",
  verbose = FALSE
)
print(summary(static_model))

# 3. Dynamic Two-Stage DID (Event Study with Binned Relative Year)
cat("\n=== 2. DYNAMIC DID (Event Study - Gardner, 2021) ===\n")
dynamic_model <- did2s(
  data = data,
  yname = "dep_var",
  treatment = "treat",
  first_stage = ~ 0 | unit + year,
  second_stage = ~ i(rel_year_binned, ref = c(-1, Inf)),
  cluster_var = "unit",
  verbose = FALSE
)
print(summary(dynamic_model))

# Simpan Plot Dynamic DID (did2s)
png("C:/Users/User/did2s/did2s/event_study_did2s.png", width = 900, height = 550, res = 120)
iplot(
  dynamic_model,
  xlab = "Waktu Relatif terhadap Perlakuan (rel_year_binned)",
  main = "Event Study: Dua Tahap DiD (Gardner, 2021)",
  sub = "Referensi: t = -1 dan Tidak Pernah Diberi Perlakuan (Inf)"
)
dev.off()
cat("Saved: event_study_did2s.png\n")

# 4. Multi-Estimator Event Study Comparison (All Modern DiD Estimators)
cat("\n=== 3. MULTI-ESTIMATOR COMPARISON ===\n")
es_all <- event_study(
  data = data,
  yname = "dep_var",
  idname = "unit",
  tname = "year",
  gname = "g",
  estimator = "all"
)

# Plot Multi-Estimator (horizon -6 s/d 6 agar fokus pada jendela event)
p_comp <- plot_event_study(es_all, horizon = c(-6, 6)) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Perbandingan Estimator Event-Study DiD",
    subtitle = "Membandingkan TWFE Bias vs Metode Robust (Gardner, CS, SA, BJS, RS)",
    x = "Waktu Relatif terhadap Perlakuan",
    y = "Estimasi Treatment Effect"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

ggsave("C:/Users/User/did2s/did2s/event_study_comparison.png", plot = p_comp, width = 10, height = 6, dpi = 300)
cat("Saved: event_study_comparison.png\n")

cat("\nDone successfully!\n")
