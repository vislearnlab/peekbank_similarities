# similarity_forest_lmer.R ---------------------------------------------------
# Forest plot of the similarity -> target-looking effect, fit with lmer.
# 4 panels (one per CLIP similarity space), SHARED x and y axes.
# x-axis = standardized slope (beta): outcome and each similarity are z-scored
#   on common global SDs, so betas are unit-free and comparable across panels.
#   CI = Wald interval (beta +/- t_crit * SE), Satterthwaite df from lmerTest.
#   A fixed coord_cartesian range clips wide CIs without dropping rows; a right
#   margin band holds the numeric CI labels so they don't overlap the bars.
# Rows sorted ONCE (by `sort_by_panel`) and reused across panels. Pooled on top.
# Input : usable_trials_summarized_with_sims  (trial level)
# ---------------------------------------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest)   # Satterthwaite df

# ~ Config -------------------------------------------------------------------

sim_cols <- c(image_similarity      = "Image",
              text_similarity       = "Text",
              multimodal_similarity = "Multimodal",
              ooo_similarity        = "Adult behavioral")

sort_by_panel           <- "Image"      # panel that defines the shared row order
include_age_interaction <- FALSE        # TRUE -> sim * age within each dataset
age_in_days             <- FALSE        # FALSE if `age` is already in months
x_limits                <- c(-0.4, 0.4) # shared beta axis; CIs beyond are clipped

# ~ Global scaling -----------------------------------------------------------

z <- function(x) as.numeric(scale(x))

md <- usable_trials_summarized_with_sims %>%
  filter(vanilla_trial == 1) %>%
  filter(!is.na(mean_target_looking_critical_window), !is.na(age)) %>%
  mutate(z_look = z(mean_target_looking_critical_window),
         z_age  = z(age),
         z_aoa  = z(aoa),
         z_sal  = z(MeanSaliencyDiff),
         z_text_similarity       = z(text_similarity),
         z_image_similarity      = z(image_similarity),
         z_ooo_similarity        = z(ooo_similarity),
         z_multimodal_similarity = z(multimodal_similarity))

ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

build_formula <- function(sim_raw, re) {
  zsim   <- paste0("z_", sim_raw)
  simrhs <- if (include_age_interaction) sprintf("%s*z_age", zsim) else c(zsim, "z_age")
  reformulate(c(simrhs, "z_aoa", "z_sal", re), response = "z_look")
}

# standardized slope + Wald CI
slope_to_beta <- function(b, se, df) {
  if (any(is.na(c(b, se, df))) || se <= 0 || df <= 0) return(tibble())
  crit <- qt(.975, df)
  tibble(estimate = b, lower_ci = b - crit * se, upper_ci = b + crit * se)
}

extract_beta <- function(m, sim_raw) {
  zsim <- paste0("z_", sim_raw)
  co <- tryCatch(summary(m)$coefficients, error = function(e) NULL)
  if (is.null(co) || !zsim %in% rownames(co)) return(tibble())
  slope_to_beta(co[zsim, "Estimate"], co[zsim, "Std. Error"], co[zsim, "df"])
}

# ~ 1. Per-dataset beta ------------------------------------------------------

fit_one <- function(df, sim_raw) {
  zsim <- paste0("z_", sim_raw)
  re   <- c(sprintf("(1 + %s | subject_id)", zsim),
            "(1 | original_target_label:img_key)")
  m <- tryCatch(lmer(build_formula(sim_raw, re),
                     data = df, REML = TRUE, control = ctrl),
                error = function(e) NULL)
  if (is.null(m)) return(tibble())
  extract_beta(m, sim_raw)
}

slopes <- map_dfr(names(sim_cols), function(sim_raw) {
  md %>%
    group_by(dataset_name) %>%
    group_modify(~ fit_one(.x, sim_raw)) %>%
    ungroup() %>%
    mutate(similarity = sim_cols[[sim_raw]])
})

if (!"estimate" %in% names(slopes))
  stop("All per-dataset fits failed -- inspect fit_one (likely a missing formula variable).")

# ~ 2. Pooled beta (global model) --------------------------------------------

pooled <- map_dfr(names(sim_cols), function(sim_raw) {
  zsim <- paste0("z_", sim_raw)
  re   <- c(sprintf("(1 + %s | subject_id)", zsim),
            "(1 | original_target_label:img_key)",
            "(1 | dataset_id)")
  m <- tryCatch(lmer(build_formula(sim_raw, re),
                     data = md, REML = TRUE, control = ctrl),
                error = function(e) NULL)
  if (is.null(m)) return(tibble())
  extract_beta(m, sim_raw) %>%
    mutate(dataset_name = "Pooled", similarity = sim_cols[[sim_raw]])
})

# ~ 3. Per-dataset metadata --------------------------------------------------

ds_meta <- md %>%
  group_by(dataset_name, subject_id) %>%
  summarise(subj_age = mean(age, na.rm = TRUE), .groups = "drop_last") %>%
  summarise(mean_age = mean(subj_age, na.rm = TRUE),
            n_subj   = n(), .groups = "drop") %>%
  left_join(
    md %>% group_by(dataset_name) %>%
      summarise(n_img = n_distinct(img_key), .groups = "drop"),
    by = "dataset_name"
  ) %>%
  mutate(mean_age_mos = if (age_in_days) mean_age / 30.44 else mean_age)

# ~ 4. Assemble --------------------------------------------------------------

plot_df <- bind_rows(slopes, pooled) %>%
  left_join(ds_meta, by = "dataset_name") %>%
  mutate(
    is_pooled = dataset_name == "Pooled",
    label = if_else(
      is_pooled, "Pooled",
      sprintf("%s (%.1f mo \u00b7 n=%d \u00b7 %d image pairs)",
              dataset_name, mean_age_mos, n_subj, n_img)),
    ci_lab = sprintf("%.2f [%.2f, %.2f]", estimate, lower_ci, upper_ci))

# single shared order: by sort_by_panel's beta (low -> high), Pooled on top
ord_rank <- slopes %>%
  filter(similarity == sort_by_panel) %>%
  select(dataset_name, .ord = estimate)

label_levels <- plot_df %>%
  distinct(dataset_name, label, is_pooled) %>%
  left_join(ord_rank, by = "dataset_name") %>%
  arrange(is_pooled, .ord) %>%
  pull(label)

plot_df$label      <- factor(plot_df$label, levels = label_levels)
plot_df$similarity <- factor(plot_df$similarity, levels = unname(sim_cols))

# ~ 5. Plot ------------------------------------------------------------------

lab_x <- x_limits[2] + 0.50          
plot_df <- plot_df %>%
  mutate(across(c(estimate, lower_ci, upper_ci),
                ~ pmin(pmax(.x, x_limits[1]), x_limits[2]), .names = "{.col}_c"))

p <- ggplot(plot_df, aes(x = estimate_c, y = label, color = is_pooled)) +
  geom_vline(xintercept = 0, linetype = 2, color = "gray50") +
  geom_errorbarh(aes(xmin = lower_ci_c, xmax = upper_ci_c), height = 0.2) +
  geom_point(aes(size = is_pooled)) +
  geom_text(aes(x = lab_x, label = ci_lab), hjust = 1, size = 2.3, show.legend = FALSE) +
  facet_wrap(~ similarity, nrow = 1) +                 # shared x AND y
  coord_cartesian(xlim = c(x_limits[1], lab_x + 0.05), clip = "off") +
  scale_x_continuous(breaks = seq(x_limits[1], x_limits[2], by = 0.3)) +
  scale_color_manual(values = c(`FALSE` = "black", `TRUE` = "#d95f02"), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 2.2, `TRUE` = 3.5), guide = "none") +
  labs(x = "Effect size (beta)", y = NULL) +
  theme_classic() +
  theme(strip.text         = element_text(size = 12, face = "bold"),
        axis.text.y        = element_text(size = 9),
        legend.position    = "none")

p
ggsave("similarity_forest_lmer.jpg", p, width = 20, height = 6)

