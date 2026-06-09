## CLIP helpers
summarized_data <- function(data, x_var, y_var, group_var = NULL) {
  group_cols <- unique(c(x_var, group_var))  # dedup if same
  return(data |>
           group_by(across(all_of(group_cols))) |>
           summarize(
             mean_value = mean(.data[[y_var]], na.rm = TRUE),
             sd_value = sd(.data[[y_var]], na.rm = TRUE),
             N = n(),
             se = sd_value / sqrt(N),
             ci = qt(0.975, N - 1) * sd_value / sqrt(N),
             lower_ci = mean_value - ci,
             upper_ci = mean_value + ci,
             .groups = 'drop'
           ) |>
           select(where(~ !all(is.na(.))))
  )
}

similarity_effect_plot <- function(data, x_var, y_var="mean_value", model_type, error_bars=FALSE) {
  sim_type <- strsplit(x_var, "_")[[1]][1]
  ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    geom_point(size = 3, alpha = 0.2) +
    (if (error_bars)
      geom_linerange(aes(ymin = .data[[y_var]] - ci, ymax = .data[[y_var]] + ci), alpha = 0.1)
     else NULL) +
    geom_smooth(method = "glm") +
    ylab("Proportion target looking in critical window") +
    scale_y_continuous(breaks = seq(0, 1, 0.2)) +
    coord_cartesian(ylim = c(0, 1)) +
    xlab(paste(model_type, sim_type, "similarity")) +
    ggpubr::stat_cor(method = "spearman")
}

similarity_age_half_plot <- function(data, x_var, y_var="mean_value", mean_age="19.5", group_var="age_half",model_type) {
  sim_type <- strsplit(x_var, "_")[[1]][1]
  ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]], color = .data[[group_var]])) +
    geom_hline(yintercept=0.5,linetype="dashed")+
    geom_point(size = 3, alpha = 0.5) +
    geom_smooth(method = "glm") +
    geom_linerange(aes(ymin = .data[[y_var]] - ci, ymax = .data[[y_var]] + ci), width = 0.02, alpha = 0.1) + 
    geom_label_repel(aes(label = paste(Trials.targetImage, "-", Trials.distractorImage)), max.overlaps = 3) +
    scale_color_brewer(palette = "Set2", name="Age half") +  # Using RColorBrewer for colors
    ylab("Proportion target looking in critical window") +
    xlab(paste(model_type,sim_type,"similarity")) +
    ggpubr::stat_cor(method = "spearman") +
    labs(caption=paste0("Labels are in the order of target-distractor. M=",mean_age," months"))
}

summarize_similarity_data <- function(data, extra_fields=NULL) {
  group_vars = c("trial_type_id", "image_description_target", "image_description_distractor", "text_similarity", "image_similarity", "multimodal_similarity")
  if (!is.null(extra_fields)) {
    group_vars = c(group_vars, extra_fields)
  }
  return(summarized_data(
    data,
    "trial_type_id", 
    "mean_target_looking_critical_window", 
    group_vars
  ))
}

summarize_similarity_data_collapsed <- function(data, extra_fields=NULL) {
  group_vars = c("unique_pair", "image_description_target", "image_description_distractor", "text_similarity", "image_similarity", "ooo_similarity", "multimodal_similarity")
  if (!is.null(extra_fields)) {
    group_vars = c(group_vars, extra_fields)
  }
  return(summarized_data(
    data,
    "unique_pair", 
    "mean_target_looking_critical_window", 
    group_vars
  ))
}

generate_multimodal_plots <- function(data, model_type, suffix = "", title="", error_bars=FALSE) {
  plots <- cowplot::plot_grid(
    similarity_effect_plot(data, paste0("text_similarity", suffix), "mean_value", model_type, error_bars),
    similarity_effect_plot(data, paste0("image_similarity", suffix), "mean_value", model_type, error_bars),
    similarity_effect_plot(data, paste0("multimodal_similarity", suffix), "mean_value", model_type, error_bars),
    similarity_effect_plot(data, paste0("ooo_similarity", suffix), "mean_value", model_type, error_bars),
    nrow = 2
  )
  title <- cowplot_title(paste0("Target looking and target-distractor similarity correlations for ", title))
  grid <- cowplot::plot_grid(title, plots, rel_heights = c(0.2, 1), ncol=1)
  cowplot::save_plot(here("figures",paste0(model_type,"_similarities.png")), grid, base_width = 10, base_height = 12, bg="white")
  grid
}

generate_multimodal_age_effect_plots <- function(data, model_type, suffix = "") {
  plots <- cowplot::plot_grid(
    similarity_age_half_plot(data, x_var=paste0("text_similarity", suffix), model_type=model_type),
    similarity_age_half_plot(data, x_var=paste0("image_similarity", suffix),  model_type=model_type),
    #similarity_age_half_plot(data, x_var=paste0("multimodal_similarity", suffix), model_type=model_type),
    nrow = 2
  )
  title <- cowplot_title(paste0("Target looking and semantic similarity correlations by age for ", model_type))
  grid <- cowplot::plot_grid(title, plots, rel_heights = c(0.2, 1), ncol=1)
  cowplot::save_plot(here("figures",paste0(model_type,"_age_similarities.png")), grid, base_width = 10, base_height = 12, bg="white")
  grid
}

# To add a title to the top of a cowplot arrangement
cowplot_title <- function(title_text) {
  title <- ggdraw() + 
    draw_label(
      title_text,
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      plot.margin = margin(0, 0, 0, 4)
    )
  return(title)
}

