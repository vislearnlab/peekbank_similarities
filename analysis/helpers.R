## CLIP helpers
summarized_data <- function(data, x_var, y_var, group_var = NULL) {
  group_cols <- unique(c(x_var, group_var))  # dedup if same
  return(data |>
           dplyr::group_by(across(all_of(group_cols))) |>
           dplyr::summarize(
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

similarity_effect_plot <- function(data, x_var, y_var = "mean_value", model_type,
                                   error_bars = FALSE, group_var = NULL, size_by_n = FALSE,
                                   show_points = TRUE, show_line = TRUE, show_se = FALSE,
                                   show_cor = TRUE, show_legend = TRUE,
                                   point_alpha = NULL, line_alpha = 1, se_alpha = 0.15, line_width = 1,
                                   overall_line = FALSE, overall_alpha = 0.9,
                                   overall_colour = "black", overall_width = 1.2, base_size=11) {
  sim_type <- strsplit(x_var, "_")[[1]][1]
  if (!is.null(group_var)) data[[group_var]] <- as.factor(data[[group_var]])
  
  p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]]))
  if (!is.null(group_var)) p <- p + aes(colour = .data[[group_var]], fill = .data[[group_var]])
  
  if (is.null(point_alpha)) point_alpha <- if (!is.null(group_var)) 0.5 else 0.2
  
  point_layer <-
    if (!show_points)      NULL
  else if (size_by_n)    geom_point(aes(size = .data[["N"]]), alpha = point_alpha)
  else                   geom_point(size = 3, alpha = point_alpha)
  
  ribbon_layer <-
    if (!(show_se && show_line)) NULL
  else if (!is.null(group_var))
    stat_smooth(method = "glm", geom = "ribbon", colour = NA, alpha = se_alpha)
  else
    stat_smooth(method = "glm", geom = "ribbon", colour = NA, fill = "grey60", alpha = se_alpha)
  
  line_layer <-
    if (!show_line) NULL
  else if (!is.null(group_var))
    stat_smooth(method = "glm", geom = "line", alpha = line_alpha, linewidth = line_width)
  else
    stat_smooth(method = "glm", geom = "line", colour = "#3366FF",
                alpha = line_alpha, linewidth = line_width)
  
  overall_layer <-
    if (!overall_line) NULL
  else stat_smooth(aes(colour = NULL, fill = NULL, group = 1),
                   method = "glm", geom = "line", colour = overall_colour,
                   alpha = overall_alpha, linewidth = overall_width)
  
  p +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    point_layer +
    (if (error_bars)
      geom_linerange(aes(ymin = .data[[y_var]] - ci, ymax = .data[[y_var]] + ci), alpha = 0.1)
     else NULL) +
    ribbon_layer +
    line_layer +
    overall_layer +
    (if (size_by_n) scale_size_continuous(range = c(1, 6), name = "N") else NULL) +
    ylab("Proportion target looking in critical window") +
    scale_y_continuous(breaks = seq(0, 1, 0.2)) +
    coord_cartesian(ylim = c(0, 1)) +
    xlab(paste(model_type, sim_type, "similarity")) +
    (if (show_cor) ggpubr::stat_cor(method = "spearman") else NULL) +
    theme_gray(base_size = base_size) +
    (if (!show_legend) theme(legend.position = "none") else NULL)
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

# Function to calculate Pearson's correlation per epoch
round_to_nearest <- function(x, round_to=3) {
  round(x / round_to) * round_to
}

summarize_similarity_data_collapsed_scaled <- function(data, extra_fields=NULL) {
  group_vars = c("original_target_label", "img_key","scaled_text_similarity", "scaled_image_similarity", "scaled_ooo_similarity", "scaled_multimodal_similarity")
  if (!is.null(extra_fields)) {
    group_vars = c(group_vars, extra_fields)
  }
  return(summarized_data(
    data,
    "trial_key", 
    "mean_target_looking_critical_window", 
    group_vars
  ))
}

summarize_similarity_data_collapsed <- function(data, extra_fields=NULL) {
  group_vars = c("original_target_label", "img_key", "text_similarity", "image_similarity", "ooo_similarity", "multimodal_similarity")
  if (!is.null(extra_fields)) {
    group_vars = c(group_vars, extra_fields)
  }
  return(summarized_data(
    data,
    # fix what this key is?
    "trial_key", 
    "mean_target_looking_critical_window", 
    group_vars
  ))
}

generate_multimodal_plots <- function(data, model_type, suffix = "", title = "",
                                      group_var = NULL, size_by_n = FALSE,
                                      show_legend = TRUE, legend_position = "bottom", ...) {
  mk <- function(sim, mt) similarity_effect_plot(
    data, paste0(sim, suffix), "mean_value", mt,
    group_var = group_var, size_by_n = size_by_n, ...)
  
  p_text  <- mk("text_similarity", model_type)
  p_image <- mk("image_similarity", model_type)
  p_multi <- mk("multimodal_similarity", model_type)
  p_ooo   <- mk("ooo_similarity", "THINGS")
  
  nolegend <- function(p) p + theme(legend.position = "none")
  body <- cowplot::plot_grid(nolegend(p_text), nolegend(p_image),
                             nolegend(p_multi), nolegend(p_ooo), nrow = 2)
  
  if (show_legend && (!is.null(group_var) || size_by_n)) {
    if (legend_position == "bottom") {
      legend <- cowplot::get_legend(
        p_text + theme(legend.position = "bottom", legend.justification = "center"))
      # body, gap, legend — raise the MIDDLE value to drop the legend lower
      plots <- cowplot::plot_grid(body, NULL, legend, ncol = 1,
                                  rel_heights = c(1, 0.08, 0.12))
    } else {  # "right"
      legend <- cowplot::get_legend(p_text + theme(legend.position = "right"))
      plots <- cowplot::plot_grid(body, legend, ncol = 2, rel_widths = c(1, 0.18))
    }
  } else {
    plots <- body
  }
  
  cowplot_title <- cowplot_title(paste0(
    "Target looking and target-distractor similarity correlations for ", title))
  grid <- cowplot::plot_grid(cowplot_title, plots, rel_heights = c(0.2, 1), ncol = 1) +
    theme(plot.margin = margin(t = 5, r = 5, b = 25, l = 5))
  
  group_tag <- if (!is.null(group_var)) paste0("_by_", group_var) else ""
  size_tag  <- if (size_by_n) "_sizeN" else ""
  cowplot::save_plot(
    here("figures", paste0(model_type, "_",
                           paste(strsplit(title, " ")[[1]], collapse = "_"),
                           group_tag, size_tag, "_similarities.png")),
    grid, base_width = 16, base_height = 13, bg = "white")
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

