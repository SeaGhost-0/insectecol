#' Age-Stage Survival Rate Curves
#'
#' Draws the age-stage survival rate s(x,j) of every developmental stage
#' (including the female and male adults) against age in days, in the
#' style of the classical TWOSEX-MSChart plots.
#'
#' @param lt A \code{life_table} object returned by
#'   \code{\link{read_life_table}}.
#' @param sxj Optional; the result of \code{\link{calc_sxj}}. Supplying it
#'   avoids recomputing the age-stage survival rates.
#' @param title Character; plot title. Defaults to the name of the csv
#'   file.
#' @param dpi Numeric; resolution (default 300). Only influences the
#'   scaling of the graphical elements (title, axis labels, legend), so
#'   that the plot looks identical at 300 and 600 dpi.
#'
#' @details To keep the figure clean, each stage is drawn only over the
#'   age window in which it actually occurs (extended by two days on both
#'   sides). Immature stages are drawn as coloured dots connected by a
#'   thin line; females are grey and males black, both with diamond
#'   points. All text of the figure is in English.
#'
#' @return A ggplot object that can be customised further or saved with
#'   \code{\link[ggplot2]{ggsave}}.
#'
#' @seealso \code{\link{calc_sxj}}, \code{\link{save_results}}
#' @export
#' @examples
#' f <- system.file("extdata", "Example.csv", package = "insectecol")
#' p <- plot_sxj(read_life_table(f))
plot_sxj <- function(lt, sxj = NULL, title = NULL, dpi = 300) {
  if (is.null(sxj)) sxj <- calc_sxj(lt)
  stage_names <- get_stage_names(lt)
  if (is.null(title)) title <- lt$file_name

  ## Data preprocessing: keep only the age windows in which each stage occurs
  filtered_data <- sxj %>% rbind(0) %>%
    mutate(deal = 0, row_id = row_number() - 1) %>%
    pivot_longer(cols = -row_id, names_to = "variable", values_to = "value") %>%
    group_by(variable) %>%
    mutate(
      non_zero = (value != 0),
      first_non_zero = if (any(non_zero)) min(row_id[non_zero]) else NA,
      last_non_zero = if (any(non_zero)) max(row_id[non_zero]) else NA,
      keep_start = pmax(first_non_zero - 2, 0, na.rm = TRUE),
      keep_end = pmin(last_non_zero + 2, nrow(sxj), na.rm = TRUE),
      keep = ifelse(is.na(keep_start) | is.na(keep_end), FALSE,
                    row_id >= keep_start & row_id <= keep_end)
    ) %>%
    ungroup() %>% filter(keep) %>%
    select(-non_zero, -first_non_zero, -last_non_zero,
           -keep_start, -keep_end, -keep) %>%
    filter(!is.na(value))

  x_min <- min(filtered_data$row_id, na.rm = TRUE)
  x_max <- ceiling(max(filtered_data$row_id, na.rm = TRUE))
  filtered_data$variable <- factor(filtered_data$variable, levels = stage_names)

  x_title <- "Age(days)"
  y_title <- "Age-Stage Survival Rate(Sxj)"

  # ===== Dynamic colours and point shapes =====
  special_colors <- c("Female" = "#999999", "Male" = "black")
  special_shapes <- c("Female" = 18, "Male" = 18)
  dev_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
                  "#FFFF33", "#A65628", "#F781BF", "#FBB4AE", "#B3CDE3")
  dev_shapes <- rep(20, length(dev_colors))
  final_colors <- c(); final_shapes <- c(); dev_idx <- 1
  for (stage_name in stage_names) {
    if (stage_name %in% names(special_colors)) {
      final_colors[stage_name] <- special_colors[stage_name]
      final_shapes[stage_name] <- special_shapes[stage_name]
    } else {
      if (dev_idx <= length(dev_colors)) {
        final_colors[stage_name] <- dev_colors[dev_idx]
        final_shapes[stage_name] <- dev_shapes[dev_idx]
      } else {
        final_colors[stage_name] <- dev_colors[(dev_idx - 1) %% length(dev_colors) + 1]
        final_shapes[stage_name] <- dev_shapes[(dev_idx - 1) %% length(dev_shapes) + 1]
      }
      dev_idx <- dev_idx + 1
    }
  }
  showtext_auto(enable = TRUE)
  font <- pkg_resolve_font("TNM")
  on.exit(showtext::showtext_auto(enable = FALSE), add = TRUE)
  # ===== Plot =====
  ggplot(filtered_data, aes(x = row_id, y = value, color = variable, shape = variable)) +
    geom_line(linewidth = 0.8) +
    geom_point(data = . %>% filter(value > 0), size = 2.5) +
    scale_x_continuous(
      limits = c(x_min, ceiling(x_max / 5) * 5),
      breaks = seq(floor(x_min / 5) * 5, ceiling(x_max / 5) * 5, by = 5),
      minor_breaks = seq(floor(x_min / 5) * 5, ceiling(x_max / 5) * 5, by = 1),
      guide = guide_axis(minor.ticks = TRUE), expand = c(0, 0)) +
    scale_y_continuous(
      limits = c(0, 1), breaks = seq(0, 1, by = 0.2),
      minor_breaks = seq(0, 1, by = 0.04),
      guide = guide_axis(minor.ticks = TRUE), expand = c(0, 0)) +
    scale_color_manual(values = final_colors, name = "", labels = stage_names,
                       breaks = stage_names, drop = FALSE) +
    scale_shape_manual(values = final_shapes, name = "", labels = stage_names,
                       breaks = stage_names, drop = FALSE) +
    labs(title = title, x = x_title, y = y_title) +
    coord_cartesian(clip = "off") +
    theme(
      text = element_text(family = font),
      plot.title.position = "panel",
      plot.title = element_text(hjust = 0.5, vjust = 2, size = 48 * (dpi / 300),
                                face = "bold", margin = margin(b = 6)),
      plot.margin = margin(0.5, 2.5, 0.2, 0.2, "cm"),
      panel.background = element_rect(fill = "white"),
      panel.grid = element_blank(),
      axis.title.y = element_text(margin = margin(r = 10, l = 5),
                                  lineheight = 0.45 * (300 / dpi)),
      axis.title.x = element_text(margin = margin(t = 5), hjust = 0.5),
      axis.title = element_text(size = 42 * (dpi / 300)),
      axis.text.x = element_text(margin = margin(t = 5)),
      axis.text.y = element_text(margin = margin(r = 5)),
      axis.text = element_text(size = 38 * (dpi / 300), color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.65),
      axis.ticks = element_line(color = "black", linewidth = 0.65),
      axis.ticks.length = unit(0.2, "cm"),
      legend.position = c(1.155, 0.65),
      legend.key = element_rect(fill = "white"),
      legend.key.height = unit(0.5, "cm"),
      legend.key.width = unit(0.5, "cm"),
      legend.text = element_text(size = 35 * (dpi / 300),
                                 margin = margin(l = -0.2), hjust = 0)
    )

}
