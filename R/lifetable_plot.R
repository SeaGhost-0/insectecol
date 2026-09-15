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
#' @param x_title Character; x axis title (default \code{"Age(days)"}).
#' @param y_title Character; y axis title (default
#'   \code{"Age-Stage Survival Rate(Sxj)"}).
#' @param legend_labels Character vector; legend labels, one per stage
#'   (immature stages plus \code{Female} and \code{Male}), e.g.
#'   \code{c("Egg", "1st instar", "Pupa", "Female", "Male")}.
#'   \code{NULL} (default) uses the stage names of the data.
#'
#' @details To keep the figure clean, each stage is drawn only over the
#'   age window in which it actually occurs (extended by two days on both
#'   sides). Immature stages are drawn as coloured dots connected by a
#'   thin line; females are grey and males black, both with diamond
#'   points. By default all text of the figure is in English; title,
#'   axis titles and legend labels can be customised.
#'
#'   The text sizes are calibrated for being drawn while showtext is
#'   active at its default internal dpi (96); \code{\link{save_results}}
#'   takes care of this when exporting. If you save the plot yourself,
#'   switch showtext on around the \code{\link[ggplot2]{ggsave}} call,
#'   otherwise the text comes out about 300/96 times too large.
#'
#' @return A ggplot object that can be customised further or saved with
#'   \code{\link[ggplot2]{ggsave}}.
#'
#' @seealso \code{\link{calc_sxj}}, \code{\link{save_results}}
#' @export
#' @examples
#' f <- system.file("extdata", "Example.csv", package = "insectecol")
#' p <- plot_sxj(read_life_table(f))
plot_sxj <- function(lt, sxj = NULL, title = NULL,
                     x_title = "Age(days)",
                     y_title = "Age-Stage Survival Rate(Sxj)",
                     legend_labels = NULL, dpi = 300) {
  if (is.null(sxj)) sxj <- calc_sxj(lt)
  stage_names <- get_stage_names(lt)
  if (is.null(title)) title <- lt$file_name
  if (is.null(legend_labels)) legend_labels <- stage_names
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

  # showtext is deliberately NOT toggled here: building a ggplot neither
  # opens nor draws on a device, so it would have no effect at this point,
  # and disabling it on exit would silently switch off showtext state that
  # is still needed when the plot is drawn later. showtext is enabled
  # where the drawing actually happens, in lt_ggsave() (see
  # lifetable_save.R) - as in the lc50 module, whose plot code contains
  # no showtext calls either.
  font <- pkg_resolve_font("TNM")

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
    scale_color_manual(values = final_colors, name = "", labels = legend_labels,
                       breaks = stage_names, drop = FALSE) +
    scale_shape_manual(values = final_shapes, name = "", labels = legend_labels,
                       breaks = stage_names, drop = FALSE) +
    labs(title = title, x = x_title, y = y_title) +
    coord_cartesian(clip = "off") +
    theme(
      text = element_text(family = font),
      plot.title.position = "panel",
      plot.title = element_text(hjust = 0.5, vjust = 2, size = 48 * (dpi / 300),
                                face = "bold", margin = margin(b = 5)),
      plot.margin = margin(0.35, 2.5, 0.2, 0.2, "cm"),
      panel.background = element_rect(fill = "white"),
      panel.grid = element_blank(),
      axis.title.y = element_text(margin = margin(r = 10, l = 5),
                                  lineheight = 0.45 * (300 / dpi)),
      axis.title.x = element_text(margin = margin(t = 5), hjust = 0.5),
      axis.title = element_text(size = 45 * (dpi / 300)),
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
                                 margin = margin(l = 2.5), hjust = 0)
    )

}
