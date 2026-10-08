# Internal: resolve where a figure should be written.
#
# `plot_file` NULL          -> the working directory
# a path with an extension  -> used as the file itself
# a path without one        -> a folder (created when missing); the figure
#                              goes inside it under `default_name`
#
# ggplot2::ggsave() stops on a path without an extension - "Either supply
# filename with a file extension or supply device" - which is exactly
# what a user hits when they pass a folder such as "D:/results". Every
# case is resolved here to one writable file path.
pkg_plot_path <- function(plot_file, default_name) {
  has_ext <- grepl("\\.[[:alnum:]]+$", plot_file)
  ## an existing extension-less file is taken literally
  if (!is.null(plot_file) && !has_ext && file.exists(plot_file) &&
      !dir.exists(plot_file))
    return(plot_file)
  if (is.null(plot_file) || !has_ext) {
    dir <- if (is.null(plot_file)) getwd() else plot_file
    if (!dir.exists(dir))
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    return(file.path(dir, default_name))
  }
  plot_file
}
