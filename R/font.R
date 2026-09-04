# Internal: register the serif font bundled with the package under the
# family name "TNM" and return the family name to use in the plots.
# The bundled font (Liberation Serif) is metric-compatible with Times
# New Roman and licensed under the SIL OFL 1.1. If it cannot be
# registered, "sans" is returned instead, which every device and
# showtext understand, so plotting still works on machines without the
# font (e.g. CRAN's Linux servers).
pkg_resolve_font <- function(font = "TNM") {
  if (!identical(font, "TNM")) return(font)      # custom font: user's job
  path <- system.file("fonts", "LiberationSerif-Regular.ttf",
                      package = "insectecol")
  if (nzchar(path) && !"TNM" %in% sysfonts::font_families()) {
    tryCatch(sysfonts::font_add("TNM", path), error = function(e) NULL)
  }
  if ("TNM" %in% sysfonts::font_families()) {
    "TNM"
  } else {
    message("Bundled font not available; falling back to the default sans font")
    "sans"
  }
}