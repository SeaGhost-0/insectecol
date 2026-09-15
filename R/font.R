# Internal: make sysfonts-registered fonts render on ALL devices
# (pdf/ps/bitmap/onscreen). Without this, pdf() resolves families
# through its own PostScript font database and knows nothing about
# the TTF we just registered -> "failed to find PDF CID font".
pkg_enable_showtext <- function() {
  ok <- requireNamespace("showtext", quietly = TRUE)
  if (ok) showtext::showtext_auto()
  ok
}

# Internal: register the serif font for the plots and return the family
# name to use.
#
# Policy (when the font argument is "TNM"):
# 1. Times New Roman installed on the system (Windows, macOS) - preferred,
#    registered under its real family name (plus the old "TNM" alias).
# 2. The Liberation Serif bundled with the package - fallback for
#    machines without Times New Roman (e.g. CRAN's Linux servers). It is
#    metric-compatible with Times New Roman and licensed under the SIL
#    OFL 1.1, so the figure layout is the same either way.
# 3. "sans" - last resort, which every device and showtext understand.
#
# The bold/italic variants are registered when their files exist, so
# face = "bold" is really bold in showtext exports (showtext cannot
# synthesise a bold weight from the regular file alone).

# Add a font family to sysfonts; returns FALSE (instead of stopping)
# when a file is missing or cannot be read
pkg_add_family <- function(family, regular, bold = "", italic = "",
                           bolditalic = "") {
  exist <- function(p) if (length(p) == 1 && nzchar(p) && file.exists(p)) p else ""
  regular <- exist(regular)
  if (!nzchar(regular)) return(FALSE)
  success <- TRUE
  tryCatch(
    sysfonts::font_add(family,
                       regular    = regular,
                       bold       = exist(bold),
                       italic     = exist(italic),
                       bolditalic = exist(bolditalic)),
    error = function(e) success <<- FALSE)
  success
}

# Candidate files of a locally installed Times New Roman
# (regular / bold / italic / bold-italic); the first row whose regular
# file exists wins
pkg_tnr_candidates <- function() {
  windir <- Sys.getenv("WINDIR")
  if (!nzchar(windir)) windir <- Sys.getenv("SystemRoot")
  if (!nzchar(windir)) windir <- "C:/Windows"
  cand <- rbind(
    c(file.path(windir, "Fonts", "times.ttf"),
      file.path(windir, "Fonts", "timesbd.ttf"),
      file.path(windir, "Fonts", "timesi.ttf"),
      file.path(windir, "Fonts", "timesbi.ttf")),
    c("C:/Windows/Fonts/times.ttf", "C:/Windows/Fonts/timesbd.ttf",
      "C:/Windows/Fonts/timesi.ttf", "C:/Windows/Fonts/timesbi.ttf"),
    c("/System/Library/Fonts/Supplemental/Times New Roman.ttf",
      "/System/Library/Fonts/Supplemental/Times New Roman Bold.ttf",
      "/System/Library/Fonts/Supplemental/Times New Roman Italic.ttf",
      "/System/Library/Fonts/Supplemental/Times New Roman Bold Italic.ttf"),
    c("/Library/Fonts/Times New Roman.ttf",
      "/Library/Fonts/Times New Roman Bold.ttf",
      "/Library/Fonts/Times New Roman Italic.ttf",
      "/Library/Fonts/Times New Roman Bold Italic.ttf")
  )
  colnames(cand) <- c("regular", "bold", "italic", "bolditalic")
  cand
}

pkg_resolve_font <- function(font = "TNM") {
  if (!identical(font, "TNM")) return(font)      # custom font: user's job
  family <- "Times New Roman"
  if (family %in% sysfonts::font_families()) return(family)   # already set up in this session

  ## 1) Times New Roman installed on the system (preferred)
  cand <- pkg_tnr_candidates()
  for (i in seq_len(nrow(cand))) {
    if (!pkg_add_family(family, cand[i, "regular"], cand[i, "bold"],
                        cand[i, "italic"], cand[i, "bolditalic"])) next
    pkg_add_family("TNM", cand[i, "regular"], cand[i, "bold"],
                   cand[i, "italic"], cand[i, "bolditalic"])    # keep the old alias
    message("Using the system Times New Roman (", cand[i, "regular"], ")")
    # Classic Windows devices resolve family names through the
    # windowsFonts() table; register the aliases so on-screen previews
    # find the font there too. showtext (the exports) and ragg do not
    # need this.
    tryCatch(
      grDevices::windowsFonts(
        `Times New Roman` = grDevices::windowsFont("Times New Roman"),
        TNM = grDevices::windowsFont("Times New Roman")),
      error = function(e) NULL)
    if (!pkg_enable_showtext() &&
        !identical(names(grDevices::dev.cur()), "windows")) {
      # showtext unavailable and we cannot guarantee rendering on
      # this device; degrade gracefully instead of erroring in check
      return("sans")
    }
    return(family)
  }

  ## 2) Bundled fallback: Liberation Serif, metric-compatible with Times
  ##    New Roman, so plotting still works on machines without the font
  ##    (e.g. CRAN's Linux servers)
  bundled <- function(f) {
    p <- system.file("fonts", f, package = "insectecol")
    if (nzchar(p)) p else ""
  }
  regular <- bundled("LiberationSerif-Regular.ttf")
  if (pkg_add_family(family, regular, bundled("LiberationSerif-Bold.ttf"),
                     bundled("LiberationSerif-Italic.ttf"),
                     bundled("LiberationSerif-BoldItalic.ttf"))) {
    pkg_add_family("TNM", regular, bundled("LiberationSerif-Bold.ttf"),
                   bundled("LiberationSerif-Italic.ttf"),
                   bundled("LiberationSerif-BoldItalic.ttf"))
    message("Times New Roman not found on this system; using the bundled ",
            "Liberation Serif (", regular, ")")
  }
  if (family %in% sysfonts::font_families()) {
    if (!pkg_enable_showtext()) return("sans")
    return(family)
  }

  ## 3) Last resort
  message("No Times New Roman and no bundled font available; ",
          "falling back to the default sans font")
  "sans"
}
