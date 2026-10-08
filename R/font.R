# Internal: 'showtext' state, as far as this package has set it.
#
# showtext_auto() returns nothing, so the state cannot be queried; it is
# tracked here instead. That is what lets every drawing function restore
# the state it found, so that plotting a gdd figure (which needs
# 'showtext' off for the per-glyph fallback) can never change how a later
# lc50 figure is rendered, and vice versa.
.pkg_font_env <- new.env(parent = emptyenv())

pkg_showtext_known <- function()
  is.logical(.pkg_font_env$showtext) && length(.pkg_font_env$showtext) == 1L

# Switch 'showtext' on or off and return the state it had before, so the
# caller can put it back with on.exit(pkg_showtext_set(prev), add = TRUE).
pkg_showtext_set <- function(on) {
  if (!requireNamespace("showtext", quietly = TRUE)) return(FALSE)
  on <- isTRUE(on)
  prev <- if (pkg_showtext_known()) .pkg_font_env$showtext else FALSE
  if (!identical(prev, on)) {
    try(showtext::showtext_auto(enable = on), silent = TRUE)
    .pkg_font_env$showtext <- on
  }
  prev
}

# Evaluate `expr` with 'showtext' in state `on`; restore afterwards.
pkg_with_showtext <- function(on, expr) {
  prev <- pkg_showtext_set(on)
  on.exit(pkg_showtext_set(prev), add = TRUE)
  force(expr)
}

# Internal: draw a plot on the current device (or on the default device,
# when none is open yet).
#
# Bitmap and screen devices keep 'showtext' off so that Chinese labels
# fall back per glyph. Vector devices cannot resolve a TrueType family at
# all - they would warn "font family not found in PostScript font
# database" and then fail with "invalid font type"/"failed to find or
# load PDF CID font" - so 'showtext' is switched on for them; it embeds
# the font, and its dpi is pinned to 72 (points per inch) to keep the
# sizes 1:1.
pkg_print <- function(plot) {
  g <- pkg_plot_grob(plot)
  dn <- names(grDevices::dev.cur())[1]
  if (!length(dn) || identical(dn, "null device")) {
    d <- getOption("device")
    dn <- if (is.function(d)) "pdf" else as.character(d)[1]
  }
  if (!dn %in% c("pdf", "postscript", "cairo_pdf", "cairo_ps", "svg",
                 "xfig", "pictex"))
    return(pkg_with_showtext(FALSE, {
      grid::grid.newpage()
      grid::grid.draw(g)
    }))
  ref <- tryCatch(showtext::showtext_opts()$dpi, error = function(e) 96)
  pkg_with_showtext(TRUE, {
    try(showtext::showtext_opts(dpi = 72), silent = TRUE)
    on.exit(try(showtext::showtext_opts(dpi = ref), silent = TRUE), add = TRUE)
    ## 'showtext' swaps the device's fonts in on the grid.newpage() hook,
    ## so the page has to start after showtext_auto() - print.ggplot()
    ## does the same, and skipping it leaves the device on its PostScript
    ## font database, where a TrueType family simply does not exist
    grid::grid.newpage()
    grid::grid.draw(g)
  })
}

# Internal: the device to write a bitmap figure with.
#
# Mixed Latin/Chinese labels need a device that falls back to another
# font for the glyphs the requested family does not contain. grDevices'
# cairo devices cannot do that, and neither can 'showtext' (it renders a
# whole string with a single font). 'ragg' can, and it leaves the
# requested family in charge of the Latin part, so digits, symbols and
# units stay in Times New Roman while Chinese is taken from the system
# CJK font. Returns NULL for devices 'ragg' does not provide (pdf, ps,
# svg, bmp, ...), which the caller then writes the classic way.
pkg_fallback_device <- function(device) {
  if (is.function(device)) return(device)
  if (!requireNamespace("ragg", quietly = TRUE)) return(NULL)
  switch(tolower(as.character(device)[1]),
         png  = ragg::agg_png,
         tiff = , tif  = ragg::agg_tiff,
         jpeg = , jpg  = ragg::agg_jpeg,
         NULL)
}

# Internal: the device function to hand to ggsave(), with 'showtext'
# switched on as soon as the device exists.
#
# ggsave() opens the device and draws, but never starts a new grid page,
# and that is the hook 'showtext' normally hangs on; calling
# showtext::showtext_begin() right after the device opens does the same
# thing without depending on the hook. Device-specific arguments are
# still forwarded untouched.
pkg_showtext_device <- function(device) {
  if (is.function(device)) {
    f <- device
  } else {
    f <- switch(tolower(as.character(device)[1]),
                pdf = grDevices::pdf,
                cairo_pdf = grDevices::cairo_pdf,
                ps = , eps = , postscript = grDevices::postscript,
                cairo_ps = grDevices::cairo_ps,
                svg = grDevices::svg,
                grDevices::pdf)
  }
  function(filename, ...) {
    ## the file argument is the first one of every device function, but
    ## ggsave() passes it as `filename` while grDevices calls it `file`
    do.call(f, c(list(filename), list(...)))
    try(showtext::showtext_begin(), silent = TRUE)
    invisible(NULL)
  }
}

pkg_vector_device <- function(device) {
  if (is.function(device)) return(FALSE)
  tolower(as.character(device)[1]) %in%
    c("pdf", "cairo_pdf", "eps", "ps", "postscript", "cairo_ps", "svg")
}

# Internal: write a ggplot with text sizes that mean what they say.
#
# Text sizes in the figures are true typographic points, on every device
# and at every dpi (see lc50_pt()/the life table theme). Bitmaps are
# written by 'ragg', vector devices by the classic device with
# 'showtext' switched on - the only way to embed a TrueType font in a
# pdf - and 'showtext' is told to work at 72 dpi, the number of points
# per inch, so that a 12 pt label is 12 pt there as well.
pkg_ggsave <- function(filename, plot, device = "png", width = 7, height = 6,
                       units = "in", dpi = 300, bg = "white", ...) {
  units <- match.arg(tolower(units), c("in", "cm", "mm", "px"))
  to_in <- function(x) switch(units, "in" = x, "cm" = x / 2.54,
                              "mm" = x / 25.4, "px" = x / dpi)
  w <- to_in(width)
  h <- to_in(height)
  dev <- pkg_fallback_device(device)
  plot <- pkg_plot_grob(plot)

  if (!is.null(dev)) {
    pkg_with_showtext(FALSE, {
      old_dev <- grDevices::dev.cur()
      dev(filename = filename, width = w, height = h, units = "in",
          res = dpi, bg = bg, ...)
      on.exit(utils::capture.output({
        grDevices::dev.off()
        if (old_dev > 1) grDevices::dev.set(old_dev)
      }), add = TRUE)
      grid::grid.draw(plot)
    })
  } else {
    ref <- tryCatch(showtext::showtext_opts()$dpi, error = function(e) 96)
    pkg_with_showtext(TRUE, {
      try(showtext::showtext_opts(dpi = 72), silent = TRUE)
      on.exit(try(showtext::showtext_opts(dpi = ref), silent = TRUE),
              add = TRUE)
      ## 'showtext' embeds the registered TrueType files, so Chinese
      ## labels are written to a pdf as well. It normally switches the
      ## device over on the grid.newpage() hook, which ggsave() never
      ## pulls, so the device is told explicitly instead - otherwise the
      ## family is looked up in the PostScript font database and the
      ## Chinese comes out missing.
      wrapped <- pkg_showtext_device(device)
      ## 'showtext' embeds the registered TrueType files, so Chinese
      ## labels are written to a pdf as well
      ggplot2::ggsave(filename, plot = plot, device = wrapped, width = w,
                      height = h, units = "in", dpi = dpi, bg = bg, ...)
    })
  }
  invisible(filename)
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
  ## font_add rejects empty-string slots; pass NULL instead
  nn <- function(p) if (nzchar(p)) p else NULL
  success <- TRUE
  tryCatch(
    sysfonts::font_add(family,
                       regular    = regular,
                       bold       = nn(exist(bold)),
                       italic     = nn(exist(italic)),
                       bolditalic = nn(exist(bolditalic))),
    error = function(e) success <<- FALSE)
  success
}

# Tell the classic devices about the family as well: unlike 'ragg', which
# matches families against the system font database, the vector devices
# resolve them through their own PostScript database and know nothing
# about a TrueType font registered in 'sysfonts'. Without this, printing
# a figure to the default device (pdf in a non-interactive session) warns
# "font family not found in PostScript font database" and then fails with
# "failed to find or load PDF CID font" on the plotmath symbols.
#
# The vector database is only asked for metrics: while no device is open,
# grid measures strings against it, and a family it does not know costs a
# "font family ... not found in PostScript font database" warning for
# every label. The glyphs themselves are drawn by 'ragg' or embedded by
# 'showtext', so what is registered here never reaches the output file.
pkg_register_device_font <- function(family, alias = NULL) {
  aliases <- unique(c(family, alias))
  aliases <- aliases[nzchar(aliases)]
  ## GDI devices keep their own database as well - Windows only, the
  ## windowsFonts()/windowsFont() functions do not exist on unix
  if (.Platform$OS.type == "windows") {
    for (nm in aliases) {
      args <- list(grDevices::windowsFont(family))
      names(args) <- nm
      tryCatch(do.call(grDevices::windowsFonts, args), error = function(e) NULL)
    }
  }
  for (db in c("pdf", "postscript")) {
    setter <- tryCatch(match.fun(paste0(db, "Fonts")),
                       error = function(e) NULL)
    if (is.null(setter)) next
    times <- tryCatch(setter()[["Times"]], error = function(e) NULL)
    if (is.null(times)) next
    for (nm in aliases) {
      args <- list(times)
      names(args) <- nm
      tryCatch(do.call(setter, args), error = function(e) NULL)
    }
  }
  invisible(NULL)
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

# Remember the serif family of the figure being drawn, and return it.
#
# A label that mixes Latin and Chinese carries only one family in its
# grob - the CJK one, see pkg_label_family() - so pkg_mixed_grob() has to
# be told separately which family the Latin runs of that same label are
# meant to be drawn in.
pkg_set_latin <- function(family) {
  .pkg_font_env$latin <- family
  family
}

pkg_latin_font <- function() {
  v <- .pkg_font_env$latin
  if (is.character(v) && length(v) == 1L && nzchar(v)) return(v)
  pkg_resolve_font("TNM")
}

pkg_resolve_font <- function(font = "TNM") {
  if (!identical(font, "TNM")) return(pkg_set_latin(font))  # user's own font
  family <- "Times New Roman"
  if (family %in% sysfonts::font_families()) {
    pkg_register_device_font(family, "TNM")   # already set up here
    return(pkg_set_latin(family))
  }

  ## 1) Times New Roman installed on the system (preferred)
  cand <- pkg_tnr_candidates()
  for (i in seq_len(nrow(cand))) {
    if (!pkg_add_family(family, cand[i, "regular"], cand[i, "bold"],
                        cand[i, "italic"], cand[i, "bolditalic"])) next
    pkg_add_family("TNM", cand[i, "regular"], cand[i, "bold"],
                   cand[i, "italic"], cand[i, "bolditalic"])    # keep the old alias
    message("Using the system Times New Roman (", cand[i, "regular"], ")")
    pkg_register_device_font(family, "TNM")
    return(pkg_set_latin(family))
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
    ## also register the vector-device font databases, so measuring a
    ## label while no device is open does not warn once per label
    pkg_register_device_font(family, "TNM")
    return(pkg_set_latin(family))
  }

  ## 3) Last resort
  message("No Times New Roman and no bundled font available; ",
          "falling back to the default sans font")
  pkg_set_latin("sans")
}

# Internal: does any drawn label contain characters outside Latin-1
# (e.g. Chinese stage names)? Labels may be character strings or
# plotmath expressions; both are deparsed before scanning. Bitmap
# devices ('ragg') take such glyphs from a fallback font; a vector
# device cannot embed them at all, hence the warning in pkg_ggsave().
pkg_has_cjk <- function(x) {
  txt <- vapply(x, function(s) paste(deparse(s), collapse = " "),
                character(1))
  txt <- txt[nzchar(txt)]
  any(grepl("[^\\x00-\\x{FF}]", txt, perl = TRUE))
}

# Candidate CJK fonts: file plus the family name the device knows it by.
# The first row whose file exists wins. SimSun comes first: it is the serif
# face that goes with Times New Roman, and it is what Chinese journals
# ask for.
#
# The family name has to be the real one: 'ragg' resolves families through
# the system font database and knows nothing about the names registered in
# 'sysfonts', so an internal name such as "insectecol-cjk" would silently
# be replaced by the default font.
pkg_cjk_candidates <- function() {
  windir <- Sys.getenv("WINDIR")
  if (!nzchar(windir)) windir <- Sys.getenv("SystemRoot")
  if (!nzchar(windir)) windir <- "C:/Windows"
  rbind(
    c(file.path(windir, "Fonts", "simsun.ttc"),  "SimSun"),
    c("C:/Windows/Fonts/simsun.ttc",             "SimSun"),
    c(file.path(windir, "Fonts", "nsimsun.ttc"), "NSimSun"),
    c("C:/Windows/Fonts/nsimsun.ttc",            "NSimSun"),
    c("/System/Library/Fonts/Supplemental/Songti.ttc", "Songti SC"),
    c("/usr/share/fonts/opentype/noto/NotoSerifCJK-Regular.ttc",
      "Noto Serif CJK SC"),
    c("/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc",
      "Noto Sans CJK SC"),
    c(file.path(windir, "Fonts", "msyh.ttc"),    "Microsoft YaHei"),
    c("C:/Windows/Fonts/msyh.ttc",               "Microsoft YaHei")
  )
}

# Internal: the family to draw Chinese (and other non-Latin) labels with,
# or NULL when no CJK font exists on this system.
#
# Only the labels that actually contain such characters are drawn with it
# (see pkg_label_family()); tick labels, numbers and symbols stay in the
# serif font. When no CJK font is found the bitmap devices used for the
# export fall back per glyph instead, so the text still shows up.
pkg_resolve_cjk <- function() {
  cand <- pkg_cjk_candidates()
  for (i in seq_len(nrow(cand))) {
    p <- cand[i, 1]
    if (!nzchar(p) || !file.exists(p)) next
    family <- cand[i, 2]
    ## registered for the 'showtext' path (vector devices) as well, under
    ## the same name, so both paths use the same font
    if (!family %in% sysfonts::font_families()) pkg_add_family(family, p)
    ## and for the vector font databases, so that measuring a Chinese
    ## label while no device is open does not warn once per label
    pkg_register_device_font(family)
    return(family)
  }
  NULL
}

# Family for a single label: the CJK font when the label contains
# characters outside Latin-1, the serif font otherwise.
#
# This is only the coarse, per-element decision, kept because it makes
# Chinese show up even when the figure is drawn by something other than
# this package (ggsave(), print()). It cannot help a label that mixes
# both scripts: one element has one family, so "Concentration (毫克/升)"
# would be set entirely in the CJK font, Latin letters included. Such
# labels are split into runs by pkg_mixed_grob() on the way out.
pkg_label_family <- function(label, font, cjk = NULL) {
  if (is.null(cjk) || !pkg_has_cjk(label)) font else cjk
}

# ---- labels that mix Latin and Chinese ----------------------------------

# Split a label into runs of characters that want the same font: Latin
# letters, digits and ASCII punctuation on one side, everything outside
# Latin-1 (Chinese, full-width punctuation) on the other.
pkg_text_runs <- function(label) {
  ch <- strsplit(label, "", fixed = TRUE)[[1]]
  if (!length(ch)) return(NULL)
  cjk <- grepl("[^\\x00-\\xFF]", ch, perl = TRUE)
  id <- cumsum(c(TRUE, cjk[-1] != cjk[-length(cjk)]))
  txt <- vapply(split(ch, id), paste, character(1), collapse = "")
  is_cjk <- vapply(split(cjk, id), any, logical(1))
  data.frame(text = unname(txt), cjk = unname(is_cjk),
             stringsAsFactors = FALSE)
}

# Width of every run in points. 'systemfonts' measures with the same
# machinery 'ragg' and 'showtext' draw with, so the runs end up where the
# device would have put them.
pkg_run_widths <- function(runs, latin, cjk, size) {
  fam <- ifelse(runs$cjk, cjk, latin)
  vapply(seq_len(nrow(runs)), function(i)
    systemfonts::string_width(runs$text[i], family = fam[i], size = size,
                              res = 72), numeric(1))
}

pkg_hjust_num <- function(h) {
  if (is.numeric(h)) return(as.numeric(h)[1])
  switch(tolower(as.character(h)[1]),
         left = 0, centre = 0.5, center = 0.5, right = 1, 0.5)
}

# Replace a text grob whose label mixes scripts with one grob per run, so
# the digits, symbols and Latin words keep the serif font and only the
# Chinese characters are taken from the CJK font.
#
# No device does this on its own: 'ragg' and the cairo devices fill the
# glyphs the requested family lacks from a fallback font the caller
# cannot choose (Microsoft YaHei on Windows), 'showtext' renders a whole
# string with a single font, and the classic vector devices have no
# fallback at all.
#
# The runs are placed along the direction the text is rotated to, so a
# y-axis title (rot = 90) stays a single upright line of text.
pkg_mixed_grob <- function(g, latin, cjk) {
  usable <- is.character(g$label) && length(g$label) == 1L &&
    pkg_has_cjk(g$label) && length(g$x) == 1L && length(g$y) == 1L &&
    !is.null(g$gp) && length(g$gp$fontsize) <= 1L
  if (!usable) return(g)
  runs <- pkg_text_runs(g$label)
  if (is.null(runs) || nrow(runs) < 2L) return(g)
  size <- g$gp$fontsize
  if (is.null(size) || length(size) != 1L || is.na(size)) size <- 12
  w <- tryCatch(pkg_run_widths(runs, latin, cjk, size),
                error = function(e) NULL)
  if (is.null(w) || length(w) != nrow(runs) || any(!is.finite(w))) return(g)
  rot <- if (is.null(g$rot) || length(g$rot) != 1L) 0 else as.numeric(g$rot)
  off <- cumsum(c(0, w[-length(w)])) + w / 2 - pkg_hjust_num(g$hjust) * sum(w)
  rad <- rot * pi / 180
  kids <- lapply(seq_len(nrow(runs)), function(i) {
    gp <- g$gp
    gp$fontfamily <- if (runs$cjk[i]) cjk else latin
    grid::textGrob(runs$text[i],
                   x = g$x + grid::unit(off[i] * cos(rad), "pt"),
                   y = g$y + grid::unit(off[i] * sin(rad), "pt"),
                   hjust = 0.5,
                   vjust = if (is.null(g$vjust)) 0.5 else g$vjust,
                   rot = rot, gp = gp, name = paste0(g$name, ".", i))
  })
  grid::grobTree(children = do.call(grid::gList, kids), name = g$name,
                 vp = g$vp)
}

# Walk a grob (or gtable) and split every text grob that needs it.
pkg_split_cjk_labels <- function(x, latin, cjk) {
  if (!is.list(x)) return(x)
  if (inherits(x, "text")) return(pkg_mixed_grob(x, latin, cjk))
  if (!is.null(x$children) && is.list(x$children))
    x$children <- lapply(x$children, pkg_split_cjk_labels, latin = latin,
                         cjk = cjk)
  if (!is.null(x$grobs) && is.list(x$grobs))
    x$grobs <- lapply(x$grobs, pkg_split_cjk_labels, latin = latin, cjk = cjk)
  x
}

# The grob to draw: a ggplot is built first, then the mixed labels in it
# are split. Returns the input unchanged when the system has no CJK font
# or when the label metrics cannot be measured.
pkg_plot_grob <- function(plot, latin = NULL) {
  g <- if (inherits(plot, "ggplot")) ggplot2::ggplotGrob(plot) else plot
  cjk <- pkg_resolve_cjk()
  if (is.null(cjk) || !is.list(g)) return(g)
  pkg_split_cjk_labels(g, if (is.null(latin)) pkg_latin_font() else latin,
                       cjk)
}
