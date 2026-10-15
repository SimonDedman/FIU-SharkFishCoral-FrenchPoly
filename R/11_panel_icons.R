# =============================================================================
# R/11_panel_icons.R
# Clean, parametric "high island" and "atoll" panel icons.
#
# Replaces the auto-traced HI-icon / Atoll-icon SVGs (which carried hundreds of
# tracing-noise paths and could never be symmetric). Everything here is drawn
# from geometric primitives, so symmetry, uniform dot spacing and uniform waves
# are guaranteed by construction.
#
# Black line-art on white: black land/seamount silhouette, black dotted reef,
# black waves. Exports standalone SVG + PDF + PNG to figures/.
#
# Dot size is identical across both icons: every icon is rendered at the same
# pixels-per-native-unit (SCALE), so a given native radius maps to the same
# on-screen size regardless of each icon's width.
# =============================================================================

library(grid)

# ---- palette ---------------------------------------------------------------
col_land <- "#111111"   # island / seamount silhouette (filled)
col_reef <- "#111111"   # dotted reef
col_wave <- "#111111"   # waves
col_bg   <- "transparent"  # background (sky + sea); "white" for solid pages

# ---- canvas / style constants ----------------------------------------------
H      <- 60     # drawing height (shared)
SEA    <- 38     # sea-surface (waterline) height
SCALE  <- 6      # pixels per native unit (keeps dot sizes identical)
DOT_R  <- 0.7    # reef dot radius (native units) - identical in both icons
DOT_SP <- 2.6    # reef dot spacing (native units)

# Shared crop frame so both icons export at exactly the same size: width just
# outside the wider island base (the high island), height just above its summit.
CROP_W <- 78     # cropped width (native)
CROP_H <- 57     # cropped height (native)
HI_CX  <- 50     # high-island centre x
AT_CX  <- 60     # atoll centre x

# ---- helpers ---------------------------------------------------------------

# Sine "wave" line from an anchor (x0, phase 0 = node) out to x1. The crest
# rises away from the anchor in both directions (abs), so anchoring at a reef
# makes the wave descend onto it identically on either side.
wave_grob <- function(x0, x1, y = SEA, amp = 1.15, wl = 7.5,
                      col = col_wave, lwd = 2.4, n = 280) {
  x  <- seq(x0, x1, length.out = n)
  yy <- y + amp * sin(2 * pi * abs(x - x0) / wl)
  linesGrob(x, yy, default.units = "native",
            gp = gpar(col = col, lwd = lwd, lineend = "round"))
}

# Uniformly spaced round dots along a dense path, resampled by arc length.
dotted_path <- function(x, y, spacing = DOT_SP, r = DOT_R, col = col_reef) {
  d  <- c(0, cumsum(sqrt(diff(x)^2 + diff(y)^2)))
  at <- seq(0, max(d), by = spacing)
  px <- approx(d, x, at)$y
  py <- approx(d, y, at)$y
  circleGrob(px, py, r = unit(r, "native"),
             gp = gpar(col = NA, fill = col), default.units = "native")
}

# Round the corners of a polyline by densifying then moving-averaging. Unlike a
# spline this never overshoots, so peaks soften instead of growing or bulging.
# Endpoints are held fixed (padded by replication).
round_corners <- function(x, y, win = 11, step = 0.5) {
  d  <- c(0, cumsum(sqrt(diff(x)^2 + diff(y)^2)))
  at <- seq(0, max(d), by = step)
  xi <- approx(d, x, at)$y
  yi <- approx(d, y, at)$y
  pad <- (win - 1) %/% 2
  ker <- rep(1 / win, win)
  sm <- function(v) {
    s <- stats::filter(c(rep(v[1], pad), v, rep(v[length(v)], pad)), ker)
    as.numeric(s)[!is.na(s)]
  }
  list(x = sm(xi), y = sm(yi))
}

# x-locations where a dense path crosses the waterline (sorted ascending).
sea_crossings <- function(x, y, sea = SEA) {
  s   <- sign(y - sea)
  idx <- which(diff(s) != 0)
  xc  <- x[idx] + (sea - y[idx]) / (y[idx + 1] - y[idx]) * (x[idx + 1] - x[idx])
  sort(xc)
}

# ---- HIGH ISLAND -----------------------------------------------------------
# Volcanic cone with a crater notch at the summit; peak above water. A short
# dotted fringing reef bows away from the slope then tucks back to touch the
# island at both ends (cf. panel 1).
high_island_grob <- function() {
  W  <- 100
  bl <- 14; br <- 86; bb <- 0          # base corners

  # volcano outline: only the summit is corner-rounded; the flanks and base are
  # kept as exact straight lines (the flank slope continues into the summit).
  vo <- round_corners(c(34,    44,   47, 50,   53, 56,   66),
                      c(37.67, 56.5, 54, 52.5, 54, 56.5, 37.67))
  top_x <- c(bl, vo$x, br)
  top_y <- c(bb, vo$y, bb)

  # fringing reef (side = +1 left, -1 right): tapers in at the base (like the
  # atoll reef), swells to a bulb that rolls over the surface and curls down to
  # touch the island below the apex. Absolute control points (left), mirrored.
  fringe <- function(side) {
    cx <- c(18.5, 23.0, 25.8, 27.5, 28.6, 31.0, 33.0, 33.7)
    cy <- c( 8.5, 19.0, 27.5, 34.0, 37.6, 41.0, 39.3, 37.0)
    i  <- seq_along(cx)
    dx <- spline(i, cx, n = 300)$y
    dy <- spline(i, cy, n = 300)$y
    if (side < 0) dx <- W - dx
    cr <- sea_crossings(dx, dy)
    xc <- if (side > 0) min(cr) else max(cr)          # outer reef @ waterline
    list(dots = dotted_path(dx, dy), xc = xc)
  }
  fL <- fringe(+1); fR <- fringe(-1)

  g <- gTree(children = gList(
    # the island (volcano with rounded summit/crater)
    polygonGrob(top_x, top_y, default.units = "native",
                gp = gpar(col = NA, fill = col_land)),
    # fringing reef, mirrored
    fL$dots, fR$dots,
    # black waves anchored at the reef (descend onto the dot), out to each edge
    wave_grob(fL$xc, 0), wave_grob(fR$xc, W)
  ), vp = viewport(xscale = c(HI_CX - CROP_W/2, HI_CX + CROP_W/2),
                   yscale = c(0, CROP_H)))
  list(grob = g, w = CROP_W, h = CROP_H)
}

# ---- ATOLL -----------------------------------------------------------------
# Submerged volcanic cone; dotted reef rims poke just above the surface and
# trail down the flanks toward the cone; lagoon between the rims. Stretched
# horizontally and with lower (less pronounced) rims.
atoll_grob <- function() {
  W <- 120; cx <- W / 2

  # submerged cone: straight triangle flanks + base (reef follows the outer
  # edge); only the summit is corner-rounded. Rim peaks sit half as high above
  # the crater floor (15) as before: 22.5 instead of 30.
  co <- round_corners(c(40,     44,   52, 60, 68, 76,   80),
                      c(16.875, 22.5, 18, 15, 18, 22.5, 16.875))
  cone_x <- c(28, co$x, 92)
  cone_y <- c( 0, co$y,  0)

  # reef cross-section: tails track DOWN the cone -> low rims above water -> dip
  rx <- c(29, 36, 41, 50, 60, 70, 79, 84, 91)
  ry <- c( 2, 24, 41, 35, 33, 35, 41, 24,  2)
  sp <- spline(rx, ry, n = 1500)
  cr <- sea_crossings(sp$x, sp$y)        # 4 crossings: Lout, Lin, Rin, Rout

  g <- gTree(children = gList(
    # submerged cone silhouette (triangle + eroded crater)
    polygonGrob(cone_x, cone_y, default.units = "native",
                gp = gpar(col = NA, fill = col_land)),
    # dotted reef ring
    dotted_path(sp$x, sp$y),
    # black ocean waves: plot edge -> reef, both sides
    wave_grob(0, cr[1]), wave_grob(cr[4], W),
    # lagoon wave: longer, shallower; reef -> reef from the inside
    wave_grob(cr[2], cr[3], amp = 0.75, wl = 13)
  ), vp = viewport(xscale = c(AT_CX - CROP_W/2, AT_CX + CROP_W/2),
                   yscale = c(0, CROP_H)))
  list(grob = g, w = CROP_W, h = CROP_H)
}

# ---- render ----------------------------------------------------------------
fig_dir <- "figures"

render_icon <- function(grob_fun, stem) {
  spec <- grob_fun()
  px_w <- round(spec$w * SCALE)
  px_h <- round(spec$h * SCALE)
  png(file.path(fig_dir, paste0(stem, ".png")), width = px_w, height = px_h,
      bg = col_bg, type = "cairo")
  grid.draw(spec$grob); dev.off()
  # vector exports skipped while iterating; set ICON_PREVIEW="" for full output
  if (!nzchar(Sys.getenv("ICON_PREVIEW"))) {
    svg(file.path(fig_dir, paste0(stem, ".svg")), width = px_w/96, height = px_h/96,
        bg = col_bg)
    grid.draw(spec$grob); dev.off()
    cairo_pdf(file.path(fig_dir, paste0(stem, ".pdf")), width = px_w/96, height = px_h/96,
              bg = col_bg)
    grid.draw(spec$grob); dev.off()
  }
  invisible(NULL)
}

render_icon(high_island_grob, "HI-icon")
render_icon(atoll_grob,       "Atoll-icon")

message("Wrote HI-icon and Atoll-icon (.png/.svg/.pdf) to ", fig_dir, "/")
