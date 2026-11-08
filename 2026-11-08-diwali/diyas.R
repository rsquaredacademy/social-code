# Diwali (Festival of Lights): row of 5 diyas (oil lamps) drawn in ggplot2
# Frames for a looping GIF (assembled with ffmpeg in make-gif.R) and the final still
# Rsquared Academy, 2026
# install.packages(c("ggplot2", "ragg"))

library(ggplot2)

# --- Helpers: Geometry generation --------------------------------------------

# Parametric teardrop flame shape
flame_poly <- function(cx, cy, w, h, id = 1, part = "out") {
  u <- seq(0, 1, length.out = 30)
  xr <- w * (1 - u) * sin(pi * u^0.65) * 1.5
  yr <- h * u
  data.frame(x = c(cx, cx + xr, cx - rev(xr)),
             y = c(cy, cy + yr, cy + rev(yr)),
             id = id, part = part)
}

# 5 diya centers and dimensions
xs <- seq(14, 86, length.out = 5)
y_diya <- 38
bw <- 7.0
bh <- 12.0

# Clay bowls (#B5562E) and darker rims (#7C2D12)
bowls <- do.call(rbind, lapply(seq_along(xs), function(i) {
  th <- seq(0, pi, length.out = 50)
  data.frame(x = xs[i] + bw * cos(th), y = y_diya - bh * (sin(th))^0.85, id = i)
}))

rims <- do.call(rbind, lapply(seq_along(xs), function(i) {
  th <- seq(0, 2 * pi, length.out = 60)
  data.frame(x = xs[i] + bw * cos(th), y = y_diya + 2.5 * sin(th), id = i)
}))

oils <- do.call(rbind, lapply(seq_along(xs), function(i) {
  th <- seq(0, 2 * pi, length.out = 50)
  data.frame(x = xs[i] + (bw - 0.8) * cos(th), y = y_diya + 1.6 * sin(th), id = i)
}))

# Wicks
wicks <- data.frame(x = xs, xend = xs + 0.4, y = y_diya + 0.5, yend = y_diya + 4.5)

# Flame layers: glow, outer (#FF7A00), inner core (#FFC93C)
# flicker: one height factor per diya (1 = still flame)
make_flames <- function(f = rep(1, length(xs))) {
  glow <- do.call(rbind, lapply(seq_along(xs), function(i) {
    th <- seq(0, 2 * pi, length.out = 60)
    do.call(rbind, lapply(1:3, function(r) data.frame(
      x = xs[i] + (2.5 + 1.5 * r) * cos(th), y = y_diya + 13 * f[i] + (7 + 3 * r) * sin(th),
      id = i * 10 + r, part = "glow", diya = i)))
  }))
  out <- do.call(rbind, lapply(seq_along(xs), function(i) cbind(flame_poly(xs[i], y_diya + 3.8, 3.2, 24.0 * f[i], i, "out"), diya = i)))
  inn <- do.call(rbind, lapply(seq_along(xs), function(i) cbind(flame_poly(xs[i], y_diya + 4.8, 1.6, 14.0 * f[i], i, "in"), diya = i)))
  rbind(glow, out, inn)
}
flames_all <- make_flames()

# Sparkles in the night sky
sparkles <- data.frame(
  x = c(8, 15, 23, 31, 40, 50, 59, 69, 77, 85, 92, 12, 28, 48, 72, 88, 20, 62, 80, 95),
  y = c(86, 72, 89, 74, 91, 75, 88, 70, 90, 76, 85, 60, 62, 58, 64, 59, 50, 52, 53, 78),
  s = c(3.0, 1.8, 3.5, 2.0, 4.0, 1.8, 3.2, 2.0, 3.8, 2.2, 3.0, 1.5, 2.0, 2.2, 1.8, 1.5, 1.2, 1.2, 1.5, 2.2),
  sh = c(8, 16, 8, 16, 8, 16, 8, 16, 8, 16, 8, 16, 8, 16, 8, 16, 16, 16, 8, 8)
)

theme_diya <- theme_void() +
  theme(plot.background = element_rect(fill = "#0f172a", colour = NA),
        plot.title   = element_text(colour = "white", size = 52, face = "bold", hjust = 0.5, margin = margin(t = 45, b = 25)),
        plot.caption = element_text(colour = "#94a3b8", size = 18, hjust = 0.5, margin = margin(t = 25, b = 45)),
        plot.margin  = margin(30, 40, 30, 40))

# --- Plotting function (<= 25 lines) -----------------------------------------

diya_plot <- function(lit = 5, title = "Happy Diwali", caption = "Made with R · Rsquared Academy",
                      flames = flames_all) {
  k <- if (length(lit) == 1) lit else max(lit)
  fl <- flames[flames$diya <= k, ]
  p <- ggplot() +
    geom_point(data = sparkles, aes(x, y, size = s, shape = factor(sh)),
               colour = "#FDE68A", alpha = 0.85, show.legend = FALSE) +
    scale_size_identity() + scale_shape_manual(values = c("16" = 16, "8" = 8)) +
    geom_polygon(data = fl[fl$part == "glow", ], aes(x, y, group = id), fill = "#FFC93C", alpha = 0.09) +
    geom_polygon(data = bowls, aes(x, y, group = id), fill = "#B5562E") +
    geom_polygon(data = rims,  aes(x, y, group = id), fill = "#7C2D12") +
    geom_polygon(data = oils,  aes(x, y, group = id), fill = "#541B08") +
    geom_segment(data = wicks, aes(x = x, y = y, xend = xend, yend = yend),
                 colour = "#271810", linewidth = 2.5, lineend = "round") +
    geom_polygon(data = fl[fl$part == "out", ], aes(x, y, group = id), fill = "#FF7A00") +
    geom_polygon(data = fl[fl$part == "in",  ], aes(x, y, group = id), fill = "#FFC93C") +
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE) +
    theme_diya
  if (!is.null(title) || !is.null(caption)) p <- p + labs(title = title, caption = caption)
  p
}

# --- Output generation -------------------------------------------------------

out_dir <- if (dir.exists("2026-11-08-diwali")) "2026-11-08-diwali" else "."
frames_dir <- file.path(out_dir, "frames")
dir.create(frames_dir, showWarnings = FALSE)

save_png <- function(plot, path) {
  ragg::agg_png(path, width = 1080, height = 1080, res = 100)
  print(plot)
  dev.off()
}

# Write frame 0 through 5 (diyas light up one by one)
for (i in 0:5) {
  save_png(diya_plot(i), file.path(frames_dir, sprintf("diya-%d.png", i)))
}

# Final graphic with title and caption
save_png(diya_plot(5), file.path(out_dir, "diyas-final.png"))
