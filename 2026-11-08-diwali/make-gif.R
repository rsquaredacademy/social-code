# Diwali GIF: diyas light up one by one, then flicker. Run from the repo root or this folder.
# Needs ggplot2, ragg and ffmpeg on the PATH.
here <- if (dir.exists("2026-11-08-diwali")) "2026-11-08-diwali" else "."
source(file.path(here, "diyas.R"))   # also rewrites frames/diya-*.png and diyas-final.png

gif_dir <- file.path(here, "gif-frames")
unlink(gif_dir, recursive = TRUE); dir.create(gif_dir)
n <- 0
put <- function(p, times = 1) {
  for (t in seq_len(times)) {
    n <<- n + 1
    ragg::agg_png(file.path(gif_dir, sprintf("f%03d.png", n)), width = 540, height = 540, res = 50)
    print(p); dev.off()
  }
}
set.seed(2026)
put(diya_plot(0), 6)                           # dark sky, 0.6 s
for (k in 1:5) put(diya_plot(k), 5)            # light one by one, 0.5 s each
for (j in 1:30) {                              # flicker, 3 s
  f <- 1 + 0.06 * sin(2 * pi * (j / 10 + (1:5) / 5)) + runif(5, -0.02, 0.02)
  put(diya_plot(5, flames = make_flames(f)))
}
put(diya_plot(5), 15)                          # hold, 1.5 s

out <- file.path(here, "diwali-2026.gif")
pal <- file.path(gif_dir, "palette.png")
system2("ffmpeg", c("-y", "-loglevel", "error", "-framerate", "10", "-i", shQuote(file.path(gif_dir, "f%03d.png")),
                    "-vf", "palettegen=stats_mode=full", shQuote(pal)))
system2("ffmpeg", c("-y", "-loglevel", "error", "-framerate", "10", "-i", shQuote(file.path(gif_dir, "f%03d.png")),
                    "-i", shQuote(pal), "-lavfi", "paletteuse=dither=sierra2_4a", "-loop", "0", shQuote(out)))
cat(sprintf("%s: %d frames, %.0f KB\n", out, n, file.size(out) / 1024))
