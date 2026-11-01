# Kannada Rajyotsava (1 November): Karnataka drawn in R and filled with the Kannada flag.
# Rsquared Academy, 2026. Map data: Natural Earth (public domain) via rnaturalearth.
# install.packages(c("sf", "ggplot2", "rnaturalearth", "ragg"))
# install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev")

library(sf)
library(ggplot2)
sf_use_s2(FALSE)

# The state outline
ka <- rnaturalearth::ne_states(country = "India", returnclass = "sf")
ka <- st_union(ka[ka$name == "Karnataka", ])

# Split it at its middle latitude: yellow above, red below (the Kannada flag)
bb  <- st_bbox(ka)
mid <- unname((bb[["ymin"]] + bb[["ymax"]]) / 2)
band <- function(y0, y1) {
  st_as_sfc(st_bbox(c(xmin = bb[["xmin"]] - 1, xmax = bb[["xmax"]] + 1,
                      ymin = y0, ymax = y1), crs = st_crs(ka)))
}
yellow <- st_intersection(ka, band(mid, bb[["ymax"]] + 1))
red    <- st_intersection(ka, band(bb[["ymin"]] - 1, mid))

p <- ggplot() +
  geom_sf(data = yellow, fill = "#FFD100", colour = NA) +
  geom_sf(data = red,    fill = "#D7261E", colour = NA) +
  geom_sf(data = ka, fill = NA, colour = "#0f172a", linewidth = 0.6) +
  labs(title    = "ಕನ್ನಡ ರಾಜ್ಯೋತ್ಸವದ ಶುಭಾಶಯಗಳು",
       subtitle = "Happy Kannada Rajyotsava · 1 November",
       caption  = "Map and flag drawn in R with sf and ggplot2 · Rsquared Academy") +
  theme_void(base_family = "Nirmala UI") +   # any font with Kannada script, e.g. "Noto Sans Kannada"
  theme(plot.title    = element_text(size = 40, face = "bold", hjust = 0.5, colour = "#0f172a",
                                     margin = margin(b = 10)),
        plot.subtitle = element_text(size = 24, hjust = 0.5, colour = "#475569", margin = margin(b = 20)),
        plot.caption  = element_text(size = 15, hjust = 0.5, colour = "#475569", margin = margin(t = 20)),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(50, 50, 40, 50))

ragg::agg_png("kannada-rajyotsava.png", width = 1080, height = 1080, res = 100)
print(p)
dev.off()
