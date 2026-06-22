# ============================================================
# Global migration chord diagram animation
# Data source: migest package
# ============================================================

install.packages(c(
  "tidyverse",
  "tweenr",
  "circlize",
  "magick"
))

# 0. Load packages --------------------------------------------------------

library(tidyverse)
library(magrittr)
library(migest)
library(tweenr)
library(circlize)
library(magick)


# 1. Load migration flow data --------------------------------------------

# Regional migration flow data
d0 <- read_csv(
  system.file("imr", "reg_flow.csv", package = "migest")
)

d0


# 2. Prepare maximum regional scale --------------------------------------

# This fixes the size of each region across frames.
# Without this, each chord diagram is rescaled every frame,
# so changes over time may be hard to see.

reg_max <- d0 %>%
  group_by(year0, orig_reg) %>%
  mutate(tot_out = sum(flow)) %>%
  group_by(year0, dest_reg) %>%
  mutate(tot_in = sum(flow)) %>%
  filter(orig_reg == dest_reg) %>%
  mutate(tot = tot_in + tot_out) %>%
  mutate(reg = orig_reg) %>%
  group_by(reg) %>%
  summarise(tot_max = max(tot) / 1e06, .groups = "drop") %$%
  `names<-`(tot_max, reg)

reg_max


# 3. Load region labels and colours --------------------------------------

d1 <- read_csv(
  system.file("vidwp", "reg_plot.csv", package = "migest")
)

d1


# 4. Create interpolated animation frames --------------------------------

# The original data are available by year intervals.
# tween_elements() creates intermediate frames between years.

d2 <- d0 %>%
  mutate(corridor = paste(orig_reg, dest_reg, sep = " -> ")) %>%
  select(corridor, year0, flow) %>%
  mutate(ease = "linear") %>%
  tween_elements(
    time = "year0",
    group = "corridor",
    ease = "ease",
    nframes = 100
  ) %>%
  as_tibble()

# Recover origin and destination region names from corridor label
d2 <- d2 %>%
  separate(
    col = .group,
    into = c("orig_reg", "dest_reg"),
    sep = " -> "
  ) %>%
  select(orig_reg, dest_reg, flow, everything()) %>%
  mutate(flow = flow / 1e06)   # convert to millions

d2


# 5. Create output folder -------------------------------------------------

dir.create("./figure", showWarnings = FALSE)


# 6. Draw one chord diagram per frame ------------------------------------

for(f in sort(unique(d2$.frame))){
  
  png(
    file = paste0("./figure/globalchord_", sprintf("%03d", f), ".png"),
    width = 1000,
    height = 1000
  )
  
  circos.clear()
  par(mar = rep(0, 4), cex = 1)
  
  circos.par(
    start.degree = 90,
    track.margin = c(-0.1, 0.1),
    gap.degree = 4,
    points.overflow.warning = FALSE
  )
  
  # Keep only the three columns required by chordDiagram()
  chord_data <- d2 %>%
    filter(.frame == f) %>%
    select(orig_reg, dest_reg, flow)
  
  # Year label for each frame
  year_label <- round(mean(d2$year0[d2$.frame == f]))
  
  chordDiagram(
    x = chord_data,
    directional = 1,
    order = d1$region,
    grid.col = d1$col1,
    annotationTrack = "grid",
    transparency = 0.25,
    annotationTrackHeight = c(0.05, 0.1),
    direction.type = c("diffHeight", "arrows"),
    link.arr.type = "big.arrow",
    diffHeight = -0.04,
    link.sort = TRUE,
    link.largest.ontop = TRUE,
    xmax = reg_max
  )
  
  # Add region labels and axes
  circos.track(
    track.index = 1,
    bg.border = NA,
    panel.fun = function(x, y) {
      
      xlim <- get.cell.meta.data("xlim")
      sector.index <- get.cell.meta.data("sector.index")
      
      reg1 <- d1 %>%
        filter(region == sector.index) %>%
        pull(reg1)
      
      reg2 <- d1 %>%
        filter(region == sector.index) %>%
        pull(reg2)
      
      circos.text(
        x = mean(xlim),
        y = ifelse(length(reg2) == 0 || is.na(reg2), 3, 4),
        labels = reg1,
        facing = "bending",
        cex = 1.1
      )
      
      if(length(reg2) > 0 && !is.na(reg2)){
        circos.text(
          x = mean(xlim),
          y = 2.75,
          labels = reg2,
          facing = "bending",
          cex = 1.1
        )
      }
      
      circos.axis(
        h = "top",
        labels.cex = 0.8,
        labels.niceFacing = FALSE,
        labels.pos.adjust = FALSE
      )
    }
  )
  
  # Add year label at the centre
  text(
    x = 0,
    y = 0,
    labels = year_label,
    cex = 3,
    font = 2
  )
  
  dev.off()
}


# 7. Combine PNG frames into GIF -----------------------------------------

files <- paste0(
  "./figure/globalchord_",
  sprintf("%03d", 0:99),
  ".png"
)

img <- image_read(files)

# Reduce image size to make GIF creation faster
img <- image_scale(img, "600x600")

animation <- image_animate(
  img,
  fps = 10,
  optimize = TRUE
)

image_write(
  animation,
  path = "./figure/global_migration_chord.gif"
)


# 8. View animation in R --------------------------------------------------

print(animation)