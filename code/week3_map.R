# ============================================================
# Spatial Demography Practice 1
# Basic Mapping with tmap
# ============================================================

# 0. Load package ---------------------------------------------------------

library(tmap)

# tmap warning such as:
# "package 'tmap' was built under R version ..."
# is usually not a problem if the package loads successfully.


# 1. Set tmap mode --------------------------------------------------------

# plot mode = static map for slides, reports, and papers
tmap_mode("plot")

# view mode = interactive map using leaflet
# tmap_mode("view")


# 2. Load example spatial data -------------------------------------------

# World is an example sf object included in the tmap package
data(World, package = "tmap")

# Check available variables
names(World)


# 3. Basic choropleth map -------------------------------------------------

# HPI = Happy Planet Index
tm_shape(World) +
  tm_polygons(fill = "HPI")


# 4. Choropleth map with colour palette ----------------------------------

tm_shape(World) +
  tm_polygons(
    fill = "life_exp",                                    # variable to map
    fill.scale = tm_scale_continuous(values = "YlGnBu"),  # continuous colour scale
    fill.legend = tm_legend(
      title = "Life Expectancy",                          # legend title
      orientation = "landscape",                          # horizontal legend
      frame = FALSE                                       # remove legend box
    )
  )


# Common palettes:
# "YlGn", "YlGnBu", "GnBu", "BuGn", "PuBu", "PuBuGn", "BuPu"
# "Blues", "Greens", "Reds", "Oranges", "Purples"
# "RdYlBu", "RdYlGn", "RdBu", "Spectral"
# "viridis", "plasma", "magma", "inferno", "cividis"


# 5. Map with fixed class intervals --------------------------------------

# Here life expectancy is divided into fixed groups:
# 0-60, 60-70, 70-80, 80-90

le <- tm_shape(World) +
  tm_polygons(
    fill = "life_exp",
    fill.scale = tm_scale_intervals(
      style = "fixed",
      breaks = c(0, 60, 70, 80, 90),
      values = "YlGnBu"
    ),
    fill.legend = tm_legend(
      title = "Life Expectancy",
      orientation = "landscape",
      frame = FALSE
    )
  )

le


# 6. Add transparency -----------------------------------------------------

# fill_alpha controls transparency.
# 1.0 = fully opaque
# 0.4 = semi-transparent
# 0.0 = fully transparent


le_a <- tm_shape(World) +
  tm_polygons(
    fill = "life_exp",
    fill_alpha = 0.5,
    fill.scale = tm_scale_intervals(
      breaks = c(0,60,70,80,90),
      values = "brewer.yl_gn_bu"
    ),
    fill.legend = tm_legend(
      title = "Life Expectancy"
    )
  )

le_a


  

# 7. Interactive map ------------------------------------------------------

# Change to interactive mode
tmap_mode("view")

le_a

# Return to static plot mode
tmap_mode("plot")

le_a


# 8. Small multiple maps --------------------------------------------------

# Compare two indicators side by side
tm_shape(World) +
  tm_polygons(
    fill = c("well_being", "life_exp"),
    fill.legend = tm_legend("")
  ) +
  tm_layout(
    panel.labels = c("A. Well-being", "B. Life Expectancy")
  )


# Compare four variables at once
tm_shape(World) +
  tm_polygons(
    fill = c("well_being", "life_exp", "pop_est", "economy")
  )


# ============================================================
# Practice 2
# Netherlands municipal data
# ============================================================

# 9. Load Netherlands municipality data ----------------------------------

data(NLD_muni, package = "tmap")

names(NLD_muni)


# 10. Map employment rate -------------------------------------------------

tm_shape(NLD_muni) +
  tm_polygons(fill = "employment_rate")


# 11. Faceted map by province --------------------------------------------

# tm_facets() creates maps separately by group.
# Here, one map is created for each province.

tm_shape(NLD_muni) +
  tm_polygons(fill = "employment_rate") +
  tm_facets(by = "province")


# 12. Compare age composition --------------------------------------------

# fill.free = FALSE makes all panels use the same colour scale.
# This is important when comparing maps.

tm_shape(NLD_muni) +
  tm_polygons(
    fill = c("pop_0_14", "pop_25_44", "pop_65plus"),
    fill.legend = tm_legend("Percentage"),
    fill.free = FALSE
  )
