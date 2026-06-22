
# 1. Install and Load packages

library(tmap)

# 2. Set tmap mode

tmap_mode("plot")
tmap_mode("view")


# 3. Load example spatial data

data(World, package = "tmap")

# Check available variables
names(World)

# 4. Basic map

tm_shape(World) +
  tm_polygons(fill = "HPI")

# Here, countries are coloured by the Happy Planet Index.

# 5. Basic map with colour palette

tm_shape(World) +
  tm_polygons(
    fill = "life_exp",
    fill.scale = tm_scale_continuous(values = "viridis"),
    fill.legend = tm_legend(
      title = "Life Expectancy",
      orientation = "landscape",
      frame = FALSE
    )
  )

# Common palettes:
# "YlGn", "YlGnBu", "GnBu", "BuGn", "PuBu", "PuBuGn", "BuPu"
# "Blues", "Greens", "Reds", "Oranges", "Purples"
# "RdYlBu", "RdYlGn", "RdBu", "Spectral"
# "viridis", "plasma", "magma", "inferno", "cividis"


# 6. Map with fixed class intervals

# Here, life expectancy is divided into fixed groups: 0–60, 60–70, 70–80, and 80–90 years

le <- tm_shape(World) +
  tm_polygons(
    fill = "life_exp",
    fill.scale = tm_scale_intervals(
      style = "fixed",
      breaks = c(0, 60, 70, 80, 90),
      values = "brewer.yl_gn_bu"
    ),
    fill.legend = tm_legend(
      title = "Life Expectancy",
      orientation = "landscape",
      frame = FALSE
    )
  )

le


# 7. Add transparency

# Transparency can be controlled using `fill_alpha`.
# `fill_alpha = 1.0` is fully opaque, while `fill_alpha = 0.0` is fully transparent.

le_a <- tm_shape(World) +
  tm_polygons(
    fill = "life_exp",
    fill_alpha = 0.5,
    fill.scale = tm_scale_intervals(
      breaks = c(0, 60, 70, 80, 90),
      values = "brewer.yl_gn_bu"
    ),
    fill.legend = tm_legend(
      title = "Life Expectancy"
    )
  )

le_a

# 8. Interactive map

tmap_mode("view")

le_a

tmap_mode("plot")

le_a


# 8. Multiple maps

# Multiple maps allow us to compare indicators side by side.


tm_shape(World) +
  tm_polygons(
    fill = c("well_being", "life_exp"),
    fill.legend = tm_legend("")
  ) +
  tm_layout(
    panel.labels = c("A. Well-being", "B. Life Expectancy")
  )

tm_shape(World) +
  tm_polygons(
    fill = c("well_being", "life_exp", "pop_est", "economy")
  )


tm_shape(World) +
  tm_polygons(
    fill = c("well_being", "life_exp", "pop_est", "economy")
  )

# 9. Faceted Maps


data(NLD_muni, package = "tmap") 

names(NLD_muni)


tm_shape(NLD_muni) 
+ tm_polygons(fill = "employment_rate")


# This map shows the national spatial distribution of employment rates.
# We now split the map by province using tm_facets().


tm_shape(NLD_muni) +
  tm_polygons(fill = "employment_rate") +
  tm_facets(by = "province")

############# practice
tm_shape(NLD_muni) +
  tm_polygons(
    fill = c("pop_0_14", "pop_15_24" , "pop_25_44", "pop_65plus"),
    fill.scale = tm_scale_continuous(values = "viridis"),
    fill.legend = tm_legend("Percentage"),
    fill.free = FALSE
  )

# Student 1: Employment rate
tm_shape(NLD_muni) +
  tm_polygons(
    fill = "employment_rate",
    fill.scale = tm_scale_continuous(values = "viridis")
  ) +
  tm_facets(by = "province")

# Student 2: Population aged 0–14
tm_shape(NLD_muni) +
  tm_polygons(
    fill = "pop_0_14",
    fill.scale = tm_scale_continuous(values = "viridis")
  ) +
  tm_facets(by = "province")

# Student 3: Population aged 25–44
tm_shape(NLD_muni) +
  tm_polygons(
    fill = "pop_25_44",
    fill.scale = tm_scale_continuous(values = "viridis")
  ) +
  tm_facets(by = "province")

# Student 4: Population aged 65+
tm_shape(NLD_muni) +
  tm_polygons(
    fill = "pop_65plus",
    fill.scale = tm_scale_continuous(values = "viridis")
  ) +
  tm_facets(by = "province")


# Student 5: Urbanity
tm_shape(NLD_muni) +
  tm_polygons(
    fill = "urbanity",
    fill.scale = tm_scale_continuous(values = "viridis")
  ) +
  tm_facets(by = "province")
