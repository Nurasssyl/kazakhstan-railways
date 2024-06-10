libs <- c(
  "tidyverse", "sf",
  "giscoR", "httr",
  "XML"
)

installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

invisible(lapply(libs, library, character.only = T))

main_path <- "C:/Users/Admin/Рабочий стол/R" # Установите ваш рабочий каталог
setwd(main_path)
kazakhstan_dir <- "kazakhstan_osm"
dir.create(kazakhstan_dir, showWarnings = FALSE)
out_dir_kazakhstan <- file.path(main_path, kazakhstan_dir)
setwd(out_dir_kazakhstan)

url <- "https://download.geofabrik.de/asia/kazakhstan.html"

get_osm_links <- function() {
  res <- httr::GET(url)
  parse <- XML::htmlParse(res)
  links <- XML::xpathSApply(parse, path = "//a", XML::xmlGetAttr, "href")
  all_links <- paste0("https://download.geofabrik.de", links)
  
  return(all_links)
}


all_links <- get_osm_links()

kazakhstan_links <- c(
  "https://download.geofabrik.de/asia/kazakhstan-latest-free.shp.zip"
)
options(timeout=3000)
for (link in kazakhstan_links) {
  download.file(link, destfile = basename(link), mode = "wb")
}

list.files()

setwd("..")
kazakhstan_clone_dir <- "kazakhstan_clone_osm"
dir.create(kazakhstan_clone_dir)
kazakhstan_clone_dir_out <- main_path |>
  paste0("/", kazakhstan_clone_dir)
setwd(out_dir_kazakhstan)
zip_files <- list.files()

for (z in 1:length(zip_files)) {
  zip_names <- grep("railways", unzip(zip_files[z], list = T)$Name,
                    ignore.case = T, value = T
  )
  unzip(
    zip_files[z],
    files = zip_names,
    exdir = kazakhstan_clone_dir_out, overwrite = F
  )
  x <- sample(1:length(zip_files), 1, replace = T)
  file_old <- c(list.files(kazakhstan_clone_dir_out)) # existing file names
  file_new <- c(paste0(x, "_", file_old))
  file.rename(
    paste0(kazakhstan_clone_dir_out, "/", file_old),
    paste0(kazakhstan_clone_dir_out, "/", file_new)
  )
  rm(file_old)
  rm(file_new)
}

setwd(kazakhstan_clone_dir_out)
list.files()

get_railways <- function() {
  railway_files <- list.files(
    path = kazakhstan_clone_dir_out,
    pattern = "*.shp", full.names = T
  )
  railway_list <- lapply(railway_files, sf::st_read)
  kazakhstan_railway_sf <- do.call(rbind, railway_list)
  return(kazakhstan_railway_sf)
}

kazakhstan_railway_sf <- get_railways()

get_kazakhstan_sf <- function() {
  kazakhstan_sf <- giscoR::gisco_get_countries(
    year = "2016", epsg = "4326",
    resolution = "3", country = "KZ"
  )
  
  return(kazakhstan_sf)
}

kazakhstan_sf <- get_kazakhstan_sf()

ggplot() +
  geom_sf(
    data = kazakhstan_sf,
    fill = "transparent", color = "#0FAAB8", size = 1.15
  ) +
  geom_sf(
    data = subset(kazakhstan_railway_sf, fclass %in% c("rail", "narrow_gauge")),
    color = "#B82178", size = 1.25
  ) +
  theme_void()

p <- ggplot() +
  geom_sf(
    data = kazakhstan_sf,
    fill = "transparent", color = "#07CFF7", 
    size = .1
  ) +
  geom_sf(
    data = subset(
      kazakhstan_railway_sf, fclass %in% c("rail", "narrow_gauge")
    ),
    color = "#FFB115", size = .15
  ) +
  labs(
    x = "",
    y = "",
    title = "Kazakhstan railways",
    subtitle = "",
    caption = 
      "Данные: ©OpenStreetMap contributors"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    panel.grid.major = element_line(color = "#010D1F", size = 0),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold", size = 24, color = "grey90", hjust = .5
    ),
    plot.caption = element_text(
      size = 10, color = "grey90",
      hjust = .5, vjust = 0
    ),
    plot.margin = unit(
      c(t = 0, r = 0, b = 0, l = 0), "lines"
    ),
    plot.background = element_rect(fill = "#010D1F", color = NA),
    panel.background = element_rect(fill = "#010D1F", color = NA),
    legend.background = element_rect(fill = "#010D1F", color = NA),
    panel.border = element_blank()
  )

ggsave(
  filename = "C:/Users/Admin/Рабочий стол/R/kz_railways.png",
  width = 7, height = 7.5, dpi = 600, device = "png", p
)