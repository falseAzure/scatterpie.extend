library(devtools)
load_all()
library(dplyr)
library(data.table)
library(readxl)
library(sf)
library(extrafont)
library(extrafontdb)

shp.gem <- st_read("../Uni_Studierende/input/shp_gemeinde_2020/STATISTIK_AUSTRIA_GEM_20200101Polygon.shp")
shp.bez <- st_read("../Uni_Studierende/input/shp_bezirke/STATISTIK_AUSTRIA_POLBEZ_20200101Polygon.shp")

# Bundesländer deklarieren
shp.gem$bl <- substr(shp.gem$id,1,1)

# Bundesländer zusammenfassen und neue Liste aus Polygonen schaffen
shp.bl <- shp.gem %>% group_by(bl) %>% summarize()

#### 1 Data ####
#### 1.1 Gemeinden ####
#### 1.1.1. Studierende ####
input_studierende_schuelerinnen <- data.table(read_xlsx("../Uni_Studierende/input/studierende_schuelerinnen.xlsx"))
input_studierende_schuelerinnen[,Typ:=factor(Typ, levels = c("Landwirtschaft", "Forst", "Hochschule", "Sonstige"))]
shp.gem <- merge(shp.gem, input_studierende_schuelerinnen, by = "id", all.x=T)

shp.gem.centroid.dt <- get_centroids(shp.gem)

#### 1.1.2. Urbanität ####
input_gemeinde <- data.table(read_xlsx("../Uni_Studierende/input/data_gem_bez.xlsx", sheet = "gem"))
bridge.urban_rural <- data.table(read_xlsx("../Uni_Studierende/input/data_gem_bez.xlsx", sheet = "urban_rural"))
input_gemeinde[,`Urban-Rural`:=factor(`Urban-Rural`, levels = bridge.urban_rural$`Urban-Rural`)]
shp.gem <- merge(shp.gem, input_gemeinde[,.(ID, `Urban-Rural`)], by.x = "id", by.y="ID", all.x=T)

urban_rural.colors <- c(
  "#8b0000",
  "#9d3107",
  "#af5013",
  "#bf6d23",
  "#ce8935",
  "#dda54b",
  "#ebc263",
  "#f8de7e",
  "#4d994d",
  "#2e6f2d",
  "#0e470e"
)

bridge.urban_rural[, urban_rural_color:=urban_rural.colors]
shp.gem <- merge(shp.gem, bridge.urban_rural[,.(`Urban-Rural`, urban_rural_color)],
                 by = "Urban-Rural", all.x=T)

typ.colors <- c("#F01C9C", "#107ab0", "#7F7F7F", "#FFFFFF")

#### 1.1.3. Bezirke ####
input_bezirke <- data.table(read_xlsx("../Uni_Studierende/input/data_gem_bez.xlsx", sheet = "bez"))

shp.bez <- merge(shp.bez, input_bezirke, by.x = "id", by.y="Bezirk", all.x=T)


#### 2.1 Studierende ####

area = T
ratio <- 0.08
var <- "studierende"
sf_object <- shp.gem
color.pie <- NA
alpha.pie <- 0.7
legend_name <- "Type"

background_map <- ggplot() +
  geom_sf(data = shp.gem, fill=shp.gem$urban_rural_color, color=alpha("white", 1), linewidth=0) +
  geom_sf(data = shp.bl, fill = alpha("white", 0), color="grey20", linewidth=0.25)

sf_object <- sf_object %>% mutate(studierende=studierende*1000)
sf_object <- sf_object %>% mutate(Landwirtschaft = ifelse(Typ=="Landwirtschaft", studierende, 0))
sf_object <- sf_object %>% mutate(Forst = ifelse(Typ=="Forst", studierende, 0))
sf_object <- sf_object %>% mutate(Sonstige = ifelse(Typ=="Sonstige", studierende, 0))
sf_object <- sf_object %>% mutate(Hochschule = ifelse(Typ=="Hochschule", studierende, 0))

plot <- plot_scatterpie(sf_object, var, cols=c("Landwirtschaft", "Forst", "Hochschule", "Sonstige"), legend_name="Hochschultyp", background_plot = background_map, scale_cut=cut_short_scale_german(), family="Century Gothic")
plot <- plot +
  xlab("") +
  ylab("") +
  theme_void(base_family = "Century Gothic") +
  theme(legend.position= "inside",
    legend.position.inside = c(0.30, 0.68),
        legend.key.size = unit(0.6, "cm"),
        plot.title = element_text(hjust = 0.2),
        plot.subtitle = element_text(hjust = 0.09)) +
  guides(fill = guide_legend(override.aes = list(colour = "black", size = 0.3)))
plot
