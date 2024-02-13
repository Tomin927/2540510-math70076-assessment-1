# Load required packages -------------------------------------------------------
library(readr)                  # loading data
library(dplyr)                  # data frame manipulation
library(stringr)                # character string manipulation
library(ggplot2)                # plotting
library(ggtext)                 # element_markdown for subtitle formatting
library(ggthemes)               # Themes for formating
library(ggpubr)                 # Combine plots
library(extrafont)              # Adding more font format
library(forcats)                # working with factors
library(grid)                   # Add grid line
library(cowplot)                # Add annotation
library(readxl)
library(getdata)                # Private package


# Load data --------------------------------------------------------------------

CSR_Nuclear_Weapons_Dataset <- read_excel("CSR Nuclear Weapons Dataset.xlsx")

# Data wrangling ---------------------------------------------------------------

df_weapon_systems <- CSR_Nuclear_Weapons_Dataset %>%
  # keep only relevant columns:
  #   weapon system, country, introduced year, and retired year
  mutate(
    country = as.factor(Country),
    weapon_sys = as.factor(`Weapon System`),
    start_year = as.Date(Introduced),
    end_year = as.Date(Retired)) %>%
  select(country, weapon_sys, start_year, end_year)


# Extract information ----------------------------------------------------------

country_names = df_weapon_systems$country

df_active_waepon <- tibble(year = 1945:2024,
                           RU = count_sys_num(df_weapon_systems, 'RU'),
                           US = count_sys_num(df_weapon_systems, 'US'),
                           CN = count_sys_num(df_weapon_systems, 'CN'),
                           FR = count_sys_num(df_weapon_systems, 'FR'),
                           UK = count_sys_num(df_weapon_systems, 'UK')) %>%
  tidyr::pivot_longer(cols = c('RU', 'US', 'CN', 'FR', 'UK'),
                      names_to = "country", values_to = "num_of_weapon") %>%
  mutate(country = fct_relevel(country,
                               'CN', 'UK', 'FR', 'RU', 'US')) %>%
  mutate(region = fct_relevel(country,
                              'CN', 'UK', 'FR', 'RU', 'US'))


g1 <- ggplot(data = df_active_waepon,
             mapping = aes(
               x=year,
               y=num_of_weapon,
               fill = country)) +
  geom_area(stat='identity') +

  scale_x_continuous(n.breaks=24,
                     name = "Year") +
  scale_y_continuous(n.breaks=12,
                     name = "Total Active Nuclear Weapons System") +

  scale_fill_manual(values=c("#DB444B",
                             "#379A8B",
                             "#EBB434",
                             "#3EBCD2",
                             "#006BA2")) +
  guides(fill=guide_legend(title="P5 Countries")) +

  labs(title = "Number of Types of Neuclear Weapons Systems 1940~2024",
       subtitle = "This chart displays the number of distinct types
         of nuclear  \n  weapons systems in each year",
       caption = "This detailed time arer chart shows that the trend of the number of neuclear weapons systems  \n  during the past and last decays. These numbers increased so fast after the World War II and  \n  decreased after 1990s. Over 90% of the total amount of neuclear weapons are held by  \n  the US, Russia, and China.") +

  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(linetype = 1,
                                      colour = "#758D99"),

    legend.position="top",
    legend.justification='left',
    legend.direction='horizontal',

    plot.margin = margin(10,5,10,5),
    plot.background = element_rect(fill = '#E9EDF0'),
    panel.background = element_rect(fill = "#B7C6CF",
                                    colour = "#B7C6CF"),

    plot.title = element_markdown(size = 15,
                                  hjust = 0,
                                  face = "bold",
                                  margin = margin(0,0,15,0)),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 14,
                                     hjust = 0,
                                     margin = margin(0,0,15,0)),
    plot.caption = element_markdown(size = 12,
                                    hjust = 0,
                                    color = "black"),
    plot.caption.position = "plot")


# Load map data
world_map <- map_data("world")
# Set colors
world_map <- mutate(
  world_map,
  fill = ifelse(region == "USA", "#006BA2",
                ifelse(region == "Russia", "#3EBCD2",
                       ifelse(region == "France", "#EBB434",
                              ifelse(region == "UK", "#379A8B",
                                     ifelse(region == "China", "#DB444B",
                                            "#758D99"))))))



g2 <- ggplot(world_map, aes(x = long, y = lat, fill = fill, group = group)) +
  geom_polygon(colour = "#3F5661") +

  guides(fill= guide_legend(title="P5 Countries", reverse = T)) +

  labs(title = "The Estimated P5 Neuclear Warhead Inventories, 2024",
       subtitle = "This world map displays the number of types
         of nuclear weapons systems in 2024") +

  geom_text(aes(label = "United State", x = -100, y = 45), color = "#E9EDF0", size = 4) +
  geom_text(aes(label = "\n  5  DOWN", x = -100, y = 40), color = "#E9EDF0", size = 3) +

  geom_text(aes(label = "Russia", x = 100, y = 65), color = "#E9EDF0", size = 4) +
  geom_text(aes(label = "\n  33  UP", x = 100, y = 60), color = "#E9EDF0", size = 3) +

  geom_text(aes(label = "China", x = 105, y = 35), color = "#E9EDF0", size = 4) +
  geom_text(aes(label = "\n  10  UP", x = 105, y = 30), color = "#E9EDF0", size = 3) +

  geom_text(aes(label = "UK", x = 0, y = 65), color = "#E9EDF0", size = 4) +
  geom_text(aes(label = "\n  1  ==", x = 0, y = 60), color = "#E9EDF0", size = 3) +

  geom_text(aes(label = "France", x = 10, y = 40), color = "#E9EDF0", size = 4) +
  geom_text(aes(label = "\n  2  ==", x = 10, y = 35), color = "#E9EDF0", size = 3) +

  annotate(geom = "text", label = 'This plot illustrate the neuclear defensing/attacking  \n  power for each P5 country in 2024. The United State  \n  and Russia hold the 90% of total amount of  \n  neuclear weapons.  \n  How to read: "==" means the number  \n  of system does not change over the past decays,  \n  "Up" and "DOWN"  means the number has been increased  \n  or decreased',
           x = -200, y = -20, size = 3,
           color = 'white' ,hjust = 'left') +

  theme_minimal() +
  theme(
    text = element_text(color = '#333333'),
    plot.title = element_markdown(size = 15,
                                  hjust = 0,
                                  face = "bold",
                                  margin = margin(0,0,15,0)),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 12,
                                     hjust = 0,
                                     margin = margin(0,0,15,0)),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = '#E9EDF0'),
    panel.background = element_rect(fill = '#B7C6CF'),
    legend.position = c(.18,.36),
    legend.background = element_blank(),
    legend.key = element_blank()
  ) + scale_fill_identity()


g <- ggarrange(g2, g1, ncol = 1, nrow = 2)

g <- annotate_figure(g,
                     top = text_grob("CSR Nuclear Weapons Dataset Exploration",
                                     color = "black", face = "bold", size = 24))


# Save plot --------------------------------------------------------------------

out_path <- "2540510-math70076-assessment-1"

ggsave(
  plot = g,
  filename = paste0(out_path, ".pdf"),
  device = "pdf",
  width = 20,
  height = 24,
  units = "cm",
  dpi = 300,
  bg = "#E9EDF0"
)

# End of file


