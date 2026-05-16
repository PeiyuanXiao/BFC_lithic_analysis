library("readxl")
library("tidyverse")
library("patchwork")

# Data -------------------------------------------------------------------------
BFC_coord <- read_excel("BFC_coord.xlsx", sheet = 1) %>%
  mutate(
    X    = as.numeric(X),
    Y    = as.numeric(Y),
    Z    = as.numeric(Z),
    Type = as.factor(Type)
  )

# Global variables -------------------------------------------------------------
target_layers <- c(3, 7, 9, 11, 12)

x_limits <- c(316908, 316913)
x_breaks <- 316908:316913
y_limits <- c(2929716, 2929723)
y_breaks <- 2929716:2929723

stone_types <- c("凹缺器", "刮削器", "石核", "石片", "石锤", "砾石",
                 "断块", "残片", "带疤砾石", "右裂片", "远端断片", "大断片",
                 "左裂片", "近端断片", "砍砸器", "烧石")

bone_types <- c("大熊猫牙", "骨片", "关节头", "颌骨", "肩胛骨", "胫骨头",
                "鹿角", "鹿牙", "牛牙", "腕骨", "犀牛牙", "犀牛牙片",
                "熊牙", "掌骨头", "肢骨", "趾骨", "椎骨", "竹鼠牙", "骨",
                "股骨头", "髌骨", "髋骨", "蚌壳", "有动物咬痕的骨片", "关节骨",
                "牙", "骨器", "胫骨", "指骨", "股骨", "烧骨", "骨器？")

target_categories <- c("Stone artifacts", "Animal fossils/Bone tools")

BFC_coord <- BFC_coord %>%
  mutate(
    Category = case_when(
      Type %in% stone_types ~ "Stone artifacts",
      Type %in% bone_types  ~ "Animal fossils/Bone tools",
      TRUE                  ~ NA_character_
    ),
    Category = as.factor(Category)
  )

# Plot -------------------------------------------------------------------------
category_style <- list(
  "Stone artifacts" = list(
    bg    = "#FFF7F3",
    fill  = scale_fill_brewer(palette = "Reds"),
    color = "#C85A5A"
  ),
  "Animal fossils/Bone tools" = list(
    bg    = "#F7FBFF",
    fill  = scale_fill_brewer(palette = "Blues"),
    color = "#5B8DB8"
  )
)

base_theme <- theme_classic(base_size = 12) +
  theme(
    panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line          = element_blank(),
    axis.ticks         = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length  = unit(2.5, "mm"),
    axis.text          = element_text(color = "black", size = 10),
    axis.title         = element_text(size = 11, face = "bold"),
    plot.title         = element_text(hjust = 0.5, face = "bold", size = 13)
  )

for (l in target_layers) {
  for (cat in target_categories) {
    
    layer_data <- BFC_coord %>% filter(Layer == l, Category == cat)
    
    if (nrow(layer_data) < 5) {
      message("Layer ", l, " — ", cat, ": n < 5, skipped.")
      next
    }
    
    style <- category_style[[cat]]
    
    p <- ggplot(layer_data, aes(x = X, y = Y)) +
      geom_vline(xintercept = x_breaks, color = "black", linewidth = 0.5, linetype = "dashed") +
      geom_hline(yintercept = y_breaks, color = "black", linewidth = 0.5, linetype = "dashed") +
      geom_density_2d_filled(aes(fill = after_stat(level)),
                             bins = 8, alpha = 0.80, show.legend = FALSE) +
      style$fill +
      scale_x_continuous(breaks = x_breaks, limits = x_limits, expand = c(0, 0)) +
      scale_y_reverse(breaks = y_breaks, limits = rev(y_limits), expand = c(0, 0)) +
      coord_fixed() +
      labs(title = paste0("Layer ", l, "  |  ", cat), x = "N (m)", y = "E (m)") +
      base_theme +
      theme(panel.background = element_rect(fill = style$bg, color = NA))
    
    print(p)
  }
}