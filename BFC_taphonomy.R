library("readxl")
library("tidyverse")
library("viridis")
library("patchwork")

# Import data ------------------------------------------------------------------
BFC_CUTMARK <- read_excel("taphnomic_cutmark.xlsx", sheet = 1) %>%
  select(ID, Length, Orientation, Location, Element) %>%
  filter(!is.na(ID))   # drop trailing empty rows

# Global variables -------------------------------------------------------------
orientation_order <- c("Oblique", "Transverse", "Parallel")

# Matches the shared plot_theme used across the project's figures.
plot_theme <- theme(
  panel.background = element_blank(),
  panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5),
  axis.line        = element_blank(),
  axis.title.x     = element_blank(),
  axis.text.x      = element_text()
)

# Cleaning ---------------------------------------------------------------------
# a) Orientation -- unit = individual cut mark
#    "two parallel" -> two parallel marks; split on "/" or ";".
orientation_marks <- BFC_CUTMARK %>%
  select(ID, Orientation) %>%
  filter(!is.na(Orientation)) %>%
  mutate(Orientation = str_to_lower(Orientation),
         Orientation = str_replace_all(Orientation, "two parallel", "parallel/parallel")) %>%
  separate_rows(Orientation, sep = "\\s*[/;]\\s*") %>%
  mutate(Orientation = str_trim(Orientation)) %>%
  filter(Orientation != "") %>%
  mutate(Orientation = str_to_title(Orientation),
         Orientation = factor(Orientation, levels = orientation_order))

# d) Length -- unit = individual cut mark; strip "mm", split on "/", to numeric.
length_marks <- BFC_CUTMARK %>%
  select(ID, Length) %>%
  filter(!is.na(Length)) %>%
  mutate(Length = str_remove_all(Length, "mm|\\s")) %>%
  separate_rows(Length, sep = "/") %>%
  filter(Length != "") %>%
  mutate(Length_mm = as.numeric(Length)) %>%
  filter(!is.na(Length_mm))

# b) Element & c) Location -- unit = specimen
element_specimens <- BFC_CUTMARK %>%
  filter(!is.na(Element)) %>%
  count(Element) %>%
  arrange(desc(n)) %>%
  mutate(Element = factor(Element, levels = Element))

location_specimens <- BFC_CUTMARK %>%
  filter(!is.na(Location)) %>%
  mutate(Location = str_replace(Location, "close to epi\\.", "epiphysis surface"),
         Location = str_remove(Location, "\\s*surface"),   # drop trailing "surface"
         Location = str_to_sentence(Location),
         Location = recode(Location,                       # long-bone diaphysis / epiphysis
                           "Diaphysis" = "LB_diaphysis",
                           "Epiphysis" = "LB_epiphysis")) %>%
  count(Location) %>%
  arrange(desc(n)) %>%
  mutate(Location = factor(Location, levels = Location))

# Report counts
cat("\nSpecimens with cut marks:", nrow(BFC_CUTMARK), "\n")
cat("Total individual cut marks (Orientation):", nrow(orientation_marks), "\n")
cat("Total individual cut marks (Length):", nrow(length_marks), "\n\n")
print(orientation_marks %>% count(Orientation))
print(element_specimens)
print(location_specimens)
cat(sprintf("\nLength (mm): mean = %.2f, sd = %.2f, min = %.2f, max = %.2f, n = %d\n",
            mean(length_marks$Length_mm), sd(length_marks$Length_mm),
            min(length_marks$Length_mm), max(length_marks$Length_mm),
            nrow(length_marks)))

# Panels -----------------------------------------------------------------------
# a) Orientation
p_orientation <- orientation_marks %>%
  count(Orientation) %>%
  mutate(percentage = round(n / sum(n) * 100, 1)) %>%
  ggplot(aes(x = Orientation, y = n, fill = Orientation)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0("n=", n, " (", percentage, "%)")), vjust = -0.4, size = 3) +
  scale_fill_viridis_d(option = "D", begin = 0, end = 0.9) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Cut-mark orientation", y = "Cut-mark count") +
  plot_theme +
  theme(legend.position = "none", axis.title.x = element_text(size = 11, color = "black"))

# b) Element
p_element <- element_specimens %>%
  ggplot(aes(x = Element, y = n, fill = Element)) +
  geom_bar(stat = "identity", width = 0.75) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.4, size = 3) +
  scale_fill_viridis_d(option = "D", begin = 0, end = 0.9) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +   # stagger labels: horizontal, no overlap
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Cut-mark element", y = "Specimen count") +
  plot_theme +
  theme(legend.position = "none", axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 11, color = "black"))

# c) Location
p_location <- location_specimens %>%
  ggplot(aes(x = Location, y = n, fill = Location)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.4, size = 3) +
  scale_fill_viridis_d(option = "D", begin = 0, end = 0.9) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Cut-mark location", y = "Specimen count") +
  plot_theme +
  theme(legend.position = "none", axis.title.x = element_text(size = 11, color = "black"))

# d) Length distribution
len_mean   <- mean(length_marks$Length_mm)
len_median <- median(length_marks$Length_mm)
p_length <- ggplot(length_marks, aes(x = Length_mm)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 2, boundary = 0,
                 fill = viridis(1, begin = 0.4), color = "white", linewidth = 0.3) +
  geom_density(color = "black", linewidth = 0.7) +
  annotate("text", x = Inf, y = Inf, hjust = 1.08, vjust = 1.5, size = 3, color = "black",
           label = sprintf("mean = %.1f mm\nmedian = %.1f mm", len_mean, len_median)) +
  scale_x_continuous(breaks = seq(0, 25, by = 5)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Cut-mark length (mm)", y = "Density") +
  plot_theme +
  theme(axis.title.x = element_text(size = 11, color = "black"))

# Composite figure -------------------------------------------
fig_cutmark <- (p_orientation | p_element) / (p_location | p_length) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 14))
print(fig_cutmark)

ggsave("BFC_taphonomy_cutmark.pdf", fig_cutmark, width = 10, height = 7.5)
ggsave("BFC_taphonomy_cutmark.png", fig_cutmark, width = 10, height = 7.5, dpi = 300, bg = "white")
