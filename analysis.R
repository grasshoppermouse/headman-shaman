library(tidyverse)
library(readxl)
library(ggrepel)
library(ggmosaic)
library(patchwork)
library(scico)

# Brain energy use. From Kuzawa et al. 2014 (SI)

males <- 
  read_excel('kuzawa.xlsx', range = cell_rows(1:17), .name_repair = ~str_squish(.x)) |> 
  select(where(~!is.logical(.x))) |> 
  mutate(
    RMR_year = 365 * `RMR (kcal/day)`,
    DER_year = 365 * `DER (kcal/day)`,
    brain_kcal_year = 4 * 365 * `Brain glucose uptake (g/day)`
  )

male_summary <-
  males |> 
  summarise(
    total_RMR = sum(365 * `RMR (kcal/day)`),
    total_DER = sum(365 * `DER (kcal/day)`),
    total_brain_kcal = 4 * sum(365 * `Brain glucose uptake (g/day)`),
    proportion_RMR = total_brain_kcal/total_RMR,
    proportion_DER = total_brain_kcal/total_DER
  )

male_long <-
  males |> 
  mutate(
    RMR_cumulative = cumsum(RMR_year),
    DER_cumulative = cumsum(DER_year),
    brain_cumulative = cumsum(brain_kcal_year)
  ) |> 
  pivot_longer(c(RMR_cumulative, DER_cumulative, brain_cumulative))

plot_kuzawa <- 
  ggplot(male_long, aes(Age, value, colour = name)) + 
  geom_line(linewidth=1) +
  scale_y_continuous(labels = scales::label_number()) +
  scico::scale_color_scico_d(palette = 'roma', end = 0.8) +
  annotate('text', label = 'Daily energy requirement', x = 15.5, y = 10.9e6, hjust = 0, size = 5) + 
  annotate('text', label = 'Resting metabolism', x = 15.5, y = 6.3e6, hjust = 0, size = 5) + 
  annotate('text', label = 'Brain metabolism', x = 15.5, y = 3e6, hjust = 0, size = 5) + 
  labs(
    title = 'Cumulative energy use',
    # subtitle = 'Data from Kuzawa et al. (2014) 10.1073/pnas.1323099111',
    x = 'Age (years)',
    y = 'kcals'
    ) +
  coord_cartesian(clip = 'off', xlim = c(0,15)) +
  theme_minimal(20) +
  theme(
    plot.margin = margin(0.5, 7, 0.5, 1, "cm"),
    legend.position = 'none',
    axis.title.y = element_text(angle = 0)
    )

# ggsave('Figures/plot_kuzawa.svg', plot_kuzawa, width = 12, height = 9)

male2 <-
  males |> 
  mutate(
    RMR_cumulative = cumsum(RMR_year),
    DER_cumulative = cumsum(DER_year),
    brain_cumulative = cumsum(brain_kcal_year),
    ratio = brain_cumulative/RMR_cumulative
  ) |> 
  dplyr::filter(Age > 0)

plot_kuzawa2 <- 
  ggplot(male2, aes(Age, ratio)) + 
  geom_line(linewidth=1) +
  scale_y_continuous(labels = scales::label_number()) +
  scico::scale_color_scico_d(palette = 'roma', end = 0.8) +
  xlim(0, 15) +
  ylim(0, 0.7) +
  labs(
    title = 'Proportion brain metabolism',
    x = 'Age (years)',
    y = 'Cumulative\nproportion'
  ) +
  theme_minimal(20) +
  theme(
    plot.margin = margin(0.5, 7, 0.5, 1, "cm"),
    legend.position = 'none',
    axis.title.y = element_text(angle = 0, hjust = 0)
  )
# plot_kuzawa2

ggsave("Figures/plot_kuzawa.svg", plot_kuzawa/plot_kuzawa2 + plot_annotation(tag_levels = 'A'), width = 12, height = 12)

females <- 
  read_excel('kuzawa.xlsx', range = cell_rows(20:36), .name_repair = ~str_squish(.x)) |> 
  select(where(~!is.logical(.x))) 

female_summary <-
  females |> 
  summarise(
    total_RMR = sum(365 * `RMR (kcal/day)`),
    total_DER = sum(365 * `DER (kcal/day)`),
    total_brain_kcal = 4 * sum(365 * `Brain glucose uptake (g/day)`),
    proportion_RMR = total_brain_kcal/total_RMR,
    proportion_DER = total_brain_kcal/total_DER
  )

# Endocranial volume

library(ggrepel)

ecv <- 
  read_excel('ProcB SI ECV dataset FINAL.xlsx', na = 'NA') %>% 
  mutate(
    mean_date = (min.date + max.date)/2,
    mean_date = ifelse(is.na(mean_date), mean.date, mean_date),
    # Aggregate Australopithecines
    Taxon = ifelse(str_detect(lump.taxon, 'Au.'), 'Australopithecus', lump.taxon),
    Taxon = factor(Taxon, levels = c('Australopithecus', 'H. habilis', 'H. erectus', 'H. heidelbergensis', 'H. sapiens')),
    thealpha = ifelse(region %in% c('EA', 'N.A', 'SA'), 1, 0.85) # Doesn't seem to work
    ) %>% 
  dplyr::filter(region %in% c('EA', 'N.A', 'SA')) |> 
  dplyr::filter(!str_detect(lump.taxon, 'P. ')) # Remove Paranthropus

plot_ecv <-
  ggplot(ecv) + 
  annotate(geom = 'rect', xmax = 2.58, xmin = 0.0117, ymin = 0, ymax = 1600, fill = '#dddddd', alpha=0.5) +
  annotate(geom = 'text', x = 3.25, y = 100, label = 'Pliocene', size = 6) +
  annotate(geom = 'text', x = 1.25, y = 100, label = 'Pleistocene', size = 6) +
  annotate(geom = 'text', x = 2.2, y = 60, label = 'Transition to meat eating', size = 4) +
  annotate(geom = 'segment', x = 2.58, xend = 1.8, y = 10, yend = 10, linewidth = 2, color = 'orange', arrow=arrow(ends = 'last', type = 'open', length = unit(0.1, "inches"))) +
  # annotate(geom = 'text', x = 2.2, y = 1275, label = 'Increased zoonotic\npathogen pressure?', size = 4, lineheight=0.75) +
  annotate(geom = 'rect', xmax = 0.0117, xmin = 0, ymin = 0, ymax = 1600, fill = '#dd0000', alpha=0.5) +
  annotate(geom = 'text', x = -0.15, y = 100, label = 'Holocene', colour = '#dd0000', size = 6, hjust='left') +
  # annotate(geom = 'text', x = -0.15, y = 90, label = 'Neolithic epidemiological\ntransition', colour = '#dd0000', size = 4, lineheight=0.75, hjust='left', vjust='top') +
  annotate(geom = 'segment', x = -0.1, xend = 0, y = 100, yend = 100, colour = '#dd0000', arrow = arrow(length = unit(0.1, "inches"))) +
  annotate(geom = 'text', x = -0.4, y = 1328, label = 'Modern human range', hjust = 'left') +
  annotate(geom = 'segment', x = -0.3, xend = -0.3, y = 1328-164, yend = 1328+164, linewidth = 2) + # Mean +/- 1 SD
  annotate(geom = 'text', x = -0.4, y = 378.7, label = 'Chimpanzee range', hjust = 'left') +
  annotate(geom = 'segment', x = -0.3, xend = -0.3, y = 378.7-30, yend = 378.7+30, linewidth = 2) +
  stat_smooth(geom='line', aes(mean_date, ecv1), span = 0.9, alpha=0.5, se=F) +
  geom_point(aes(mean_date, ecv1, colour = Taxon, shape = Taxon, fill = Taxon), size = 2) + 
  geom_text_repel(aes(mean_date, ecv1, colour = Taxon, label = ID), size = 3, alpha = 0.7, show.legend = F, max.overlaps = Inf) +
  geom_segment(x = -0.06, xend = -0.06, y = 0, yend = 1600, linetype = 'dotted') + # Out of Africa: 60 kya
  annotate(geom = 'text', x = -0.15, y = 190, label = 'Out of Africa', size = 4, hjust = 'left') +
  annotate(geom = 'segment', x = -0.1, xend = 0.06, y = 190, yend = 190, arrow = arrow(length = unit(0.1, "inches"))) +
  scale_shape_manual(values = 21:25) +
  scale_x_reverse() +
  coord_cartesian(
    xlim = c(3.5, 0),
    clip = 'off'
  ) +
  guides(
    colour = guide_legend(reverse=F, override.aes = list(size = 5)),
    shape = guide_legend(reverse=F, override.aes = list(size = 5)),
    fill = guide_legend(reverse=F, override.aes = list(size = 5))
    # alpha = guide_none()
    ) +
  labs(
    x = '\nMillions of years before present', 
    y = '\nEndocranial\nvolume (ml)',
    title = '', # 'Hominin endocranial volume',
    # subtitle = "Data from: 10.1098/rspb.2017.2738, 10.1126/sciadv.aao5961",
    colour = '',
    shape = '',
    fill = ''
    ) +
  theme_minimal(15) +
  theme(
    plot.margin = margin(0.5, 7, 0.5, 1, "cm"), 
    legend.position = "top",
    plot.subtitle = element_text(size = 10, colour = '#999999'),
    axis.title.y = element_text(angle = 0)
  )
ggsave("Figures/ecv.svg", plot_ecv, width = 12, height = 8)


# Drugs & culture ---------------------------------------------------------

d <- 
  read_excel("Alrashedy drugs culture.xlsx", sheet = 1) |> 
  mutate(
    Mechanism = str_remove(`Mechanism of action`, '\\(.*\\)'),
    Mechanism = str_remove_all(Mechanism, ",|\\."),
    Mechanism = str_squish(Mechanism),
    Mechanism = str_to_title(Mechanism),
    Mechanism = str_replace(Mechanism, "Aprhodisiac", "Aphrodisiac"),
    Mechanism = str_replace(Mechanism, "Hallucinogenic", "Hallucinogen"),
    Mechanism = str_replace(Mechanism, "Hallucinoge", "Hallucinogen"),
    Mechanism = str_replace(Mechanism, "Hallucinogenn", "Hallucinogen"),
    
    Culture_area = str_remove(`Indigenous psychoactive culture`, ' \\(.*\\)'),
    Culture_area = str_replace_all(Culture_area, "\\.|\\;", ","),
    Culture_area = str_replace(Culture_area, 'African/Middle Eastern Australasian', 'African and Middle Eastern, Australasian'),
    Culture_area = str_replace(Culture_area, 'Africa ', 'African '),
    Culture_area = str_replace(Culture_area, 'African,', 'African and Middle Eastern,'),
    Culture_area = str_replace(Culture_area, 'Middl ', 'Middle '),
    Culture_area = str_replace(Culture_area, 'Australiasia,', 'Australasian,'),
    Culture_area = str_replace(Culture_area, 'Indomalaya$', 'Indomalayan'),
    Culture_area = str_replace(Culture_area, 'Indomalaya,', 'Indomalayan'),
    Culture_area = str_replace(Culture_area, 'America$', 'American')
  ) |> 
  separate_longer_delim(Mechanism, delim = " ") |> 
  separate_longer_delim(Culture_area, delim = ", ") |> 
  dplyr::filter(Mechanism != 'And')

mnams <- names(sort(table(d$Mechanism)))
d$Mechanism <- factor(d$Mechanism, levels = mnams)

cnams <- names(sort(table(d$Culture_area)))
d$Culture_area <- factor(d$Culture_area, levels = rev(cnams))

#+ fig.width=12
plot_psycho <-
  ggplot(data = d) + 
  geom_mosaic(aes(x = product(Culture_area, Mechanism), fill = Culture_area)) + 
  geom_mosaic_text(aes(x = product(Culture_area, Mechanism), label = after_stat(.wt)), size = 7, colour = 'white', as.label=F) +
  scale_fill_scico_d(palette = 'roma') +
  labs(x='', y='') +
  guides(fill = 'none') +
  theme_minimal(15)
plot_psycho
ggsave("Figures/plot_psycho.pdf", width = 16, height = 6)
ggsave("Figures/plot_psycho.png", width = 16, height = 6)

# d$Mechanism <- factor(d$Mechanism, levels = rev(mnams))
# 
# #+ fig.width=12
# ggplot(data = d) + 
#   geom_mosaic(aes(x = product(Mechanism, Culture_area), fill = Mechanism)) + 
#   scale_fill_scico_d(palette = 'roma') +
#   labs(x='', y='') +
#   guides(fill = 'none') +
#   theme_minimal(15)

# d2 <- read_excel("Alrashedy drugs culture.xlsx", sheet = 2)


# Human vs Chimp productivity ---------------------------------------------


# DATA_file.xls – available on Open Science Framework (OSF); https://osf.io/ag5yp/
# Excel spreadsheet containing subsistence and life history data tables. 
# Rows for each age with columns showing
# (1) age, 
# (2) production (Px) in kcal/day, 
# (3) demand (Dx) in kcal/day, 
# (4) fertility (mx) in daughters per year, 
# (5) annual survival probabilities (px). Separate worksheets show data averaged over (1) wild chimpanzees, (2) hunter-
#   gatherers, and (3) forager-horticulturalists.

# download.file("https://osf.io/download/e5srh/", "davison_DATA_file.xlsx", "auto")

chimp <- read_excel("davison_DATA_file.xlsx")
chimp$Species <- 'Chimpanzees'

human <- read_excel("davison_DATA_file.xlsx", sheet = 2)
human$Species <- 'Human foragers'

chimp_human <- bind_rows(chimp, human)

cols <- viridisLite::magma(11)[c(4,8)]
arrw <- arrow(ends = 'both', type = 'closed', angle = 30, length = unit(2, 'mm'))

plot_productivity <- 
  ggplot(chimp_human, aes(Age, Production - Demand, colour = Species)) + 
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0, linetype = 'dotted') + 
    guides(colour = guide_legend(override.aes = list(linewidth = 3))) +
    scale_color_manual(values = cols) +
  
    annotate('text', label = 'Self-sufficient', x = 5, y = 120, colour = cols[1]) +
    annotate('point', x = 5, y = 0, size = 3, colour = cols[1]) +
    
    annotate('text', label = 'Self-sufficient', x = 30, y = 80, colour = cols[2]) +
    annotate('point', x = 30, y = -65, size = 3, colour = cols[2]) +
    
    annotate('text', label = 'Age at first birth', x = 15, y = 430, colour = cols[1]) +
    annotate('point', x = 15, y = 310, size = 3, colour = cols[1]) +
    
    annotate('text', label = 'Age at first birth', x = 21, y =-500, hjust=0, colour = cols[2]) +
    annotate('point', x = 20, y = -500, size = 3, colour = cols[2]) +
    
    annotate('text', label = 'Long juvenile dependency\nImportance of learning for skill-intensive niche', x = 15, y =-1650, hjust=0.5, colour = cols[2]) +
    annotate('segment', x = 0, xend = 30, y = -1475, yend = -1475, linewidth = 2, colour = cols[2], arrow = arrw) +
    
    annotate('text', label = 'Low adult mortality (longevity), Female surplus production\nIntergenerational transfers from grandmothers, alloparents', x = 55, y = 1000, hjust=0.5, colour = cols[2]) +
    annotate('segment', x = 40, xend = 70, y = 720, yend = 720, linewidth = 2, colour = cols[2], arrow = arrw) +
    
    annotate('text', label = 'Approximate surplus production of middle-age Hadza men', x = 40, y = 2400, hjust=0.5, colour = cols[2]) +
    annotate('segment', x = 30, xend = 50, y = 2500, yend = 2500, linetype = 'dashed', linewidth = 1, colour = cols[2]) +
  
    # annotate('text', label = 'High adult mortality (chimps)', x = 50, y = -100, colour = cols[1]) +
    labs(
      # title = 'Net productivity: human foragers vs. chimpanzees (females)',
      # subtitle = 'Data from Davison and Gurven (2021, 2022)',
      y = 'Production - Demand (kcals)'
    ) +
    coord_cartesian(clip = 'off') +
    theme_minimal(15) +
    theme(legend.position = 'top', legend.title = element_blank())
plot_productivity

ggsave("Figures/plot_productivity.pdf", plot_productivity, width = 10, height = 10)
ggsave("Figures/plot_productivity.png", plot_productivity, width = 10, height = 10)


# Altriciality ------------------------------------------------------------

gomez <- 
  read_csv("Gómez-Robles/final_compilation with percentage values.R1.csv") |> 
  rename(Order = Order2) |> 
  mutate(
    Species = str_replace(Binomial...1, "_", " "),
    .keep = 'unused'
  ) |> 
  relocate(Species)

ggplot(gomez, aes(GenTime, NeoBrain, colour = Order)) + 
  geom_point(size = 4) +
  geom_smooth(method = 'lm', se = F) +
  geom_text_repel(
    data = gomez[gomez$Family == 'Hominidae',], 
    aes(GenTime, NeoBrain, colour = Order2, label = Species),
    force = 1000
  ) +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal(15)


ggplot(gomez, aes(GenTime, AdultBrain, colour = Order)) + 
  geom_point(size = 4) +
  geom_smooth(method = 'lm', se = F) +
  geom_text_repel(
    data = gomez[gomez$Family == 'Hominidae',], 
    aes(GenTime, AdultBrain, colour = Order2, label = Species),
    force = 1000
  ) +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal(15)

ggplot(gomez[gomez$Order == 'Primates',], aes(GenTime, AdultBrain, colour = Family)) + 
  geom_point(size = 4) +
  geom_text_repel(aes(label = Species), max.overlaps = Inf, force = 50) +
  # geom_smooth(method = 'lm', se = F) +
  # geom_text_repel(
  #   data = gomez[gomez$Family == 'Hominidae',], 
  #   aes(GenTime, AdultBrain, colour = Order2, label = `Binomial...1`),
  #   force = 1000
  # ) +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal(15)


# ggplot(chimp_human, aes(Age, Fertility, colour = Species)) + 
#   geom_line(linewidth = 1) +
#   hagenutils::scale_colour_binary() +
#   theme_minimal(15)

# Cejal -------------------------------------------------------------------

# cejal1998 |> 
#   mutate(FatherID = as.numeric(FatherID)) |> 
#   dplyr::filter(FatherID < 200) |>
#   group_by(FatherID) |> 
#   summarise(
#     N = n()
#   ) |> 
#   left_join(cejal1998[c('ID', 'TotalGardenProductivity', 'Shaman')], by = c('FatherID' = 'ID')) |> 
#   ggplot(aes(TotalGardenProductivity, N, colour = Shaman)) +
#   geom_count() +
#   geom_smooth(method = 'lm')
