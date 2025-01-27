# Plot yield classes across climate change scenarios (year 2080)

library(tidyverse)
library(ggplot2)
library(glue)
library(patchwork)

source('plot_scripts/gridnet_init.R')

rcp <- c('rcp26', 'rcp45', 'rcp60', 'rcp85')
clim_model_member <- c('01', '04', '06', '15')
rcp_names <- list(rcp26 = 'RCP 2.6', rcp45 = 'RCP 4.5', rcp60 = 'RCP 6.0', rcp85 = 'RCP 8.5')

combs <- expand.grid(clim_scen_string = rcp, climate_model_member = clim_model_member)

yc <- pmap(combs, function(clim_scen_string, climate_model_member) {
  df <- read_csv(glue::glue("output/tables/yield_class/yield_class_{clim_scen_string}_{climate_model_member}.csv"))
})

names(yc) <- glue::glue("{rcp_names[combs$clim_scen_string]} (Model {combs$climate_model_member})")

df_yc <- yc %>%
  bind_rows(.id = 'scen')

unique_yc_SS <- unique(df_yc$yc_SS) %>% sort()
unique_yc_POK <- unique(df_yc$yc_POK) %>% sort()

df <- right_join(gridnet, df_yc, by = 'new2kid') %>%
  mutate(yield_class_SS = factor(yc_SS, unique_yc_SS, as.character(unique_yc_SS))) %>%
  mutate(yield_class_POK = factor(yc_POK, unique_yc_POK, as.character(unique_yc_POK))) %>%
  mutate(yield_class_SS = factor(yield_class_SS, as.character(c(2,4,6,8,10,12,14,16,18,20))))%>%
  mutate(yield_class_POK = factor(yield_class_POK, as.character(c(2,4,6,8,10,12,14,16,18,20))))

plot_SS <- ggplot() +
  geom_sf(data = gb_border, color = "gray20", fill = "#f7fcb94D", lwd = 0.2, inherit.aes = FALSE) +
  geom_sf(data = df, aes(fill = yield_class_SS), lwd = 0, color = NA) +
  labs(fill = 'Yield Class')+
  scale_fill_brewer(palette = 'RdYlGn') +
  facet_wrap(vars(scen), nrow = 2) +
  theme_void() +
  theme(legend.position = 'bottom') +
  ggtitle("Conifer (sitka spruce) Predicted Yield Class (2080)")


plot_POK <- ggplot() +
  geom_sf(data = gb_border, color = "gray20", fill = "#f7fcb94D", lwd = 0.2, inherit.aes = FALSE) +
  geom_sf(data = df, aes(fill = yield_class_POK), lwd = 0, color = NA) +
  labs(fill = 'Yield Class')+
  scale_fill_brewer(palette = 'PuOr') +
  facet_wrap(vars(scen), nrow = 2) +
  theme_void()+
  theme(legend.position = 'bottom')+
  ggtitle("Broadleaf (pedunculate oak) Predicted Yield Class (2080)")


ggsave("output/figures/sdfig4_yc.png", plot_SS / plot_POK, width = 30, height = 30, units = 'cm', dpi = 300)
ggsave("output/figures/sfig6_yc.pdf", plot_SS / plot_POK, width = 30, height = 30, units = 'cm', dpi = 300)


# Plot rotation periods
timber_c_ss <- read_csv('data/timberC/TimberC_SS.csv', col_names = F)
colnames(timber_c_ss) <- paste0('SS_', 1:ncol(timber_c_ss))

timber_c_pok <- read_csv('data/timberC/TimberC_POK.csv', col_names = F)
colnames(timber_c_pok) <- paste0('POK_', 1:ncol(timber_c_pok))
timber_c_pok <- timber_c_pok[1:nrow(timber_c_ss),]

timber_c <- bind_cols(timber_c_ss, timber_c_pok) %>%
  mutate(year = 2019 + 1:(nrow(timber_c_ss)))

timber_c <- timber_c[,colSums(timber_c)>0]

ss_plt <- timber_c %>%
  pivot_longer(c(everything(), -year), names_sep = '_', names_to = c('species', 'yield_class')) %>%
  group_by(species, yield_class) %>%
  arrange(species, yield_class, year) %>%
  mutate(cumsum = cumsum(value)) %>%
  mutate(yield_class = factor(yield_class, c(2,4,6,8,10,12,14,16,18,20))) %>%
  filter(species == 'SS') %>%
  mutate(group = paste(species, yield_class, sep = '_')) %>%
  ggplot(aes(x = year, y = cumsum, group = group, color = yield_class), show.legend = TRUE) +
  geom_line(show.legend = TRUE) +
  scale_x_continuous('Year') +
  scale_y_continuous('Carbon per ha', limits = c(0, 300)) +
  labs(color = 'Yield Class') +
  scale_color_brewer(palette = 'RdYlGn') +
  theme_bw() +
  coord_cartesian(expand = F) +
  ggtitle('Coniferous (Sitka Spruce)') +
  theme(legend.position = 'bottom',
        panel.grid = element_blank())

pok_plt <- timber_c %>%
  pivot_longer(c(everything(), -year), names_sep = '_', names_to = c('species', 'yield_class')) %>%
  group_by(species, yield_class) %>%
  arrange(species, yield_class, year) %>%
  mutate(cumsum = cumsum(value)) %>%
  mutate(yield_class = factor(yield_class, c(2,4,6,8,10,12,14,16,18,20))) %>%
  filter(species == 'POK') %>%
  mutate(group = paste(species, yield_class, sep = '_')) %>%
  ggplot(aes(x = year, y = cumsum, group = group, color = yield_class), show.legend = TRUE) +
  geom_line(show.legend = TRUE) +
  scale_x_continuous('Year') +
  scale_y_continuous('', limits = c(0, 300)) +
  labs(color = 'Yield Class') +
  scale_color_brewer(palette = 'PuOr') +
  theme_bw() +
  coord_cartesian(expand = F) +
  ggtitle('Broadleaf (Pedunculate Oak)') +
  theme(legend.position = 'bottom',
        panel.grid = element_blank())

ss_plt + pok_plt +
  plot_annotation(title = 'Carbon storage per hectare by yield class')
ggsave("output/figures/sdfig5_yc_ts.png", width = 23, height = 12, units = 'cm', dpi = 300)
ggsave("output/figures/sfig5_yc_ts.pdf", width = 23, height = 12, units = 'cm', dpi = 300)
