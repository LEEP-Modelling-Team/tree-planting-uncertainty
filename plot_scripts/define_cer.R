# Define "focus" climate-economy realisations
CER <- c(457, 2544, 3083)
cer_names <- c('NH', 'ME', 'HE')
p_cer_names <- c("P-NH", "P-ME", "P-HE")
p_cer_colors <- ggsci::pal_nejm()(8)[c(1,2,3)]
cer_colors <- ggsci::pal_nejm()(8)[c(1,2,3)] %>%
  purrr::modify(function(x) paste0(substr(x, 1,7), "AA"))