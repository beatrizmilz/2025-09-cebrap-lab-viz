# Carregar pacotes -------------------------------------
library(abjData)
library(tidyverse)
library(patchwork)
library(geobr)


# Plot 1 - Mapa ------------------------------------------
estados <- geobr::read_state()

plot_mapa <- estados |> 
  ggplot() +
  geom_sf(aes(fill = name_region), show.legend = FALSE) +
  theme_void() +
  scale_fill_brewer(palette = "Set2") 

plot_mapa

# Plot 2 - Gráfico de colunas -----------------------
  
# preparando os dados. Fiz um join pois a tabela 
# abjData::pnud_uf não tem a coluna do nome da região
pnud_mais_recente <- abjData::pnud_uf |> 
  left_join(estados, by = c("uf" = "code_state")) |> 
  filter(ano == max(ano)) |> 
  select(ano, uf, ufn, idhm, name_region) |>
  mutate(ufn_fct = fct_reorder(ufn, idhm))

plot_colunas <- pnud_mais_recente |> 
  ggplot() +
  aes(x = idhm, y = ufn_fct, fill = name_region) +
  geom_col() +
  scale_x_continuous(limits = c(0, 1)) +
  coord_cartesian(expand = FALSE) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "IDH", y = "", fill = "Região",
       caption = "Fonte: dados do PNUD, obtidos no pacote {abjData}.",
       title = "IDH por estado, em 2010") +
  theme_minimal() +
  theme(legend.position = "bottom")

plot_colunas


# Juntar os dois plots com patchwork --------------
# o argumento widths define o tamanho de cada plot
wrap_plots(plot_colunas, plot_mapa, widths = c(2, 1))

