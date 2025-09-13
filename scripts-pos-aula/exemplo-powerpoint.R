# exemplo simples: 
# salvar um gráfico em ppt editável

# carregar pacotes
library(tidyverse)
library(esquisse)

# criando um gráfico simples e salvando em um objeto
grafico_1 <- starwars |>
  ggplot() +
  aes(x = height, y = mass) +
  geom_point()

grafico_2 <- starwars |>
  mutate(
    species_fct = forcats::fct_lump_min(species, min = 2)) |> 
  count(species_fct) |>
  mutate(species_fct = forcats::fct_reorder(species_fct, n)) |> 
  ggplot() +
  aes(x = n, y = species_fct) +
  geom_col()

# a função ggplot_to_ppt é do pacote esquisse. 
# ela recebe apenas um argumento: um vetor (c()) com textos (entre aspas)
# contendo os nomes dos objetos.
# não indicamos onde o arquivo será salvo, 
# e a função abre o powerpoint com o resultado.
# cada gráfico ficará em um slide do powerpoint
ggplot_to_ppt(c("grafico_1", "grafico_2"))

