library(tidyverse)
# install.packages("esquisse")
library(esquisse)

# Addins > ggplot2 builder
esquisser(viewer = "browser")

# Colar o código gerado: 
ggplot(dplyr::starwars) +
  aes(x = gender, y = height, fill = gender) +
  geom_boxplot() +
  # paleta de cores
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  # legenda dos eixos
  labs(y = "Altura") +
  # adicionar tema
  ggthemes::theme_solarized() +
  # personalizar algo do tema
  theme(legend.position = "top")


# não esquecer de copiar o código
# se for pedir ajuda para IA:
# ofereça o código do gráfico que você já tem
# ofereça também o resultado do glimpse
glimpse(starwars)
