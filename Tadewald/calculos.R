library(tidyverse)

## Funções auxiliares
to_monthly <- function(r){
  # Input: taxa de juros anual (r; double)
  # Output: taxa de juros mensal.
  return((1+r)^(1/12) - 1)
}

find_minimum <- function(fv = 973, r = 0.05, t = 7*12){
  # Inputs: valor mensal a ser recebido no futuro (fv; double), taxa anual (r; double),
  # período máximo de espera (t; double)
  # Outputs: valor mínimo de depósitos ao longo do período para se obter as parcelas 
  # no futuro.
  r <- to_monthly(r)
  p <- fv / r
  return((p*r)/((1+r)^t-1))
}

## Modelo básico de crescimento do patrimônio
total_revenue <- function(n, r){
  # Inputs: n períodos (double) a uma taxa r (double) por período.
  # Outputs: valor total dos juros; para obter o valor futuro dos depósitos, 
  # multiplique pelo depósito por período.
  # Detalhe: arrendontamentos fazem uma baita diferença; perceba que o código
  # não é muito eficiente, é possível simplificar o loop em uma única linha
  # mas como se trata de um experimento simples, não temos problemas em realizar
  # dessa maneira.
  rate = 1+r
  total = 0
  for(i in seq(1, n)){
    total = total + round((rate)^(i-1), digits = 5)
  }
  return(total)
}


## Parâmetros determinados
t = 7*12
r = map_dbl(seq(0.05, 0.1, by = 0.01), ~to_monthly(.x))
r_names = c('5%', '6%', '7%', '8%', '9%', '10%')
minimum_patrimony = 937/r

## Gerando dados

returns <- map(r, function(x) map_dbl(seq(1, t), ~total_revenue(.x, x)))

df <- tibble(r_names, returns) %>%
  unnest(returns) %>% 
  group_by(r_names) %>% 
  mutate(id = row_number()) %>% 
  ungroup()

df2 <- tibble(r_names, minimum_patrimony) %>% 
  mutate(r_value = seq(0.05, 0.1, by = 0.01),
         minimum_deposits = find_minimum(973, r = r_value, t = t))

df <- df %>%
  pivot_wider(names_from = r_names, values_from = returns) %>% 
  mutate(
    `5%` = `5%` * df2$minimum_deposits[1],
    `6%` = `6%` * df2$minimum_deposits[2],
    `7%` = `7%` * df2$minimum_deposits[3],
    `8%` = `8%` * df2$minimum_deposits[4],
    `9%` = `9%` * df2$minimum_deposits[4],
    `10%` = `10%` * df2$minimum_deposits[5]
  ) %>% 
  pivot_longer(cols = ends_with("%"), names_to = "r_names", values_to = "returns")

## Visualizações
# Configuração básica
theme_set(theme_minimal())

# Gráfico de patrimônio
p1 <- df2 %>%
  mutate(r_names = fct_reorder(r_names, minimum_patrimony, .desc = TRUE)) %>% 
  ggplot(aes(r_names, minimum_patrimony, fill = r_names)) + 
  geom_col() +
  geom_label(aes(label = round(minimum_patrimony)), fill = 'white', nudge_y = 10000) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$", big.mark = ".", decimal.mark = ",")) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Greens", direction = -1) +
  labs(y = "Patrimônio (em reais)",
       x = "Taxa de juros anual",
       title = "Patrimônio necessário para se obter um salário mínimo por mês em rendimentos",
       subtitle = "Levamos em conta o valor do  s.m. de 2017",
       caption = "Oráculo de Delfos (2021)")
  


# Gráfico de depósitos mínimos

p2 <- df2 %>%
  mutate(r_names = fct_reorder(r_names, minimum_deposits, .desc = TRUE)) %>% 
  ggplot(aes(r_names, minimum_deposits, fill = r_names)) + 
  geom_col() +
  geom_label(aes(label = round(minimum_deposits)), fill = 'white', nudge_y = 100) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$", big.mark = ".", decimal.mark = ",")) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Greens", direction = -1) +
  labs(y = "Depósito mínimo (em reais)",
       x = "Taxa de juros anual",
       title = "Depósitos mínimos para se obter um salário mínimo por mês em rendimentos",
       subtitle = "Levamos em conta o valor do  s.m. de 2017, período de 7 anos",
       caption = "Oráculo de Delfos (2021)")


# Gráfico de simulações
p3 <- df %>% 
  ggplot(aes(x = id, y = returns, color = r_names)) +
  geom_line(size = 1.5, alpha = 0.7) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$", big.mark = ".", decimal.mark = ",")) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Greens") +
  labs(y = "Valor do patrimônio (em reais)",
       x = "Mês (ordinal)",
       title = "Trajetória do patrimônio ao longo de 7 anos",
       subtitle = "Aportes mensais referentes aos depósitos mínimos. Ao fim do período, o retorno mensal será de 1 s.m.",
       caption = "Oráculo de Delfos (2021)")

# Salvando os resultados

map(c('p1', 'p2', 'p3'), function(x) ggsave(paste0(x, ".png"), plot = get(x)))
