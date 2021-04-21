library(quantmod)
library(broom)
library(purrr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(ggplot2)

# Coletando dados
getSymbols(c("^BVSP","^GSPC", "BRL=X", "BOVA11.SA"))

# Transformando
data <- map(list(BVSP, GSPC, `BRL=X`), tidy) %>% 
  reduce(left_join, by = "index") %>% 
  drop_na

# Manipulando
close <- data %>% 
  select(index, series.x, value.x, series.y, value.y) %>% 
  filter(str_detect(series.x, "Close"), str_detect(series.y, "Close")) %>% 
  mutate(year = year(index)) %>% 
  group_by(year) %>% 
  drop_na %>% 
  summarise(bvsp_var = var(value.x, na.rm = TRUE), gspc_var = var(value.y, na.rm = TRUE)) %>% 
  mutate(bvsp_var = (bvsp_var - min(bvsp_var))/(max(bvsp_var) - min(bvsp_var)), 
         gspc_var = (gspc_var - min(gspc_var))/(max(gspc_var) - min(gspc_var))) %>% 
  pivot_longer(cols = !year)

close2 <- data %>% 
  select(index, series.x, value.x, series.y, value.y) %>% 
  filter(str_detect(series.x, "Close"), str_detect(series.y, "Close")) %>% 
  mutate(year = year(index)) %>% 
  group_by(year) %>% 
  drop_na %>% 
  summarise(bvsp_var = var(value.x, na.rm = TRUE), gspc_var = var(value.y, na.rm = TRUE)) %>% 
  pivot_longer(cols = !year)

close3 <- data %>% 
  filter(str_detect(series.x, "Close"), 
         str_detect(series.y, "Close"), 
         str_detect(series, "Close")) %>% 
  mutate(value.x = value.x/value) %>% 
  select(-series, -value) %>% 
  mutate(year = year(index)) %>% 
  group_by(year) %>% 
  drop_na %>% 
  summarise(bvsp_var = var(value.x, na.rm = TRUE), gspc_var = var(value.y, na.rm = TRUE)) %>% 
  pivot_longer(cols = !year)

theme_set(theme_minimal())
p1 <- close %>%
  ggplot(aes(year, value)) +
  geom_line(aes(color = name)) +
  scale_color_discrete(labels = c("Ibovespa", "S&P500")) +
  labs(y = "Variabilidade",
       x = "Ano",
       title = "Comparação entre variabilidade entre índice Bovespa e S&P 500",
       subtitle = "Variância foi transformada de modo que fique entre 0 e 1",
       caption = "Oráculo de Delfos (2021)",
       color = "Índice")

p2 <- close2 %>%
  ggplot(aes(year, value)) +
  geom_line(aes(color = name)) +
  scale_color_discrete(labels = c("Ibovespa", "S&P500")) +
  scale_y_log10()+
  labs(y = "Variabilidade",
       x = "Ano",
       title = "Comparação entre variabilidade entre índice Bovespa e S&P 500",
       subtitle = "Variância foi transformada de modo que fique entre 0 e 1",
       caption = "Oráculo de Delfos (2021)",
       color = "Índice")

p3 <- close3 %>%
  ggplot(aes(year, value)) +
  geom_line(aes(color = name)) +
  scale_color_discrete(labels = c("Ibovespa", "S&P500")) +
  scale_y_log10()+
  labs(y = "Variabilidade",
       x = "Ano",
       title = "Comparação entre variabilidade entre índice Bovespa e S&P 500",
       subtitle = "Variância foi transformada de modo que fique entre 0 e 1",
       caption = "Oráculo de Delfos (2021)",
       color = "Índice")


# Experimento1 ------------------------------------------------------------

experiment_data <- BOVA11.SA %>%
  tidy() %>% 
  filter(str_detect(series, "Close"), index >= "2020-01-01", index < "2021-01-01") %>% 
  select(index, series, value) %>% 
  distinct() %>% 
  mutate(return = value / lag(value)) %>% 
  drop_na() %>% 
  mutate(cumulated = cumprod(return), final_value = 99600 * cumulated)


p4 <- experiment_data %>% 
  ggplot(aes(x = index, y = final_value))+
  geom_line(color = "firebrick2", size = 1.5, alpha = 0.7) +
  geom_vline(xintercept = date("2020-03-23"), linetype = 2) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$", big.mark = ".", decimal.mark = ",")) +
  labs(x = "Mês",
       y = "Valor da carteira (em R$)",
       title = "Aporte único em BOVA11",
       subtitle  = "Leva em conta aporte que ocorreu em 01/01/2021",
       caption = "Oráculo de Delfos (2021)")


# Experimento 2 -----------------------------------------------------------

total_data <- map(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
                    "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01",
                    "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01"),
                  function(x) BOVA11.SA %>%
                    tidy() %>% 
                    filter(str_detect(series, "Close"), index >= x, index < "2021-01-01") %>% 
                    select(index, series, value) %>% 
                    mutate(return = value / lag(value)) %>% 
                    drop_na() %>% 
                    mutate(cumulated = cumprod(return))
) %>% 
  reduce(left_join, by = "index") %>% 
  select(index, contains("cumulated")) 


# Valores investidos comprados:
value_invested <- experiment_data %>% 
  select(index, value) %>% 
  mutate(month = month(index)) %>% 
  filter(!duplicated(month)) %>% 
  mutate(invested = floor(8333/value),
         invested_value = invested*value) %>% 
  .$invested_value


final <- total_data %>% 
  select(-index)%>% 
  map2(., value_invested, function(x, y) x * y) %>% 
  reduce(bind_cols) %>% 
  rowwise() %>% 
  mutate(total = sum(c_across("...1":"...12"), na.rm = TRUE)) %>% 
  select(total) %>% 
  ungroup()

final$index <- total_data$index

p5 <- final %>% 
  ggplot(aes(x = index, y = total))+
  geom_line(color = "firebrick2", size = 1.5, alpha = 0.7) +
  geom_vline(xintercept = date("2020-03-23"), linetype = 2) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$", big.mark = ".", decimal.mark = ",")) +
  labs(x = "Mês",
       y = "Valor da carteira (em R$)",
       title = "Aportes mensais em BOVA11",
       subtitle  = "Leva em conta aportes próximos de R$8.333,00 mensais em todo dia primeiro de cada mês",
       caption = "Oráculo de Delfos (2021)")

# Experimento 3 -----------------------------------------------------------

returns <- ifelse(experiment_data$return-1 > 0, yes = 0, -1*(experiment_data$return-1))
investment_vector <- function(vec, prices){
  # Input: quedas em decimais com valores positivos (vec; double),
  #        preços diários (prices, double)
  # Output: tabela com o número de cotas compradas (number_quotas),
  #         o resto de dinheiro que sobrou após aportes (amount_left) e
  #         outras variáveis auxiliares que tratam do dinheiro aportado mensalmente
  
  # Inicializando variáveis
  amount_to <- rep(0, length(vec))
  bank <- rep(0, length(vec))
  amount_left <- rep(100000, length(vec))
  number_quotas <- rep(0, length(vec))
  invested <- rep(0, length(vec))
  for(i in seq(1, length(vec))){
    # Se o índice é maior que um, então ele pode olhar índices anteriores a ele
    if (i > 1) {
      # Calcula o valor que poderia ser aportado em decorrência da queda
      amount_to[i] <- ceiling(amount_left[i-1] * vec[i])
      # Se o valor é menor que o preço no dia, então esse dinheiro fica reservado
      if(amount_to[i] < prices[i]){
        # É possível que não haja queda num determinado dia, essa parte do código considera isso
        # Não havendo queda, não há necessidade de se aportar e os valores do dia anterior são copiados
        if(vec[i] == 0){
          bank[i] <- bank[i-1]
          amount_left[i] <- amount_left[i-1]
          amount_to[i] <- 0
        }else{
          bank[i] <- bank[i-1] + amount_to[i]
          amount_left[i] <- amount_left[i-1]
          amount_to[i] <- 0
        }
      }else{
        number_quotas[i] <- (bank[i-1] + amount_to[i ]) %/% prices[i]
        invested[i] <- number_quotas[i] * prices[i]
        bank[i] <- (bank[i-1] + amount_to[i]) - (prices[i] * number_quotas[i])
        amount_left[i] <- amount_left[i-1] - invested[i]
      }

    } else {
      # Essa parte do código inicia o valor da primeira linha
      amount_to[i] <- ceiling(100000 * vec[i])
      number_quotas[i] <- amount_to[i] %/% prices[i]
      bank[i] <- (amount_to[i]) - (prices[i] * number_quotas[i])
      amount_left[i] <- amount_left[i] - amount_to[i]
      invested[i] <- number_quotas[i] * prices[i]
    }
  }
  return(tibble(vec, prices, bank, amount_to, amount_left, number_quotas, invested))
}

# Dados referentes a aportes e quantidades de cotas
new_experiment <- investment_vector(returns, prices = experiment_data$value)
new_experiment$index <- experiment_data$index
new_experiment

# Dados de retornos

new_experiment_returns <- map(new_experiment$index,
                              function(x) BOVA11.SA %>%
                                tidy() %>% 
                                filter(str_detect(series, "Close"), index >= x, index < "2021-01-01") %>% 
                                select(index, series, value) %>% 
                                mutate(return = value / lag(value)) %>% 
                                drop_na() %>% 
                                mutate(cumulated = cumprod(return))
) %>% 
  reduce(left_join, by = "index") %>% 
  select(index, contains("cumulated"))


final_experiment <- new_experiment_returns %>% 
  select(-index)%>% 
  map2(., new_experiment$invested, function(x, y) x * y) %>% 
  reduce(bind_cols) %>% 
  rowwise() %>% 
  mutate(total = sum(c_across("...1":"...245"), na.rm = TRUE)) %>% 
  select(total) %>% 
  ungroup()

final_experiment$index <- new_experiment_returns$index

p6 <- final_experiment %>% 
  ggplot(aes(x = index, y = total))+
  geom_line(color = "firebrick2", size = 1.5, alpha = 0.7) +
  geom_vline(xintercept = date("2020-03-23"), linetype = 2) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$", big.mark = ".", decimal.mark = ",")) +
  labs(x = "Mês",
       y = "Valor da carteira (em R$)",
       title = "Aportes diários em BOVA11",
       subtitle  = "Leva em conta aportes diários baseados nas quedas da bolsa",
       caption = "Oráculo de Delfos (2021)")
