library(tidyverse)
library(lubridate)
library(here)

message("Lendo dados brutos de eventos")

events = read_csv(here::here("data/events_log.csv.gz"))

# events = events %>% slice(1:4e5) # Útil para testar código em dados pequenos. Comente na hora de processá-los para valer.

message("Transformando em dados por busca")

events = events %>% 
    group_by(session_id) %>% 
    arrange(timestamp) %>% 
    mutate(num_searches = cumsum(action == "searchResultPage")) # contador para as buscas nessa sessão.

searches = events %>% 
    group_by(session_id, num_searches) %>% 
    arrange(timestamp) %>% 
    summarise(
        first_event_timestamp = first(timestamp),
        first_event_date_time = ymd_hms(first(timestamp)),
        group = first(group), # eventos de uma mesma sessão são de um mesmo grupo
        results = max(n_results, na.rm = TRUE), # se não houver busca, retorna -Inf
        num_clicks = sum(action == "visitPage"), 
        first_click = ifelse(num_clicks == 0, 
                             NA_integer_, 
                             first(na.omit(result_position))
        )
    ) %>% 
    filter(num_searches > 0) # Apenas search sessions

out_file = here("data/search_data.csv")

message("Salvando em ", out_file)

searches %>% 
    write_csv(out_file)
