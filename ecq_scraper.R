library(tidyverse)
library(rvest)

# in 2017 there are N electorates
N <- 93
electorate_url  <- vector("list", N)
electorate_data <- vector("list", N)
electorate_name <- vector("list", N)
electorate_results <- vector("list", N)

for (i in 1:N){
  electorate_url[[i]] <- paste("http://results.ecq.qld.gov.au/elections/state/State2017/results/district",
                               i,
                               ".html", sep="")
  
  electorate_data[[i]] <- read_html(electorate_url[[i]])
  
  electorate_data[[i]] <- electorate_data[[i]] %>% html_nodes("title") %>%
    gsub(x=., pattern = "<title>Elections - 2017 State General Election - ", replacement= "") %>%
    gsub(x=., pattern = " - \r\n              District Summary\r\n            </title>\n", replacement="")
  
  electorate_results[[i]] <- (electorate_data[[i]] %>% html_nodes("table.resultTableBorder"))[[2]] %>% html_table(trim = T, fill=T, head=F) %>%
    filter(row_number() > 1) %>%
    filter(!is.na(X7)) %>%
    select(Candidate = X1, Party = X2, Votes = X3, Percent = X7) %>%
    mutate_at(.vars = vars(Votes, Percent), funs(parse_number)) %>%
    mutate(Electorate = electorate_name[[i]])
  
}

primary_votes <- bind_rows(electorate_results)

mutate(primary_votes, Party = if_else(Party == "", "IND", Party)) %>%
  ggplot(data=.,aes(x=Electorate, y=Percent)) + 
  geom_col(aes(fill=Party)) +
  coord_flip()
