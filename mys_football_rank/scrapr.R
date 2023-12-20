# Scraping

# Get all Ranks
library(tidyverse)
library(rvest)

years = year(seq(as.Date("1993/1/1"), as.Date("2022/1/1"), "years"))
country = 'mys'

#1993 to 2022

rank_df = data.frame()
for(i in 1:length(years)){
  a = paste0("http://en.fifaranking.net/nations/",country,"/ranking_y.php?d=",years[i])
  b = read_html(a) %>% html_table()
  c = b[[1]]
  rank_df = bind_rows(rank_df,c)
}

rm(a,b,c)

rank_df2 = rank_df %>% select(-Pre) %>% 
  mutate(date_rank = as.Date(paste0(Date," 01"),format = "%b %Y %d"))

# Get Annual Ranks
# d = "http://en.fifaranking.net/nations/mys/ranking_d.php?d="
year_get = c('2019','2009','2002')

rank_annual = data.frame()

for(i in 1:length(year_get)){
  d = paste0("http://en.fifaranking.net/nations/",country,"/ranking_d.php?d=",year_get[i])
  e = read_html(d) %>% html_table()
  f = e[[1]]
  rank_annual = bind_rows(rank_annual,f)
}

rank_annual2 = rank_annual %>% distinct(Date,.keep_all = T) %>% mutate(Rank = as.integer(gsub("#","",Rank)),
                                                                       Points = as.integer(gsub("pts","",Points))) %>% 
  select(-Pre)

write_csv(rank_annual2,"data/rank_annual_mys.csv")
