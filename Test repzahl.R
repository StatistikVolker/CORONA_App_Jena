# Reproduktionszahl Thüringen
require(EpiEstim)

names(dftsI)
OurTools::Table(dftsI$county)


names(RKI_df)

dftsI_TH <- dftsI %>% filter(county == "SK Leipzig" | Bundesland == "Thüringen" )  %>%
  group_by(county) %>% 
  mutate(idx = which(infected != 0)[1] - 1) %>%
  filter(id >= idx) %>%
  group_by(county) %>%
  mutate(id = row_number()) %>%
  select(-idx) %>%
  rename(i = infected)



dfestR <- dftsI_TH %>% filter(county == "SK Leipzig" )


estimate_R(incid = dfestR$i, method = c("si_from_data"),
           config = make_config(incid = dfestR$i, method = "si_from_data"))
