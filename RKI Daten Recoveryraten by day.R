
names(RKI_df)

dfalter <- RKI_df %>% group_by(Geschlecht) %>% 
  summarize( i = sum(AnzahlFall),
             r = sum(AnzahlGenesen),
             d = sum(AnzahlTodesfall)) %>%
  mutate(a = i - r - d,
         ap = a/i,
         rp = r/i,
         dp = d/i)


names(RKI_df)

dfalter2 <- RKI_df %>% 
  mutate(ak2 = case_when(Altersgruppe == "A00-A04" ~ "A00-A15",
                         Altersgruppe == "A05-A14" ~ "A00-A15",
                         TRUE ~ Altersgruppe
                         )
         ) %>%
         
  group_by(ak2,Bundesland) %>% 
  summarize( i = sum(AnzahlFall),
             r = sum(AnzahlGenesen),
             d = sum(AnzahlTodesfall)) %>%
  mutate(a = i - r - d,
         ap = a/i,
         rp = r/i,
         dp = d/i) %>%
  #select(Altersgruppe,Bundesland,d,dp)  %>%
  #filter(Altersgruppe == "A80+") %>%
  arrange(desc(dp)) %>%
  filter(ak2 != "unbekannt") %>%
  filter(Bundesland == "Bremen")


ggplot(dfalter2,aes(y = rp,x = Bundesland,color = ak2,group = ak2))+
  geom_point(size = 2) +
  geom_line(size = 1) +
  coord_flip()


glmdata <- RKI_df %>% filter(Geschlecht != "unbekannt" & Bundesland == "Thüringen")

glm(data = glmdata,AnzahlFall ~ Altersgruppe) #+Bundesland)
