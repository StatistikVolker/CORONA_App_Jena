id <- c(1,2,3,4,5,6,7,8,9,10)
i <- c(1,2,3,1,0,5,7,2,1,1);length(inf)
df<-cbind.data.frame(id,i)
print(df)
dim(df)

weight<-c(1,0.90,0.6,0.1,0);length(weight)
wj = length(weight)

for (k in id){
  print(k)
  if (k + wj < dim(df)[1])   {df[paste0("col",k)] <- c(rep(0,k),df$i[k] * weight,rep(df$i[k] * weight[wj],dim(df)[1]-(k+wj)))}
  if (k + wj == dim(df)[1])  {df[paste0("col",k)] <- c(rep(0,k),df$i[k] * weight)}
  if (k + wj > dim(df)[1] & k < dim(df)[1])   {df[paste0("col",k)] <- c(rep(0,k),df$i[k] * weight[1:(dim(df)[1]-k)])}
  if (k == dim(df)[1])   {df[paste0("col",k)] <- rep(0,k)}
}

df

df<-df %>% mutate(rowSums(.[2:11]),
              i = cumsum(inf),
              r = i-a)

names(df)

#Datensatz für LK
dfplot <- df %>%
  select(id,i,a,r) %>%
  pivot_longer(-id,names_to ="status",values_to = "nsum")
  dfplot


ggplot() +
  geom_point(data = dfplot,aes(x = id,y = nsum, color = status, group = status),size = 3) +
  geom_step(data = dfplot,aes(x = id,y = nsum, color = status, group = status),size = 2) +
  scale_colour_manual(values = c("darkblue","forestgreen","orange")) + 
  #geom_line(data = dffit,aes(x = datum,y = projn, group = progtyp),size = 0.8) +
  theme_minimal() +
  theme(legend.position =  "bottom") +
  theme(legend.position =  "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        #panel.grid.minor.x = element_line(colour="blue", size=0.5), 
        panel.grid.major.x = element_line(colour="red", size=0.75)) + 
  #scale_y_continuous(breaks = seq(0,200,25),labels =seq(0,200,25)) +
  labs(y = "absolute Anzahl",x = "Datum") #+
  #scale_x_date(breaks = seq(mindat,maxdat,by="week"),
  #             date_labels = "%b %d", minor_breaks = "1 day") +
  #annotate("rect", xmin = mindat-1, xmax = maxdat+2, ymin = -10, ymax = -4,fill = "white") +
  #annotate("text",y=-6,x = c(seq(mindat,maxdat,"day")),
  #         label = c(rep(c("S","M","D","M","D","F","S"),poltweeks),"S"), 
  #         color =c(rep(c("red",rep("blue",5),"orange"),poltweeks),"red"))


