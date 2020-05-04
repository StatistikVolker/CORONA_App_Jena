fcn.fitdistr.mom.gamma<-function(x){
  x.mean=mean(x)
  x.var=mean(x^2) - (x.mean^2)
  rate.mom=x.mean/x.var
  shape.mom=(x.mean^2)/x.var
  result=c(shape=shape.mom, rate=rate.mom)
  return(result)
}

names(dftsI)
Jinfected <- dftsI %>% filter(county == "SK Jena") %>% pull(infected)
Jinfected
length(Jinfected)
Jinfected <- Jinfected[min(which(Jinfected > 0)):length(Jinfected)]
Jinfected[Jinfected== 0]<-0.000000001
data.precipitation.mom.gamma<-fcn.fitdistr.mom.gamma(Jinfected)

library(MASS)

# Plot using density points
dfplot <- dftsI %>% filter(county == "SK Leipzig") %>% slice(min(which(infected > 0)):length(infected)) %>%
  mutate(infected = if_else(infected == 0,"0.000000001",as.character(infected)),
         infected = as.numeric(infected),
         id = row_number())

options(scipen = 999)
gfun <- gamma(dfplot$infected);gfun
mean(gfun);sd(gfun)
den <- density(dfplot$infected)
den$x
dat <- data.frame(x = den$x, y = den$y)
ggplot(data = dat, aes(x = x, y = y)) + 
  geom_point(size = 3) +
  theme_classic()


fit.params <- fitdistr(dfplot$infected, "gamma")#, lower = c(0, 0))
fit.params
# Plot using density points

ggplot(data = dat, aes(x = x,y = y)) + 
  geom_point(size = 3) +     
  geom_line(aes(x=dat$x, y=dgamma(dat$x,fit.params$estimate["shape"], fit.params$estimate["rate"])), color="red", size = 1) + 
  theme_classic()





OurTools::Table(dfplot$infected)
fit.params <- fitdistr(dfplot$infected, "gamma")#, lower = c(0, 0))
fit.params
class(dfplot$Datum)
ggplot(data = dfplot, aes(x = id,y = infected)) + 
  geom_point(size = 3) +     
  geom_line(aes(x=id, y=dgamma(id,fit.params$estimate["shape"], fit.params$estimate["rate"])), color="red", size = 1) + 
  theme_classic()


barplot(Jinfected,nclass=15,
     main="Precipitation Data (nclass=15)\n(Le Cam and Neyman)")

print(data.precipitation.mom.gamma)


y <-gamma(Jinfected, shape = data.precipitation.mom.gamma[1], rate = data.precipitation.mom.gamma[2])
plot(y)

dfplot <- dftsI %>% filter(county == "SK Jena") #%>% filter(infected > 0)
names(dfplot)
ggplot(data = dfplot,aes(x= Datum, y= infected) ) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "Gamma"),
              se = FALSE, 
              colour = "black", size = 0.8)


fit<-glm(infected ~ Datum, 
      data = dfplot, family = Gamma(link = "inverse"))

plot(predict(fit))
