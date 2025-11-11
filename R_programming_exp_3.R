library(dplyr)
data("mtcars")
data("iris")

mtcars %>%filter(hp>100)

mtcars%>%filter(cyl%in%c(4,6)&c(4,6)&am==0)

iris%>%filter(Species == "Setosa" & Petal.Length)

mtcars%>%select(mpg,cyl,hp)

mtcars%>%select(-drat,-wt)

iris%>%select(starts_with("Sepal"))

mtcars%>%
  group_by(cyl)%>%
  summarise(avg_mpg = mean(mpg),avg_hp = mean(hp))

iris%>%
  group_by(Species)%>%
  summarise(max_sepal_length = max(Sepal.Length),max_sepal_width = max(Sepal.Width))

mtcars%>%
  filter(vs == 0) %>%
  select(mpg,qsec,am)%>%
  group_by(am)%>%
  summarise(mean_mpg = mean(mpg))

iris%>%
  group_by(Species)%>%
  filter(Petal.Width>1)%>%
  summarise(mean_petal_length = mean(Petal.Length))


library(ggplot2)
library(patchwork)

p1<-mtcars%>%
  group_by(gear)%>%
  summarise(avg_mpg = mean(mpg))%>%
  ggplot((aes(x=factor(gear),y=avg_mpg,fill = factor(gear))))+
  geom_bar(stat = "identity")+
  labs(title = "Average MPG by Gear",x="Gear",y="Average MPG")+
  theme_minimal()

p2<-iris%>%
  group_by(Species)%>%
  summarise(avg_petal_length = mean(Petal.Length))%>%
  ggplot(aes(x=Species,y=avg_petal_length,fill=Species))+
  geom_bar(stat = "identity")+
  labs(title = "Average Petal Length by Species",y="Average Petal Length")+
  theme_minimal()

p1+p2+plot_layout(widths = c(2,2))
