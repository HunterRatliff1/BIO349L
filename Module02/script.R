require(googlesheets)
require(dplyr)
require(tidyr)
require(ggplot2)
require(ggthemes)

df <- gs_title("Module 02") %>% gs_read_csv("Data") %>% mutate(
  ch = (ch1+ch2)/2,
  mpq = (mpq1+mpq2)/2,
  A1 = 0.5*ch1*ch2*sin(a*pi/180),
  A2 = 0.5*mpq1*mpq2*sin(b*pi/180),
  R  = (A2-A1)/A1
)

df %>% select(Gene:Index, ch, mpq, a, b) %>%
  mutate(
    VHA  = ch*cos((a/2)*(pi/180)),
    VMA = mpq*cos((b/2)*(pi/180)),
    Length = mpq/ch,
    Height = VMA/VHA) %>% 
  select(-a, -b) %>%
  # group_by(Gene) %>% summarise(Mean = mean(Height), sd = sd(Height))
  ggplot(aes(x=Height)) + geom_density(aes(fill=Gene, linetype=Group), alpha=0.5)
  ggplot(aes(x=Gene, y=Height, fill=Group)) + geom_boxplot() + labs(x="Mutant", y="VMA / VHA Ratio") +
  # ggplot(aes(x=VHA*(369/200), y=VMA*(369/200), fill=Group)) + geom_point() + labs(y="VMA Rise", x="VHA Rise") + facet_wrap("Gene") +
  scale_fill_fivethirtyeight(name="") + theme_fivethirtyeight() + theme(axis.title=element_text())
# bc3140
  tbl_df()
  gather(Measure, Ratio, Length:Height) %>%
  ggplot() + geom_boxplot(aes(x=Gene, y=Ratio, fill=Group)) + facet_wrap("Measure")

  
  
  
  
df %>% select(Gene:Index, ch, mpq, a, b) %>%
  mutate(
    VHA  = ch*cos((a/2)*(pi/180)),
    VMA = mpq*cos((b/2)*(pi/180)),
    Length = mpq/ch,
    Height = VMA/VHA) %>% 
  select(-a, -b) %>% 
  ggplot(aes(x=VHA*(369/200), y=VMA*(369/200), color=Group)) + 
  geom_point() + 
  labs(y="VMA Rise", x="VHA Rise") + 
  facet_wrap("Gene") + coord_fixed() +
  scale_color_fivethirtyeight(name="") + theme_fivethirtyeight() + theme(axis.title=element_text())
  
df %>% ggplot(aes(color=Group, label=Gene)) + geom_text(aes(x=ch, y=mpq)) + coord_equal() + 
  scale_color_fivethirtyeight() + theme_fivethirtyeight() + theme(axis.title=element_text())

df %>% ggplot(aes(color=Group, label=Gene)) + geom_text(aes(x=a, y=b)) + coord_equal() + 
  scale_color_fivethirtyeight() + theme_fivethirtyeight() + theme(axis.title=element_text())

df %>% ggplot(aes(color=Group, label=Gene)) +
  geom_text(aes(x=ch1/mpq1, y=ch2/mpq2)) + coord_fixed() +
  scale_color_fivethirtyeight() + theme_fivethirtyeight() + theme(axis.title=element_text()) 

df %>% ggplot(aes(color=Group, label=Gene)) + 
  # geom_text(aes(x=mpq1+mpq2, y=ch1+ch2)) + coord_fixed() +
  geom_text(aes(x=(mpq1+mpq2)/(ch1+ch2), y=R)) + #coord_fixed() +
  # facet_grid(Gene~Group) +
  scale_color_fivethirtyeight() + theme_fivethirtyeight() + theme(axis.title=element_text()) 