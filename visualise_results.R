primary_votes <- read_csv("Data/primary_votes.csv")

cols <- list(ALP = "#DE3533",
             GRN = "#39b54a",
             LNP = "#1456F1",
             IND = "#808080",
             KAP = "#b50204",
             ONP = "#F8F16F",
             CR = "purple") %>%
  unlist

windows(width=20, height=10)

to_plot <- mutate(primary_votes,
                  Party = if_else(is.na(Party), 
                                  "IND", 
                                  Party)) %>%
  group_by(Electorate) %>%
  arrange(Percent) %>%
  mutate(group = 1:n()) %>%
  ungroup %>%
  arrange(Electorate, Party)

ggplot(data=to_plot,
       aes(x=0, y=Percent)) + 
  geom_col(aes(fill=Party, group=group), 
           position=position_stack(),
           color="black") +
  coord_polar(theta = "y") +
  facet_wrap( ~ Electorate, ncol=16) +
  theme_minimal() + 
  theme(legend.position="right", 
        axis.text = element_blank(), 
        strip.text = element_text(size=7)) +
  scale_fill_manual(values = cols) +
  xlab("") + ylab("")

filter(to_plot, Percent > 50) %>%
  select(Electorate) %>%
  inner_join(to_plot) %>%
  ggplot(data=.,aes(x=0, y=Percent)) + 
  geom_col(aes(fill=Party, group=group), 
           position=position_stack(),
           color="black") +
  coord_polar(theta = "y") +
  facet_wrap( ~ Electorate) +
  theme_minimal() + 
  theme(legend.position="right", 
        axis.text = element_blank(),
        strip.text = element_text(size=7)) +
  scale_fill_manual(values = cols) +
  xlab("") + ylab("") + 
  ggtitle(label = "Queensland State Election 2017", 
          subtitle = "Winners on primary vote")

ABC_2CP <- read_csv("Data/2CP_votes.csv")

to_plot_2CP <- list(Primary = to_plot,
                    `2CP` = ABC_2CP) %>%
  bind_rows(.id = "Type")

# an example of any three-cornered contests
three_cornered <- filter(to_plot, Percent > 25) %>%
  count(Electorate) %>%
  filter(n >= 3) %>%
  select(Electorate)



to_plot_2CP %>%
  ggplot(data = .,
         aes(x = Type, y = Percent)) +
  geom_col(aes(fill = Party, group = group),
           position = position_stack(), 
           width = 1,
           color = "black") +
  coord_polar(theta = "y") +
  facet_wrap( ~ Electorate, ncol = 16) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_blank(),
    strip.text = element_text(size = 7)
  ) +
  scale_fill_manual(values = cols) +
  xlab("") + ylab("") +
  geom_text(
    data = filter(to_plot_2CP, Type == "2CP", group == 2),
    aes(x = Type, y = Percent / 2, label = Percent),
    size = 2,
    color = "white"
  )
