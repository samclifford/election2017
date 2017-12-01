primary_votes <- read_csv("Data/primary_votes.csv")

cols <- list(ALP = "#DE3533",
             GRN = "#39b54a",
             LNP = "#1456F1",
             IND = "#808080",
             KAP = "#b50204",
             ONP = "#F8F16F",
             CR = "#000000") %>%
  unlist

windows(width=20, height=10)
mutate(primary_votes, Party = if_else(Party == "", "IND", Party)) %>%
  group_by(Electorate) %>%
  arrange(Percent) %>%
  mutate(group = 1:n()) %>%
  ggplot(data=.,aes(x=0, y=Percent)) + 
  geom_col(aes(fill=Party, group=group), 
           position=position_stack(),
           color="black") +
  coord_polar(theta = "y") +
  facet_wrap( ~ Electorate, ncol=16) +
  theme_minimal() + 
  theme(legend.position="right", axis.text = element_blank(), strip.text = element_text(size=7)) +
  scale_fill_manual(values = cols) +
  xlab("") + ylab("")

windows()
to_plot <- mutate(primary_votes, Party = if_else(Party == "", "IND", Party)) %>%
  group_by(Electorate) %>%
  arrange(Percent) %>%
  mutate(group = 1:n())

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
  theme(legend.position="right", axis.text = element_blank(), strip.text = element_text(size=7)) +
  scale_fill_manual(values = cols) +
  xlab("") + ylab("")
