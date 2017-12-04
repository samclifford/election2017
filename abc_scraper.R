# devtools::install_github('milesmcbain/datapasta')
library(datapasta)

convert_abc <- function(x) {
  matrix(x, ncol = 5, byrow = T) %>%
    data.frame %>%
    rename(
      Party_ABC = X1,
      Candidate = X2,
      Votes = X3,
      `Percent` = X4,
      Swing = X5
    ) %>%
    mutate_at(.vars = vars(Votes:Swing), .funs = parse_number) %>%
    mutate_at(.vars = vars(Party_ABC, Candidate), .funs = parse_character) %>%
    mutate(Candidate = substr(
      Candidate,
      start = 1,
      stop = nchar(as.character(Candidate)) / 2
    ))
}


ABC_2CP <- list(
  Algester = c(
    "Labor",
    "Leeanne EnochLeeanne Enoch",
    "17,820",
    "64.3%",
    "+4.7%",
    "LNP",
    "Clinton PattisonClinton Pattison",
    "9,876",
    "35.7%",
    "-4.7%"
  )
  ,
  Aspley = c(
    "Labor",
    "Bart MellishBart Mellish",
    "16,376",
    "51.2%",
    "+4.3%",
    "LNP",
    "Tracy DavisTracy Davis",
    "15,633",
    "48.8%",
    "-4.3%"
  )
  ,
  Bancroft = c(
    "Labor",
    "Chris WhitingChris Whiting",
    "15,425",
    "56.0%",
    "-2.4%",
    "LNP",
    "Kara ThomasKara Thomas",
    "12,144",
    "44.0%",
    "+2.4%"
  )
  ,
  `Barron River` = c(
    "Labor",
    "Craig CrawfordCraig Crawford",
    "14,530",
    "52.6%",
    "-1.0%",
    "LNP",
    "Michael TroutMichael Trout",
    "13,109",
    "47.4%",
    "+1.0%"
  )
  ,
  Bonney = c(
    "LNP",
    "Sam O'ConnorSam O'Connor",
    "13,172",
    "51.4%",
    "-0.7%",
    "Labor",
    "Rowan HolzbergerRowan Holzberger",
    "12,456",
    "48.6%",
    "+0.7%"
  )
  ,
  Broadwater = c(
    "LNP",
    "David CrisafulliDavid Crisafulli",
    "18,041",
    "68.7%",
    "+2.4%",
    "Labor",
    "Peter FloriPeter Flori",
    "8,202",
    "31.3%",
    "-2.4%"
  )
  ,
  Buderim = c(
    "LNP",
    "Brent MickelbergBrent Mickelberg",
    "17,982",
    "63.3%",
    "+1.5%",
    "One Nation",
    "Steve DicksonSteve Dickson",
    "10,437",
    "36.7%",
    "-1.5%"
  )
  ,
  Bulimba = c(
    "Labor",
    "Di FarmerDi Farmer",
    "19,069",
    "60.3%",
    "+4.1%",
    "LNP",
    "Fiona WardFiona Ward",
    "12,560",
    "39.7%",
    "-4.1%"
  ),
  
  Bundaberg = c(
    "LNP",
    "David BattDavid Batt",
    "16,051",
    "54.2%",
    "+4.7%",
    "Labor",
    "Leanne DonaldsonLeanne Donaldson",
    "13,563",
    "45.8%",
    "-4.7%"
  )
  ,
  Bundamba = c(
    "Labor",
    "Jo-Ann MillerJo-Ann Miller",
    "18,379",
    "72.0%",
    "-3.9%",
    "LNP",
    "Patrick HerbertPatrick Herbert",
    "7,142",
    "28.0%",
    "+3.9%"
  ),
  
  Burdekin = c(
    "LNP",
    "Dale LastDale Last",
    "14,338",
    "50.6%",
    "+2.0%",
    "Labor",
    "Michael BrunkerMichael Brunker",
    "13,974",
    "49.4%",
    "-2.0%"
  )
  ,
  Burleigh = c(
    "LNP",
    "Michael HartMichael Hart",
    "15,035",
    "55.3%",
    "-0.1%",
    "Labor",
    "Gail HislopGail Hislop",
    "12,138",
    "44.7%",
    "+0.1%"
  )
  ,
  Burnett = c(
    "LNP",
    "Stephen BennettStephen Bennett",
    "17,526",
    "61.0%",
    "+4.4%",
    "Labor",
    "Lee HarveyLee Harvey",
    "11,219",
    "39.0%",
    "-4.4%"
  ),
  Cairns = c(
    "Labor",
    "Michael HealyMichael Healy",
    "14,669",
    "53.5%",
    "-4.0%",
    "LNP",
    "Sam MarinoSam Marino",
    "12,753",
    "46.5%",
    "+4.0%"
  )
  ,
  Callide = c(
    "LNP",
    "Colin BoyceColin Boyce",
    "14,023",
    "54.2%",
    "-5.6%",
    "One Nation",
    "Sharon LohseSharon Lohse",
    "11,848",
    "45.8%",
    "+5.6%"
  )
  ,
  Caloundra = c(
    "LNP",
    "Mark McArdleMark McArdle",
    "15,488",
    "53.7%",
    "-1.0%",
    "Labor",
    "Jason HuntJason Hunt",
    "13,369",
    "46.3%",
    "+1.0%"
  ),
  Capalaba = c(
    "Labor",
    "Don BrownDon Brown",
    "16,237",
    "57.3%",
    "+0.8%",
    "LNP",
    "Cameron LeafeCameron Leafe",
    "12,081",
    "42.7%",
    "-0.8%"
  )
  ,
  
  Chatsworth = c(
    "LNP",
    "Steve MinnikinSteve Minnikin",
    "15,466",
    "52.9%",
    "-0.4%",
    "Labor",
    "Paul KeenePaul Keene",
    "13,758",
    "47.1%",
    "+0.4%"
  )
  ,
  Clayfield = c(
    "LNP",
    "Tim NichollsTim Nicholls",
    "15,470",
    "52.8%",
    "-3.8%",
    "Labor",
    "Philip AnthonyPhilip Anthony",
    "13,822",
    "47.2%",
    "+3.8%"
  )
  ,
  Condamine = c(
    "LNP",
    "Pat WeirPat Weir",
    "19,146",
    "59.4%",
    "-7.8%",
    "One Nation",
    "Frank AshmanFrank Ashman",
    "13,110",
    "40.6%",
    "+7.8%"
  )
  ,
  Cook = c(
    "Labor",
    "Cynthia LuiCynthia Lui",
    "13,438",
    "54.2%",
    "-2.6%",
    "One Nation",
    "Jen SackleyJen Sackley",
    "11,375",
    "45.8%",
    "+2.6%"
  )
  ,
  Coomera = c(
    "LNP",
    "Michael CrandonMichael Crandon",
    "15,269",
    "53.1%",
    "-2.7%",
    "Labor",
    "Christopher JohnsonChristopher Johnson",
    "13,461",
    "46.9%",
    "+2.7%"
  )
  ,
  
  Cooper = c(
    "Labor",
    "Kate JonesKate Jones",
    "19,524",
    "60.6%",
    "+7.3%",
    "LNP",
    "Robert ShearmanRobert Shearman",
    "12,674",
    "39.4%",
    "-7.3%"
  ),
  Currumbin = c(
    "LNP",
    "Jann StuckeyJann Stuckey",
    "14,663",
    "53.2%",
    "-2.5%",
    "Labor",
    "Georgi LeaderGeorgi Leader",
    "12,902",
    "46.8%",
    "+2.5%"
  ),
  Everton = c(
    "LNP",
    "Tim ManderTim Mander",
    "17,600",
    "55.0%",
    "+2.9%",
    "Labor",
    "David GreeneDavid Greene",
    "14,421",
    "45.0%",
    "-2.9%"
  )
  ,
  `Ferny Grove` = c(
    "Labor",
    "Mark FurnerMark Furner",
    "15,541",
    "54.7%",
    "-0.6%",
    "LNP",
    "Nick ElstonNick Elston",
    "12,869",
    "45.3%",
    "+0.6%"
  )
  ,
  Gaven = c(
    "Labor",
    "Meaghan ScanlonMeaghan Scanlon",
    "13,413",
    "50.9%",
    "+3.6%",
    "LNP",
    "Sid CrampSid Cramp",
    "12,949",
    "49.1%",
    "-3.6%"
  )
  ,
  Gladstone = c(
    "Labor",
    "Glenn ButcherGlenn Butcher",
    "18,089",
    "71.1%",
    "+8.1%",
    "One Nation",
    "Amy LohseAmy Lohse",
    "7,342",
    "28.9%",
    "-8.1%"
  )
  ,
  `Glass House` = c(
    "LNP",
    "Andrew PowellAndrew Powell",
    "14,968",
    "52.7%",
    "+1.8%",
    "Labor",
    "Brent HampsteadBrent Hampstead",
    "13,432",
    "47.3%",
    "-1.8%"
  ),
  `Pine Rivers` = c(
    "Labor",
    "Nikki BoydNikki Boyd",
    "18,106",
    "55.8%",
    "+1.6%",
    "LNP",
    "Chris ThompsonChris Thompson",
    "14,356",
    "44.2%",
    "-1.6%"
  )
  ,
  `South Brisbane` = c(
    "Labor",
    "Jackie TradJackie Trad",
    "15,100",
    "54.7%",
    "-9.1%",
    "Greens",
    "Amy MacMahonAmy MacMahon",
    "12,498",
    "45.3%",
    "+9.1%"
  )
  ,
  Maiwar = c(
    "Greens",
    "Michael BerkmanMichael Berkman",
    "15,889",
    "52.3%",
    "+5.3%",
    "LNP",
    "Scott EmersonScott Emerson",
    "14,471",
    "47.7%",
    "-5.3%"
  ),
  Noosa = c(
    "Independent",
    "Sandy Bolton",
    "17,574",
    "61.7%",
    "+18.5%",
    "LNP",
    "Glen ElmesGlen Elmes",
    "10,921",
    "38.3%",
    "-18.5%"
  ),
  Nudgee = c(
    "Labor",
    "Leanne LinardLeanne Linard",
    "19,122",
    "64.9%",
    "+5.7%",
    "LNP",
    "Debbie GlazeDebbie Glaze",
    "10,336",
    "35.1%",
    "-5.7%"
  )
  ,
  Oodgeroo = c(
    "LNP",
    "Mark RobinsonMark Robinson",
    "15,817",
    "57.1%",
    "+1.4%",
    "Labor",
    "Tony AustinTony Austin",
    "11,878",
    "42.9%",
    "-1.4%"
  )

  
) %>%
  map_df(~ convert_abc(.x), .id = "Electorate")

library(purrr)
party_key <- list(LNP = "LNP",
                  Labor = "ALP",
                  Greens = "GRN",
                  `One Nation` = "ONP",
                  `Katter's Australia` = "KAP",
                  Independent = "IND") %>%
  map(~ data.frame(Party = .x)) %>%
  bind_rows(.id = "Party_ABC") %>%
  mutate_all(parse_character)

ABC_2CP <- inner_join(ABC_2CP, party_key) %>%
  group_by(Electorate) %>%
  arrange(Electorate, Percent) %>%
  mutate(group = 1:n())

write_csv(ABC_2CP, "Data/2CP_votes.csv")