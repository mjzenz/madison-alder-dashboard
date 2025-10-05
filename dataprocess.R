
library(googlesheets4)
library(tidyverse)
library(kableExtra)

## Import Alder vote data from google sheets
source("functions/calcs.R")
source("functions/format.R")
# Authenticate with Google
gs4_auth(email = "michael.zenz@gmail.com")
# Load data from Alder Votes sheet  at this link:
#vote.data.query.old <- read_sheet("1_zbvWvikBUMhwV0xVh4_bSQW4tCNdWBVeMP2m_vwWKA",
#                              sheet = "YIMBY Votes")
alder.data <- read_sheet("1_zbvWvikBUMhwV0xVh4_bSQW4tCNdWBVeMP2m_vwWKA",
                         sheet = "Alder Districts")

#Read in csv file Votes2025.csv
vote.data.query <- read_csv("website/Votes2025.csv")

#transform list of names seperated by a ; into vector
vote.data <- vote.data.query |>
 # filter(`Development Proposal`) |>
  mutate(Yes = strsplit(Y, ";"), 
         No = strsplit(N, ";"),
         Abstain = strsplit(as.character(ABS), ";")
  ) |>
  select(-Y, -N, -ABS) |>
  mutate(`Yes Votes` = lengths(Yes),
         `No Votes` = lengths(No),
         `Abstain Votes` = lengths(Abstain)) 


alder.votes.yes <- vote.data |>
  mutate(Vote = "Yes") |>
  select(Date,`Minutes url` ,Legistar,`Legistar url` , `Short title`, `Vote #`, YIMBY,Vote, Yes) |>
  #Make each item in the lists Yes, No, and Abstain into a row
  unnest_longer(Yes, values_to = "Alder")


alder.votes.no <- vote.data |>
  mutate(Vote = "No") |>
  select(Date,`Minutes url` ,Legistar,`Legistar url` ,`Short title`, `Vote #`, YIMBY,Vote, No) |>
  #Make each item in the lists Yes, No, and Abstain into a row
  unnest_longer(No, values_to = "Alder")


alder.votes.abs <- vote.data |>
  mutate(Vote = "No") |>
  select(Date,`Minutes url` ,Legistar,`Legistar url` ,`Short title`, `Vote #`, YIMBY,Vote, Abstain) |>
  #Make each item in the lists Yes, No, and Abstain into a row
  unnest_longer(Abstain, values_to = "Alder")

alder.votes <- bind_rows(alder.votes.yes, alder.votes.no, alder.votes.abs) |>
  mutate(Alder = str_trim(Alder),
         `YIMBY Vote` = ifelse(YIMBY, 
                               ifelse(Vote == "Yes", 
                                      TRUE, FALSE),
                               ifelse(Vote == "No",
                                      TRUE, FALSE))) |>
  group_by(Date, `Minutes url` ,Legistar,`Legistar url` , `Short title`, `Vote #`, YIMBY,Vote, Alder) |>
  summarise(`YIMBY Vote` = mean(`YIMBY Vote`)) |>
  filter(!is.na(Alder)) |>
  inner_join(alder.data, by = c("Alder" = "Name"))


## Alder Vote Percentages

YIMBY.prop <- alder.votes |>
  filter(`Development Proposal`) |>
  filter(is.na(`End Date`)) |>
  YIMBYpropCalc()

YIMBY.vote.list <- alder.votes %>%
  ungroup() %>%
  filter(`Development Proposal`) |>
  filter(is.na(`End Date`)) %>%
  formatVotes() 


  # arrange(Date, `Legistar`, `Vote #`) %>%
  # mutate(date = Date,
  #        Legistar = cell_spec(`Legistar`, "html", link = `Legistar url`),
  #        Date = cell_spec(format(Date, "%b %d %Y"), "html", link = `Minutes url`)) %>%
  # select(date,Date, Legistar,  `Vote #`,`Short title`, Vote, YIMBY, `Aldermanic District`) %>%
  # group_by(date,Date, Legistar,  `Vote #`, `Short title`,`Aldermanic District`) %>%
  # mutate(Vote = substr(Vote, 1, 1), 
  #        `Vote #` = ifelse(is.na(`Vote #`), 1, `Vote #`)) %>%
  # pivot_wider(names_from = `Aldermanic District`, values_from = `Vote`, values_fill = " ") %>%
  # select(date,Date, Legistar, `Vote #`, `Short Title` = `Short title`, YIMBY, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`,
  #        `11`, `12`, `13`, `14`, `15`, `16`,`17`, `18`, `19`,`20` ) %>%
  # ungroup() %>%
  # arrange(desc(date), Legistar, `Vote #`)

other.vote.list <- alder.votes %>%
  ungroup() %>%
  filter(!`Development Proposal`) |>
  filter(is.na(`End Date`)) |>
  formatVotes()

save.image(file="website/alder_data_processed.RData")


### Old Alder vote data file

## Import Alder vote data from google sheets
# Authenticate with Google
# Load data from Alder Votes sheet  at this link:
vote.data.query <- read_sheet("1_zbvWvikBUMhwV0xVh4_bSQW4tCNdWBVeMP2m_vwWKA",
                              sheet = "YIMBY Votes")

#transform list of names seperated by a ; into vector
vote.data <- vote.data.query |>
  filter(`Development Proposal`) |>
  mutate(Yes = strsplit(Y, ";"), 
         No = strsplit(N, ";"),
         Abstain = strsplit(as.character(ABS), ";")
  ) |>
  select(-Y, -N, -ABS) |>
  mutate(`Yes Votes` = lengths(Yes),
         `No Votes` = lengths(No),
         `Abstain Votes` = lengths(Abstain)) 


alder.votes.yes <- vote.data |>
  mutate(Vote = "Yes") |>
  select(Date,`Minutes url` ,Legistar,`Legistar url` , `Short title`, `Vote #`, YIMBY,Vote, Yes) |>
  #Make each item in the lists Yes, No, and Abstain into a row
  unnest_longer(Yes, values_to = "Alder")


alder.votes.no <- vote.data |>
  mutate(Vote = "No") |>
  select(Date,`Minutes url` ,Legistar,`Legistar url` ,`Short title`, `Vote #`, YIMBY,Vote, No) |>
  #Make each item in the lists Yes, No, and Abstain into a row
  unnest_longer(No, values_to = "Alder")


alder.votes.abs <- vote.data |>
  mutate(Vote = "No") |>
  select(Date,`Minutes url` ,Legistar,`Legistar url` ,`Short title`, `Vote #`, YIMBY,Vote, Abstain) |>
  #Make each item in the lists Yes, No, and Abstain into a row
  unnest_longer(Abstain, values_to = "Alder")

alder.votes <- bind_rows(alder.votes.yes, alder.votes.no, alder.votes.abs) |>
  mutate(Alder = str_trim(Alder),
         `YIMBY Vote` = ifelse(YIMBY, 
                               ifelse(Vote == "Yes", 
                                      TRUE, FALSE),
                               ifelse(Vote == "No",
                                      TRUE, FALSE))) |>
  group_by(Date, `Minutes url` ,Legistar,`Legistar url` , `Short title`, `Vote #`, YIMBY,Vote, Alder) |>
  summarise(`YIMBY Vote` = mean(`YIMBY Vote`)) |>
  filter(!is.na(Alder)) |>
  inner_join(alder.data, by = c("Alder" = "Name"))


## Alder Vote Percentages

YIMBY.prop <- alder.votes |>
  filter(is.na(`End Date`) | `End Date` < "2025-04-15") |>
  YIMBYpropCalc()

YIMBY.vote.list <- alder.votes %>%
  ungroup() %>%
  filter(is.na(`End Date`) | `End Date` < "2025-04-15") |>
  arrange(Date, `Legistar`, `Vote #`) %>%
  mutate(date = Date,
         Legistar = cell_spec(`Legistar`, "html", link = `Legistar url`),
         Date = cell_spec(format(Date, "%b %d %Y"), "html", link = `Minutes url`)) %>%
  select(date,Date, Legistar,  `Vote #`,`Short title`, Vote, YIMBY, `Aldermanic District`) %>%
  group_by(date,Date, Legistar,  `Vote #`, `Short title`,`Aldermanic District`) %>%
  mutate(Vote = substr(Vote, 1, 1), 
         `Vote #` = ifelse(is.na(`Vote #`), 1, `Vote #`)) %>%
  pivot_wider(names_from = `Aldermanic District`, values_from = `Vote`, values_fill = " ") %>%
  select(date,Date, Legistar, `Vote #`, `Short Title` = `Short title`, YIMBY, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`,
         `11`, `12`, `13`, `14`, `15`, `16`,`17`, `18`, `19`,`20` ) %>%
  ungroup() %>%
  arrange(desc(date), Legistar, `Vote #`)

save.image(file="website/alder_data_processed_old.RData")




