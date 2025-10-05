### Functions for formatting

formatVotes <- function(df){
  # Format the votes data frame for display
  # Input: df - a data frame with columns Date, Minutes url, Legistar, Legistar url, Vote #, Short title, Alder, Aldermanic District, YIMBY Vote
  # Output: a data frame with columns Date, Legistar, Vote #, Short title, YIMBY, 1, 2, ..., 20
  # where 1-20 are the aldermanic districts and the values are the votes (Y, N, A, etc.)
  
  library(tidyr)
  library(kableExtra)
  library(dplyr)

  df %>%
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

}