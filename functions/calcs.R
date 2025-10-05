### Common Councilytics Functions

YIMBYpropCalc <- function(df){
  # Calculate the proportion of YIMBY votes by alderman
  # Input: df - a data frame with columns Date, Minutes url, Legistar, Legistar url, Alder, Aldermanic District, YIMBY Vote
  # Output: a data frame with columns Alder, Aldermanic District, n Votes, % YIMBY
  
  library(dplyr)
  
    df |>
    group_by(Date, `Minutes url` ,Legistar,`Legistar url`,  Alder, `Aldermanic District`) |>
    summarize(`YIMBY Vote` = mean(`YIMBY Vote`, na.rm = TRUE), 
              `n Votes` = n()) |>
    group_by(Alder, `Aldermanic District`) |>
    summarize(`n Votes` = sum(`n Votes`, na.rm = TRUE),
              `YIMBY prop` = mean(`YIMBY Vote`, na.rm = TRUE) ) |>
    arrange(desc(`YIMBY prop`))  |>
    mutate(`% YIMBY` = round(100 * `YIMBY prop`, 1)) |>
    select(-`YIMBY prop`) 
  
  
}