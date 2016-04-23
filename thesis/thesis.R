getStatistics <- function(matches){
  features = c("home_goals","away_goals"               
            ,"home_goals_half_time","away_goals_half_time","home_shots"               
            ,"away_shots","home_shots_on_target","away_shots_on_target"     
            ,"home_corners","away_corners","home_fouls"               
            ,"away_fouls","home_yellows","away_yellows"             
            ,"home_reds","away_reds","home_shots_outside_target"
            ,"away_shots_outside_target", "home_pos", "away_pos")
  res = c()
  for(feature in features){
    min = min(matches[[feature]])
    max = max(matches[[feature]])
    mean = mean(matches[[feature]])
    sd = sd(matches[[feature]])
    median = median(matches[[feature]])
    
    line = paste(feature, min, mean, sd, median, max, sep = " & ")
    line = paste(line, " \\", sep = "")
    res = append(res, line)
    res = append(res,"\\hline")
  }
  res = paste(res, sep = "\n")
  print(res)
  
}