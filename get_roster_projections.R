library(devtools)
#install_github("hkingsley-code/ffscrapr")
library(ffscrapr)
library(dplyr)
library(shiny)
library(gsheet)

# I hope to automate this at some point but for now.... just pulling from fangraphs into here
batting <- read.csv("zips_batting_04_05_2022.csv")
batting <- batting%>%
  mutate(points = H + X2B + X3B + HR + R +RBI + BB - SO + HBP + (2*SB)  )%>%
  select(Name, playerid, points)
# NO BS
pitching <- read.csv("zips_pitching_04_05_2022.csv")
pitching <- pitching%>%
  mutate(points = (IP * 3) + (3*W) + (-3 * L) + (7*SV) + (5*HLD) - H + (-2 * ER) - HR + SO - BB )%>%
  select(Name, playerid, points)

#group by for ohtani
projections <- batting%>%
  union(pitching)%>%
  group_by(Name, playerid)%>%
  summarise(points = sum(points))




## an id map that is maintained on smart fantasybaseball.com
id_map <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1JgczhD5VDQ1EiXqVG-blttZcVwbZd5_Ne_mefUGwJnk/pubhtml?gid=0&single=true')


con <- espn_connect(season = 2022, league_id = 85601 )
rosters <- ff_rosters(con)

map <- read.csv("espn_id_map.csv")
projections <- read.csv("zips_projections_3_23_2022.csv")


rosters <- rosters%>%
  left_join(id_map, by = c("player_id" = "ESPNID" ))%>%
  left_join(projections, by = c("IDFANGRAPHS" = "playerid"))%>%
  select(franchise_name, player_name, POS, points)

write.csv(rosters, "draft_results_3_28_2022.csv")





#ff_league(con)
#ff_standings(con)
#draft <- ff_draft(con)
#ff_transactions(con)



#con <- espn_connect(season = 2021, league_id = 2033051 )
#ff_league(con)

