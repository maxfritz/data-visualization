# left merge on a ranking set (with right table: player ids) to provide full player data with rankings
# default return range is 1..1000 unless params specify
rankings_full_info <- function(date_sel, rank_high, rank_low) {
    if(missing(rank_high) & missing(rank_low)) {
      rank_high = 1
      rank_low = 1000
    } else if(missing(rank_low)){
      rank_low = 1000
    } else if(missing(rank_high)){
      rank_high=1
    }
  
  rankings00s%>%
    filter(date==date_sel)%>%
    filter(rank<=rank_low & rank>=rank_high)%>%
    merge(,y = players, by.x = "player_id", by.y="id", all.x = TRUE)%>%
    arrange(rank)
}

rankings_full_info(20000110)
rankings_full_info(20000110, 1,10)
rankings_full_info(20000110, 500)