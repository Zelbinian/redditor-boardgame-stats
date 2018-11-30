# This first goal of this script is to:
# - get the usernames of all the members of the Redditors guild on bgg
# - get the collection of each member
# - compute the average rating for each game for each member
# - get all the extra details of each game you can
# - create a data.frame that holds all this data
# - clean data
# - be able to filter this data.frame based on criteria

####################################################################################
# SCRIPT SETUP
####################################################################################

# Requiring the xml2 package because this is what the BoardGameGeek API returns

require("xml2")
require("httr")
require("magrittr")
require("lubridate")

sleeptime__ <- 5 # global setting for how long to sleep between API calls
bggApiUrl <- "https://www.boardgamegeek.com/xmlapi2/"
ua <- "httr | https://github.com/Zelbinian/redditor-boardgame-stats"
dummy_votes <- 50 # the number of dummy votes to add to each rating

####################################################################################
# FUNCTION REPOSITORY
# Because scripts run top to bottom, functions need to be defined first before the 
# script runs.
####################################################################################

# The BGG API/server is wonky enough that sometimes queries fail unexpectedly. This should
# protect against that.

queryBGG <- function(request) {
    
    response <- GET(paste0("https://www.boardgamegeek.com/xmlapi2/",request))
    
    if (response$status_code >= 500) {
        
        paste0("Request for '",request,"' encountered a ",response$status_code," on try ",try,".") %>% print()
        
        # If we get a 502 or 504, that means the server is pegged and/or
        # we've been rate limited. In this case, sit and wait a good long while
        # before making another attempt.
        
        Sys.sleep(60)
        
        paste("BGG returned status", response$status_code, "for query:\n", request) %>% 
            warning()
        
        # try again
        response <- GET(request)
    }
    
    return(response$content)
    
}

# This does the real work to retrieve the usernames from the guild information.
# It's wrapped with retrieveAllUserNames because the data is paginated, so getMemberNodes
# extracts the data from a single page while retrieveAllUserNames feeds it a page at a
# time.

getMembers <- function(guild_id, page = 1) {
  
  cat("Requesting member page",page,"\n")
  
  bggResp <- RETRY(verb = "GET", 
                   url = paste0(bggApiUrl,"guild"),
                   query = list(id = guild_id, page = page, members = 1), 
                   user_agent(ua),
                   body = FALSE,
                   pause_min = sleeptime__,
                   times = 5)
  
  stop_for_status(bggResp, "retrieve usernames from BGG. Investigate and try again later.")
  
  members <- content(bggResp) %>% xml_find_all("/guild/members/member/@name") %>% xml_text()
  
  if(length(members) == 0 || length(members < 10)) {
    return(members)
  } else {
    return(c(members, getMembers(guild_id, page + 1)))
  }
    
}

getMemberRatings <- function(member) {
  
  # make the request
  bggResp <- GET(url = paste0(bggApiUrl,"collection"),
                 query = list(username = member, rated = 1, stats = 1,
                              excludesubtype= "boardgameexpansion"), 
                 user_agent(ua),
                 body = FALSE)
  
  if(bggResp$status_code != 200) {
    return(NULL)
  } else {
    return(items = content(bggResp) %>% xml_find_all("//item"))
  }
  
}

assembleGameDataFile <- function(game_ratings) {
    
    # some helper variables for the while loop
    num_games <- length(game_ratings$ID)       # quick access to length of the list
    start_id <- 1                       # <
    end_id <- slice_size <- 200         # size of batch / query
                      
    game_data <- data.frame(Name = character(0), # master data.frame, empty to start
                            Year = integer(0),
                            BGGRating = numeric(0),
                            BGGRank = integer(0),
                            Weight = numeric(0),
                            MinPlayers = integer(0),
                            MaxPlayers = integer(0),
                            MinTime = integer(0),
                            MaxTime = integer(0),
                            MinAge = integer(0),
                            CopiesOwned = integer(0))  
    
    # The while loop lets us batch the requests so we don't have to do this one game
    # at a time.
    
    while (start_id <= num_games) {
        
        paste0("Retrieving info for game ",start_id,"/",num_games) %>% print()
        
        if (end_id > num_games) end_id <- num_games # preventing reading past the end
        
        # get a comma-delimited list of games for this batch
        paste0(game_ratings$ID[start_id:end_id], collapse = ",") %>%
        # use it to retrieve the xml for this particular batch of games
        paste0("thing?id=",
               .,  # the previous call is being passed here
               "&stats=1")  %>%
            queryBGG()      %>%
            read_xml()      ->
            games_batch
        
        # run the xpath for each piece of data we want
        names <- games_batch %>% 
            xml_find_all("/items/item/name[@type='primary']/@value") %>% 
            xml_text()
        years <- games_batch %>% 
            xml_find_all("/items/item/yearpublished/@value") %>% 
            xml_text() %>% as.integer()
        bgg_ratings <- games_batch %>% 
            xml_find_all("/items/item/statistics/ratings/average/@value") %>% 
            xml_text() %>% as.numeric()
        bgg_ranks <- games_batch %>% 
            xml_find_all("/items/item/statistics/ratings/ranks/rank[@name='boardgame']/@value") %>% 
            xml_text() %>% as.integer()
        weights <- games_batch %>% 
            xml_find_all("/items/item/statistics/ratings/averageweight/@value") %>% 
            xml_text() %>% as.numeric()
        min_players <- games_batch %>% 
            xml_find_all("/items/item/minplayers/@value") %>% 
            xml_text() %>% as.integer()
        max_players <- games_batch %>% 
            xml_find_all("/items/item/maxplayers/@value") %>% 
            xml_text() %>% as.integer()
        min_times <- games_batch %>% 
            xml_find_all("/items/item/minplaytime/@value") %>% 
            xml_text() %>% as.integer()
        max_times <- games_batch %>% 
            xml_find_all("/items/item/maxplaytime/@value") %>% 
            xml_text() %>% as.integer()
        min_ages <- games_batch %>% 
            xml_find_all("/items/item/minage/@value") %>% 
            xml_text() %>% as.integer()
        copies_owned <- games_batch %>% 
            xml_find_all("/items/item/statistics/ratings/owned/@value") %>% 
            xml_text() %>% as.integer()
        
        # create a new, temp data.frame that represents this batch and add the rows
        # to the master data frame
        game_data <- rbind(game_data,
              data.frame(Name = names,
                         Year = years,
                         BGGRating = bgg_ratings,
                         BGGRank = bgg_ranks,
                         Weight = weights,
                         MinPlayers = min_players,
                         MaxPlayers = max_players,
                         MinTime = min_times,
                         MaxTime = max_times,
                         MinAge = min_ages,
                         CopiesOwned = copies_owned))
        
        # move the chains for the next batch
        start_id <- start_id + slice_size
        end_id <- end_id + slice_size
        
        # being nice to the poor servers by throttling the requests
        Sys.sleep(sleeptime__)
    }
    
    # add ID and MemberRating columns to the data.frame
    game_data$ID <- game_ratings$ID
    game_data$MemberRating <- game_ratings$MemberRating
    game_data$NumRatings <- game_ratings$NumRatings
    
    # return the data.frame with columns in the expected order
    
    return(game_data)
}

calcRankChange <- function(game, prev_game_ratings) {
    
    if (game$ID %in% prev_game_ratings$ID) {
        # the game is on the previous list so let's extract the row from the 
        # previous list for processing and figure out the difference in rank
        
        prev_rank <- prev_game_ratings[prev_game_ratings$ID == game$ID,]$Rank
        rank_diff <- as.integer(game$Rank) - as.integer(prev_rank)
        
        # at this point there are only three possiblities (remembering that lower 
        # Rank is better):
        # 1. the game is in the same spot as it was before
        # 2. the game moved up some spots
        # 3. the game moved down some spots 
        
        if (rank_diff) { #if it's anything but zero, it'll be "true"
            if (rank_diff > 0) {
                # the game has a WORSE rating than it did before
                return(paste0("*▼",rank_diff,"*"))
            } else {
                return(paste0("**▲",abs(rank_diff),"**"))
            }
            
        } else {
            return("--")
        }
        
    } else {
        
        # if this game is NOT in the previous list, it's a new entry
        return("~~★~~")
    }
    
}

exportTop100 <- function(cur_game_ratings, prev_game_ratings, filename) {
    
    # first, write cur_game_ratings as a CSV
    write.csv(cur_game_ratings, paste0(filename, ".csv"))
    
    mdfile <- paste0(filename, ".md")
    
    cat("Rank|Game|Sub Rating|+/-|# Ratings|BGG Rating|BGG Rank|BGG Weight\n",
        file = mdfile,
        append = FALSE)
    cat(":-|:-|:-|:-|:-|:-|:-|:-\n",
        file = mdfile,
        append = TRUE)
    
    for (i in 1:100) {
        gameline <- c(i,
                      paste0("[",cur_game_ratings[i,]$Name,
                             "](http://www.boardgamegeek.com/boardgame/",
                             cur_game_ratings[i,]$ID,") (",cur_game_ratings[i,]$Year,")"),
                      cur_game_ratings[i,]$MemberRating,
                      calcRankChange(cur_game_ratings[i,], prev_game_ratings),
                      cur_game_ratings[i,]$NumRatings,
                      cur_game_ratings[i,]$BGGRating,
                      cur_game_ratings[i,]$BGGRank,
                      cur_game_ratings[i,]$Weight)
        cat(iconv(gameline, to = "UTF-8"), sep = "|", file = mdfile, append = TRUE)
        cat("\n", file = mdfile, append = TRUE)
        
    }
    
}

exportTop10 <- function(cur_game_ratings, prev_game_ratings, filename) {
    mdfile <- paste0(filename, ".md")
    
    cat("Rank|Game|Rating|+/-\n",
        file = mdfile,
        append = FALSE)
    cat("|--:|:-----------|---------------:|--:\n",
        file = mdfile,
        append = TRUE)
    
    for (i in 1:10) {
        gameline <- c(i,
                      paste0("[",cur_game_ratings[i,]$Name,
                             "](http://www.boardgamegeek.com/boardgame/",
                             cur_game_ratings[i,]$ID,")"),
                      cur_game_ratings[i,]$MemberRating,
                      calcRankChange(cur_game_ratings[i,], prev_game_ratings))
        cat(iconv(gameline, to = "UTF-8"), sep = "|", file = mdfile, append = TRUE)
        cat("\n", file = mdfile, append = TRUE)
                      
    }
}

####################################################################################
# STEP 1: Get the usernames of each member in the Redditors guild on BGG
####################################################################################

# 1290 is the guild ID, which the function uses to query the BGG API

members <- getMembers(1290)

####################################################################################
# STEP 2: Get each member's collection of rated games (and the ratings for them, too)
####################################################################################

# storing some descriptive statistics
guildSize <- length(members)
emptyCollections <- 0

# vectors of information to extract, starting empty
ids <- character()
ratings <- numeric()

# from here on in we treat the members variable like a queue. We pop one off, process it,
# and perhaps put it back on if processing it didn't work out right away

while(!is.null(members)) {
  
  # pop
  curMember <- members[1]
  if (length(members) > 1) {
    members <- members[2:length(members)]
  } else {
    members <- NULL
  }
  
  items <- getMemberRatings(curMember)
  
  # If getMemberRatings returned null, it means BGG hasn't finished processing the member's collection yet
  # re-queue the member but... not all the way back. Just far back enough to give BGG a bit more time,
  # but not far back enough that the member may have time to change their collection again
  if(is.null(items)) {
    members %<>% append(curMember, 4) # append already checks to see if the index is larger than the length of the vector
    
  } else {
    
    # if the list is empty, increment the number of empty collections
    if(length(items) == 0) {
      emptyCollections <- emptyCollections + 1
      
    } else { # if not, extract the information and save it
      ids <- c(ids, items %>% xml_find_all("//@objectid") %>% xml_text())
      ratings <- c(ratings, items %>% xml_find_all("//stats/rating/@value") %>% xml_double())
    }
  }
  
  # sleep to avoid getting rate limited
  Sys.sleep(sleeptime__)
}

####################################################################################
# STEP 3: Aggregate the ratings and prune the list
####################################################################################

# Not as complex as it looks. What this says is: "Aggregate the MemberRatings across
# unique IDs, and do so with this anonymous function. The anonymous function takes
# the mean of the ratings, rounds that mean to 3 significant digits, then returns the
# value.

avg_game_ratings <- game_ratings %>%
    aggregate(MemberRating ~ ID,
              data = .,
              FUN = . %>% mean() %>% round(3) %>% return())

# making use of the table function to figure out # of ratings for each game 
avg_game_ratings$NumRatings <- table(game_ratings) %>% rowSums()

# pruning list so games with low numbers of ratings don't skew the data
# The arbitrary threshold: 7.5% of the game with the most ratings

avg_game_ratings <- avg_game_ratings[avg_game_ratings$NumRatings >= dummy_votes,]

# storing the number of total ratings for stat tracking

total_ratings <- avg_game_ratings$NumRatings %>% sum

# modifying the ratings with some pseudo-Bayesian averaging

avg_game_ratings$MemberRating <- ((avg_game_ratings$MemberRating * avg_game_ratings$NumRatings + dummy_votes * 5.5) 
                                  / (avg_game_ratings$NumRatings + dummy_votes)) %>% round(3)

####################################################################################
# STEP 4: Gather additional details about each game by id and build the final df
####################################################################################

# The additional needed data:
#     - Name
#     - BGG Rating
#     - Min Playtime
#     - Max Playtime
#     - Game Weight
#     - Min Recommended Age
#     - Min Players
#     - Max Players
#     - Year Released
#     - BGG Rank
#     - Copies Owned

# look up the game data using the ids we've gathered
# then parse that data to build the final games list with all the things!
game_list_df <- assembleGameDataFile(avg_game_ratings)

####################################################################################
# STEP 5: Clean up unneeded variables.
####################################################################################
rm(avg_game_ratings, game_ratings, members, sleeptime__, table_ratings,
   collection, cur_username, id, member_rating, numMembers, request, response)

####################################################################################
# STEP 6: Look for inconsistencies in the data and cleaning them up
####################################################################################

# inserting true NAs
game_list_df[game_list_df == "NA"] <- NA
game_list_df$Year[game_list_df$Year == 0] <- NA
game_list_df$MinAge[game_list_df$MinAge == 0] <- NA

# the weights are to a crazy number of digits so we'll round those a bit

game_list_df$Weight <- game_list_df$Weight %>% round(2)

####################################################################################
# STEP 7: Export highest ranked games to top100 and top 10 files (diff formats)
####################################################################################

# sorting the list by member rating, decending order
game_list_df <- game_list_df[with(game_list_df, order(-MemberRating)),]
# adding a rank column 
game_list_df$Rank <- c(1:nrow(game_list_df))

# exporting "top xx" lists
#game_list_df %>% exportTop100(read.csv("top100-2018-01-31.csv"),paste0("top100-",today()))

#game_list_df %>% exportTop10(read.csv("top100-2018-01-31.csv"),paste0("top10-",today()))