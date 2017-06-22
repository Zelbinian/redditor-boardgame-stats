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
        
        paste0("Request for '",request,"' encountered a ",status," on try ",try,".") %>% print()
        
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

getMembers <- function(guild_id, page) {
    
    paste("Requesting member page",page) %>% print()
    
    paste0("guild?id=", guild_id, "&members=1&page=", page) %>% # build the request string for the API
        queryBGG()                  %>% # make the request to get the XML
        read_xml()                  %>% # read it in
        xml_find_all("/guild/members/member/@name") %>%  # find them
        return()                        # return them
    
}

retrieveAllUserNames <- function(guild_id) {
  
  # initializing helper variables
  page <- 0
  username_list <- vector(mode = "character")
  
  # the API results are paginated, so we need to do this for every page
  repeat {
    
    # grabbing the next page of members
    page <- page + 1
    members <- getMembers(guild_id, page)
    
    # when we get past the end of the list, the XML returned by the API no longer has
    # any "member" elements, so that's how the script can tell when we are done
    if ( length(members) == 0 ) break
    
    # usernames are in the "name" attribute inside each member node
    # xml_attr pries them out and we append that vector to the existing one
    members %>% xml_text() %>% c(username_list, .) -> username_list
    
    # to prevent from throttling the server
    Sys.sleep(sleeptime__)
  }
  
  # if the while loop has ended, we have all of our usernames
  return(username_list)
  
}

getRatedGames <- function(username) {
  
    # stitch together the url for the api request
    paste0("collection?",                               # api path for collection info
           "username=",gsub(" ", "%20", username),      # for this user (sanitized string)
           "&rated=1",                                  # only rated games
           "&stats=1",                                  # full stats (including ratings)
           "&excludesubtype=boardgameexpansion") %>%    # exclude expansions
        queryBGG() %>%
        read_xml() -> collection
  
  # sometimes the response will either be an error or the user has rated no games
  if (xml_find_all(collection, "//item") %>% length() == 0) {
      
      # in this case, throw a warning to the console...
      warning(paste("The collection of rated games for user",
                    username,
                    "could not be found."))
      
      # .. and return an empty vector
      return(NULL)
  }
  
  #
  
  return(collection)
                    
}

getGuildsRatedGames <- function(guild_usernames) {
    
    games_list <- data.frame(ID = integer(0), MemberRating = numeric(0))
    
    # we want to get the game ratings for each user in the guild
    for (i in 1:length(guild_usernames)) {
        
        paste0("Retrieving collection for user ",i,"/",length(guild_usernames)) %>% print()
        
        # call a helper function to actually get the games for this user
        users_rated_games <- getRatedGames(guild_usernames[i])
        
        # sometimes a user hasn't actually rated anything, so checking for that.
        if ( !is.null(users_rated_games) ) {
            
            # if this user has rated games, use xpath to add the ids and ratings
            
            id <- users_rated_games %>% 
                xml_find_all("/items/item/@objectid") %>% xml_text() %>% as.integer()
            member_rating <- users_rated_games %>% 
                xml_find_all("/items/item/stats/rating/@value") %>% xml_text() %>% as.numeric()
            
            # then we stitch these vectors together into a data.frame
            # and attach it to the master data.frame
            games_list <- data.frame(ID = id, MemberRating = member_rating)   %>%
               rbind(games_list, .)  
        }
        
        # sleeping in order not to peg the server
        Sys.sleep(sleeptime__)
    }
    
    return(games_list)
}

pruneRatings <- function(ratings, guild_size) {
    
    num_ratings <- game_ratings %>%
        aggregate(MemberRating ~ ID,
                  data = .,
                  FUN = length)
    
    avg_game_ratings$NumRatings <- num_ratings[,2]
    
    # threshold shall be 5 or 1% of guild_size, whichever is larger
    percentage <- guild_size * .025
    threshold <- ifelse(percentage > 5, percentage, 5)
    
    table_ratings <- table(ratings)
    to_keep <- table_ratings[rowSums(table_ratings) >= threshold,]
    
    return(ratings[ratings$ID %in% rownames(to_keep),])
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
        cat(gameline, sep = "|", file = mdfile, append = TRUE)
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
        cat(gameline, sep = "|", file = mdfile, append = TRUE)
        cat("\n", file = mdfile, append = TRUE)
                      
    }
}

####################################################################################
# STEP 1: Get the usernames of each member in the Redditors guild on BGG
####################################################################################

# 1290 is the guild ID, which the function uses to query the BGG API

usernames_to_process <- retrieveAllUserNames(1290)

####################################################################################
# STEP 2: Get each member's collection of rated games (and the ratings for them, too)
####################################################################################

# apply or any of the other usual tricks for looping in R won't work because the processing
# is very complicated, so to grab the list of rated games (and their ratings) for
# each guild member we have to do it the old fashioned way.


game_ratings <- data.frame(ID = integer(0), MemberRating = numeric(0))
guild_size <- length(usernames_to_process)
loops <- 0
empty_collections <- 0

while(length(usernames_to_process) > 0) {
  # some simple setup
  num_usernames <- length(usernames_to_process)
  paste("Usernames in queue: ", num_usernames) %>% print()
  loops <- loops + 1
  paste("Total Operations:",loops) %>% print()
  
  # pop
  cur_username <- usernames_to_process[1]
  if (num_usernames > 1) {
    usernames_to_process <- usernames_to_process[2:num_usernames]
  } else {
    usernames_to_process <- NULL
  }
  
  # make the request
  # stitch together the url for the api request
  paste0("Retrieving collection for ",cur_username) %>% print()
  request <- paste0("https://www.boardgamegeek.com/xmlapi2/collection?", # api path for collection info
         "username=",gsub(" ", "%20", cur_username),          # for this user (sanitized string)
         "&rated=1",                                          # only rated games
         "&stats=1",                                          # full stats (including ratings)
         "&excludesubtype=boardgameexpansion")                # exclude expansions
  response <- GET(request)
  
  # if the request comes back anything other than a 200, push in the back of the queue
  
  if (response$status_code != 200) {
    # in this case, push the username back onto the queue
    paste("Got a",response$status_code,"for",cur_username,", adding back into the queue.") %>% print()
    usernames_to_process <- c(usernames_to_process, cur_username)
  } else {
  
    collection <- read_xml(response$content)
    # then evaluate the request to see if it's what ya need
    
    # is there anything in this collection?
    if (xml_find_all(collection, "//item") %>% length() == 0) {
        empty_collections <- empty_collections + 1
        
    # if so, extract the info
    } else {
      id <- collection %>% 
        xml_find_all("/items/item/@objectid") %>% xml_text() %>% as.integer()
      member_rating <- collection %>% 
        xml_find_all("/items/item/stats/rating/@value") %>% xml_text() %>% as.numeric()
      
      # then we stitch these vectors together into a data.frame
      # and attach it to the master data.frame
      game_ratings <- data.frame(ID = id, MemberRating = member_rating)   %>%
        rbind(game_ratings, .)  
    }
  }
  Sys.sleep(sleeptime__)
}

# Games that have only been rated by a few people skew the data, so also pruning

game_ratings <- pruneRatings(game_ratings, guild_size)

# storing the number of total ratings for stat tracking

total_ratings <- nrow(game_ratings)

####################################################################################
# STEP 3: Aggregate the ratings
####################################################################################

# Not as complex as it looks. What this says is: "Aggregate the MemberRatings across
# unique IDs, and do so with this anonymous function. The anonymous function takes
# the mean of the ratings, rounds that mean to 3 significant digits, then returns the
# value.

avg_game_ratings <- game_ratings %>%
    aggregate(MemberRating ~ ID,
              data = .,
              FUN = . %>% mean() %>% round(3) %>% return())


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
rm(avg_game_ratings, game_ratings, usernames_to_process, sleeptime__)

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
rownames(game_list_df) <- c(1:nrow(game_list_df))

# exporting "top xx" lists
game_list_df %>% exportTop100(paste0("top100-",today()))

game_list_df %>% exportTop10(paste0("top10-",today()))
