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

sleeptime__ <- 4 # global setting for how long to sleep between API calls

####################################################################################
# FUNCTION REPOSITORY
# Because scripts run top to bottom, functions need to be defined first before the 
# script runs.
####################################################################################

# This does the real work to retrieve the usernames from the guild information.
# It's wrapped with retrieveAllUserNames because the data is paginated, so getMemberNodes
# extracts the data from a single page while retrieveAllUserNames feeds it a page at a
# time.

getMemberNodes <- function(req_url, page) {
    
    # these two lines get the xml
    cur_req_url <- paste0(req_url, "&page=", page)
    inner_xml <- read_xml(cur_req_url)
    
    # and this returns the member nodes inside
    return(xml_find_all(inner_xml, "//member"))
    
}

retrieveAllUserNames <- function(req_url) {
  
  # initializing helper variables
  page <- 0
  username_list <- vector(mode = "character")
  
  # the API results are paginated, so we need to do this for every page
  repeat {
    
    # grabbing the next page of members
    page <- page + 1
    members <- getMemberNodes(req_url, page)
    
    # when we get past the end of the list, the XML returned by the API no longer has
    # any "member" elements, so that's how the script can tell when we are done
    if ( length(members) == 0 ) break
    
    # usernames are in the "name" attribute inside each member node
    # xml_attr pries them out and we append that vector to the existing one
    username_list <- c(username_list, 
                       xml_attr(members, "name"))
    
    # to prevent from throttling the server
    Sys.sleep(sleeptime__)
  }
  
  # if the while loop has ended, we have all of our usernames
  return(username_list)
  
}

getRatedGames <- function(username) {
  
  # stitch together the url for the api request
  request <- paste0("https://www.boardgamegeek.com/xmlapi2/collection?", # api path for collection info
                    "username=",gsub(" ", "%20", username),              # for this user (sanitized string)
                    "&rated=1",                                          # only rated games
                    "&stats=1",                                          # full stats (including ratings)
                    "&excludesubtype=boardgameexpansion")                # exclude expansions
  
  # The API works a little weirdly. Sometimes you have to wait for the server to
  # retrieve the result. Unfortunately you can't use a callback or a promise. If the
  # data is not ready yet, you get an http status of 202.
  
  response <- NULL
  
  repeat{
      response <- GET(request)
      
      # if we get a 200 we're good
      if (response$status_code == 200) break
      
      # protecting against some other thing going wrong
      if (response$status_code != 202) {
          
          # in this case, throw a warning to the console...
          warning(paste("The collection of rated games for user",
                        username,
                        "could not be found."))
          
          return(list())
      }
    
    # wait before re-requesting to minimize the number of times this loop will run
    Sys.sleep(sleeptime__) 
  }
  
  collection <- read_xml(response$content)
  
  # sometimes the response might actually be an error,
  if (length(xml_find_all(collection, "//error")) > 0) {
      
      # in this case, throw a warning to the console...
      warning(paste("The collection of rated games for user",
                    username,
                    "could not be found."))
      
      # .. and return an empty vector
      return(list())
  }
  
  return(xml_find_all(collection, "//item"))
                    
}

addUsersGames <- function(games, games_list) {
    
    # no need to process an empty list! will probably never happen, but just in case
    if ( length(games) == 0 ) return(games_list)
    
    # the "games" object is xml, so here we're extracting the values we need
    id <- as.integer(xml_text(xml_find_all(games, "@objectid")))
    member_rating <- as.numeric(xml_text(xml_find_all(games, "//rating/@value")))
    
    # then we stitch these vectors together into a data.frame..
    new_games_list <- data.frame(ID = id, MemberRating = member_rating)
    
    # and return a concatenation of both lists
    return (rbind(games_list, new_games_list))
    
}

getGuildsRatedGames <- function(guild_usernames) {
    
    games_list <- data.frame(ID = integer(0), MemberRating = numeric(0))
    
    # we want to get the game ratings for each user in the guild
    for (i in 1:length(guild_usernames)) {
        
        # call a helper function to actually get the games for this user
        users_rated_games <- getRatedGames(guild_usernames[i])
        
        # sometimes a user hasn't actually rated anything, so checking for that.
        if ( length(users_rated_games) > 0 ) {
            
            # if they have, addUsersGames concatenates the new list of ratings
            # with the old
            games_list <- addUsersGames(users_rated_games, games_list)
        }
        
        # sleeping in order not to peg the server
        Sys.sleep(sleeptime__)
    }
    
    return(games_list)
}

pruneRatings <- function(threshold = 5, ratings) {
    table_ratings <- table(ratings)
    to_keep <- table_ratings[rowSums(table_ratings) >= threshold,]
    
    return(ratings[ratings$ID %in% rownames(to_keep),])
}

assembleGameDataFile <- function(game_ratings) {
    
    # some helper variables for the while loop
    num_games <- length(game_ratings$ID)       # quick access to length of the list
    start_id <- 1                       # <
    end_id <- 400                       # 400 games / query
    slice_size <- 400                   # <
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
    
    # The while loop is cuz 400 is about the limit of game data the API can return at
    # one time without falling over, so we have to batch it.
    
    while (start_id <= num_games) {
        
        if (end_id > num_games) end_id <- num_games # preventing reading past the end
        
        # get a comma-delimited list of games for this batch
        ids <- paste0(game_ratings$ID[start_id:end_id], collapse = ",")
        
        # build the api query for this batch and store the results
        games_batch <- read_xml(paste0("https://www.boardgamegeek.com/xmlapi2/thing?id=",
                                       ids,
                                       "&stats=1"))
        
        # run the xpath for each piece of data we want
        names <- xml_text(xml_find_all(games_batch, "//name[@type='primary']/@value"))
        years <- as.integer(xml_text(xml_find_all(games_batch, "//yearpublished/@value")))
        bgg_ratings <- as.numeric(xml_text(xml_find_all(games_batch, "//ratings/average/@value")))
        bgg_ranks <- as.numeric(xml_text(xml_find_all(games_batch, "//rank[@name='boardgame']/@value")))
        weights <- as.numeric(xml_text(xml_find_all(games_batch, "//averageweight/@value")))
        min_players <- as.integer(xml_text(xml_find_all(games_batch, "//minplayers/@value")))
        max_players <- as.integer(xml_text(xml_find_all(games_batch, "//maxplayers/@value")))
        min_times <- as.integer(xml_text(xml_find_all(games_batch, "//minplaytime/@value")))
        max_times <- as.integer(xml_text(xml_find_all(games_batch, "//maxplaytime/@value")))
        min_ages <- as.integer(xml_text(xml_find_all(games_batch, "//minage/@value")))
        copies_owned <- as.integer(xml_text(xml_find_all(games_batch, "//owned/@value")))
        
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
    
    # return the data.frame with columns in the expected order
    
    return(game_data[c("ID","Name","Year","MemberRating","BGGRating","BGGRank","Weight",
                       "MinPlayers","MaxPlayers","MinTime","MaxTime","MinAge","CopiesOwned")])
}

####################################################################################
# STEP 1: Get the usernames of each member in the Redditors guild on BGG
####################################################################################

# The request url, at least initially. The results are paginated, so multiple requests
# will be necessary.

# real guild id = 1290

guild_data_url <- "https://www.boardgamegeek.com/xmlapi2/guild?id=805&members=1"
start_time <- Sys.time()
guild_usernames <- retrieveAllUserNames(guild_data_url)

####################################################################################
# STEP 2: Get each member's collection of rated games (and the ratings for them, too)
####################################################################################

# apply or any of the other usual tricks for looping in R won't work because guild_usernames
# is just a simple vector, so to grab the list of rated games (and their ratings) for
# each guild member we have to do it the old fashioned way.

game_ratings <- getGuildsRatedGames(guild_usernames)

# games that have only been rated by a few people skew the data, so pruning
# default threshold is 5
game_ratings <- pruneRatings(3, game_ratings)

####################################################################################
# STEP 3: Aggregate the ratings
####################################################################################

# Not as complex as it looks. What this says is: "Aggregate the MemberRatings across
# unique IDs, and do so with this anonymous function. The anonymous function takes
# the mean of the ratings, rounds that mean to 3 significant digits, then returns the
# value.

avg_game_ratings <- aggregate(MemberRating ~ ID, data = game_ratings,
                             FUN = function(ratings) {
                                   return(round(mean(ratings),3))
                                 })

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
end_time <- Sys.time()
####################################################################################
# STEP 5: Clean up unneeded variables.
####################################################################################
rm(avg_game_ratings, game_ratings, guild_usernames, sleeptime__)

####################################################################################
# STEP 6: Look for inconsistencies in the data and cleaning them up
####################################################################################

# inserting true NAs
game_list_df[game_list_df == "NA"] <- NA
game_list_df$Year[game_list_df$Year == 0] <- NA
game_list_df$MinAge[game_list_df$MinAge == 0] <- NA

# Some times max playtimes are not listed so they get reported as "0". A reasonable guess
# in these circumstances is to have the max playtime equal the min playtime.
# First step is to create a logical vector to tell us the offending rows.
lower_max_time <- game_list_df$MaxTime < game_list_df$MinTime

# Then, if there are rows for which this is true, update them
if (sum(lower_max_time) > 0) { # this means there are some rows where this condition holds

    # write this info to a file so we can submit corrections to BGG
    write.table(game_list_df[lower_max_time,],"badplaytimes.txt")

    game_list_df[lower_max_time,]$MaxTime <- game_list_df[lower_max_time,]$MinTime

}

# We'll do a similar thing with player counts
# Although the missing data replacement will be a bit more sophisticated in this case
# We start the same way, by obtaining a logical vector telling us the offending rows
lower_max_pcount <- game_list_df$MaxPlayers < game_list_df$MinPlayers

if (sum(lower_max_pcount) > 0) {

    # write this out to a file so we can submit corrections to BGG
    write.table(game_list_df[lower_max_pcount,],"badplayercounts.txt")

    # Here's what we're doing: for each entry, use it's min player count to find the median
    # MAX player count for that min player count and then update the max player count.
    # An example: Let's say game is 3 players min. Let's also say the median MAX player
    # count for 3 player games in our dataset is 5. That's what we'd set the max to for
    # the individual game. Crude, but effective.

    for (i in which(lower_max_pcount)) {
        min_play_count <- game_list_df[i,]$MinPlayers
        game_list_df[i,]$MaxPlayers <- median(
            game_list_df[game_list_df$MinPlayers == min_play_count,]$MaxPlayers)
    }

}


