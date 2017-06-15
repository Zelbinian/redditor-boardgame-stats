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

sleeptime__ <- 5 # global setting for how long to sleep between API calls

####################################################################################
# FUNCTION REPOSITORY
# Because scripts run top to bottom, functions need to be defined first before the 
# script runs.
####################################################################################

# The BGG API/server is wonky enough that sometimes queries fail unexpectedly. This should
# protect against that.

queryBGG <- function(request, tries = 15) {
    try <- 1
    
    repeat {
        
        if (try > tries) {
            http_status(reponse)
            paste0("BGG cannot currently execute the query:\n", request,
                   "\nPlease check server status.") %>% stop()
        }
        
        response <- GET(request)
        status <- response$status_code
        
        if (status == 200) {
            return(response$content)
        }
        else if (status == 202) { 
            
            paste0("Request for '",request,"' encountered a 202 on try ",try,".") %>% print()
            
            Sys.sleep(sleeptime__ * try)
            
            # for 202s, we'll try infinitely. It WILL work, it just might take a bit.
            try <- ifelse(try == tries, tries, try + 1)
            
        } else if (status >= 500) {
            
            paste0("Request for '",request,"' encountered a ",status," on try ",try,".") %>% print()
            
            # If we get a 502 or 504, that means the server is pegged and/or
            # we've been rate limited. In this case, sit and wait a good long while
            # before making another attempt.
            
            Sys.sleep(60)
            
            paste("BGG returned status", response$status_code, "for query:\n", request) %>% 
                warning()
            
            try <- try + 1
        }
        
    }
}

# This does the real work to retrieve the usernames from the guild information.
# It's wrapped with retrieveAllUserNames because the data is paginated, so getMemberNodes
# extracts the data from a single page while retrieveAllUserNames feeds it a page at a
# time.

getMembers <- function(req_url, page) {
    
    paste("Requesting member page",page) %>% print()
    
    paste0(req_url, "&page=", page) %>% # build the request string for the API
        queryBGG()                  %>% # make the request to get the XML
        read_xml()                  %>% # read it in
        xml_find_all("/guild/members/member/@name") %>%  # find them
        return()                        # return them
    
}

retrieveAllUserNames <- function(req_url) {
  
  # initializing helper variables
  page <- 0
  username_list <- vector(mode = "character")
  
  # the API results are paginated, so we need to do this for every page
  repeat {
    
    # grabbing the next page of members
    page <- page + 1
    members <- getMembers(req_url, page)
    
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
    paste0("https://www.boardgamegeek.com/xmlapi2/collection?", # api path for collection info
           "username=",gsub(" ", "%20", username),              # for this user (sanitized string)
           "&rated=1",                                          # only rated games
           "&stats=1",                                          # full stats (including ratings)
           "&excludesubtype=boardgameexpansion") %>%            # exclude expansions
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
    
    # threshold shall be 5 or 1% of guild_size, whichever is larger
    one_percent <- guild_size * .01
    threshold <- ifelse(one_percent > 5, one_percent, 5)
    
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
        paste0("https://www.boardgamegeek.com/xmlapi2/thing?id=",
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
guild_data_url <- "https://www.boardgamegeek.com/xmlapi2/guild?id=138&members=1"

guild_usernames <- retrieveAllUserNames(guild_data_url)

####################################################################################
# STEP 2: Get each member's collection of rated games (and the ratings for them, too)
####################################################################################

# apply or any of the other usual tricks for looping in R won't work because guild_usernames
# is just a simple vector, so to grab the list of rated games (and their ratings) for
# each guild member we have to do it the old fashioned way.
# 
# Games that have only been rated by a few people skew the data, so also pruning

guild_username_sections <- split(guild_usernames, ceiling(seq_along(guild_usernames)/10))

game_ratings <- data.frame(ID = integer(0), MemberRating = numeric(0))

for (usernames in guild_username_sections) {
  game_ratings <- usernames %>% getGuildsRatedGames() %>% rbind(game_ratings, .)
}

# game_ratings <- guild_usernames %>%
#     getGuildsRatedGames()       %>%
  
game_ratings <- pruneRatings(game_ratings, length(guild_usernames))

# Some memory optimization
rm(guild_usernames)

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

# Some memory optimization
paste("Total game ratings for group:",nrow(game_ratings)) %>% print()
rm(game_ratings)

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


