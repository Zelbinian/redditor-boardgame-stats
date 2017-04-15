# This first goal of this script is to:
# - get the usernames of all the members of the Redditors guild on bgg
# - get the collection of each member
# - compute the average rating for each game for each member
# - get all the extra details of each game you can
# - create a data.frame that holds all this data
# - be able to filter this data.frame based on criteria

####################################################################################
# SCRIPT SETUP
####################################################################################

# Requiring the xml2 package because this is what the BoardGameGeek API returns

require("xml2")
require("httr")
require("magrittr")

####################################################################################
# FUNCTION REPOSITORY
# Because scripts run top to bottom, functions need to be defined first before the 
# script runs.
####################################################################################

getMemberNodes <- function(req_url, page) {
  
  cur_req_url <- paste0(req_url, "&page=", page)
  inner_xml <- read_xml(cur_req_url)
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
    Sys.sleep(5)
  }
  
  # if the while loop has ended, we have all of our usernames
  return(username_list)
  
}

getRatedGames <- function(username) {
  
  # stitch together the url for the api request
  request <- paste0("https://www.boardgamegeek.com/xmlapi2/collection?", # api path for collection info
                    "username=",username,                                # for this user
                    "&rated=1",                                          # only rated games
                    "&stats=1",                                          # full stats (including ratings)
                    "&excludesubtype=boardgameexpansion")                # exclude expansions
  
  # The API works a little weirdly. Sometimes you have to wait for the server to
  # retrieve the result. Unfortunately you can't use a callback or a promise. If the
  # data is not ready yet, you get a <message> node telling you so. 
  
  repeat{
    collection <- read_xml(request)
    
    if (length(xml_find_all(collection, "//message")) == 0 ) {
      
      # sometimes the message warning might actually be an error, not a queuing response
      if (length(xml_find_all(collection, "//error")) > 0) {
        
        # in this case, throw a warning to the console...
        warning(paste("The collection of rated games for user",
                       username,
                       "could not be found."))
        
        # .. and return an empty vector
        return(list())
      }
      
      break
    }
    
    # wait before re-requesting to minimize the number of times this loop will run
    Sys.sleep(10) 
  }
  
  return(xml_find_all(collection, "//item"))
                    
}

addToGamesList <- function(game_nodes) {
    if ( length(game_nodes) == 0 ) return()
    
    
}



####################################################################################
# STEP 1: Get the usernames of each member in the Redditors guild on BGG
####################################################################################

# The request url, at least initially. The results are paginated, so multiple requests
# will be necessary.

# real guild id = 1290

guild_data_url <- "https://www.boardgamegeek.com/xmlapi2/guild?id=432&members=1"

guild_usernames <- retrieveAllUserNames(guild_data_url)

####################################################################################
# STEP 2: Get each member's collection and, for each game they have rated, put the 
# full details of that game in a data.frame
####################################################################################

games_list <- data.frame(ID = integer(0), Name = character(0), Ratings = numeric(0), 
                         GuildRating = numeric(0), BGGRating = numeric(0), 
                         MinPlayers = integer(0), MaxPlayers = integer(0), 
                         MinPlaytime = integer(0), MaxPlaytime = integer(0), 
                         CopiesOwned = integer(0), stringsAsFactors = FALSE)
