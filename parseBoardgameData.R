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

library(xml2)
library(httr)
library(magrittr)
library(lubridate)
library(dplyr)
library(tibble)
library(tcltk)

sleeptime__ <- 2 # global setting for how long to sleep between API calls
bggApiUrl <- "https://www.boardgamegeek.com/xmlapi2/"
ua <- "httr | https://github.com/Zelbinian/redditor-boardgame-stats"

####################################################################################
# FUNCTION REPOSITORY
# Because scripts run top to bottom, functions need to be defined first before the 
# script runs.
####################################################################################

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
  
  Sys.sleep(sleeptime__)
  
  if(length(members) == 0 || length(members) < 25) {
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

getGameData <- function(ratingData) { 
  
  bggResp <- RETRY(verb = "GET", 
                   url = paste0(bggApiUrl,"thing"),
                   query = list(stats = 1, id = paste0(ratingData$ID, collapse = ",")), 
                   user_agent(ua),
                   body = FALSE,
                   pause_min = sleeptime__,
                   times = 5)
  
  stop_for_status(bggResp, "retrieve game data. Try again later.")
  
  gameData <- content(bggResp)
        
  # run the xpath for each piece of data we want
  ratingData %<>% mutate(
      Name = gameData %>% 
          xml_find_all("/items/item/name[@type='primary']/@value") %>% 
          xml_text(),
      Year = gameData %>% 
          xml_find_all("/items/item/yearpublished/@value") %>% 
          xml_integer(),
      `BGG Rating` = gameData %>% 
          xml_find_all("/items/item/statistics/ratings/average/@value") %>% 
          xml_double() %>% round(3),
      `BGG Rank` = gameData %>% 
          xml_find_all("/items/item/statistics/ratings/ranks/rank[@name='boardgame']/@value") %>% 
          xml_integer(),
      Weight = gameData %>% 
          xml_find_all("/items/item/statistics/ratings/averageweight/@value") %>% 
          xml_double() %>% round(3))
    
    return(ratingData)
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

exportTop100 <- function(cur_game_ratings, prev_game_ratings, filename = paste0("top100-", today())) {
    
    # first, write cur_game_ratings as a CSV
    write.csv(cur_game_ratings, paste0(filename, ".csv"))
    
    outputFile <- file(paste0(filename,".txt"), open = "w+", encoding = "native.enc")
    
    writeLines(c("Rank|Game|Sub Rating|+/-|# Ratings|BGG Rating|BGG Rank|BGG Weight",
                 ":-|:-|:-|:-|:-|:-|:-|:-"),
               outputFile,
               useBytes = T)
    
    for (i in 1:100) {
        
        paste(i,
              paste0("[",cur_game_ratings[i,]$Name, 
                     "](http://www.boardgamegeek.com/boardgame/",
                     cur_game_ratings[i,]$ID,") (",cur_game_ratings[i,]$Year,")"),
              cur_game_ratings[i,]$`Average Rating`,
              calcRankChange(cur_game_ratings[i,], prev_game_ratings),
              cur_game_ratings[i,]$Ratings,
              cur_game_ratings[i,]$`BGG Rating`,
              cur_game_ratings[i,]$`BGG Rank`,
              cur_game_ratings[i,]$Weight,
              sep = "|") %>%
        writeLines(outputFile, useBytes = T)
        
    }
    
    close(outputFile)
    
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
                      cur_game_ratings[i,]$`Average Rating`,
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

# oh, and a little progress bar, how nice
pb <- tkProgressBar(min = 0, max = guildSize, width = 450, title = "Getting member ratings..", label = "0%")

while(!is.null(members)) {
  
  # pop
  curMember <- members[1]
  if (length(members) > 1) {
    members <- members[2:length(members)]
  } else {
    members <- NULL
  }
  
  index <- guildSize - length(members)
  
  # update progress bar 
  setTkProgressBar(pb, index, 
                   label = paste0(round((index/guildSize)*100,2),
                                 "% - processing: ",curMember))
  
  items <- getMemberRatings(curMember)
  
  # If getMemberRatings returned null, it means BGG hasn't finished processing the member's collection yet
  # re-queue the member but... not all the way back. Just far back enough to give BGG a bit more time,
  # but not far back enough that the member may have time to change their collection again
  if(is.null(items)) {
    members %<>% append(curMember, 4) # append already checks to see if the index is larger than the length of the vector
    
  } else {
    
    numItems <- length(items)
    
    # if the list is empty, increment the number of empty collections
    if(numItems >= 10)  { # if not, extract the information and save it
      ids <- c(ids, items %>% xml_find_all("//@objectid") %>% xml_text())
      ratings <- c(ratings, items %>% xml_find_all("//stats/rating/@value") %>% xml_double())
    } else {
      if (numItems == 0) {
        emptyCollections <- emptyCollections + 1
      }
    }
  }
  
  # sleep to avoid getting rate limited
  Sys.sleep(sleeptime__)
}

close(pb)

####################################################################################
# STEP 3: Aggregate the ratings and prune the list
####################################################################################

# determine how many vote to use to weight things towards the center
# via function based on the size of the guild; the last part just lops off the decimal
# it's a good thing the guild is as large as it is otherwise this wouldn't work
# this is... such guess work
threshold <- ifelse(guildSize * .01 > 5, (guildSize * .01) %/% 1, 5)

gameRatings <- tibble("ID" = ids, "Rating" = ratings)  %>%  # combining our vectors into a tibble
  group_by(ID) %>%                                             # and for each game
  summarise("Average Rating" = round(mean(Rating), 3),         # get the average rating
            "Ratings" = rowSums(table(ID,Rating)))           # and number of ratings

# storing the number of total ratings for stat tracking

totalRatings <- gameRatings$Ratings %>% sum

# modifying the ratings with some pseudo-Bayesian averaging
# first, use the data we have to figure out the expected value of a rating for this data set
expectedValue <- mean(gameRatings$`Average Rating`)

gameRatings$`Average Rating` <- ((gameRatings$`Average Rating` * gameRatings$Ratings + threshold * expectedValue) 
                                  / (gameRatings$Ratings + threshold)) %>% round(3)

####################################################################################
# STEP 4: Gather additional details about each game by id and build the final df
####################################################################################

# The additional needed data:
#     - Name
#     - BGG Rating
#     - Game Weight
#     - Year Released
#     - BGG Rank

# subset the current data to focus on, as a separate variable for now just in case
gameRatingsSorted <- gameRatings %>% arrange(desc(`Average Rating`))
gameRatingsTop <- gameRatingsSorted[1:110,]

# look up the game data using the ids we've gathered
# then parse that data to build the final games list with all the things!
gameRatingsTop %<>% getGameData()

# adding a rank column 
gameRatingsTop %<>% mutate(Rank = row_number())

####################################################################################
# STEP 5: Export highest ranked games to top100 and top 10 files (diff formats)
####################################################################################

# exporting "top xx" lists
#game_list_df %>% exportTop100(read.csv("top100-2018-01-31.csv"),paste0("top100-",today()))

#game_list_df %>% exportTop10(read.csv("top100-2018-01-31.csv"),paste0("top10-",today()))