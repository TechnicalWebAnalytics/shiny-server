#-----------| GA REPORTING |-----------#

# A simple script to extract GA data and parse into
# any existing xlsx.

# This focuses particularly on SITE PERFORMANCE and CHANNELS

# Data is parsed out by ROWS

# New WORKSHEETS are created by the dates data is extracted.

# Stand by for eventual updates. One main focus
# will result in parsing data by specific cells.

# Another update will import an xlsx and utilizing the parsed
# method annotated above. We should be able to apply changes to
# a the primary xlsx which will completely remove the need
# for manual configurations.

# And for the nerdiness top off, I want to use system time
# to schedule data extraction so the xlsx will automatically
# update itself by a specifc schedule.

# RESOURCES:
# RGoogleAnalytics Set up API and Data: http://www.tatvic.com/blog/google-analytics-data-extraction-in-r/

#-------------| SET WORKDESK |--------------#

# setwd("/home/mechelle/Google Drive/Work/Syntax/R")

# Authorize the Google Analytics account
# This need not be executed in every session once the token object is created and saved

#-------------| DEPENDENCIES |--------------#

# Install Dependencies
source("~/Google Drive/Work/Syntax/R/Snippets/initialDependencies.R")

#-------------| AUTHORIZATIONS |--------------#


# # Google Analytics
tokens <- c(
  # analytics@bvaccel.com
  "/home/mechelle/Google Drive/Work/Syntax/R/GATokens/.gatoken1.rds",
  # analytics2@bvaccel.com
  "/home/mechelle/Google Drive/Work/Syntax/R/GATokens/.gatoken2.rds")

authorize(cache=tokens[2])

# checkautho <- function(id){
#   for (i in 1:length(tokens)) {
#     if(id %in% data.frame(list_profiles())[,2] == FALSE){
#       x <- id %in% data.frame(list_profiles())[,2]
#       authorize(cache = tokens[i])
#       }else if(id %in% data.frame(list_profiles())[,2] == TRUE){
#         break
#       }
#     }
#   }

# # GDrive
gs_auth(cache = "~/Google Drive/Work/Syntax/R/GDriveTokens/.gdrivetoken1.rds")

#-------------Write to Sheets---------------#

# Get list of all sheets
(my_sheets <- gs_ls())

# Get Sheet by Key
googleSheet <- gs_key("19kp_tf8mSBV0oOH85qeAKb4ABdUF7OWbhjjhGL5VeX4")

# View all worksheets in spreadsheet
gs_ws_ls(googleSheet)


#-------------| SET UP GA ACCOUNTS |--------------#

# Get list of GA Views
ga_profiles <- list_profiles()

account <- c(
  "10482547"
)

uaid <- c(
  "UA-10482547-1"
)

view <- c(
  "21119138")

for (i in 1:length(account)) {
  print("start")
  checkautho(account[i])
  x <- paste0("profile",i);
  tmp <- get_profile(
    account[i],
    uaid[i],
    view[i]
  )
  assign(x,tmp)
  print("end")
}

#-------------| SET DATES |--------------#

current <- "TRUE" # Set to true if using current date for End Dates

# Set Current Date
date <- unlist(strsplit(toString(Sys.Date()),"-"))
year <- date[1]
month <- date[2]
day <- date[3]

# Start Dates #
startYear <- "2016" # YEAR
startMonth <- "06" # MONTH
startDay <- "01" # DAY

if( current == "TRUE" ){
  # End Dates #
  endYear <- year # YEAR
  endMonth <- month # MONTH
  endDay <- day # DAY
} else {
  endYear <- "2016" # YEAR
  endMonth <- "08" # MONTH
  endDay <- "31" # DAY
}

# Set Dates ! DO NOT MODIFY #
startDate <- sprintf("%s-%s-%s", startYear, startMonth, startDay)
endDate <- sprintf("%s-%s-%s", endYear, endMonth, endDay)

#-------------| HOSTNAMES |---------------#

# Display all site traffic

hostNames <- get_ga(view,
                    start.date = startDate,
                    end.date = endDate,
                    metrics = "ga:sessions,ga:pageviews",
                    dimensions = "ga:hostname",
                    sort = "-ga:sessions",
                    fetch.by = "week"
)

colnames(hostNames) <- NULL
gs_edit_cells(googleSheet,ws="Hostname Audit",input=hostNames,byrow = TRUE, anchor="C2")

#-------------| UTM DATA |---------------#

utmData <- get_ga(view,
                  start.date = startDate,
                  end.date = endDate,
                  metrics = "ga:sessions",
                  dimensions = "ga:channelGrouping,ga:medium,ga:source,ga:campaign",
                  sort = "-ga:sessions",
                  # fetch.by = "week",
                  max.results = 500
)

colnames(utmData) <- NULL
gs_edit_cells(googleSheet,ws="UTM Audit",input=utmData,byrow = TRUE, anchor="C2")

#-------------| EVENTS |---------------#

events <- get_ga(view,
                 start.date = startDate,
                 end.date = endDate,
                 metrics = "ga:totalEvents,ga:uniqueEvents",
                 dimensions = "ga:eventCategory,ga:eventAction,ga:eventLabel",
                 sort = "-ga:totalEvents",
                 max.results = 500
)

colnames(events) <- NULL
gs_edit_cells(googleSheet,ws="Event Audit",input=events,byrow = TRUE, anchor="C2")

#-------------| QUERY EXCLUSIONS |---------------#

pages <- get_ga(view,
                start.date = startDate,
                end.date = endDate,
                metrics = "ga:sessions",
                dimensions = "ga:pagePath",
                sort = "-ga:sessions",
                # fetch.by = "week",
                max.results = 10000
)

query <- data.frame(query=rep("", 0),stringsAsFactors=FALSE)
count <- 1

pq <- pages[grepl("\\?",pages$pagePath),]
for( i in 1:nrow(pq) ){
  print(i)
  x <- names(parse_url(pq[i,1])$query)
  for( ii in 1:length(x) ){
    pq[i,paste("Query", ii, sep="")] <- x[ii]
  }
}

for(ii in 1:nrow(pages)){
  url <- parse_url(pages[ii,1])
  print(ii)
  for(i in 1:length(url$query)){
    count <- count + 1
    q <- names(url$query[i])
    if(!is.null(url$query)){
      if(!q == ""){
        query[count, ] <- c(toString(q))
      }
    }
  }
}

uniqueQueries <- paste(unique(na.omit(query))[(1:nrow(unique(na.omit(query)))),1], sep = ',',collapse = ',')
colnames(uniqueQueries) <- NULL
colnames(pq) <- NULL

count <- 3
for(i in 1:nrow(pq)){
  count <- count+1
  gs_edit_cells(googleSheet,ws="Query Parameters Exclusions",input=pq[i,],byrow = TRUE, anchor=paste("A",count,sep=""))
  print(i)
}

gs_edit_cells(googleSheet,ws="Query Parameters Exclusions",input=uniqueQueries,byrow = TRUE, anchor="B1")

#-------------| FILTERS |---------------#

## Display all site traffic

filters <- list_filters(view, start.index = NULL, max.results = NULL)
colnames(filters) <- NULL

gs_edit_cells(googleSheet,ws="Filters",input=filters,byrow = TRUE, anchor="C2")

#-------------| GOALS |---------------#

rowCount <- 2
for(i in 1:999){
  print(i)
  
  goal <- data.frame(get_goal(account,uaid,view,i))
  colnames(goal) <- NULL
  gs_edit_cells(googleSheet,ws="Goal Conversion Audit",byrow = TRUE,input=goal, anchor=paste("A",rowCount,sep=""))
  rowCount <- nrow(goal) + rowCount
}

#-------------| GA ACCOUNT INFO ( Property & View ) |---------------#

property <- setDT(data.frame(as.matrix(get_webproperty(account,uaid))),keep.rownames = TRUE)
view <- setDT(data.frame(as.matrix(get_profile(account,uaid,view))),keep.rownames = TRUE)

gs_edit_cells(googleSheet,ws="Raw Account Data",input=property,byrow = TRUE, anchor="A1")
gs_edit_cells(googleSheet,ws="Raw Account Data",input=view,byrow = TRUE, anchor="C1")
