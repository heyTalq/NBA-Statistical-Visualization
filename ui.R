library(shiny)
library(leaflet)
library(rvest)
library(ggplot2)
library(DT)
require(xml2)
require(selectr)

################################################################################################################################################################
## DATA THAT DOESN"T NEED TO CHANGE ##

# Reading Table from Wkipedia
arenaUrl = read_html("https://web.archive.org/web/20170413014755/https://en.wikipedia.org/wiki/List_of_National_Basketball_Association_arenas")
nbaTable = data.frame(arenaUrl %>%
                        html_nodes(xpath = '//*[@id="mw-content-text"]/table[1]') %>%
                        html_table())
nbaTable = subset(nbaTable, select = -c(1,5,6,7)) # delete unecessary rows

# Separate Clippers from Lakers
lakersRow = nbaTable[20,]
nbaTable = rbind(nbaTable[1:20,], lakersRow, nbaTable[21:29,])
rownames(nbaTable) = 1:30
nbaTable[20,3] = "Los Angeles Clippers"
nbaTable[21,3] = "Los Angeles Lakers"

# Sorting and Tidying up
colnames(nbaTable) = c("arena", "city", "team")
nbaTable = nbaTable[order(nbaTable$team),] # order by team alphabetically
rownames(nbaTable) = 1:30
nbaTable = nbaTable[c("team", "arena", "city")] # changed column order

# Adding coordinates
lng = c(-84.39632,  -71.06215,  -73.97503,  -80.83924, -87.67418,  -81.68822, -96.81035, -105.00771,  -83.24579, -122.20305,
        -95.36210,  -86.15546, -118.26725, -118.26725, -90.05047,  -80.18697,  -87.91720,  -93.27619,  -90.08206,  -73.99344,
        -97.51512,  -81.38386,  -75.17198, -112.07120, -122.66684, -121.51813, -98.43750,  -79.37910, -111.90109,  -77.02088)
lat = c(33.75729, 42.36620, 40.68250, 35.22514, 41.88069, 41.49656, 32.79051, 39.74866, 42.69688, 37.75029,
        29.75076, 39.76393, 34.04302, 35.04302, 35.13820, 25.78140, 43.04365, 44.97945, 29.94904, 41.75050,
        35.46342, 28.53926, 39.90120, 33.44574, 45.53157, 38.64904, 29.42694, 43.64347, 40.76827, 38.89813)
arenaCord = data.frame(lng, lat)
nbaTable = cbind(nbaTable,arenaCord) # appending coordinates to nbaTable

# Making list of Abbreviations needed to search image url's
abbUrl = read_html("https://en.wikipedia.org/w/index.php?title=Wikipedia:WikiProject_National_Basketball_Association/National_Basketball_Association_team_abbreviations&oldid=759837249")
abbTable = data.frame(abbUrl %>%
                        html_nodes(xpath ='//*[@id="mw-content-text"]/div/table') %>%
                        html_table())
abbTable = abbTable[2:31,1]
abbTable[2] = "BOS" # Reordering boston and brooklyn properly
abbTable[3] = "BKN"
iconList = abbTable
for (i in 1:30) {
  iconList[i] = paste0("http://i.cdn.turner.com/nba/nba/.element/img/1.0/teamsites/logos/teamlogos_500x500/",abbTable[i],".png")
}

# Adding icon url's to nbaTable
nbaTable = cbind(nbaTable,iconList)
colnames(nbaTable) = c("team", "arena", "city", "lng", "lat", "icon") # proper column names updated
nbaTable$icon = tolower(nbaTable$icon)
nbaTable$icon[30] = "http://a4.espncdn.com/combiner/i?img=%2Fi%2Fteamlogos%2Fnba%2F500%2Fwsh.png" # changing wizards logo

# Preparation for iterating through popups
shortNames = nbaTable[,1] # "Atlanta Hawks", etc.
for (i in 1:30) {
  shortNames[i] = gsub("^.* ","",nbaTable[i,1]) # Removes prefixes, "Atlanta Hawks" -> "Hawks"
}
nbaData = vector("list", 30)
names(nbaData) = nbaTable$team
rownames(nbaTable) = nbaTable$team

games = 1:82 # variable used for ggplot
seasonRecord = vector("list", 30) # variable used for every team's record
################################################################################################################################################################
## SHINY / LEAFLET ##
ui <- navbarPage("NBA Visualization", id = "nav",
                 
                 tabPanel("Map",
                          div(class="outer",
                              tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),     #Interface improvements
                              tags$head(
                                tags$style( HTML(
                                  "@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700'); h2 {
                                  font-family: 'Lobster', cursive; font-weight: 500; line-height: 1.1; color: #F06233;}"
                                ))
                              ),
                              
                              leafletOutput("nbaMap", width = "100%", height = "100%"), # Leaflet Map
                              
                              # Set width equal to something or else panel does weird stuff
                              absolutePanel(top=350, right=50, left = "auto", bottom = "auto", height = "auto", width = "13%" , fixed = TRUE,  style = "opacity: 0.70",
                                            cursor = c("auto", "move", "default", "inherit"), draggable = TRUE, class = "panel panel-default", # panel-default give slightly round edges and shadow
                                            
                                            div(
                                              style="padding: 8px; border-bottom: 1px solid #CCC; background: #F8F8FF;", # this adds a little bit of padding to the select boxes and color
                                              
                                              selectInput("teams", h2("Team"), # Selection Box
                                                          list("Select a Team",
                                                               `Atlantic` = c( "Boston Celtics", "Brooklyn Nets","New York Knicks","Philadelphia 76ers","Toronto Raptors"),
                                                               `Central` = c("Chicago Bulls","Cleveland Cavaliers","Detroit Pistons","Indiana Pacers","Milwaukee Bucks"),
                                                               `Northwest` = c("Denver Nuggets", "Minnesota Timberwolves","Oklahoma City Thunder","Portland Trail Blazers","Utah Jazz"),
                                                               `Pacific` =  c("Golden State Warriors", "Los Angeles Clippers","Los Angeles Lakers","Phoenix Suns","Sacramento Kings"),
                                                               `Southeast` = c("Atlanta Hawks","Charlotte Hornets","Miami Heat","Orlando Magic","Washington Wizards"),
                                                               `Southwest` = c("Dallas Mavericks", "Houston Rockets", "Memphis Grizzlies","New Orleans Pelicans","San Antonio Spurs")),
                                                          selected="Select a Team"
                                              ),
                                              
                                              selectInput("seasons", h2("Season"), # Selection Box
                                                          list("2014-15", "2015-16", "2016-17"),
                                                          selected ="2016-17"
                                              )
                                            )),
                              
                              # Panel for  graph output
                              
                              absolutePanel(top=650, right= 80, left = "auto", bottom = "auto", height = "auto", width = "26%" , fixed = TRUE,  style = "opacity: 1.00",
                                            cursor = c("auto", "move", "default", "inherit"), draggable = TRUE,
                                            div(
                                              style="transparent",
                                              #  We use conditional panel here incase no team is selected
                                              conditionalPanel(condition = "input.teams != 'Select a Team'", plotOutput("barDifferential", height = 300 ,width= 500)
                                              ))
                              )
                          )),
                 
                 
                 tabPanel("Data Table",
                          fluidRow(
                            column(3,
                                   selectInput("datateams", h2("Team"), # Selection Box
                                               list("",`Atlantic` = c( "Boston Celtics", "Brooklyn Nets","New York Knicks","Philadelphia 76ers","Toronto Raptors"),
                                                    `Central` = c("Chicago Bulls","Cleveland Cavaliers","Detroit Pistons","Indiana Pacers","Milwaukee Bucks"),
                                                    `Northwest` = c("Denver Nuggets", "Minnesota Timberwolves","Oklahoma City Thunder","Portland Trail Blazers","Utah Jazz"),
                                                    `Pacific` =  c("Golden State Warriors", "Los Angeles Clippers","Los Angeles Lakers","Phoenix Suns","Sacramento Kings"),
                                                    `Southeast` = c("Atlanta Hawks","Charlotte Hornets","Miami Heat","Orlando Magic","Washington Wizards"),
                                                    `Southwest` = c("Dallas Mavericks", "Houston Rockets", "Memphis Grizzlies","New Orleans Pelicans","San Antonio Spurs")),
                                               selected=""),multiple=FALSE),
                            column(3,
                                   selectInput("dataopp", h2("Opponent"), # Selection Box
                                               list("None",`Atlantic` = c( "Boston Celtics", "Brooklyn Nets","New York Knicks","Philadelphia 76ers","Toronto Raptors"),
                                                    `Central` = c("Chicago Bulls","Cleveland Cavaliers","Detroit Pistons","Indiana Pacers","Milwaukee Bucks"),
                                                    `Northwest` = c("Denver Nuggets", "Minnesota Timberwolves","Oklahoma City Thunder","Portland Trail Blazers","Utah Jazz"),
                                                    `Pacific` =  c("Golden State Warriors", "Los Angeles Clippers","Los Angeles Lakers","Phoenix Suns","Sacramento Kings"),
                                                    `Southeast` = c("Atlanta Hawks","Charlotte Hornets","Miami Heat","Orlando Magic","Washington Wizards"),
                                                    `Southwest` = c("Dallas Mavericks", "Houston Rockets", "Memphis Grizzlies","New Orleans Pelicans","San Antonio Spurs")),
                                               selected="None"),multiple=FALSE),
                            column(3,
                                   selectInput("dataseasons", h2("Season"), # Selection Box
                                               list("2014-15", "2015-16", "2016-17"), selected = "2016-17"), multiple=TRUE)
                          ),
                          hr(),
                          dataTableOutput("datatable")
                 )
)
