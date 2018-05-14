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

server <- function(input, output, session) {
  
  # Initial Map
  output$nbaMap = renderLeaflet( {
    leaflet(options = leafletOptions(minZoom = 4, maxZoom = 7)) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      setView(-88.583333,39.833333, zoom = 5)
  })
  
  ################################################################################################################################################################
  observeEvent(
    {input$teams
      input$seasons},{   # observe changes to team/season input
        
        # Reading CSV files iteratively and formatting them
        csvFileNames =  system.file('extdata', input$seasons, paste0(shortNames,".csv"), package = "nbaVisualization") # "Hawks.csv" , etc. in correct directories
        
        for (i in 1:30) {
          nbaData[[i]] = read.csv(csvFileNames[i],
                                  colClasses = c("integer", rep("character", 8), rep("integer",4), "character", "character"))
          nbaData[[i]] = subset(nbaData[[i]], select = c(2,3,6,7,8,10,11,12,13))
          colnames(nbaData[[i]]) = c("date", "time","home", "opp", "result", "teamScore",
                                     "oppScore", "winCounter", "lossCounter")
          nbaData[[i]] = nbaData[[i]][c("opp", "date", "time", "home", "result", "teamScore",
                                        "oppScore", "winCounter", "lossCounter")]
          nbaData[[i]]$date = gsub("^.*? ","", nbaData[[i]]$date) # formatting dates and time
          nbaData[[i]]$time = gsub(' [A-z ]*', '' , nbaData[[i]]$time)
          differential = nbaData[[i]]$teamScore - nbaData[[i]]$oppScore
          nbaData[[i]] = cbind(nbaData[[i]],differential)
          nbaData[[i]]$sign = ifelse(nbaData[[i]]$differential >= 0, "positive", "negative")
          vs = nbaData[[i]]$opp
          nbaData[[i]]$home = ifelse(nbaData[[i]]$home == "@", nbaTable[vs,2], nbaTable[i,2])
          seasonRecord[i] = ifelse(nbaData[[i]][82,8] == nbaData[[i]][82,9], paste0("<strong>",nbaData[[i]][82,8],"-",nbaData[[i]][82,9],"</strong></span>"),
                                   ifelse(nbaData[[i]][82,8] > nbaData[[i]][82,9],
                                          paste0("<span style='color: #008000'><strong>",nbaData[[i]][82,8],"-",nbaData[[i]][82,9],"</strong></span>"),
                                          paste0("<span style='color: #800000'><strong>",nbaData[[i]][82,8],"-",nbaData[[i]][82,9],"</strong></span>")
                                   )
          )
        }
        
        # Reading special CSV file of season stats to display in popups when no team is selected
        default<-system.file('extdata', input$seasons, "default.csv", package = "nbaVisualization")
        defData = read.csv(default, colClasses = c("integer", "character", rep("numeric", 23)))
        defData = defData[-31,-3]
        colnames(defData) = c("Rank", "Team", "Minutes Played", "Field Goals", "Field Goal Attempts", "Field Goal %", "3-Pt Field Goals",
                              "3-Pt Field Goal Attempts", "3-Pt Field Goal %", "2-Pt Field Goals", "2-Pt Field Goal Attempts", "2-Pt Field Goal %",
                              "Free Throws", "Free Throw Attempts", "Free Throw %", "Offensive Rebounds", "Defensive Rebounds", "Total Rebounds",
                              "Assists", "Steals", "Blocks", "Turnovers", "Personal Fouls", "Points")
        defData = defData[order(defData$Team),]
        rownames(defData) = nbaTable$team
        defData$`Field Goal %` = 100 * defData$`Field Goal %`
        defData$`3-Pt Field Goal %` = 100 * defData$`3-Pt Field Goal %`
        defData$`2-Pt Field Goal %` = 100 * defData$`2-Pt Field Goal %`
        defData$`Free Throw %` = 100 * defData$`Free Throw %`
        defData$Record = seasonRecord
        
        pop = paste0("<span style='color: #000000'><strong>", nbaTable$team, "</strong></span>", "<br>", nbaTable$arena)
        popDefault = paste0(pop, "<br>",
                            "<br><strong>Record:  </strong>", defData$Record,
                            "<br><strong>Rank:  </strong>", defData$Rank,
                            "<br><strong>Field Goal %:  </strong>", defData$`Field Goal %`,
                            "<br><strong>3-Pt Field Goals:  </strong>", defData$`3-Pt Field Goals`,
                            "<br><strong>3-Pt Field %:  </strong>", defData$`3-Pt Field Goal %`,
                            "<br><strong>2-Pt Field Goals:  </strong>", defData$`2-Pt Field Goals`,
                            "<br><strong>2-Pt Field %:  </strong>", defData$`2-Pt Field Goal %`,
                            "<br><strong>Free Throw %:  </strong>", defData$`Free Throw %`,
                            "<br><strong>Offensive Rebounds:  </strong>", defData$`Offensive Rebounds`,
                            "<br><strong>Defensive Rebounds:  </strong>", defData$`Defensive Rebounds`,
                            "<br><strong>Assists:  </strong>", defData$Assists,
                            "<br><strong>Steals:  </strong>", defData$Steals,
                            "<br><strong>Blocks:  </strong>", defData$Blocks,
                            "<br><strong>Turnovers: </strong>", defData$Turnovers)
        
        proxy = leafletProxy("nbaMap")
        proxy %>% clearMarkers()
        inputTeam = input$teams
        
        if (inputTeam != "Select a Team") {
          output$barDifferential = renderPlot({ggplot(nbaData[[inputTeam]], aes(x = games, y = nbaData[[inputTeam]]$differential, fill = sign)) +
              geom_bar(stat = "identity") + # needed for plot to display
              scale_fill_manual(values = c("positive" = "lime green", "negative" = "red")) +        # Color
              ggtitle("Point Differential by Game") + labs(x = "Games", y = "Point Differential") +  # Axes
              theme(plot.title = element_text(color="#F06233", face = "bold", size= 18, hjust=0)) + # Fonts
              theme(axis.title = element_text(color="#F06233", face = "bold", size= 18)) +
              theme(axis.text.x = element_text(size=13)) + theme(axis.text.y = element_text(size=13)) +
              theme(
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                panel.background = element_blank(),
                plot.background = element_blank(),
                panel.border = element_rect(colour = "black", fill=NA, size= 0.5))+
              guides(fill=FALSE) # Remove legend
          }, bg="transparent")
        }
        
        for(i in 1:30) { #iterate through teams to make popups
          if (inputTeam == "Select a Team") {
            proxy %>%
              addMarkers(lng = nbaTable$lng, lat = nbaTable$lat, popup = popDefault, popupOptions = popupOptions(maxHeight = 500, maxWidth = 500),
                         options = markerOptions(riseOnHover=TRUE), icon = icons(iconUrl = nbaTable$icon, iconWidth = 100, iconHeight = 100))
            break() # skip everything if no team is selected
          }
          if (inputTeam == nbaTable$team[i]) {
            next() # skip selected team's popup
          }
          inputTeamDF = nbaData[[inputTeam]] # Getting data for inputTeam
          inputSub = subset(inputTeamDF, inputTeamDF[["opp"]] == nbaTable$team[i]) # subsetting data for all opponent through for loop
          # Marking Wins and Loses with color
          wCounter = 0
          lCounter = 0
          for ( j in seq_along(inputSub[["result"]])) {
            if (inputSub[["result"]][j] == "W") {
              inputSub[["date"]][j] = paste0("<span style='color: #008000'><strong>", inputSub[["date"]][j], "</strong></span>")
              wCounter = wCounter + 1 }
            if (inputSub[["result"]][j] == "L") {
              inputSub[["date"]][j] = paste0("<span style='color: #800000'><strong>", inputSub[["date"]][j], "</strong></span>")
              lCounter = lCounter + 1 }
          }
          head2head = ifelse(wCounter == lCounter, paste0("<strong>",wCounter,"-",lCounter,"</strong></span>"),
                             ifelse(wCounter > lCounter,
                                    paste0("<span style='color: #008000'><strong>",wCounter,"-",lCounter,"</strong></span>"),
                                    paste0("<span style='color: #800000'><strong>",wCounter,"-",lCounter,"</strong></span>")
                             )
          )
          
          # Define popup for each iterated team and add it to the map
          pop = paste (inputSub[["date"]],"<br>", inputSub[["home"]], "<br>", "Result: ", inputSub[["result"]], "<br>",
                       inputTeam,": ", inputSub[["teamScore"]],"<br>", nbaTable$team[i],": ", inputSub[["oppScore"]], "<br>", sep ="", collapse = "<br>")
          proxy %>%
            addMarkers(lng = nbaTable$lng[i], lat = nbaTable$lat[i],
                       popup = paste0("<strong>", nbaTable$team[i], "</strong></span>",
                                      "<br>", head2head, "<br>", "<br>", pop) ,
                       popupOptions = popupOptions(maxHeight = 385, maxWidth = 500, minWidth = 180),
                       options = markerOptions(riseOnHover=TRUE), icon = icons(iconUrl = nbaTable$icon[i], iconWidth = 100, iconHeight = 100))
        }
      })
  ######
  
  observeEvent(
    {input$datateams
      input$dataopp
      input$dataseasons}, {
        
        if (input$datateams != "") {
          inputdataTeam = input$datateams
          inputdataOpp = input$dataopp
          inputShortTeam = gsub("^.* ","",inputdataTeam)
          system.file('extdata', input$dataseasons, paste0(inputShortTeam,".csv"), package = "nbaVisualization")
          csvData = read.csv(system.file('extdata', input$dataseasons, paste0(inputShortTeam,".csv"), package = "nbaVisualization"),
                             colClasses = c("integer", rep("character", 8), rep("integer",4), "character", "character"))
          csvData = subset(csvData, select = c(2,3,6,7,8,10,11,12,13,14))
          colnames(csvData) = c("Date", "Time (ET)","Location", "Opponent", "Result", "Team Score",
                                "Opponent Score", "Win Counter", "Loss Counter", "Streak")
          csvData = csvData[c("Date", "Time (ET)","Location", "Opponent", "Result", "Team Score",
                              "Opponent Score", "Win Counter", "Loss Counter", "Streak")]
          csvData$Date = gsub("^.*? ","", csvData$Date) # formatting dates and time
          csvData$`Time (ET)` = gsub(' [A-z ]*', '' , csvData$Time)
          VS = csvData$Opponent
          csvData$Location = ifelse(csvData$Location == "@", nbaTable[VS,2], nbaTable[inputdataTeam,2])
          if (inputdataOpp == "None") {
            output$datatable = renderDataTable(csvData, server = TRUE)
          }
          if (inputdataOpp != "None") {
            csvData = subset(csvData, csvData$Opponent==inputdataOpp)
            output$datatable = renderDataTable(csvData, server = TRUE)
          }
        }
      }
  )
}
