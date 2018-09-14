
library(shiny)

library(ggplot2)
library(ggimage)
library(magick)

library(imager)
library(readbitmap)
# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("CFB Plot"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      checkboxGroupInput(inputId = "year",
                         label = "Choose a year:",
                         choices = c("2015", "2016", "2017"), selected = 2)
      
    ), 
    checkboxGroupInput(inputId = "conference",
                label = "Choose a Conference:",
                choices = c("SEC", "ACC", "MW", "MAC", "Pac-12", "Sun Belt", "FBS Indep.", "Big 12", "C-USA", "American", "Big Ten"), selected = 2)
    ), 
  
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput("cfbtable")
      
      )
    )



server <- function(input, output) {
  
  
  teamtalent <- read.csv(file = '247 2017 team talent.csv', fill = T, stringsAsFactors = F, fileEncoding="UTF-8-BOM")
  library(ggplot2)
  library(shiny)
  library(plotly)
  
  
  
  
  
  
  
  
  teams <- as.data.frame(teamtalent[seq(9, nrow(teamtalent), 9),])
  rating <- teamtalent[seq(16, nrow(teamtalent), 9),]
  averagerating <- teamtalent[seq(15, nrow(teamtalent), 9),]
  fivestars <- teamtalent[seq(12, nrow(teamtalent), 9),]
  fourstars <- teamtalent[seq(13, nrow(teamtalent), 9),]
  threestars <- teamtalent[seq(14, nrow(teamtalent), 9),]
  
  
  teams <- as.data.frame(teams)
  rating <- as.data.frame(rating)
  averagerating <- as.data.frame(averagerating)
  fivestars <- as.data.frame(fivestars)
  fourstars <- as.data.frame(fourstars)
  threestars <- as.data.frame(threestars)
  cfb17 <- cbind(teams, rating, averagerating, fivestars, fourstars, threestars)
  colnames(cfb17)[1] <- 'team'
  
  
  
  espnrating <- read.csv(file = '2017 espn cfb ratings.csv', fill = T, stringsAsFactors = F, fileEncoding="UTF-8-BOM")
  colnames(espnrating)[2] <- 'team'
  espnrating <- espnrating[-seq(21, nrow(espnrating), 21),]
  
  for(i in 1:nrow(espnrating)){
    teamconf <- espnrating[i, 'team']
    teamconf <- gsub("Miami, ACC", "miami-fl", teamconf)
    teamconf <- gsub("Miami-(oh), MAC", "miami-oh", teamconf)
    
    split <- strsplit(teamconf, ', ', fixed = T)
    espnrating[i, 'team'] <- split[[1]][1]
    espnrating[i, 'conference'] <- split[[1]][2]
    
  }
  
  
  
  for(i in 1:nrow(espnrating)){
    espnrating[i, 'team'] <- gsub('OSU', 'ohio state', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('Miss St', 'mississippi state', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('NC State', 'north carolina state', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('VT', 'virginia tech', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('FSU', 'florida state', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('Washington St', 'washington state', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('FAU', 'florida atlantic', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('Pitt', 'pittsburgh', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('Appalachian St', 'appalachian state', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('Cal', 'california', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('N Illinois', 'northern illinois', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('UNC', 'north carolina', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('UVA', 'virginia', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('W Michigan', 'western michigan', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('E Michigan', 'eastern michigan', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('UTSA', 'texas san anotonio', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('ECU', 'eastern carolina', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('Ga Southern', 'georgia southern', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('C. Carolina', 'coastal carolina', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("Hawai'i", 'hawaii', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("UConn", 'connecticut', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("W Kentucky", 'western kentucky', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("UL Monroe", 'louisiana monroe', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("BYU", 'bringham young', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("New Mexico St", 'new mexico state', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("Cent Michigan", 'central michigan', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("LA Tech", 'louisiana tech', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("Mid Tennessee", 'middle tennessee', espnrating[i, 'team'])
  }
  
  
  
  
  cfb17$team <- tolower(cfb17$team)
  espnrating$team <- tolower(espnrating$team)
  
  
  
  cfbtotal17 <- merge(cfb17, espnrating, by = 'team')
  
  cfbtotal17$OVERALL <- as.numeric(as.character(cfbtotal17$OVERALL))
  cfbtotal17$rating <- as.numeric(as.character(cfbtotal17$rating))
  
  colnames(cfbtotal17)[colnames(cfbtotal17) == 'OVERALL'] <- 'FPI'
  colnames(cfbtotal17)[colnames(cfbtotal17) == 'rating'] <- 'teamtalent'
  cfbtotal17$pointsperrating <- cfbtotal17$teamtalent / cfbtotal17$FPI
  cfbtotal17$year <- '2017'
  
  
  
  for(i in 1:nrow(cfbtotal17)){
    team <- as.character(cfbtotal17[i, 'team'])
    team <- tolower(team)
    cfbtotal17[i, 'team'] <- team
    if(!(is.element(cfbtotal17[i, 'team'], espnrating$team))){
      print(cfbtotal17[i, 'team'])
    }
  }
  
  
  
  
  
  
  teamtalent <- read.csv(file = '247 2016 team talent.csv', fill = T, stringsAsFactors = F, fileEncoding="UTF-8-BOM")
  library(ggplot2)
  library(shiny)
  library(plotly)
  
  
  
  
  
  
  
  
  teams <- as.data.frame(teamtalent[seq(9, nrow(teamtalent), 9),])
  rating <- teamtalent[seq(16, nrow(teamtalent), 9),]
  averagerating <- teamtalent[seq(15, nrow(teamtalent), 9),]
  fivestars <- teamtalent[seq(12, nrow(teamtalent), 9),]
  fourstars <- teamtalent[seq(13, nrow(teamtalent), 9),]
  threestars <- teamtalent[seq(14, nrow(teamtalent), 9),]
  
  
  teams <- as.data.frame(teams)
  rating <- as.data.frame(rating)
  averagerating <- as.data.frame(averagerating)
  fivestars <- as.data.frame(fivestars)
  fourstars <- as.data.frame(fourstars)
  threestars <- as.data.frame(threestars)
  cfb16 <- cbind(teams, rating, averagerating, fivestars, fourstars, threestars)
  colnames(cfb16)[1] <- 'team'
  
  
  
  espnrating <- read.csv(file = '2016 espn cfb ratings.csv', fill = T, stringsAsFactors = F, fileEncoding="UTF-8-BOM")
  colnames(espnrating)[2] <- 'team'
  espnrating <- espnrating[-seq(21, nrow(espnrating), 21),]
  
  for(i in 1:nrow(espnrating)){
    teamconf <- espnrating[i, 'team']
    teamconf <- gsub("Miami, ACC", "miami-fl", teamconf)
    teamconf <- gsub("Miami-(oh), MAC", "miami-oh", teamconf)
    
    split <- strsplit(teamconf, ', ', fixed = T)
    espnrating[i, 'team'] <- split[[1]][1]
    espnrating[i, 'conference'] <- split[[1]][2]
    
  }
  
  
  
  for(i in 1:nrow(espnrating)){
    espnrating[i, 'team'] <- gsub('OSU', 'ohio state', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('Miss St', 'mississippi state', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('NC State', 'north carolina state', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('VT', 'virginia tech', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('FSU', 'florida state', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('Washington St', 'washington state', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('FAU', 'florida atlantic', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('Pitt', 'pittsburgh', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('Appalachian St', 'appalachian state', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('Cal', 'california', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('N Illinois', 'northern illinois', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('UNC', 'north carolina', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('UVA', 'virginia', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('W Michigan', 'western michigan', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('E Michigan', 'eastern michigan', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('UTSA', 'texas san anotonio', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('ECU', 'eastern carolina', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('Ga Southern', 'georgia southern', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('C. Carolina', 'coastal carolina', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("Hawai'i", 'hawaii', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("UConn", 'connecticut', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("W Kentucky", 'western kentucky', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("UL Monroe", 'louisiana monroe', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("BYU", 'bringham young', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("New Mexico St", 'new mexico state', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("Cent Michigan", 'central michigan', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("LA Tech", 'louisiana tech', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("Mid Tennessee", 'middle tennessee', espnrating[i, 'team'])
  }
  
  
  
  
  cfb16$team <- tolower(cfb16$team)
  espnrating$team <- tolower(espnrating$team)
  
  cfbtotal16 <- merge(cfb16, espnrating, by = 'team')
  
  cfbtotal16$OVERALL <- as.numeric(as.character(cfbtotal16$OVERALL))
  cfbtotal16$rating <- as.numeric(as.character(cfbtotal16$rating))
  
  colnames(cfbtotal16)[colnames(cfbtotal16) == 'OVERALL'] <- 'FPI'
  colnames(cfbtotal16)[colnames(cfbtotal16) == 'rating'] <- 'teamtalent'
  cfbtotal16$pointsperrating <- cfbtotal16$teamtalent / cfbtotal16$FPI
  cfbtotal16$year <- '2016'
  
  
  
  for(i in 1:nrow(cfbtotal16)){
    team <- as.character(cfbtotal16[i, 'team'])
    team <- tolower(team)
    cfbtotal16[i, 'team'] <- team
    if(!(is.element(cfbtotal16[i, 'team'], espnrating$team))){
      print(cfbtotal16[i, 'team'])
    }
  }
  
  
  
  
  
  teamtalent <- read.csv(file = '247 2015 team talent.csv', fill = T, stringsAsFactors = F, fileEncoding="UTF-8-BOM")
  library(ggplot2)
  library(shiny)
  library(plotly)
  
  
  
  
  
  
  
  
  teams <- as.data.frame(teamtalent[seq(9, nrow(teamtalent), 9),])
  rating <- teamtalent[seq(16, nrow(teamtalent), 9),]
  averagerating <- teamtalent[seq(15, nrow(teamtalent), 9),]
  fivestars <- teamtalent[seq(12, nrow(teamtalent), 9),]
  fourstars <- teamtalent[seq(13, nrow(teamtalent), 9),]
  threestars <- teamtalent[seq(14, nrow(teamtalent), 9),]
  
  
  teams <- as.data.frame(teams)
  rating <- as.data.frame(rating)
  averagerating <- as.data.frame(averagerating)
  fivestars <- as.data.frame(fivestars)
  fourstars <- as.data.frame(fourstars)
  threestars <- as.data.frame(threestars)
  cfb15 <- cbind(teams, rating, averagerating, fivestars, fourstars, threestars)
  colnames(cfb15)[1] <- 'team'
  
  
  
  espnrating <- read.csv(file = '2015 espn cfb ratings.csv', fill = T, stringsAsFactors = F, fileEncoding="UTF-8-BOM")
  colnames(espnrating)[2] <- 'team'
  espnrating <- espnrating[-seq(21, nrow(espnrating), 21),]
  
  for(i in 1:nrow(espnrating)){
    teamconf <- espnrating[i, 'team']
    teamconf <- gsub("Miami, ACC", "miami-fl", teamconf)
    teamconf <- gsub("Miami-(oh), MAC", "miami-oh", teamconf)
    
    split <- strsplit(teamconf, ', ', fixed = T)
    espnrating[i, 'team'] <- split[[1]][1]
    espnrating[i, 'conference'] <- split[[1]][2]
    
  }
  
  
  
  for(i in 1:nrow(espnrating)){
    espnrating[i, 'team'] <- gsub('OSU', 'ohio state', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('Miss St', 'mississippi state', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('NC State', 'north carolina state', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('VT', 'virginia tech', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('FSU', 'florida state', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('Washington St', 'washington state', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('FAU', 'florida atlantic', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('Pitt', 'pittsburgh', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('Appalachian St', 'appalachian state', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('Cal', 'california', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('N Illinois', 'northern illinois', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('UNC', 'north carolina', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('UVA', 'virginia', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('W Michigan', 'western michigan', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('E Michigan', 'eastern michigan', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('UTSA', 'texas san anotonio', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('ECU', 'eastern carolina', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('Ga Southern', 'georgia southern', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub('C. Carolina', 'coastal carolina', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("Hawai'i", 'hawaii', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("UConn", 'connecticut', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("W Kentucky", 'western kentucky', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("UL Monroe", 'louisiana monroe', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("BYU", 'bringham young', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("New Mexico St", 'new mexico state', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("Cent Michigan", 'central michigan', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("LA Tech", 'louisiana tech', espnrating[i, 'team'])
    espnrating[i, 'team'] <- gsub("Mid Tennessee", 'middle tennessee', espnrating[i, 'team'])
  }
  
  
  
  
  cfb15$team <- tolower(cfb15$team)
  espnrating$team <- tolower(espnrating$team)
  
  cfbtotal15 <- merge(cfb15, espnrating, by = 'team')
  
  cfbtotal15$OVERALL <- as.numeric(as.character(cfbtotal15$OVERALL))
  cfbtotal15$rating <- as.numeric(as.character(cfbtotal15$rating))
  
  colnames(cfbtotal15)[colnames(cfbtotal15) == 'OVERALL'] <- 'FPI'
  colnames(cfbtotal15)[colnames(cfbtotal15) == 'rating'] <- 'teamtalent'
  cfbtotal15$pointsperrating <- cfbtotal15$teamtalent / cfbtotal15$FPI
  cfbtotal15$year <- '2015'
  
  
  
  for(i in 1:nrow(cfbtotal15)){
    team <- as.character(cfbtotal15[i, 'team'])
    team <- tolower(team)
    cfbtotal15[i, 'team'] <- team
    if(!(is.element(cfbtotal15[i, 'team'], espnrating$team))){
      print(cfbtotal15[i, 'team'])
    }
  }
  allcfb <- rbind(cfbtotal15, cfbtotal16, cfbtotal17)
  logotemp <- 'https://d2p3bygnnzw9w3.cloudfront.net/req/201807242/tlogo/ncaa/georgia.png'
  for(i in 1:nrow(allcfb)){
    
    allcfb[i, 'logo'] <- gsub('georgia', tolower(allcfb[i, 'team']), logotemp)
    allcfb[i, 'logo'] <- gsub('/lsu.', '/louisiana-state.', allcfb[i, 'logo'])
    allcfb[i, 'logo'] <- gsub('ole miss', 'mississippi', allcfb[i, 'logo'])
    allcfb[i, 'logo'] <- gsub(' ', '-', allcfb[i, 'logo'])
    allcfb[i, 'logo'] <- gsub('&', '', allcfb[i, 'logo'])
    allcfb[i, 'logo'] <- gsub('unlv', 'nevada-las-vegas', allcfb[i, 'logo'])
    allcfb[i, 'logo'] <- gsub('usc', 'southern-california', allcfb[i, 'logo'])
    allcfb[i, 'logo'] <- gsub("miami-(oh)", 'miami-oh', allcfb[i, 'logo'])
    allcfb[i, 'logo'] <- gsub("tcu", 'texas-christian', allcfb[i, 'logo'])
    allcfb[i, 'logo'] <- gsub("fiu", 'florida-international', allcfb[i, 'logo'])
    allcfb[i, 'logo'] <- gsub("fau", 'florida-atlantic', allcfb[i, 'logo'])
    allcfb[i, 'logo'] <- gsub("southern-miss", 'southern-mississippi', allcfb[i, 'logo'])
    allcfb[i, 'logo'] <- gsub("/uab.", '/alabama-birmingham.', allcfb[i, 'logo'])
    allcfb[i, 'logo'] <- gsub("smu", 'southern-methodist', allcfb[i, 'logo'])
    allcfb[i, 'logo'] <- gsub("ucf", 'central-florida', allcfb[i, 'logo'])
    allcfb[i, 'logo'] <- gsub("usf", 'south-florida', allcfb[i, 'logo'])
    allcfb[i, 'logo'] <- gsub("/miami.png", '/miami-fl.png', allcfb[i, 'logo'])
    allcfb[i, 'logo'] <- gsub("ncaa/miami-(oh).png", 'ncaa/miami-oh.png', allcfb[i, 'logo'], fixed = T)
    allcfb[i, 'logo'] <- gsub("ncaa/louisiana.png", 'ncaa/louisiana-lafayette.png', allcfb[i, 'logo'], fixed = T)
    allcfb[i,'logo'] <- gsub("/utep.png", '/texas-el-paso.png', allcfb[i, 'logo'])
  }
  
  
  # Return the requested dataset ----
  ted <- reactive({
    
    a <- allcfb[which(allcfb$year %in% input$year),]
    
    
    cfbconf <- a[which(a$conference %in% input$conference ),]
    
    
  return(cfbconf)
    
    
  })
  
 



  
  output$cfbtable <- renderPlot({
    sec <- ted()
  
    ggplot(data = sec, mapping = aes(sec$FPI, sec$teamtalent)) + 
      xlab(label = 'ESPN FPI Rating') + ylab(label = '247 Team Talent') + 
      ggtitle("CFB Plot") + 
      geom_image(aes(image=sec$logo), size=.05) + 
      theme_bw() + 
      xlim(min(allcfb$FPI), max(allcfb$FPI)) + 
      ylim(min(allcfb$teamtalent), max(allcfb$teamtalent)) + 
      geom_smooth(method = 'lm', se = F)
     #geom_vline(xintercept = mean(allcfb$FPI)) + geom_hline(yintercept=mean(allcfb$teamtalent))
  })
  

  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)