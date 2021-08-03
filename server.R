####  Meier Cider Public version 1.0 ####

### Define server logic for Meier Cider Shiny application
shinyServer(function(input, output, session) {
  
  ##### Summary tab server logic ####################################################################
  
  ### Summary: Construct content for summaryYearSelect sidebar drop-down ####
  ### use renderUI to use textInput when there is only one year of data
  output$summaryYearSelect <- renderUI({
    if(nrow(theYears)==1){
      textInput(inputId = "summaryYearChoice", "Select a year:", value = theYears)
    } else {
      selectInput(inputId = "summaryYearChoice", "Select a year:", choices = theYears,
                  selectize = TRUE, multiple = FALSE)
    }
  })
  
  
  
  ### Summary: Create eventReactive list object with tables and bar graph ####
  summaryOutput <- eventReactive(input$summaryButton, {
    ##  Gather data for 'Batch Summary' table
    #   Get 'batches' data for selected year and reduce to desired summary columns
    tempBatches <- batches %>% 
      filter(year==input$summaryYearChoice) %>%
      select(year, batchID, batchName, initialBatchVol_L, targetStyle, startDate, initialSG, initialTA)
    
    #   Calculate total sulfites from 'ferment' data and join with 'tempBatches'
    totSulfites <- ferment %>% dplyr::group_by(batchID) %>%
      dplyr::summarise(totalSO2 = sum(sulfites))
    
    tempBatches <- inner_join(tempBatches, totSulfites, by = "batchID")
    
    #   Calculate current volume from 'ferment' data and join with 'tempBatches'
    batchVol_gal <- ferment %>% dplyr::group_by(batchID) %>%
      filter(date==max(date)) %>%
      select(batchID, volume_gal)
    
    tempBatches <- inner_join(tempBatches, batchVol_gal, by = "batchID")
    
    ##  Calculate current ABV (%) and 'daysSinceSG'
    #   Get 'ferment' data, reduce to latest measurement per batch
    tempFerment <- ferment %>% dplyr::group_by(batchID) %>%
      filter(!is.na(SG)) %>%
      filter(date==max(date)) %>%
      select(-year, -temp, -batchName, -sulfites, -volume_gal, -volume_L, -remarks) %>%
      arrange(batchID)
      
    #   Join 'tempFerment' with 'tempBatches' for calculation
    tempBatches <- inner_join(tempBatches, tempFerment, by = "batchID") %>%
      mutate(ABV = formatC((initialSG - SG)*131.25, digits = 1, format = "f")) %>%
      mutate(daysSinceSG = ifelse(action!='bottle'| is.na(action), Sys.Date()-date, "bottled")) %>%
      select(-year, -blendID, -action) %>%
      rename(latestDateSG = date, latestSG = SG) %>%
      arrange(batchID)
    
    #   Determine rackCount per batch from 'ferment' data and join with 'tempBatches'
    racking <- ferment %>% dplyr::group_by(batchID) %>%
      dplyr::filter(action=="rack") %>%
      dplyr::summarise(rackCount = n())
    
    tempBatches <- dplyr::inner_join(tempBatches, racking, by = "batchID")
    
    #   Format numeric output to include trailing zeroes if present
    tempBatches$initialTA <- formatC(tempBatches$initialTA, digits = 1, format = "f")
    tempBatches$initialSG <- formatC(tempBatches$initialSG, digits = 4, format = "f")
    tempBatches$latestSG <- formatC(tempBatches$latestSG, digits = 4, format = "f")
    tempBatches$volume_gal <- formatC(tempBatches$volume_gal, digits = 1, format = "f")
    
    #   Calculate total current volume cider for selected year
    ciderVolume <- formatC(sum(as.numeric(tempBatches$volume_gal)), digits = 1, format = "f")
    
    #   Calculate total juice pressed for selected year (gal)
    juiceVolume <- round((sum(tempBatches$initialBatchVol_L))/3.78541, digits = 1)
    
    #   Order columns and rename specific columns
    tempBatches <- tempBatches[c("batchID", "batchName", "targetStyle", "startDate", "initialTA", "initialSG", "latestSG",
                                 "latestDateSG", "daysSinceSG", "rackCount", "volume_gal", "totalSO2", "ABV")]
    
    colnames(tempBatches) <- c("batchID", "Batch Name", "Target Style", "Start Date", "Juice TA (g/L MAE)", "Juice SG",
                               "SG", "SG Date", "Days Since SG", "Rack Count", "Volume (gal)", "Total SO2 (ppm)", "ABV (%)")
    
    
    ##  Calculate total mass apples collected from 'juice' table and pressing efficiency
    #   Calculate bushels collected and filter to year
    bushelsPressed <- juice %>%
      filter(pressed == TRUE) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(bushels = sum(fruitYield)) %>%
      filter(year==input$summaryYearChoice)
      
    #   Select and store bushels value
    bushelsPressed <- bushelsPressed$bushels
    
    #   Calculate pressing efficiency
    efficiency <- round(((juiceVolume*3.78541)/(bushelsPressed*18.143695))*100, digits = 1)
    
    # Create summary dataframe for table display
    variables <- c("Current Cider Vol (gal)", "Juice Pressed (gal)", "Apples Processed (bu)", "Pressing Efficiency (%)")
    processData <- c(ciderVolume, juiceVolume, bushelsPressed, efficiency)
    processDF <- data.frame(cbind(variables, processData))
    
    
    
    ### Summarise total juice volume by treeID for selected year and create horizontal bar graph
    #   Identify all blends for selected year
    tempBlends <- blends %>% dplyr::filter(year==input$summaryYearChoice)
    
    #   Create dataframe to hold tree composition summary results
    treeComp <- data.frame()
    compWarn <- NULL
    
    #   Identify juiceIDs and juicePercent data for selected year
    for(i in 1:nrow(tempBlends)){
      juiceIDs <- unlist(tempBlends$juiceIDs[i])
      blendID <- rep(tempBlends$atBlendID[i], times = length(juiceIDs))
      blendVol <- rep(tempBlends$galTotalVol[i], times = length(juiceIDs))
      
      if(grepl("\\|", tempBlends$juicePercent[i])){
        jPercent <- stringr::str_split(tempBlends$juicePercent[i], "\\|")
        jProp <- as.numeric(jPercent[[1]])/100
      } else {
        jProp <- as.numeric(tempBlends$juicePercent[i])/100
      }
      
      if(length(juiceIDs)==length(jProp)){
        tempJuice <- data.frame(blendID, blendVol, juiceIDs, jProp, stringsAsFactors = FALSE)
        treeComp <- rbind(treeComp, tempJuice)
      } else {
        treeComp <- treeComp
        compWarn <- "Mismatch between number of juiceIDs and juicePercents for at least one blend."
      }
    } # end 'for' loop
    rm(juiceIDs, blendID, blendVol, jPercent, jProp, tempJuice)
    
    #   Calculate volume of each juice within the blends
    treeComp <- treeComp %>%
      dplyr::filter(!is.na(jProp)) %>%
      dplyr::mutate(juiceVol = blendVol*jProp)
    
    #   Join treeComp with 'juice' table to get treeNames and treePercent for each juiceID
    treeComp <- juice %>%
      dplyr::select(atJuiceID, treeName, treePercent) %>%
      dplyr::right_join(treeComp, by = c("atJuiceID"="juiceIDs"))
    
    
    
    ### Check for missing tree percent data and warn; otherwise create summary bar chart
    treeComp$compCheck <- NA
    for(i in 1:nrow(treeComp)){
      if(length(treeComp$treeName[i][[1]])==length(stringr::str_split(treeComp$treePercent[i], "\\|")[[1]])){
        treeComp$compCheck[i] <- TRUE
      } else {
        treeComp$compCheck[i] <- FALSE
      }
    }
    
    if(FALSE %in% treeComp$compCheck){
      compWarn <- paste(compWarn, "Mismatch between number of treeNames and treePercents for at least one blend.")
      treeComp <- NULL
      appleBar <- NULL
    } else {
      # For each juiceID, expand if multiple trees contribute to juice
      appleDF <- data.frame()
      for(i in 1:nrow(treeComp)){
        if(grepl("\\|", treeComp$treePercent[i])){
          tName <- unlist(treeComp$treeName[i])
          tPercent <- stringr::str_split(treeComp$treePercent[i], "\\|")
          tProp <- as.numeric(tPercent[[1]])/100
          juiceID <- rep(treeComp$atJuiceID[i], times = length(tName))
        } else {
          tName <- unlist(treeComp$treeName[i])
          tPercent <- treeComp$treePercent[i]
          tProp <- as.numeric(tPercent)/100
          juiceID <- treeComp$atJuiceID[i]
        }
        temp <- data.frame(juiceID, tName, tProp, stringsAsFactors = FALSE)
        appleDF <- rbind(appleDF, temp)
      } # end 'for' loop
      rm(tName, tPercent, tProp, juiceID, temp)
      
      # Remove duplicates from appleDF
      appleDF <- appleDF %>% distinct()
      
      treeComp <- treeComp %>%
        dplyr::select(atJuiceID, blendID, blendVol, jProp, juiceVol) %>%
        dplyr::right_join(appleDF, by = c("atJuiceID"="juiceID")) %>%
        dplyr::mutate(treeVol = juiceVol*tProp) %>%
        dplyr::group_by(tName) %>%
        dplyr::summarise(treeJuiceVol = sum(treeVol)) %>%
        dplyr::mutate(tName = paste0("  ", tName, " "))
      
      treeComp$tName <- factor(treeComp$tName, levels = unique(treeComp$tName)[order(treeComp$treeJuiceVol)])
      treeComp$treeJuiceVol <- round(treeComp$treeJuiceVol, digits = 1)
      
      # Summary bar chart: Juice Volume by Apple Variety
      appleBar <- plot_ly(data = treeComp, x = ~treeJuiceVol, y = ~tName, type = "bar", orientation = "h") %>%
        layout(
          title = "Juice Volume by Apple Variety",
          xaxis = list(title="Juice Volume (gal)"),
          yaxis = list(title="Apple Variety")
        )
    } # end 'if/else' comp check
    
    
    ##  Create output list object 
    output <- list(summaryData=tempBatches, processData=processDF, appleBar=appleBar, compWarn=compWarn)
    
  }) # end summaryOutput eventReactive
  
  
  
  ### Summary: Batch-level apple composition ####
  batchAppleComp <- reactive({
    #   Account for null input before table row is selected
    shiny::validate(
      need(!is.na(input$summaryDataTable_rows_selected) && length(input$summaryDataTable_rows_selected) != 0, "")
    )
    
    #   Identify user-selected row of summary table, corresponding batchID and batchVolume
    summaryRow <- input$summaryDataTable_rows_selected
    theBatch <- summaryOutput()$summaryData[summaryRow, "batchID"]
    theBatchVol <- as.numeric(summaryOutput()$summaryData[summaryRow, "Volume (gal)"])
    
    #   Identify blendID and batchName from 'batches' table
    theBlend <- batches[batches$batchID==theBatch, "blendID"]
    theBatchName <- batches[batches$batchID==theBatch, "batchName"]
    
    
    ##  Determine juice composition of theBlend from 'blends' table
    blendComp <- blends %>% dplyr::filter(atBlendID==theBlend)
    
    #   Identify juiceIDs and juicePercent data for theBatch based on theBlend
    blendJuiceIDs <- unlist(blendComp$juiceIDs)
    blendID <- rep(blendComp$atBlendID, times = length(blendJuiceIDs))
    batchVol <- rep(theBatchVol, times = length(blendJuiceIDs)) 
    
    if(grepl("\\|", blendComp$juicePercent)){
      jBlendPerc <- stringr::str_split(blendComp$juicePercent, "\\|")
      jBlendProp <- as.numeric(jBlendPerc[[1]])/100
    } else {
      jBlendProp <- as.numeric(blendComp$juicePercent)/100
    }
    
    blendComp <- data.frame(blendID, batchVol, blendJuiceIDs, jBlendProp, stringsAsFactors = FALSE)
    rm(blendJuiceIDs, blendID, batchVol, jBlendProp)
    
    #   Calculate volume of each juice within the theBlend
    blendComp <- blendComp %>% 
      dplyr::filter(!is.na(jBlendProp)) %>%
      dplyr::mutate(juiceVol = batchVol*jBlendProp)
    
    #   Join blendComp with 'juice' table to get treeNames and treePercent for each juiceID
    blendComp <- juice %>%
      dplyr::select(atJuiceID, treeName, treePercent) %>%
      dplyr::right_join(blendComp, by = c("atJuiceID"="blendJuiceIDs"))
    
    #   For each juiceID in theBlend, expand if multiple trees contribute to juice
    appleDF <- data.frame()
    for(i in 1:nrow(blendComp)){
      if(grepl("\\|", blendComp$treePercent[i])){
        tName <- unlist(blendComp$treeName[i])
        tPercent <- stringr::str_split(blendComp$treePercent[i], "\\|")
        tProp <- as.numeric(tPercent[[1]])/100
        juiceID <- rep(blendComp$atJuiceID[i], times = length(tName))
      } else {
        tName <- unlist(blendComp$treeName[i])
        tPercent <- blendComp$treePercent[i]
        tProp <- as.numeric(tPercent)/100
        juiceID <- blendComp$atJuiceID[i]
      }
      temp <- data.frame(juiceID, tName, tProp, stringsAsFactors = FALSE)
      appleDF <- rbind(appleDF, temp)
    } # end 'for' loop
    rm(tName, tPercent, tProp, juiceID, temp)
    
    #   Remove duplicates from appleDF
    appleDF <- appleDF %>% distinct()
    
    #   Join blendComp with appleDF and calculate juice volume by appleVariety/tree
    blendComp <- blendComp %>%
      dplyr::select(atJuiceID, blendID, batchVol, jBlendProp, juiceVol) %>%
      dplyr::right_join(appleDF, by = c("atJuiceID"="juiceID")) %>%
      dplyr::mutate(treeVol = juiceVol*tProp) %>%
      dplyr::group_by(tName) %>%
      dplyr::summarise(treeJuiceVol = sum(treeVol)) %>%
      dplyr::mutate(tName = paste0("  ", tName, " "))
    
    blendComp$tName <- factor(blendComp$tName, levels = unique(blendComp$tName)[order(blendComp$treeJuiceVol)])
    blendComp$treeJuiceVol <- round(blendComp$treeJuiceVol, digits = 1)
    
    #   Create batch composition bar chart
    batchBar <- plot_ly(data = blendComp, x = ~treeJuiceVol, y = ~tName, type = "bar", orientation = "h") %>%
      layout(
        title = paste0("Batch Composition: ", theBatchName),
        xaxis = list(title="Juice Volume (gal)"),
        yaxis = list(title="Apple Variety")
      )
    
    
    ##  Bundle output for display
    output <- list(batchBar=batchBar)
    return(output)
    
  }) # end batchAppleComp reactive()
  
  
  
  ### Summary: Create reactive outputs from 'summaryOutput' eventReactive object ####
  #   Summary Data table
  output$summaryDataTable <- DT::renderDataTable(
    DT::datatable(
      temp <- summaryOutput()$summaryData %>% select(-batchID),
      caption = "Click a row in the table to display the apple varieties in the batch.",
      escape = FALSE, filter = "none", rownames = FALSE, selection = "single",
      options = list(paging=FALSE, searching=FALSE)
    )
  )
  
  #   Title for summary table
  output$titleSummaryTable <- renderText({
    paste("Batch Summary Data:", input$summaryYearChoice, sep = " ")
  })
  
  #   Process Data table for output
  output$summaryProcessTable <- renderTable({
    summaryOutput()$processData
  }, digits=1, colnames = FALSE)
  
  #   Juice and Tree Composition warning
  output$compWarn <- renderText({
    summaryOutput()$compWarn
  })
  
  #   Bar chart: Juice volume by apple variety for year
  output$appleBar <- renderPlotly({
    summaryOutput()$appleBar
  })
  
  #   Bar chart: Juice volume by apple variety for batch
  output$batchBar <- renderPlotly({
    batchAppleComp()$batchBar
  })
  
  
  
  ### renderUI for summaryMainPanel
  output$summaryMainPanel <- renderUI({
    # Account for null input before year is selected
    shiny::validate(
      need(length(summaryOutput())!=0, "")
    )
    
    ### Define rendered output for display
    div(
      wellPanel(
      #   table title output
      span(h4(textOutput("titleSummaryTable")), style="color:#F15A29"),
      ), # End wellPanel
      
      #   dataTable output
      br(),
      fluidRow(
        column(width = 12, DT::dataTableOutput("summaryDataTable"))
        ),
      
      #   plotly barchart output
      br(),
      fluidRow(
        #column(width = 1, p("")),
        column(width = 7, plotlyOutput("appleBar", width = "100%", height = "400px")),
        column(width = 5, plotlyOutput("batchBar", width = "100%", height = "400px"))#,
        #column(width = 1, p(""))
      ),
      br(),
      fluidRow(p(""))
    ) # end div
    
  }) # End renderUI summaryMainPanel
  
  
  
  ##### Fermentation tab server logic ###############################################################
  
  ### Construct content for fermentYearSelect sidebar drop-down; use renderUI to use textInput when 
  ### there is only one year of data.
  output$fermentYearSelect <- renderUI({
    if(nrow(theYears)==1){
      textInput(inputId = "yearChoice", "Select a year:", value = theYears)
    } else {
      selectInput(inputId = "yearChoice", "Select a year:", choices = theYears,
                  selectize = TRUE, multiple = FALSE)
    }
  })
  
  
  ### Construct content for batchName sidebar drop-down
  ##  Create filtered batchID list based on selected year
  batchNames <- reactive({
    # Provide full list of batches if no 'year' is selected
    if(length(input$yearChoice)==0){
      temp <- ferment %>%
        mutate(batchNameID = paste0(year, ": ", batchName)) %>%
        select(batchNameID) %>%
        distinct()
    } else {
      # Reduce list of batchNames to chosen year
      temp <- ferment %>%
        mutate(batchNameID = paste0(year, ": ", batchName)) %>%
        filter(year==input$yearChoice) %>%
        select(batchNameID) %>%
        distinct()
    }
  })

  
  ##  Render batchSelect drop-down
  output$fermentBatchSelect <- renderUI({
    # Use textInput when list has one item to account for drop-down choice labels when list has one item
    if(nrow(batchNames())==1){
      textInput("fermentBatchChoice", "Select a batch to plot:", value = batchNames())

    # Use selectInput when there is more than one batch from which to choose
    } else {
      selectInput("fermentBatchChoice", "Select a batch to plot:", c(Choose = '', choices = batchNames()),
                  selectize = TRUE, multiple = FALSE)
    }
  })
  
  
  
  ### Create eventReactive list object with SG and FSU plots, as well as filtered ferment dataframe
  fermentOutput <- eventReactive(input$plotButton ,{
    # Extract batchName from batchSelect chosen by user
    selectedName <- stringr::str_split(input$fermentBatchChoice, ": ")
    selectedName <- selectedName[[1]][2]
    
    # Filter to obtain batchData based on selectedName
    batchData <- ferment %>% filter(year==input$yearChoice, batchName==selectedName)
    
    # Create 'batchID' for display
    selectedBatchID <- unique(batchData$batchID)
    
    
    ##  Create blend composition summary table
    #   Get juiceIDs and proportions for selected batch
    blendSelect <- unique(batchData$blendID)
    blendChoice <- blends %>% filter(blendID == blendSelect)
    
    juiceIDs <- unlist(blendChoice$juiceIDs)
    
    #   Extract corresponding juicePercent values and duplicate blendVolume required number of times
    if(grepl("\\|", blendChoice$juicePercent[1])){
      juicePercent <- stringr::str_split(blendChoice$juicePercent[1], "\\|")
      juicePercent <- as.numeric(juicePercent[[1]])
    } else {
      juicePercent <- as.numeric(blendChoice$juicePercent[1])
    }
    
    #   Check if juiceIDs and Percents match, and create dataframe; create warning if necessary
    if(length(juiceIDs)==length(juicePercent)){
      blendComp <- data.frame(juiceIDs, juicePercent, stringsAsFactors = FALSE)
      blendComp <- juice %>%
        dplyr::select(atJuiceID, juiceID, treePercent, sweatDuration, SG, juiceTA, pH) %>%
        dplyr::right_join(blendComp, by = c("atJuiceID"="juiceIDs")) %>%
        dplyr::select(-atJuiceID) %>%
        dplyr::arrange(desc(juicePercent))
      
      blendComp$sweatDuration <- formatC(blendComp$sweatDuration, digits = 0, format = "f")
      blendComp$SG <- formatC(blendComp$SG, digits = 4, format = "f")
      blendComp$juiceTA <- formatC(blendComp$juiceTA, digits = 1, format = "f")
      blendComp$pH <- formatC(blendComp$pH, digits = 1, format = "f")
      blendComp$juicePercent <- formatC(blendComp$juicePercent, digits=0, format="f")
      colnames(blendComp) <- c("juiceID", "Tree %","Sweat Duration", "SG", "TA (g/L MAE)", "pH", "Percent Blend")
      
      blendWarn <- ""
      
    } else {
      blendCompDF <- data.frame()
      blendWarn <- "Missing required juiceID and/or proportion data for selected blend"
    }
    
    
    ##  Create ferment data for plotting: Filter to remove rows with no SG data
    #   Create 'startDate', 'prevDate', 'days', and 'interval' then calculate FSU
    fermentData <- batchData %>% filter(!is.na(SG))
    
    if(nrow(fermentData)==1){
      # Manually set calculated fields when only one row of data
      fermentData$startDate <- NA
      fermentData$prevDate <- NA
      fermentData$days <- 0
      fermentData$interval <- 0
    } else {
      # Create date, interval, and prevSG fields required for FSU calculation when data rows > 1
      fermentData$startDate <- rep(min(fermentData$date), times=nrow(fermentData))
      fermentData$prevDate <- c(min(fermentData$date), fermentData[1:nrow(fermentData)-1, "date"])
      fermentData$days <- as.integer(fermentData$date - fermentData$startDate)
      fermentData$interval <- as.integer(fermentData$date - fermentData$prevDate)
      fermentData$prevSG <- c(fermentData[fermentData$date==min(fermentData$date), "SG"], fermentData[1:nrow(fermentData)-1, "SG"])
      
      # Calculate FSU and volume_L, and account for NA values in 'sulfites' column
      fermentData <- fermentData %>%
        mutate(FSU = ifelse(interval==0, 0, round(((prevSG-SG)/interval)*100000, digits=1)))
    }
    
    
    # Create SG plotly object
    sgPlotly <- plot_ly(data = fermentData, x = ~days, y = ~SG, name = 'Specific Gravity',
                        type = 'scatter', mode = 'lines+markers') %>%
      layout(
        xaxis = list(title = 'Fermentation Days'),
        yaxis = list(title = 'Specific Gravity', range = c(0.995, 1.085)),
        title = 'Specific Gravity Drop',
        showlegend = FALSE
      )
    
    # Create FSU plotly object
    fsuPlotly <- plot_ly(data = fermentData, x = ~days, y = ~FSU, name = 'Fermentation Speed',
                         type = 'scatter', mode = 'lines+markers', 
                         line = list(color = '#F15A29')) %>%
      
      layout(
        xaxis = list(title = 'Fermentation Days'),
        yaxis = list(title = 'FSU'),
        title = 'Fermentation Speed',
        showlegend = FALSE
      )
    
    # Retrieve latest data and determine volume
    latestData <- batchData %>% filter(date==max(date))
    gVolume <- latestData$volume_gal
    lVolume <- latestData$volume_L
    
    # Determine age of batch
    startData <- batchData %>% filter(date==min(date))
    age <- ifelse(latestData$action != "bottle" | is.na(latestData$action), as.integer(Sys.Date()-startData$date), "bottled")
    
    # Calculate total sulfites for display
    totSulfites <- sum(batchData$sulfites)
    
    # Calculate current ABV for display
    abv <- round((max(fermentData$SG)-min(fermentData$SG))*131.25, digits = 1)
    
    # Obtain 'initialTA', 'mustpH', 'hardTannin', 'softTannin', 'targetStyle' from 'batches' table for display
    tempBat <- batches %>% filter(batchID==selectedBatchID)
    batchSummaryData <- c(tempBat[,"initialTA"], tempBat[,"pH"], tempBat[,"hardTannin"],
                          tempBat[,"softTannin"], tempBat[,"targetStyle"])
    
    # Create summary dataframe for table display
    variables <- c("age (d)", "vol (gal)", "vol (L)", "Tot SO2 (ppm)", "ABV (%)", "Initial TA (g/L MAE)",
                   "Must pH", "Hard Tannin (rank)", "Soft Tannin (rank)", "Target Style")
    summaryData <- c(age, gVolume, lVolume, totSulfites, abv, batchSummaryData)
    summaryDF <- data.frame(cbind(variables, summaryData))
    
    ##  Create data for display
    displayData <- batchData %>% 
      select(-volume_gal, -volume_L) %>%
      mutate(days = as.integer(date - startData$date))
    
    tempFSU <- fermentData %>% select(fermentID, FSU)
    displayData <- left_join(displayData, tempFSU, by = "fermentID")
    displayData$SG <- formatC(displayData$SG, digits = 4, format = "f")
    displayData$FSU <- formatC(displayData$FSU, digits = 1, format = "f")
    
    displayData <- displayData[c("date", "days", "action", "temp", "SG", "FSU", "sulfites", "remarks")]
    
    # Create output list
    output <- list(batchID=selectedBatchID, name=selectedName, blendComp=blendComp, blendWarn = blendWarn,
                   data=displayData, sg=sgPlotly, fsu=fsuPlotly, summaryData=summaryDF)
    return(output)

  })
  
  
  
  ### Create outputs for 'mainPanel' renderUI
  #   Batch identifiers for display above data plots
  output$selectedBatchID <- renderText({
    fermentOutput()$batchID
  })
  
  output$selectedBatchName <- renderText({
    fermentOutput()$name
  })
  
  #   Summary table
  output$summaryTable <- renderTable({
    fermentOutput()$summaryData
  }, digits=1, colnames = FALSE)
  
  #   Blend Comp table
  output$blendComp <- renderTable({
    fermentOutput()$blendComp
  })
  
  #   Blend warning
  output$blendWarn <- renderText({
    fermentOutput()$blendWarn
  })
  
  #   SG plotly object
  output$sgPlot <- renderPlotly({
    fermentOutput()$sg
  })
  
  #   FSU plotly object
  output$fsuPlot <- renderPlotly({
    fermentOutput()$fsu
  })
  
  #   Data table
  output$selectedBatchData <- DT::renderDataTable(
    DT::datatable(
      fermentOutput()$data,
      escape = FALSE, filter = "bottom", rownames = FALSE,
      options = list(paging=FALSE)
    )
  )
  
  
  
  ### Organize fermentMainPanel outputs from above for display via renderUI
  output$fermentMainPanel <- renderUI({
    # Account for null input before batch is selected
    shiny::validate(
      need(length(fermentOutput())!=0, "")
    )
    
    ### Define rendered output for display
    div(
    wellPanel(
      #   fluidRow for batchName and batchID
      fluidRow(
        column(width = 6,
               span(h4("Batch Name:"), style="color:#F15A29"),
               h4(textOutput("selectedBatchName"))), # column for name
        column(width = 6,
               span(h4("Batch ID:"), style="color:#F15A29"),
               h4(textOutput("selectedBatchID"))) #  column for batchID
    ), # end fluidRow
    
    #   fluidRow for Blend Comp table
    fluidRow(
      column(width = 12,
             span(h4("Batch Composition:"), style="color:#F15A29"),
             tableOutput("blendComp"),
             textOutput("blendWarn")
             ) #  column for Blend Comp
    ) # end fluidRow
    
    ), #  end wellPanel
    
    #   fluidRow for SG and FSU plotly objects
    br(),
    span(h4("Fermentation Plots"), style="color:#F15A29"),
    fluidRow(
      column(width=6, plotlyOutput("sgPlot")),
      column(width=6, plotlyOutput("fsuPlot"))
    ), # end second fluidRow
    
    #   dataTable output
    br(),
    span(h4("Fermentation Data"), style="color:#F15A29"),
    DT::dataTableOutput("selectedBatchData")
    ) # end div
    
  }) #  End renderUI:fermentMainPanel
  

  
  
  ##### Sulfite tab server logic ####################################################################
  
  ### Calculate 'SO2ppm' and 'KMS g L-1' based on pH input ##########################################
  
  ##  Determine SO2ppm and KMSgPerL values given pHInput from user
  so2 <- reactive({
    # Get SO2ppm value
    s <- round(predict(l, input$pHChoice), digits = 0)
    
    # Get KMS value
    kms <- (s/1000)*2
    
    # Bundle reactive output
    out <- list(s=s, kms=kms)
    return(out)
  })
  
  output$so2Output <- renderText({
    so2()$s
  })
  
  output$kmsOutput <- renderText({
    so2()$kms
  })
  
 
  ##  Create output plotly object
  output$pHPlot <- renderPlotly({
    p <- pHPlotly
    p <- add_markers(pHPlotly, x = ~input$pHChoice, y = ~so2()$s, showlegend = FALSE, 
                     marker=list(color='#F15A29', size=15,
                                 line = list(color = '#BFBFBF', width = 1)
                                 )
                     ) 
    return(p)
    
  })
  
  
  
  ### Calculate 'KMS g' and 'Campden Num' based on targetSO2 input and volume inputs ################
  
  ##  Determine input cider volume from user in both gallons and liters
  #   Liters calculation
  litersCider <- reactive({
    if (input$units=='gallons') {
      v <- input$quantity*3.78541
    } else {
      v <- input$quantity
    }
    return(v)
  })
  
  #   Gallons calculation
  gallonsCider <- reactive({
    if (input$units=='liters') {
      v <- input$quantity*0.264172
    } else {
      v <- input$quantity
    }
    return(v)
  })
  
  
  ##  Calculate 'KMS g' from targetSO2 and volume input
  massKMS <- eventReactive(input$so2Button, {
    g <- litersCider()*(input$targetSO2/1000)*2
  })
  
  output$kmsGrams <- renderText({
    if(input$so2Button==0){
      k <- "NA"
      k
    } else {
    k <- round(massKMS(), digits = 2)
    k
    }
  })
  
  
  ##  Calculate 'campdenNum' from targetSO2 and volume input
  campdenCount <- eventReactive(input$so2Button, {
    n <- (input$targetSO2*gallonsCider())/60
  })
  
  output$campdenNum <- renderText({
    if(input$so2Button==0){
      c <- "NA"
      c
    } else {
      c <- round(campdenCount(), digits = 1)
      c
    }
  })
  
  
  
  
  ##### Bottling: Server logic ###################################################################
  
  ### Obtain and display carbonation values for user-selected quantity of fermentable sugar
  ##  Create reactive variable for carbonation data retrieval using loess models
  carb <- reactive({
    # Get 'Dissolved CO2' value
    dCO2_l <- round(predict(dCO2_loess, input$sugarChoice), digits = 2)
    
    # Get 'SG drop' value
    sgDrop_l <- round(predict(sg_loess, input$sugarChoice), digits = 4)
    
    # Get 'DAPppm' value
    dap_l <- round(predict(dap_loess, input$sugarChoice), digits = 1)
    
    # Determine Carbonation Style
    carbStyle <- if(input$sugarChoice < 1.25){
      c <- "still"
    } else if(input$sugarChoice < 2.95) {
      c <- "perlant"
    } else if(input$sugarChoice < 10.25){
      c <- "pétillant"
    } else if(input$sugarChoice < 21.05){
      c <- "sparkling"
    } else {
      c <- "explosive"
    }
    
    # Bundle reactive output
    out <- list(dCO2 = dCO2_l, sgDrop = sgDrop_l, dap = dap_l, carbStyle = carbStyle)
    return(out)
  })
  
  
  ##  Render loess model output for display in sidebarPanel
  #   Dissolved CO2 output
  output$dissolvedCO2 <- renderText({
    carb()$dCO2
  })
  
  #   SG Drop output
  output$sgDrop <- renderText({
    carb()$sgDrop
  })
  
  #   DAPppm output
  output$dap <- renderText({
    carb()$dap
  })
  
  #   Carb Style output
  output$carbStyle <- renderText({
    carb()$carbStyle
  })
  
  
  
  ### Bottling: Calculate 'Priming Sugar g' and 'DAP g' based on volume fermentable sugar inputs ################
  
  #   Determine input cider volume in liters
  bottlingLitersCider <- reactive({
    if (input$ciderVolUnits=='gallons') {
      v <- input$ciderVolume*3.78541
    } else {
      v <- input$ciderVolume
    }
    return(v)
  })

  #   Calculate 'sugarMass' output from 'sgTargetDrop' and 'ciderVolume' inputs
  sugarMass <- eventReactive(input$carbButton, {
    sug_l <- round(predict(sugDrop_loess, input$sgTargetDrop), digits = 2)
    s <- bottlingLitersCider()*sug_l
  })
  
  output$sugarGrams <- renderText({
    if(input$carbButton==0){
      sg <- "NA"
      sg
    } else {
      sg <- round(sugarMass(), digits = 2)
      sg
    }
  })
  
  #   Calculate 'dapMass' output from 'sgTargetDrop' and 'ciderVolume' inputs
  dapMass <- eventReactive(input$carbButton, {
    dapDrop_l <- round(predict(dapDrop_loess, input$sgTargetDrop), digits = 1)
    d <- bottlingLitersCider()*(dapDrop_l/1000)
  })
  
  output$dapGrams <- renderText({
    if(input$carbButton==0){
      d <- "NA"
      d
    } else {
      d <- round(dapMass(), digits = 2)
      d
    }
  })
  
  #   Calculate 'bottleNumber' output from 'ciderVolume' and 'bottleVolume' inputs
  mlBottleVol <- reactive({
    if (input$bottleVolUnits=='oz') {
      mL <- input$bottleVolume*29.5735
    } else {
      mL <- input$bottleVolume
    }
    return(mL)
  })
  
  bottleNum <- eventReactive(input$carbButton, {
    b <- (bottlingLitersCider()*1000)/mlBottleVol()
  })
  
  output$bottleNumber <- renderText({
    if(input$carbButton==0){
      b <- "NA"
      b
    } else {
      b <- round(bottleNum(), digits = 1)
      b
    }
  })
  
  
  
  ### Create plotly output for carbonation
  output$carbPlot <- renderPlotly({
    p <- carbPlotly
    p <- add_markers(carbPlotly, x = ~input$sugarChoice, y = ~carb()$dCO2, showlegend = FALSE,
                     marker = list(color='#F15A29', size=15,
                                   line = list(color = '#BFBFBF', width = 1)
                                   )
    )
    return(p)
  })
  
  
  
  ### Render NCMH table for output to mainPanel
  output$carbTableNCMH <- renderTable({
    carbNCMH
  })
  
  
  
  
  
  ##### Tree Map: Server logic ###################################################################
  
  
  ### Tree Map: Define input 'trees' dataset based on user input ####
  treeData <- reactive({
    shiny::req(
      input$mapFilterRadio
    )
    
    
    # Use input$mapFilterRadio to determine which data to retrieve and labels to create
    if(input$mapFilterRadio=="trait"){
      shiny::validate(
        need(
          input$treeTypePicker!="" &&
          input$accessPicker!="" &&
          input$statusPicker!="" &&
          input$fruitYearChoice!="", ""
        )
      )
      
      ##  Filter 'trees' data by traits
      if(is.null(input$treeFinder)){
        temp <- trees %>% filter(
          treeType %in% input$treeTypePicker,
          locationAccess %in% input$accessPicker, 
          statusAccess %in% input$statusPicker,
          !!as.symbol(input$fruitYearChoice) %in% input$fruitSetPicker
        )
      } else {
        temp <- trees %>% filter(treeName %in% input$treeFinder)
      }
      
      ##  Define required outputs not used when input$mapFilterRadio=="trait"
      mapBatchSumm <- NA
      mapWarn <- NA
      
      
      ##  Filter 'trees' data by batch composition
    } else if(input$mapFilterRadio=="batch"){
      ##  Account for blank mapBatchChoice input
      shiny::validate(
        need(input$mapBatchChoice!="", "")
      )
      
      #   Get 'year' and 'batchName' from input$mapBatchChoice
      yearChoice <- as.numeric(stringr::str_split(input$mapBatchChoice, pattern = ": ")[[1]][1])
      batchChoice <- stringr::str_split(input$mapBatchChoice, pattern = ": ")[[1]][2]
      
      
      ##  Look-up batch data for summary table and juice comp calculations
      mapBatchVol <- round(batches[batches$year==yearChoice & batches$batchName==batchChoice, "initialBatchVol_L"] * 0.26417, digits = 2)
      mapBlend <- batches[batches$year==yearChoice & batches$batchName==batchChoice, "blendID"]
      mapBatchSG <- formatC(batches[batches$year==yearChoice & batches$batchName==batchChoice, "initialSG"], digits = 4, format = "f")
      mapBatchTA <- formatC(batches[batches$year==yearChoice & batches$batchName==batchChoice, "initialTA"], digits = 2, format = "f")
      mapBatchPH <- formatC(batches[batches$year==yearChoice & batches$batchName==batchChoice, "pH"], digits = 2, format = "f")
      mapBatchHT <- formatC(batches[batches$year==yearChoice & batches$batchName==batchChoice, "hardTannin"], digits = 0, format = "f")
      mapBatchST <- formatC(batches[batches$year==yearChoice & batches$batchName==batchChoice, "softTannin"], digits = 0, format = "f")
      mapBatchStyle <- batches[batches$year==yearChoice & batches$batchName==batchChoice, "targetStyle"]
      
      #   Create variable names for display
      mapBatchVars <- c("Volume (gal)", "Init SG", "Init TA (g/L MAE)", "Init pH", "Hard Tannin Rank", "Soft Tannin Rank", "Target Style")
      mapBatchSumm <- data.frame(cbind(mapBatchVars, c(mapBatchVol, mapBatchSG, mapBatchTA, mapBatchPH, mapBatchHT, mapBatchST, mapBatchStyle)))
      
      
      ##  Determine juice composition of mapBlend from 'blends' table
      mapBlendComp <- blends %>% dplyr::filter(atBlendID==mapBlend)
      mapJuiceIDs <- unlist(mapBlendComp$juiceIDs)
      
      #   Extract juice proportion data from mapBlendComp record
      if(grepl("\\|", mapBlendComp$juicePercent)){
        mapBlendPerc <- stringr::str_split(mapBlendComp$juicePercent, "\\|")
        mapBlendProp <- as.numeric(mapBlendPerc[[1]])/100
      } else {
        mapBlendProp <- as.numeric(mapBlendComp$juicePercent)/100
      }
      
      #   Create data frame with blendID, batchVolume, and juice proportions
      blendID <- rep(mapBlendComp$atBlendID, times = length(mapJuiceIDs))
      batchVol <- rep(mapBatchVol, times = length(mapJuiceIDs))
      mapBlendComp <- data.frame(blendID, batchVol, mapJuiceIDs, mapBlendProp, stringsAsFactors = FALSE)
      rm(mapJuiceIDs, blendID, batchVol, mapBlendProp)
      
      #   Calculate volume of each juice within mapBlend
      mapBlendComp <- mapBlendComp %>%
        dplyr::filter(!is.na(mapBlendProp)) %>%
        dplyr::mutate(mapJuiceVol = batchVol * mapBlendProp)
      
      #   Join mapBlendComp with 'juice' table to get treeNames and treePercent for each juiceID
      mapBlendComp <- juice %>%
        dplyr::select(atJuiceID, treeName, treePercent) %>%
        dplyr::right_join(mapBlendComp, by = c("atJuiceID"="mapJuiceIDs"))
      
      #   For each juiceID in the selected blend, expand if multiple trees contribute to juice
      appleDF <- data.frame()
      for(i in 1:nrow(mapBlendComp)){
        if(grepl("\\|", mapBlendComp$treePercent[i])){
          tName <- unlist(mapBlendComp$treeName[i])
          tPercent <- stringr::str_split(mapBlendComp$treePercent[i], "\\|")
          tProp <- as.numeric(tPercent[[1]])/100
          juiceID <- rep(mapBlendComp$atJuiceID[i], times = length(tName))
        } else {
          tName <- unlist(mapBlendComp$treeName[i])
          tPercent <- mapBlendComp$treePercent[i]
          tProp <- as.numeric(tPercent)/100
          juiceID <- mapBlendComp$atJuiceID[i]
        }
        temp <- data.frame(juiceID, tName, tProp, stringsAsFactors = FALSE)
        appleDF <- rbind(appleDF, temp)
      } # end 'for' loop
      rm(tName, tPercent, tProp, juiceID, temp)
      
      #   Remove duplicates from appleDF
      appleDF <- appleDF %>% distinct() ######--------------------------------> There's something wrong here; see Trello card https://trello.com/c/Q80vTy7d
      
      #   Join mapBlendComp with appleDF and calculate juice volume by tree
      mapBlendComp <- mapBlendComp %>%
        dplyr::select(atJuiceID, blendID, batchVol, mapBlendProp, mapJuiceVol) %>%
        dplyr::right_join(appleDF, by = c("atJuiceID"="juiceID")) %>%
        dplyr::mutate(treeVol = round(mapJuiceVol*tProp, digits = 1)) %>%
        dplyr::group_by(tName) %>%
        dplyr::summarise(treeJuiceVol = sum(treeVol))
      
      #   Join mapBlendComp with 'trees' to bring in data required for mapping; filter NAs to remove purchased juice
      #   with remove==TRUE in 'trees' dataframe
      mapBlendComp <- mapBlendComp %>%
        dplyr::left_join(trees, by = c("tName"="treeName")) %>%
        dplyr::rename(treeName = tName) %>%
        dplyr::select(treeName, treeJuiceVol, latitude, longitude, treeType, treeSize, address, locationDescription,
                      locationAccess, statusAccess, meanSG, meanTA, fruitSet2021, remove)
      
      #   Create message if no data exist - i.e., all or some of juice was purchased for batch
      if(NA %in% mapBlendComp$latitude | NA %in% mapBlendComp$longitude){
        temp <- mapBlendComp %>% filter(!is.na(latitude) & !is.na(longitude))
        if(nrow(temp)==0){
          mapWarn <- "All juice purchased, no trees displayed."
        } else {
          purchaseCount <- nrow(mapBlendComp)-nrow(temp)
          mapWarn <- glue::glue("Juice from {purchaseCount} varieties purchased and not shown.")
        }
        
      } else {
        temp <- mapBlendComp
        mapWarn <- NULL
      }
      
    } # end 'if' for mapFilterRadio=='batch'
    
    
    ##  Create map labels for filtered trees
    if(input$mapFilterRadio=="trait"){
      mapLabs <- lapply(seq(nrow(temp)), function(i) {
        paste0( '<p>', "Name: ", temp[i, "treeName"], '<br/>',
                "Type: ", temp[i, "treeType"], '<br/>',
                "Location: ", temp[i, "locationAccess"], '<br/>',
                "Permission: ", temp[i, "statusAccess"], '<br/>',
                "Crop: ", temp[i, "fruitSet2021"], '<br/>',
                "SG: ", temp[i, "meanSG"], '<br/>',
                "TA (MAE): ", temp[i, "meanTA"], '</p>') 
      })
    }
    
    # Add 'treeJuiceVol' to label if filtering by batch
    if(input$mapFilterRadio=="batch"){
      mapLabs <- lapply(seq(nrow(temp)), function(i) {
        paste0( '<p>', "Name: ", temp[i, "treeName"], '<br/>',
                "Type: ", temp[i, "treeType"], '<br/>',
                "Location: ", temp[i, "locationAccess"], '<br/>',
                "Permission: ", temp[i, "statusAccess"], '<br/>',
                "Crop: ", temp[i, "fruitSet2021"], '<br/>',
                "Juice Vol: ", temp[i, "treeJuiceVol"], '<br/>',
                "SG: ", temp[i, "meanSG"], '<br/>',
                "TA (MAE): ", temp[i, "meanTA"], '</p>') 
      })
    }
    
    
    ##  Create list output
    output <- list(data=temp, mapLabs=mapLabs, mapWarn=mapWarn, mapBatchSumm=mapBatchSumm)
  })
  
  
  
  ### Tree Map: Define color palette for mapped trees based on user input ####
  treeColor <- reactive({
    # Use 'req' to only compute when required variables variables present
    shiny::req(
      input$mapFilterRadio,
      input$treeChemRadio,
      treeData()
    )
    
    
    ### Define colors when filtering by Trait Data
    if(input$mapFilterRadio=="trait"){
    
    ##  Colors to use when multiple trees filtered via pickerInputs are shown
    if(is.null(input$treeFinder)){
      # Create map color palette if input$treeChemRadio=="sugar"
      if(input$treeChemRadio=="sugar"){
        colorPal <- colorNumeric(palette = "YlGnBu", domain = treeData()$data$meanSG, na.color = "#B3B3B3")
        colorVal <- treeData()$data$meanSG
        legendTitle <- "Mean SG"
      }
      
      # Create map color palette if input$treeChemRadio=="acid"
      if(input$treeChemRadio=="acid"){
        colorPal <- colorNumeric(palette = "Reds", domain = treeData()$data$meanTA, na.color = "#B3B3B3")
        colorVal <- treeData()$data$meanTA
        legendTitle <- "Mean TA"
      }
      
    ##  Colors to use when treeFinder is used  
    } else {
      colorPal <- "#339900"
      colorVal <- NA
      legendTitle <- NA
    }
      
    
    
    ### Define colors when filtering by Batch Data
    } else if(input$mapFilterRadio=="batch"){
      
      
      ##  Colors to use when multiple tree types used for batch
      if(nrow(treeData()$data) > 2){
        # Create map color palette if input$treeChemRadio=="sugar"
        if(input$treeChemRadio=="sugar"){
          colorPal <- colorNumeric(palette = "YlGnBu", domain = treeData()$data$meanSG, na.color = "#B3B3B3")
          colorVal <- treeData()$data$meanSG
          legendTitle <- "Mean SG"
        }
        
        # Create map color palette if input$treeChemRadio=="acid"
        if(input$treeChemRadio=="acid"){
          colorPal <- colorNumeric(palette = "Reds", domain = treeData()$data$meanTA, na.color = "#B3B3B3")
          colorVal <- treeData()$data$meanTA
          legendTitle <- "Mean TA"
        }
      } # end colors for multiple tree types
      
      
      ##  Color to use when batch 2 or fewer apple types
      if(nrow(treeData()$data) <= 2){
        colorPal <- "#339900"
        colorVal <- NA
        legendTitle <- NA
      } # end color for single-varietal
      
    }# end Trait Data color conditional
    
    output <- list(color = colorPal, values = colorVal, title = legendTitle)
    return(output)
  })
  
  
  
  ### Tree Map: Render base leaflet map object for output ####
  output$treeMap <- renderLeaflet({
    leaflet(trees) %>% 
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  
  
  
  ### Tree Map: Leaflet proxy to add points and legend with specified color to base map ####
  observe({
    pal <- treeColor()$color
    value <- treeColor()$values
    title <- treeColor()$title
    treeDat <- treeData()$data
    labs <- treeData()$mapLabs
    
    
    ##  Symbols to add when trees filtered by Trait Data
    if(input$mapFilterRadio=="trait"){
      #   Symbols to add when multiple trees filtered via pickerInputs are shown
      if(is.null(input$treeFinder)){
        leafletProxy("treeMap", data = treeDat) %>%
          clearMarkers() %>%
          addCircleMarkers(lng = ~longitude,
                           lat = ~latitude,
                           stroke = TRUE,
                           color = "#808080",
                           opacity = 0.9,
                           weight = 1,
                           radius = 8,
                           fillOpacity = 0.8,
                           label = lapply(labs, htmltools::HTML),
                           fillColor = ~pal(value)
          ) %>%
          clearControls() %>%
          addLegend(position = "bottomleft", pal = pal, values = value, title = title)
        
        #   Remove legend when treeFinder is used
      } else {
        leafletProxy("treeMap", data = treeDat) %>%
          clearMarkers() %>%
          addCircleMarkers(lng = ~longitude,
                           lat = ~latitude,
                           stroke = TRUE,
                           color = "#808080",
                           opacity = 0.9,
                           weight = 1,
                           radius = 8,
                           fillOpacity = 0.8,
                           label = lapply(labs, htmltools::HTML),
                           fillColor = pal
          ) %>%
          clearControls()
        }
    } # end Trait Data conditional
    
    
    ##  Symbols to add when trees filtered by Batch Data
    if(input$mapFilterRadio=="batch"){
      # Add legend when 3 or more tree types used in batch
      if(nrow(treeDat) > 2){
        leafletProxy("treeMap", data = treeDat) %>%
          clearMarkers() %>%
          addCircleMarkers(lng = ~longitude,
                           lat = ~latitude,
                           stroke = TRUE,
                           color = "#808080",
                           opacity = 0.9,
                           weight = 1,
                           radius = ~(log(treeJuiceVol)+3)*3, # Empirical scaling factor
                           fillOpacity = 0.8,
                           label = lapply(labs, htmltools::HTML),
                           fillColor = ~pal(value)
          ) %>%
          clearControls() %>%
          addLegend(position = "bottomleft", pal = pal, values = value, title = title)
      } else {
        leafletProxy("treeMap", data = treeDat) %>%
          clearMarkers() %>%
          addCircleMarkers(lng = ~longitude,
                           lat = ~latitude,
                           stroke = TRUE,
                           color = "#808080",
                           opacity = 0.9,
                           weight = 1,
                           radius = ~(log(treeJuiceVol)+3)*3, # Empirical scaling factor
                           fillOpacity = 0.8,
                           label = lapply(labs, htmltools::HTML),
                           fillColor = pal
          ) %>%
          clearControls()
      }
    } # End Batch Data conditional
    
  }) # End Tree Map: Observe for symbol plotting
  
  
  
  ### Tree Map: Render batch data and warnings for display ####
  #   Render warning if some juice not mapped
  output$mapWarn <- renderText({
    treeData()$mapWarn
  })
  
  #   Render map batch summary table
  output$mapBatchData <- renderTable({
    treeData()$mapBatchSumm
  }, colnames = FALSE)
  
  #   Render summary data and UI elements
  output$mapBatch <- renderUI({
    req(input$mapBatchChoice)
    
    div(
      h5("Batch Summary Data"),
      tableOutput("mapBatchData")
    )
  })
  
  
  
  # ### Tree Map: Create download file of filtered map data ####
  # # Create download .csv
  # output$downloadTrees <- downloadHandler(
  #   filename = function() { 
  #     paste0(paste("ciderTrees", Sys.Date(), sep = "_"), '.csv') 
  #   },
  #   content = function(file) {
  #     temp <- treeData()$data %>%
  #       select(treeName, latitude, longitude, address, locationDescription, locationAccess, statusAccess, 
  #              treeSize, treeType, remarks, !!as.symbol(input$fruitYearChoice), remove) %>%
  #       arrange(treeName)
  #     write.csv(temp, file, row.names = FALSE)
  #   }
  # )
  
  
  
  ### Tree Map: RenderUI for Map Controls based on input$mapFilterRadio ####
  output$mapControls <- renderUI({
    # Default 'trait' choice UI controls
    if(input$mapFilterRadio=="trait"){
      div(
        h4("Filter by tree traits"),
        
        wellPanel(
        # Selectize-input: Tree Name
        h5("Find trees by name:"),
        selectizeInput(inputId = "treeFinder",
                       label = NULL,
                       choices = treeIDs,
                       multiple = TRUE,
                       options = list(
                         placeholder = 'Enter tree name(s)'
                       )),
        
        ##  Conditional panel to hide color chooser and pickerInputs when value entered into treeFinder
        conditionalPanel(
          condition = "input.treeFinder==''",
            # Multi-select: Tree type
            h5("Filter trees by type:"),
            pickerInput(inputId = "treeTypePicker", label = NULL, choices = types, selected = types,
                        options = list(`actions-box` = TRUE), multiple = TRUE),
            # Multi-select: Public/private accessibility
            h5("Filter trees by access:"),
            pickerInput(inputId = "accessPicker", label = NULL, choices = access, selected = access,
                        options = list(`actions-box` = TRUE), multiple = TRUE),
            # Multi-select: Permission status
            h5("Filter trees by permission:"),
            pickerInput(inputId = "statusPicker", label = NULL, choices = status, selected = status,
                        options = list(`actions-box` = TRUE), multiple = TRUE),
            # Single-select: Fruit-set year
            h5("Filter trees by fruit set:"),
            selectInput(inputId = "fruitYearChoice", label = NULL, choices = fruitSetYears,
                        selectize = TRUE, multiple = FALSE),
            # Multi-select: Fruit-set status by selected year
            h5("Filter by fruit-set status:"),
            pickerInput(inputId = "fruitSetPicker", label = NULL, choices = fruitYearStatus,
                        selected = fruitYearStatus, options = list(`actions-box` = TRUE), multiple = TRUE)
          ) # End conditionalPanel
        ) # end wellPanel
      ) # end div
    } else if (input$mapFilterRadio=="batch"){
      div(
        h4("Filter by batch composition"),
        h5("Select a year and batch to map:"),
        fluidRow(
          column(width = 4, uiOutput("mapYearSelect")),
          column(width = 7, uiOutput("mapBatchSelect")),
          column(width = 1, p(""))
        ),
        uiOutput("mapBatch"),
        textOutput("mapWarn"),
        tags$head(tags$style("#mapWarn{color:#C40000}"))
      ) # end div
    } # end 'if' statement
  })
  
  
  
  ### Construct content for mapYearSelect sidebar drop-down; use renderUI to use textInput when 
  ### there is only one year of data.
  output$mapYearSelect <- renderUI({
    if(nrow(theYears)==1){
      textInput(inputId = "mapYearChoice", label = NULL, value = theYears)
    } else {
      selectInput(inputId = "mapYearChoice", label = NULL, choices = theYears,
                  selectize = TRUE, multiple = FALSE)
    }
  })
  
  
  ### Construct content for batchName sidebar drop-down
  ##  Create filtered batchID list based on selected year
  mapBatchNames <- reactive({
    # Provide full list of batches if no 'year' is selected
    if(length(input$mapYearChoice)==0){
      temp <- ferment %>%
        mutate(batchNameID = paste0(year, ": ", batchName)) %>%
        select(batchNameID) %>%
        distinct()
    } else {
      # Reduce list of batchNames to chosen year
      temp <- ferment %>%
        mutate(batchNameID = paste0(year, ": ", batchName)) %>%
        filter(year==input$mapYearChoice) %>%
        select(batchNameID) %>%
        distinct()
    }
  })
  
  
  ##  Render batchSelect drop-down
  output$mapBatchSelect <- renderUI({
    # Use textInput when list has one item to account for drop-down choice labels when list has one item
    if(nrow(mapBatchNames())==1){
      textInput(inputId = "mapBatchChoice", label = NULL, value = mapBatchNames())
      
      # Use selectInput when there is more than one batch from which to choose
    } else {
      selectInput(inputId = "mapBatchChoice", label = NULL, c(Choose = '', choices = mapBatchNames()),
                  selectize = TRUE, multiple = FALSE)
    }
  })
  
  
  
  ### Tree Map: RenderUI for mainPanel content after authentication successful ####
  output$treesUI <- renderUI({
    # Define div for tab
    div(class = "outer", style = "padding-top:55px;",
    
    # Render leaflet map
    leafletOutput(outputId = "treeMap", width = "100%", height = "950px"),
    
    # Create floating control panel
    absolutePanel(id = "mapControls", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                  top = 140, left = "auto", right = 20, bottom = "auto", width = 350, height = "auto",
                  style = "opacity: 0.9",
                  
                  # div to organize contents
                  div(style="text-align: center; padding-top:10px;",
                  
                  # Meier Cider logo
                  img(src = "meierCiderLogo_small.png"),
                  br(),
                  
                  div(style = "text-align: left; padding-top:20px; padding-left:20px;",
                  
                  # Parent radio buttons to determine how trees are filtered
                  h5("Find trees by:"),
                  radioButtons(inputId = "mapFilterRadio", label = NULL, inline = TRUE, width = "80%",
                               choiceNames = c("Trait Data", "Batch Data"), choiceValues = c("trait", "batch"),
                               selected = "trait"),
                  # Radio buttons: Tree chemistry color coding
                  h5("Color trees by:"),
                  radioButtons(inputId = "treeChemRadio", label = NULL, inline = TRUE, width = "80%",
                               choiceNames = c("Sugar (SG)", "Acid (MAE g/L)"), choiceValues = c("sugar", "acid"),
                               selected = "sugar"),
                      
                  uiOutput("mapControls"),
                  br(),
                  
                  # # Tree Map download button
                  # downloadButton('downloadTrees', 'Download Trees', class="btn btn-primary"),
                  fluidRow(p())
                  ) # End div-left
                  ) # End div-centered
                  ) # End absolutePanel
    
    ) # End div
  }) # End renderUI
  
  
}) #  End shinyServer function

