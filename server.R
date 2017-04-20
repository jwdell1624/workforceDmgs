shinyServer(function(input, output, session) {
  
  # DYNAMIC UI -------------------------------------------------------------------------------------
  
  # TODO - remove WAdashboard naming conventions e.g. from "wddSelOrg" to "selOrg", "wddDataset" etc.
  # TODO - remove explicit printing of 'ttl' see wiki for a code example
  # Dynamic UI for Location/Group plot
  output$msg <- renderUI({
    
    data.msg2()
    
  })
  
  
  output$dt <- renderUI({
    
    snpsht_dt <- format(max(df$Snapshot_Date), "%d.%m.%Y")
    paste0("Data is as at ", snpsht_dt)
    
  })
  
  output$locnGrpTitle <- renderUI({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
      
      if (input$wddSelOrg != "Site"){
        ttl <- "Position Location" 
      } else { 
        ttl <- "Group by BSL"
      }
      # print object
      ttl
      
    })
    
  })
  
  # dynamic ui for Job Family/Group plot
  output$jfGrpTitle <- renderUI({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
      
      if (input$wddSelOrg != "Job Family"){
        ttl <- "Job Family" 
      } else { 
        ttl <- "Group by BSL"
      }
      # print object
      ttl
      
    })
    
  })
  
  # DATA SUBSET ------------------------------------------------------------------------------------
  
  # TODO remove explicit printing of "dfSbst"
  # Subset all data by employment type option
  wddDataset <- reactive({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
      
      if (input$wddSelEmpTyp == "All Employment Types"){
        dfSbst <- df
      } else {
        dfSbst <- subset(df, df$Perm_Temp == input$wddSelEmpTyp)
      }
      # print dataframe
      dfSbst
      
    })
    
  })
  
  # Then subset by lens option
  wddDataset2 <- reactive({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
      switch(input$wddSelOrg
             , "ATO"             = wddDataset()
             , "Group"           = subset(wddDataset(), wddDataset()$Subplan == input$wddSelGrp)
             , "BSL"             = subset(wddDataset(), wddDataset()$BSL == input$wddSelBSL)
             , "Branch"          = subset(wddDataset(), wddDataset()$Org_Unit_Branch == input$wddSelBranch)
             , "Team/Org.Unit"   = subset(wddDataset(), wddDataset()$Org_Unit_Team == input$wddSelTeam)
             , "Cost Centre"     = subset(wddDataset(), wddDataset()$Cost_Centre_Code == input$wddSelCstCntr)
             , "Classification"  = subset(wddDataset(), wddDataset()$Actual_Classification == input$wddSelClassn)
             , "Job Family"      = subset(wddDataset(), wddDataset()$Job_Family == input$wddSelJob)
             , "Site"            = subset(wddDataset(), wddDataset()$Position_Location == input$wddSelSite)
             , "Manager"         = subset(wddDataset(), wddDataset()$Manager_Indicator == input$wddSelMgr)
             , "Gender"          = subset(wddDataset(), wddDataset()$Gender == input$wddSelGndr)
             , "NESB"            = subset(wddDataset(), wddDataset()$NESB_Sum == input$wddSelNESB)
             , "Disability"      = subset(wddDataset(), wddDataset()$Disability_HC == input$wddSelDsbl)
             , "Indigenous"      = subset(wddDataset(), wddDataset()$Indigenous_HC == input$wddSelIndg)
      )
    })
  })
  
  # DATA WRANGLING LOGIC ----------------------------------------------------------------
  
  # TODO - create fn to adhere to DRY when creating HC/% using dplyr e.g. :
  # demoFn <- function(df, columns){
  #   df() %>% 
  #     select(columns) %>% 
  #     group_by(columns) %>% 
  #     summarise(HC = n()) %>% 
  #     ungroup() %>% 
  #     mutate(Percent = round(HC/sum(HC)*100, 2)) -> x
  #  return(x)
  # }
  
  # Group/bsl data
  wddOrgPrfl <- reactive({
    
    wddDataset2() %>% 
      select(Subplan
             , BSL) %>% 
      group_by(Subplan
               , BSL) %>% 
      summarise(HC = n()) %>% 
      ungroup() %>% 
      mutate(Percent = round(HC/sum(HC)*100, 2)) ->
    orgPrfl
    
  })
  
  # Age/tenure data 
  wddAgeTnrPrfl <- reactive({
    
    wddDataset2() %>% 
      select(Age_Range_5yr
             , ATO_Tenure_Range) %>% 
      group_by(Age_Range_5yr
               , ATO_Tenure_Range) %>% 
      summarise(HC = n()) %>% 
      ungroup() %>% 
      mutate(Percent = round(HC/sum(HC)*100, 2)) ->
    ageTnrPrfl  
    
  })
  
  # Age data - 5yrs
  wddAgePrfl <- reactive({
    
    wddDataset2() %>% 
      select(Age_Range_5yr) %>% 
      group_by(Age_Range_5yr) %>%
      summarise(HC = n()) %>% 
      ungroup() %>% 
      mutate(Percent = round(HC/sum(HC)*100, 2)) ->
    agePrfl
    
  })
  
  # Classification data
  wddClassnPrfl <- reactive({
    
    wddDataset2() %>% 
      select(Actual_Classification) %>% 
      group_by(Actual_Classification) %>%
      summarise(HC = n()) %>% 
      ungroup() %>% 
      mutate(Percent = round(HC/sum(HC)*100, 2)) ->
    classnPrfl    
    
  })
  
  # ATO Tenure data
  wddTnrPrfl <- reactive({
    
    wddDataset2() %>% 
      select(ATO_Tenure_Range) %>% 
      group_by(ATO_Tenure_Range) %>%
      summarise(HC = n()) %>% 
      ungroup() %>% 
      mutate(Percent = round(HC/sum(HC)*100, 2)) ->
    tnrPrfl    
    
  })
  
  # Job Family data
  wddJobPrfl <- reactive({
    
    wddDataset2() %>% 
      select(Job_Family) %>% 
      group_by(Job_Family) %>%
      summarise(HC = n()) %>% 
      ungroup() %>% 
      mutate(Percent = round(HC/sum(HC)*100, 2)) ->
    jfPrfl
    
  })
  
  # TODO - explore bring "wddCommsPrfl"/"wddFuncPrfl" into plotly calls
  # Comms Persona data
  wddCommsPrfl <- reactive({
    
    as.data.frame(table(wddDataset2()$Comms_Persona))
    
  })
  
  # Workforce Function data
  wddFuncPrfl <- reactive({
    
    as.data.frame(table(wddDataset2()$Work_Function))
    
  })
  
  # Position Location data
  wddLocnPrfl <- reactive({
    
    wddDataset2() %>% 
      select(Position_Location) %>% 
      group_by(Position_Location) %>%
      summarise(HC = n()) %>% 
      ungroup() %>% 
      mutate(Percent = round(HC/sum(HC)*100, 2)) ->
      locnPrfl
    
  })
  
  # Diversity summary
  divPrfl <- reactive({
    
    wddDataset2() %>%
      select(Female       = Gender
             , NESB       = NESB_Sum
             , Disability = Disability_HC
             , Indigenous = Indigenous_HC) %>%
      mutate(Female = ifelse(Female == "Female", "Yes", "No")) %>% 
      gather() %>% # convert to long format
      group_by(key, value) %>%
      summarise(n = n()) %>%
      mutate(perc = ifelse(key == "Indigenous_HC" & sum(n) < 100, "*"
                           , ifelse(key == "Disability_HC" & sum(n) <100, "*"
                           , paste0(round(n / sum(n) * 100, 2), "%")))
             , percCount = ifelse(perc == "*", "*", paste(perc, paste0("(", n, ")")))) %>%
      select(key, value, percCount) %>%
      spread(value, percCount) %>% # convert back to short format
      rename('Indicator' = key) ->
    divPrfl

    # Below 2 replace functions replace NAs in table for Female and NESB metrics with "0% (0)"
    
    divPrfl <- replace(divPrfl, is.na(divPrfl) & divPrfl$Indicator == "Female", "0% (0)")
    divPrfl <- replace(divPrfl, is.na(divPrfl) & divPrfl$Indicator == "NESB", "0% (0)")

    # Below if statement replaces NAs in table for Disability 
    # and Indigenous metrics with "0% (0)" and "*" accordingly.
    
    if  (grepl('100', divPrfl[1,3]) || 
         grepl('100', divPrfl[1,2]) || 
         grepl('100', divPrfl[3,3]) || 
         grepl('100', divPrfl[3,2])) 
    {
      
        divPrfl <- replace(divPrfl, is.na(divPrfl) & divPrfl$Indicator == "Disability", "0% (0)")
        divPrfl <- replace(divPrfl, is.na(divPrfl) & divPrfl$Indicator == "Indigenous", "0% (0)")
    
    } else if (grepl('*', divPrfl[1,3]) || 
               grepl('*', divPrfl[1,2]) || 
               grepl('*', divPrfl[3,3]) || 
               grepl('*', divPrfl[3,2])) 
    {
      
        divPrfl <- replace(divPrfl, is.na(divPrfl) & divPrfl$Indicator == "Disability", "*")
        divPrfl <- replace(divPrfl, is.na(divPrfl) & divPrfl$Indicator == "Indigenous", "*")
    
    } else
    {
      
        divPrfl
      
    }

  })
  
  # MDP plot data
  mdpPrfl <- reactive({
    
    wddDataset2() %>% 
      select(MDP_Status) %>% 
      group_by(MDP_Status) %>% 
      summarise(n = n()) %>% 
      mutate(perc = paste0(round(n / sum(n) * 100, 2), "%")
             , completion.numbers = paste(n, paste0("(", perc, ")"))) %>% 
      select(MDP_Status, completion.numbers) %>% 
      rename('MDP Status' = MDP_Status
             , 'Completion Numbers' = completion.numbers) ->
    mdpPrfl

  })
  
  # F2F, eLRN and External training data
  ldPrfl <- reactive({
    
    wddDataset2() %>% 
      select(F2F_Count
             , eLRN_Count
             , External_Count) %>% 
      summarise(HC = n()
                , F2F = round(sum(F2F_Count)/HC, 2)
                , eLEARN = round(sum(eLRN_Count)/HC, 2)
                , EXTERNAL = round(sum(External_Count)/HC, 2)) %>% 
      gather() %>% 
      slice(2:4) %>% 
      rename('Indicator' = key
             , 'Events per HC' = value) ->
    ldPrfl
    
  })
  
  # External training cost data
  costPrfl <- reactive({
    
    wddDataset2() %>% 
      select(External_Cost) %>% 
      summarise(sumCost = sum(External_Cost)
                , HC = n()
                , avgCost = round(sumCost/HC, 2)
                , maxCost = max(External_Cost)) %>% 
      select(`Total Cost` = sumCost
             , Headcount   = HC
             , `Average Cost` = avgCost
             , `Maximum Cost` = maxCost) %>% 
      gather() ->
    costPrfl
    
    # Rename cols for table output
    colnames(costPrfl) <- c("Measure", "Dollars ($)")
    
    # Format costs for UI
    costPrfl$`Dollars ($)` <- formatC(costPrfl$`Dollars ($)`, format = "d", big.mark = ",")
    
    # Print dataframe
    costPrfl
    
  })

  # Mobility summary
  mobPrfl <- reactive({
    
    wddDataset2() %>%
      select(`Mobility Register` = Mobility_Indicator
             , `Order of Merit`  = OOM_Indicator) %>% 
      gather() %>% # convert to long format
      group_by(key, value) %>%
      summarise(n=n()) %>%
      mutate(perc = paste0(round(n/sum(n) * 100, 2), "%")
             , percCount = paste(perc, paste0("(", n, ")"))) %>%
      select(key, value, percCount) %>% 
      spread(value, percCount) %>% # convert back to short format
      rename('Indicator' = key) %>% 
      replace(is.na(.), "0% (0)") ->
    mobPrfl
    
    # replace(mobPrfl, is.na(mobPrfl), "0% (0)")
    
  })
  
  ####################################################################################################
  
  # Plots ####
  
  # Age plot 5yr ####
  output$agePlot <- renderPlotly({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
      
      # plot variables
      x <- list(title = "")
      y <- list(title = input$wddSelView)
      m <- list(t = 10, r = 30, b = 50)
      
      if (input$wddSelView == "Headcount"){
        
        # plotly build
        p <- plot_ly(data = wddAgePrfl()
                     , x = Age_Range_5yr
                     , y = HC
                     , type = "bar") %>% 
          layout(xaxis = x
                 , yaxis = y
                 , margin = m) %>% 
          config(displayModeBar = F)
        
        # print plotly build
        p
        
      } else if (input$wddSelView == "Percentage"){
        
        # plotly layout
        p <- plot_ly(data = wddAgePrfl()
                     , x = Age_Range_5yr
                     , y = Percent
                     , type = "bar") %>% 
          layout(xaxis = x
                 , yaxis = y
                 , margin = m) %>% 
          config(displayModeBar = F)
        
        # print plotly build
        p
        
      }
      
    })
    
  })
  
  #__________________________________________________________________________________________________#
  
  # Age by Tenure plot ####
  output$ageTnrPlot <- renderPlotly({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
 
      # plot variables
      x.order = list("< 20","20 - 24","25 - 29","30 - 34","35 - 39", "40 - 44"
                     , "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", ">= 70")
      x <- list(title = "", categoryorder = "array", categoryarray = x.order)
      y <- list(title = input$wddSelView)
      m <- list(t = 10, r = 30, b = 50)
      
      if (input$wddSelView == "Headcount"){
        
        # plotly layout
        p <- plot_ly(data = wddAgeTnrPrfl()
                     , x = Age_Range_5yr
                     , y = HC
                     , group = ATO_Tenure_Range
                     , type = "bar") %>% 
          layout(barmode = "stack"
                 , xaxis = x
                 , yaxis = y
                 , margin = m
                 , showlegend = F) %>% 
          config(displayModeBar = F)
        
        # print plotly build
        p
        
      } else if (input$wddSelView == "Percentage"){
        
        # plotly layout
        p <- plot_ly(data = wddAgeTnrPrfl()
                     , x = Age_Range_5yr
                     , y = Percent
                     , group = ATO_Tenure_Range
                     , type = "bar") %>% 
          layout(barmode = "stack"
                 , xaxis = x
                 , yaxis = y
                 , margin = m
                 , showlegend = F) %>% 
          config(displayModeBar = F)
        
        # print plotly build
        p
        
      }
      
    })
    
  }) 
  
  #__________________________________________________________________________________________________#
  
  # Classification profile plot ####
  output$classnPlot <- renderPlotly({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
  
      # plot variables
      x <- list(title = "")
      y <- list(title = input$wddSelView)
      m <- list(t = 10, r = 30)
      
      if (input$wddSelView == "Headcount"){
        
        # plotly layout
        p <- plot_ly(data = wddClassnPrfl()
                     , x = Actual_Classification
                     , y = HC
                     , type = "bar") %>% 
          layout(xaxis = x
                 , yaxis = y
                 , margin = m) %>% 
          config(displayModeBar = F)
        
        # print plotly build
        p
        
      } else if (input$wddSelView == "Percentage"){
        
        # plotly layout
        p <- plot_ly(data = wddClassnPrfl()
                     , x = Actual_Classification
                     , y = Percent
                     , type = "bar") %>% 
          layout(xaxis = x
                 , yaxis = y
                 , margin = m) %>% 
          config(displayModeBar = F)
        
        # print plotly build
        p
        
      }
      
    })
    
  })
  
  #__________________________________________________________________________________________________#
  
  # ATO Tenure profile plot ####
  output$atoPlot <- renderPlotly({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
 
      # plot variables
      x <- list(title = "")
      y <- list(title = input$wddSelView)
      m <- list(t = 10, r = 30, b = 50)
      
      if (input$wddSelView == "Headcount"){
        
        # plotly layout
        p <- plot_ly(data = wddTnrPrfl()
                     , x = ATO_Tenure_Range
                     , y = HC
                     , type = "bar") %>% 
          layout(xaxis = x
                 , yaxis = y
                 , margin = m) %>% 
          config(displayModeBar = F)
        
        # print plotly build
        p
        
      } else if (input$wddSelView == "Percentage"){
        
        # plotly layout
        p <- plot_ly(data = wddTnrPrfl()
                     , x = ATO_Tenure_Range
                     , y = Percent
                     , type = "bar") %>% 
          layout(xaxis = x
                 , yaxis = y
                 , margin = m) %>% 
          config(displayModeBar = F)
        
        # print plotly build
        p
        
      }
      
    })
    
  })
  
  #__________________________________________________________________________________________________#
  
  # Job Family profile plot ####
  output$jfPlot <- renderPlotly({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
      
      # plot variables
      x <- list(title = "")
      y <- list(title = input$wddSelView)
      
      if (input$wddSelView == "Headcount" & input$wddSelOrg != "Job Family"){
        
        # local margin
        m <- list(t = 10, r = 30, b = 80)
        
        # plotly layout
        p <- plot_ly(data = wddJobPrfl()
                     , x = Job_Family
                     , y = HC
                     , type = "bar") %>% 
          layout(xaxis = x
                 , yaxis = y
                 , margin = m) %>% 
          config(displayModeBar = F)
        
        # print plotly build
        p
        
      } else if (input$wddSelView == "Percentage" & input$wddSelOrg != "Job Family"){
        
        # local margin
        m <- list(t = 10, r = 30, b = 80)
        
        # plotly layout
        p <- plot_ly(data = wddJobPrfl()
                     , x = Job_Family
                     , y = Percent
                     , type = "bar") %>% 
          layout(xaxis = x
                 , yaxis = y
                 , margin = m) %>% 
          config(displayModeBar = F)
        
        # print plotly build
        p
        
      } else if (input$wddSelView == "Headcount" & input$wddSelOrg == "Job Family"){
        
        # local margin
        m <- list(t = 10, r = 30)
        
        # plotly layout
        p <- plot_ly(wddOrgPrfl()
                     , x = Subplan
                     , y = HC
                     , group = BSL
                     , type = "bar") %>% 
          layout(barmode = "stack"
                 , xaxis = x
                 , yaxis = y
                 , margin = m
                 , showlegend = F) %>% 
          config(displayModeBar = F)
        
        # print plotly build
        p
        
      } else if (input$wddSelView == "Percentage" & input$wddSelOrg == "Job Family"){
        
        # local margin
        m <- list(t = 10, r = 30)
        
        # plotly layout
        p <- plot_ly(wddOrgPrfl()
                     , x = Subplan
                     , y = Percent
                     , group = BSL
                     , type = "bar") %>% 
          layout(barmode ="stack"
                 , xaxis = x
                 , yaxis = y
                 , margin = m
                 , showlegend = F) %>% 
          config(displayModeBar = F)
        
        # print plotly build
        p
        
      }
      
    })
    
  })
  
  #__________________________________________________________________________________________________#
  
  # Position Location profile plot
  output$locnPlot <- renderPlotly({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
      
      # plot variables
      x <- list(title = "")
      y <- list(title = input$wddSelView)
      
      if (input$wddSelView == "Headcount" & input$wddSelOrg != "Site"){
        
        # local margin 
        m <- list(t = 10, r = 30, b = 80)
        
        # plotly layout
        p <- plot_ly(data = wddLocnPrfl()
                     , x = Position_Location
                     , y = HC
                     , type = "bar") %>% 
          layout(xaxis = x
                 , yaxis = y
                 , margin = m) %>% 
          config(displayModeBar = F)
        
        # print plotly build
        p
        
      } else if (input$wddSelView == "Percentage" & input$wddSelOrg != "Site"){
        
        # local margin 
        m <- list(t = 10, r = 30, b = 80)
        
        # plotly layout
        p <- plot_ly(data = wddLocnPrfl()
                     , x = Position_Location
                     , y = Percent
                     , type = "bar") %>% 
          layout(xaxis = x
                 , yaxis = y
                 , margin = m) %>% 
          config(displayModeBar = F)
        
        # print plotly build
        p
        
      } else if (input$wddSelView == "Headcount" & input$wddSelOrg == "Site"){
        
        # local margin 
        m <- list(t = 10, b = 30, l = 60, r = 30)
        
        # plotly layout 
        p <- plot_ly(wddOrgPrfl()
                     , x = Subplan
                     , y = HC
                     , group = BSL
                     , type = "bar") %>% 
          layout(barmode ="stack"
                 , xaxis = x
                 , yaxis = y
                 , margin = m
                 , showlegend = F) %>% 
          config(displayModeBar = F)
        
        # print plotly build
        p
        
      } else if (input$wddSelView == "Percentage" & input$wddSelOrg == "Site"){
        
        # local margin 
        m <- list(t = 10, b = 30, l = 60, r = 30)
        
        # plotly layout
        p <- plot_ly(wddOrgPrfl()
                     , x = Subplan
                     , y = Percent
                     , group = BSL
                     , type = "bar") %>% 
          layout(barmode ="stack"
                 , xaxis = x
                 , yaxis = y
                 , margin = m
                 , showlegend = F) %>% 
          config(displayModeBar = F)
        
        # print plotly build
        p
        
      }
      
    })
    
  })
  
  #__________________________________________________________________________________________________#

  # Diversity Table
  output$divTable <- renderTable({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
      
      divPrfl()
      
    })
    
  }, align = 'c')
  
  #__________________________________________________________________________________________________#
  
  # Learning and development tables ####
  output$mdpTbl <- renderTable({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
 
      mdpPrfl()
      
    })
    
  }, align = 'c')
  
  output$ldTbl <- renderTable({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({

      ldPrfl()
      
    })
    
  }, align = 'c')
  
  # External training cost plot
  output$costTbl <- renderTable({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({

      costPrfl()
      
    })
    
  }, align = 'c')
  
  #__________________________________________________________________________________________________#
  
  # Mobility Table
  output$mobTable <- renderTable({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({

      mobPrfl()
      
    })
    
  }, align = 'c')
  
  #________________________________________________________________________________________________#
  
  output$mnPnl <- reactive({
    nrow(wddDataset2())
  })
  outputOptions(output, 'mnPnl', suspendWhenHidden = FALSE)
  
})