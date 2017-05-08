shinyServer(function(input, output, session) {
  
  # DYNAMIC UI -------------------------------------------------------------------------------------
  
  # Dynamic UI for Location/Group plot
  output$dt <- renderUI({
    
    snpsht_dt <- format(max(df$Snapshot_Date), "%d.%m.%Y")
    paste0("Data is as at ", snpsht_dt)
    
  })
  
  # Dynamic title for Classification plot heading
  output$clssnTitle <- renderUI({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
      
      if (input$selOrg == "Job Family"){
        "Classification and Group by BSL" 
      } else { 
        "Classification and Job Family"
      }
      
    })
    
  })
  
  # Dynamic title for Classification tabset panel - tab 2 plot heading
  output$jfGrpTitle <- renderUI({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
      
      if (input$selOrg == "Job Family"){
        "Group by BSL"
      } else { 
        "Job Family"
      }
      
    })
    
  })
  
  # Dynamic title for plot 3 - Position Location/Group by BSL
  output$locnGrpTitle <- renderUI({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({

      if (input$selOrg == "Site"){
        "Group by BSL"
      } else {
        "Position Location"
      }

    })
    
  })
  
  # DATA SUBSET ------------------------------------------------------------------------------------
  
  # Subset all data by employment type option
  dataset <- reactive({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
      
      if (input$selEmpTyp == "All Employment Types"){
        df
      } else {
        subset(df, df$Perm_Temp == input$selEmpTyp)
      }

    })
    
  })
  
  # Then subset by lens option
  dataset2 <- reactive({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
      switch(input$selOrg
             , "ATO"             = dataset()
             , "Group"           = subset(dataset(), dataset()$Subplan == input$selGrp)
             , "BSL"             = subset(dataset(), dataset()$BSL == input$selBSL)
             , "Branch"          = subset(dataset(), dataset()$Org_Unit_Branch == input$selBranch)
             , "Team/Org.Unit"   = subset(dataset(), dataset()$Org_Unit_Team == input$selTeam)
             , "Cost Centre"     = subset(dataset(), dataset()$Cost_Centre_Code == input$selCstCntr)
             , "Classification"  = subset(dataset()
                                          , dataset()$Actual_Classification == input$selClssn)
             , "Classification (Grouped)"
                                 = subset(dataset(), dataset()$clssnCat == input$selClssnGrp)
             , "Job Family"      = subset(dataset(), dataset()$Job_Family == input$selJob)
             , "Site"            = subset(dataset(), dataset()$Position_Location == input$selSite)
             , "Manager"         = subset(dataset(), dataset()$Manager_Indicator == input$selMgr)
             , "Gender"          = subset(dataset(), dataset()$Gender == input$selGndr)
             , "NESB"            = subset(dataset(), dataset()$NESB_Sum == input$selNESB)
             , "Disability"      = subset(dataset(), dataset()$Disability_HC == input$selDsbl)
             , "Indigenous"      = subset(dataset(), dataset()$Indigenous_HC == input$selIndg)
      )
    })
  })
  
  # DATA WRANGLING LOGIC ---------------------------------------------------------------------------
  
  # Age data - 5yrs
  agePrfl <- reactive({
    
    scaleHC(dataset2(), "Age_Range_5yr", input$selView)
    
  })
  
  # ATO Tenure data
  tnrPrfl <- reactive({
    
    scaleHC(dataset2(), "ATO_Tenure_Range", input$selView)
    
  })
  
  # Age/tenure data 
  ageTnrPrfl <- reactive({
    
    scaleHC(dataset2(), c("Age_Range_5yr", "ATO_Tenure_Range"), input$selView)
    
  })
  
  # Classification data
  clssnPrfl <- reactive({
    
    scaleHC(dataset2(), "Actual_Classification", input$selView)
    
  })
  
  # Job Family data
  jobPrfl <- reactive({
    
    scaleHC(dataset2(), "Job_Family", input$selView)
    
  })
  
  # Group/bsl data
  orgPrfl <- reactive({
    
    scaleHC(dataset2(), c("Subplan", "BSL"), input$selView)
    
  })
  
  # Position Location data
  locnPrfl <- reactive({
    
    scaleHC(dataset2(), "Position_Location", input$selView)
    
  })
  
  # Grouped Classification data
  clssnGrpPrfl <- reactive({
    
    scaleHC(dataset2(), "clssnCat")
    
  })
  
  # Diversity summary
  divPrfl <- reactive({
    
    divDataset <- dataset2()
    
    divDataset$Gender <- ifelse(divDataset$Gender == "Female", "Yes", "No")
    
    divDataset %>%
      select(Gender, NESB_Sum, Disability_HC, Indigenous_HC) %>%
      gather() %>% # convert to long format
      group_by(key, value) %>%
      summarise(n=n()) %>%
      mutate(perc = ifelse(key == "Indigenous_HC" & sum(n) < 100
                           , "*"
                           , ifelse(key == "Disability_HC" & sum(n) <100
                                    , "*"
                                    , ifelse(key == "NESB_Sum" & sum(n) <100
                                             , "*"
                                             , paste0(round(n / sum(n) * 100, 2), "%"))))
             , percCount = ifelse(perc == "*", "*", paste(perc, paste0("(", n, ")")))) %>%
      select(key, value, percCount) %>%
      spread(value, percCount) %>% # convert back to short format
      select('Indicator' = key, Yes, No) ->
      divPrfl
    
    # Rename row names
    divPrfl <- replace(divPrfl, (divPrfl == "Gender"), "Female")
    divPrfl <- replace(divPrfl, (divPrfl == "NESB_Sum"), "NESB")
    divPrfl <- replace(divPrfl, (divPrfl == "Disability_HC"), "Disability")
    divPrfl <- replace(divPrfl, (divPrfl == "Indigenous_HC"), "Indigenous")
    
    # Replace NAs in table for Female with "0% (0)"
    divPrfl <- replace(divPrfl, is.na(divPrfl) & divPrfl$Indicator == "Female", "0% (0)")
    
    # Replace NAs in table for Disability, Indigenous 
    # and NESB metrics with "0% (0)" and "*" accordingly.
    if  (grepl('100%', divPrfl[1,3]) || 
         grepl('100%', divPrfl[1,2]) || 
         grepl('100%', divPrfl[3,3]) || 
         grepl('100%', divPrfl[3,2]) ||
         grepl('100%', divPrfl[4,3]) ||
         grepl('100%', divPrfl[4,2])
         ) 
    {
      
      divPrfl <- replace(divPrfl, is.na(divPrfl) & divPrfl$Indicator == "Disability", "0% (0)")
      divPrfl <- replace(divPrfl, is.na(divPrfl) & divPrfl$Indicator == "Indigenous", "0% (0)")
      divPrfl <- replace(divPrfl, is.na(divPrfl) & divPrfl$Indicator == "NESB", "0% (0)")
      
    } else if (grepl('*', divPrfl[1,3]) || 
               grepl('*', divPrfl[1,2]) || 
               grepl('*', divPrfl[3,3]) || 
               grepl('*', divPrfl[3,2]) ||
               grepl('*', divPrfl[4,3]) ||
               grepl('*', divPrfl[4,2])
               ) 
    {
      
      divPrfl <- replace(divPrfl, is.na(divPrfl) & divPrfl$Indicator == "Disability", "*")
      divPrfl <- replace(divPrfl, is.na(divPrfl) & divPrfl$Indicator == "Indigenous", "*")
      divPrfl <- replace(divPrfl, is.na(divPrfl) & divPrfl$Indicator == "NESB", "*")
      
    }

  })
  
  # MDP plot data
  mdpPrfl <- reactive({
    
    scaleHC(dataset2(), "MDP_Status") %>% 
      mutate(completion.numbers = paste(HC, paste0("(", Percent, "%)"))) %>% 
      select(MDP_Status, completion.numbers) %>% 
      rename('MDP Status' = MDP_Status
             , 'Completion Numbers' = completion.numbers) ->
      mdpPrfl

  })
  
  # F2F, eLRN and External training data
  ldPrfl <- reactive({
    
    dataset2() %>% 
      select(F2F_Count
             , eLRN_Count
             , External_Count) %>% 
      summarise(HC = n()
                , F2F = round(sum(F2F_Count)/HC, 2)
                , eLEARN = round(sum(eLRN_Count)/HC, 2)
                , EXTERNAL = round(sum(External_Count)/HC, 2)) %>% 
      gather() %>% # convert to long format
      slice(2:4) %>% 
      rename('Indicator' = key
             , 'Events per HC' = value) ->
    ldPrfl
    
  })
  
  # External training cost data
  costPrfl <- reactive({
    
    dataset2() %>% 
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
    
    dataset2() %>%
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
  
  # Plots ------------------------------------------------------------------------------------------
  
  # Age plot 5yr
  output$agePlot <- renderPlotly({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
      
      # plot variables
      x <- list(title = "")
      y <- list(title = input$selView)
      m <- list(t = 10, r = 30, b = 50)

      p <- plot_ly(data = agePrfl()
                   , x = Age_Range_5yr
                   , y = Measure
                   , type = "bar") %>%
        layout(xaxis = x
               , yaxis = y
               , margin = m) 
        
        # print plotly build
        p
      
    })
    
  })
  
  # ATO Tenure profile plot
  output$atoPlot <- renderPlotly({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
      
      # plot variables
      x <- list(title = "")
      y <- list(title = input$selView)
      m <- list(t = 10, r = 30, b = 50)
      
      # plotly layout
      p <- plot_ly(data = tnrPrfl()
                   , x = ATO_Tenure_Range
                   , y = Measure
                   , type = "bar") %>%
        layout(xaxis = x
               , yaxis = y
               , margin = m) 
        
      # print plotly build
      p
      
    })
    
  })
  
  # Age by Tenure plot
  output$ageTnrPlot <- renderPlotly({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
 
      # plot variables
      x.order = list("< 20","20 - 24","25 - 29","30 - 34","35 - 39", "40 - 44"
                     , "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", ">= 70")
      x <- list(title = "", categoryorder = "array", categoryarray = x.order)
      y <- list(title = input$selView)
      m <- list(t = 10, r = 30, b = 50)
      
      # plotly layout
      p <- plot_ly(data = ageTnrPrfl()
                   , x = Age_Range_5yr
                   , y = Measure
                   , group = ATO_Tenure_Range
                   , type = "bar") %>% 
      layout(barmode = "stack"
             , xaxis = x
             , yaxis = y
             , margin = m
             , showlegend = F) 
        
      # print plotly build
      p
      
    })
    
  }) 
  
  # Classification profile plot
  output$clssnPlot <- renderPlotly({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
  
      # plot variables
      x <- list(title = "")
      y <- list(title = input$selView)
      m <- list(t = 10, r = 30)
        
      # plotly layout
      p <- plot_ly(data = clssnPrfl()
                   , x = Actual_Classification
                   , y = Measure
                   , type = "bar") %>%
        layout(xaxis = x
               , yaxis = y
               , margin = m)
      
      # print plotly build
      p
      
    })
    
  })
  
  # Job Family profile plot
  output$jfPlot <- renderPlotly({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
      
      # plot variables
      x <- list(title = "")
      y <- list(title = input$selView)
      
      if (input$selOrg != "Job Family"){
        
        # local margin
        m <- list(t = 10, r = 30, b = 80)
        
        # plotly layout
        p <- plot_ly(data = jobPrfl()
                     , x = Job_Family
                     , y = Measure
                     , type = "bar") %>% 
          layout(xaxis = x
                 , yaxis = y
                 , margin = m) 
        
        # print plotly build
        p
        
      } else if (input$selOrg == "Job Family"){
        
        # local margin
        m <- list(t = 10, r = 30)
        
        # plotly layout
        p <- plot_ly(orgPrfl()
                     , x = Subplan
                     , y = Measure
                     , group = BSL
                     , type = "bar") %>% 
          layout(barmode = "stack"
                 , xaxis = x
                 , yaxis = y
                 , margin = m
                 , showlegend = F) 
        
        # print plotly build
        p
        
      }
      
    })
    
  })
  
  # Position Location profile plot
  output$locnPlot <- renderPlotly({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
      
      # plot variables
      x <- list(title = "")
      y <- list(title = input$selView)
      
      if (input$selOrg != "Site"){
        
        # local margin 
        m <- list(t = 10, r = 30, b = 80)
        
        # plotly layout
        p <- plot_ly(data = locnPrfl()
                     , x = Position_Location
                     , y = Measure
                     , type = "bar") %>% 
          layout(xaxis = x
                 , yaxis = y
                 , margin = m) 
        
        # print plotly build
        p
        
      } else if (input$selOrg == "Site"){
        
        # local margin 
        m <- list(t = 10, b = 30, l = 60, r = 30)
        
        # plotly layout 
        p <- plot_ly(orgPrfl()
                     , x = Subplan
                     , y = Measure
                     , group = BSL
                     , type = "bar") %>% 
          layout(barmode ="stack"
                 , xaxis = x
                 , yaxis = y
                 , margin = m
                 , showlegend = F) 
        
        # print plotly build
        p
        
      }
      
    })
    
  })
  
  # Tables -----------------------------------------------------------------------------------------
  
  # Diversity Table
  output$divTable <- renderTable({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({
      
      divPrfl()
      
    })
    
  }, align = 'c')
  
  # Learning and development tables
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
  
  # Mobility Table
  output$mobTable <- renderTable({
    
    # Take a dependency on action button
    input$buildDashboard
    
    isolate({

      mobPrfl()
      
    })
    
  }, align = 'c')
  
  # Return number of rows in dataset - if zero an error message will display from ui.R -------------
  output$mnPnl <- reactive({
    nrow(dataset2())
  })
  outputOptions(output, 'mnPnl', suspendWhenHidden = FALSE)
  
})