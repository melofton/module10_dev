shinyServer(function(input, output, session) {
  
  #### Presentation ----

  #** Recap Presentation slides ----
  output$slides <- renderSlickR({
    slickR(recap_slides) + settings(dots = TRUE)
  })
  
  #### Introduction ----
  # Hide download button until report is generated
  output$handoutbuilt <- reactive({
    return(file.exists("report.docx"))
  })
  outputOptions(output, 'handoutbuilt', suspendWhenHidden= FALSE)
  
  handout_file <- "Student_handout.docx"
  
  output$stud_dl <-  downloadHandler(
    filename = function() {
      handout_file
    },
    content = function(file) {
      file.copy("report.docx", file)
    }
  )
  
  #### Activity A ----
  
  #** Objective 1: DBP formation/thresholds slides ----
  output$dbp_formation_thresholds_slides <- renderSlickR({
    slickR(obj1_slides) + settings(dots = TRUE)
  })
  
  #** Objective 2: DBP tradeoffs slides ----
  output$dbp_tradeoffs_slides <- renderSlickR({
    slickR(obj2_slides) + settings(dots = TRUE)
  })
  
  #### Activity B ----
  
  #** Objective 3: Select/view reservoir ----
  # LTREB Sites datatable ----
  output$table01 <- DT::renderDT(
    sites_df[, c(1:2)], selection = "single", options = list(stateSave = TRUE, dom = 't'), server = FALSE
  ) 
  
  observe({
    if(input$row_num != "") {
      dt_proxy <- dataTableProxy("table01")
      selectRows(dt_proxy, input$row_num)
    }
  })
  
  # to keep track of previously selected row
  prev_row <- reactiveVal()
  siteID <- reactiveValues(lab = NULL)
  
  # new icon style
  my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')
  
  
  # Select NEON DT rows ----
  lake_data <- reactiveValues(df = NULL,
                              wtemp = NULL,
                              do = NULL,
                              turb = NULL)
  site_photo_file <- reactiveValues(img = NULL)
  
  observeEvent(input$table01_rows_selected, {
    
    siteID$lab <- input$table01_rows_selected
    
    row_selected = sites_df[input$table01_rows_selected, ]
    proxy <- leafletProxy('ltrebmap')
    proxy %>%
      addAwesomeMarkers(layerId = as.character(row_selected$SiteID),
                        lng=row_selected$Longitude,
                        lat=row_selected$Latitude,
                        icon = my_icon)
    
    # Reset previously selected marker
    if(!is.null(prev_row()))
    {
      proxy %>%
        removeMarker(layerId = as.character(prev_row()$SiteID))
    }
    # set new value to reactiveVal
    prev_row(row_selected)
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Loading LTREB data",
                 detail = "This may take a while. This window will disappear
                     when it is loaded.", value = 0.33)
    
    #load LTREB data
    lake_data$df <- reservoir_data %>%
      filter(site_id == pull(sites_df[input$table01_rows_selected, "SiteID"]),
             variable %in% c("Chla_ugL_mean","fDOM_QSU_mean")) %>%
      mutate(observation = round(observation, 1),
             depth_ft = round(depth_m*3.28,1))
    
    #retrieve site photooutput$display.image <- renderImage({
    site_photo_file$img <- paste("www/",row_selected$SiteID,".jpg",sep="")
    
    #show site info
    output$site_info <- renderText({
      module_text[row_selected$SiteID, ]
    })
    
    #pull recent data
    lake_data$fdom <- lake_data$df %>%
      select(datetime, variable, depth_m, depth_ft, observation) %>%
      filter(variable == "fDOM_QSU_mean")
    
    lake_data$chla <- lake_data$df %>%
      select(datetime, variable, depth_m, depth_ft, observation) %>%
      filter(variable == "Chla_ugL_mean") # remove metalimnion
    
    focal_year <- ifelse(pull(sites_df[input$table01_rows_selected, "SiteID"]) == "fcre",2023,2022)
    
    progress$set(value = 1)
    
  })
  
  # LTREB map ----
  output$ltrebmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = sites_df,
                 layerId = ~SiteID, clusterOptions = markerClusterOptions(),
                 label = ~ReservoirName, icon = ~ltrebIcons[1])
    
  })
  
  # Show reservoir image ----
  output$site_photo <- renderImage({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in the table.")
    )
    list(src = site_photo_file$img,
         alt = "Image failed to render.",
         height = 256,
         width = 320)
  }, deleteFile = FALSE)
  
  #** Objective 3: Plot fDOM ----
  
  #*# fDOM slides
  output$fdom_slides <- renderSlickR({
  slickR(obj3_slides) + settings(dots = TRUE)
   })

  #*# Plot fDOM
  plot.fDOM <- reactiveValues(main=NULL)
  
  observe({
    
    output$fDOM_plot <- renderPlotly({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in the Introduction.")
      )
      validate(
        need(!is.null(lake_data$df),
             message = "Please select a site in the Introduction.")
      )
      validate(
        need(input$plot_fDOM > 0,
             message = "Click 'Plot fDOM'")
      )
      
      df <- lake_data$fdom %>% filter(year(datetime) == 2024)
      
      p <- ggplot(data = df, aes(x = datetime, y = observation))+
        geom_point(aes(color = "surface water fDOM"))+
        xlab("")+
        ylab("fDOM (QSU)")+
        scale_color_manual(values = c("surface water fDOM" = "brown"), name = "")+
        theme_bw()
      
      plot.fDOM$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE, tooltip=c("x", "y", "color")))
      
    })
    
  })
  
  #*# chla slides
  output$chla_slides <- renderSlickR({
    slickR(obj4_slides) + settings(dots = TRUE)
  })
  
  #*# Plot chla
  plot.chla <- reactiveValues(main=NULL)
  
  observe({
    
    output$chla_plot <- renderPlotly({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in the Introduction.")
      )
      validate(
        need(!is.null(lake_data$df),
             message = "Please select a site in the Introduction.")
      )
      validate(
        need(input$plot_chla > 0,
             message = "Click 'Plot chlorophyll-a'")
      )
      
      df <- lake_data$chla %>% filter(year(datetime) == 2024)
      
      p <- ggplot(data = df, aes(x = datetime, y = observation))+
        geom_point(aes(color = "surface water chlorophyll-a"))+
        xlab("")+
        ylab("fDOM (QSU)")+
        scale_color_manual(values = c("surface water chlorophyll-a" = "green"), name = "")+
        theme_bw()
      
      plot.chla$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE, tooltip=c("x", "y", "color")))
      
    })
    
  })
  
  
  #### Navigating Tabs ----
    
    # Navigating Tabs ----
    #* Main Tab ====
    rv1 <- reactiveValues(prev = 0, nxt = 2)
    observeEvent(input$maintab, {
      curr_tab1 <- input$maintab
      rv1$prev <- readr::parse_number(curr_tab1) - 1
      rv1$nxt <- readr::parse_number(curr_tab1) + 1
    })
    
    observe({
      
      toggleState(id = "prevBtn1", condition = rv1$prev > 0)
      if(rv1$nxt > 4) {
        shinyjs::disable("nextBtn1")
      } else {
        shinyjs::enable("nextBtn1")
      }
      hide(selector = ".page")
    })
    
    
    # Next button
    observe({
      curr_tab1 <- input$maintab
      idx <- which(tab_names$tab_id == curr_tab1)
      new_nam <- tab_names$name[idx + 1]
      if(curr_tab1 == "mtab4" & rv1$nxt > 4) {
        updateActionButton(session, inputId = "nextBtn1", label = paste("End of module"))
      } else {
        updateActionButton(session, inputId = "nextBtn1", label = paste(new_nam, ">"))
      }   
      })
    
    # Previous button
    observe({
      curr_tab1 <- input$maintab
      idx <- which(tab_names$tab_id == curr_tab1)
      new_nam <- tab_names$name[idx - 1]
      if(curr_tab1 == "mtab1") {
        updateActionButton(session, inputId = "prevBtn1", label = paste("Module begins"))
      } else {
        # shinyjs::show(id = "prevBtn1")
        updateActionButton(session, inputId = "prevBtn1", label = paste("<", new_nam))
      }
    })
    
    
    # Advancing Tabs
    observeEvent(input$nextBtn1, {
      
      curr_tab1 <- input$maintab
      idx <- which(tab_names$tab_id == curr_tab1)
        updateTabsetPanel(session, "maintab",
                          selected = paste0("mtab", rv1$nxt))
      
      shinyjs::runjs("window.scrollTo(0, 0)") # scroll to top of page
    })
    
    # Moving back through tabs
    observeEvent(input$prevBtn1, {
      curr_tab1 <- input$maintab
      idx <- which(tab_names$tab_id == curr_tab1)
        updateTabsetPanel(session, "maintab",
                          selected = paste0("mtab", rv1$prev))
      shinyjs::runjs("window.scrollTo(0, 0)")
      
    })
    
  # Help buttons ----
  observeEvent(input$help, {
    introjs(session, events = list(onbeforechange = readCallback("switchTabs")))
  })
  observeEvent(input$help2, {
    shinyalert(title = "Resume Progress", text = "Use this field to upload your '.eddie' file to resume your progress.", type = "info")
  })
  
})

# end
