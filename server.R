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
  
  # Select site
  
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
                              fdom = NULL,
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
             variable %in% c("Turbidity_FNU_mean","fDOM_QSU_mean")) %>%
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
    
    lake_data$turb <- lake_data$df %>%
      select(datetime, variable, depth_m, depth_ft, observation) %>%
      filter(variable == "Turbidity_FNU_mean") # remove metalimnion
    
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
                 label = ~ReservoirName, icon = ~ltrebIcons[1]) %>%
      setView(lng = -79.826954, lat = 37.311227, zoom = 14)
    
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
  
  observe({
    
    output$site_photo_credit <- renderUI({
      
      validate(
        need(!is.null(lake_data$df),
             message = "Please select a site in the Introduction.")
      )
      
      site = pull(sites_df[input$table01_rows_selected, "SiteID"])
      
      if(site == "fcre"){
        site_photo_credit <- paste("<b>","Falling Creek Reservoir (photo credit Adrienne Breef-Pilz)","</b>", sep = "")
      }
      if(site == "bvre"){
        site_photo_credit <- paste("<b>","Beaverdam Reservoir (photo credit Ricardo Paiz)","</b>", sep = "")
      }
      
      
      HTML(paste(site_photo_credit))
    })
    
  })
  
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

  # Output site name
  observe({
    
    output$site_name <- renderUI({
      
      validate(
        need(!is.null(lake_data$df),
             message = "Please select a site in the Introduction.")
      )
      
      site = pull(sites_df[input$table01_rows_selected, "SiteID"])
      
      if(site == "fcre"){
        site_name <- paste("<b>","Falling Creek Reservoir","</b>", sep = "")
      }
      if(site == "bvre"){
        site_name <- paste("<b>","Beaverdam Reservoir","</b>", sep = "")
      }
      
      
      HTML(paste(site_name))
    })
    
  })
  
  
  # Output site photo credit again
  observe({
    
    output$site_photo_credit1 <- renderUI({
      
      validate(
        need(!is.null(lake_data$df),
             message = "Please select a site in the Introduction.")
      )
      
      site = pull(sites_df[input$table01_rows_selected, "SiteID"])
      
      if(site == "fcre"){
        site_photo_credit1 <- paste("<b>","Falling Creek Reservoir (photo credit Mary Lofton)","</b>", sep = "")
      }
      if(site == "bvre"){
        site_photo_credit1 <- paste("<b>","Beaverdam Reservoir (photo credit Ricardo Paiz)","</b>", sep = "")
      }
      
      
      HTML(paste(site_photo_credit1))
    })
    
  })
  
  # Show reservoir image again ----
  output$site_photo1 <- renderImage({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in the Introduction.")
    )
    list(src = site_photo_file$img,
         alt = "Image failed to render.",
         height = 384,
         width = 480)
  }, deleteFile = FALSE)
  
  #** Objective 4: Plot fDOM ----
  
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
             message = "Please select a focal reservoir site.")
      )
      validate(
        need(!is.null(lake_data$df),
             message = "Please select a focal reservoir site.")
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
  
  # convert fdom to toc
  toc.out <- reactiveValues(toc=NULL)
  
  output$toc_out <- renderText({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a focal reservoir site.")
    )
    validate(
      need(!is.null(lake_data$df),
           message = "Please select a focal reservoir site.")
    )
    validate(
      need(input$convert_fDOM > 0,
           message = "Click 'Convert fDOM to TOC'")
    )
    
    observeEvent(input$convert_fDOM,{
      
      site_id = pull(sites_df[input$table01_rows_selected, "SiteID"])
      
      if(site_id == "fcre"){
        toc.out$toc = 1.86859 + 0.10846*input$fdom
      }
      if(site_id == "bvre"){
        toc.out$toc = 2.24794 + 0.10522*input$fdom
      }
      
    })
    
    paste0("The estimated TOC level associated with this level of fDOM is ~",round(toc.out$toc, 1)," mg/L.")
  })
  
  # Download EPA rule
  output$rulebuilt <- reactive({
    return(file.exists("Stage-1-Disinfection-By-products-fact-sheet.pdf"))
  })
  outputOptions(output, 'rulebuilt', suspendWhenHidden= FALSE)
  
  rule_file <- "EPA_816-F-01-014.pdf"
  
  output$rule_dl <-  downloadHandler(
    filename = function() {
      rule_file
    },
    content = function(file) {
      file.copy("Stage-1-Disinfection-By-products-fact-sheet.pdf", file)
    }
  )
  
  #### Activity C ----
  
  # Objective 5
  
  # convert fdom to toc
  toc.out1 <- reactiveValues(toc=NULL)
  
  output$toc_out1 <- renderText({
    
    validate(
      need(input$convert_fDOM1 > 0,
           message = "Click 'Convert fDOM to TOC'")
    )
    
    observeEvent(input$convert_fDOM1,{
      
        toc.out1$toc = 1.86859 + 0.10846*input$fdom1
      
    })
    
    paste0("The estimated TOC level associated with this level of fDOM is ~",round(toc.out1$toc, 1)," mg/L.")
  })
  
  #*# Plot fDOM (increasing)
  plot.fDOM.inc <- reactiveValues(main=NULL)
  
  observe({
    
    output$fDOM_plot_inc <- renderPlotly({ 
      
      df <- reservoir_data %>%
        filter(variable %in% c("fDOM_QSU_mean" ) & site_id == "fcre" & datetime >= "2019-07-20" & datetime <= "2019-08-20") %>%
        mutate(observation = observation^1.5)
      
      p <- ggplot(data = df, aes(x = datetime, y = observation))+
        geom_point(aes(color = "surface water fDOM"))+
        geom_line(aes(color = "surface water fDOM"))+
        xlab("")+
        ylab("fDOM (QSU)")+
        scale_color_manual(values = c("surface water fDOM" = "brown"), name = "")+
        theme_bw()
      
      plot.fDOM.inc$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE, tooltip=c("x", "y", "color")))
      
    })
    
  })
  
  # convert fdom to toc
  toc.out2 <- reactiveValues(toc=NULL)
  
  output$toc_out2 <- renderText({
    
    validate(
      need(input$convert_fDOM2 > 0,
           message = "Click 'Convert fDOM to TOC'")
    )
    
    observeEvent(input$convert_fDOM2,{
      
      toc.out2$toc = 1.86859 + 0.10846*input$fdom2
      
    })
    
    paste0("The estimated TOC level associated with this level of fDOM is ~",round(toc.out2$toc, 1)," mg/L.")
  })
  
  #*# Plot fDOM (decreasing)
  plot.fDOM.dec <- reactiveValues(main=NULL)
  
  observe({
    
    output$fDOM_plot_dec <- renderPlotly({ 
      
      df <- reservoir_data %>%
        filter(variable %in% c("fDOM_QSU_mean" ) & site_id == "fcre" & datetime >= "2021-01-20" & datetime <= "2021-02-20") %>%
        mutate(observation = observation^1.5)
      
      p <- ggplot(data = df, aes(x = datetime, y = observation))+
        geom_point(aes(color = "surface water fDOM"))+
        geom_line(aes(color = "surface water fDOM"))+
        xlab("")+
        ylab("fDOM (QSU)")+
        scale_color_manual(values = c("surface water fDOM" = "brown"), name = "")+
        theme_bw()
      
      plot.fDOM.dec$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE, tooltip=c("x", "y", "color")))
      
    })
    
  })
  
  # convert fdom to toc
  toc.out3 <- reactiveValues(toc=NULL)
  
  output$toc_out3 <- renderText({
    
    validate(
      need(input$convert_fDOM3 > 0,
           message = "Click 'Convert fDOM to TOC'")
    )
    
    observeEvent(input$convert_fDOM3,{
      
      toc.out3$toc = 1.86859 + 0.10846*input$fdom3
      
    })
    
    paste0("The estimated TOC level associated with this level of fDOM is ~",round(toc.out3$toc, 1)," mg/L.")
  })
  
  #*# Plot fDOM (variable)
  plot.fDOM.var <- reactiveValues(main=NULL)
  
  observe({
    
    output$fDOM_plot_var <- renderPlotly({ 
      
      df <- reservoir_data %>%
        filter(variable %in% c("fDOM_QSU_mean" ) & site_id == "fcre" & datetime >= "2022-03-15" & datetime <= "2022-04-15") %>%
        mutate(observation = observation^1.5)
      
      p <- ggplot(data = df, aes(x = datetime, y = observation))+
        geom_point(aes(color = "surface water fDOM"))+
        geom_line(aes(color = "surface water fDOM"))+
        xlab("")+
        ylab("fDOM (QSU)")+
        scale_color_manual(values = c("surface water fDOM" = "brown"), name = "")+
        theme_bw()
      
      plot.fDOM.var$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE, tooltip=c("x", "y", "color")))
      
    })
    
  })
  
  # convert fdom to toc
  toc.out4 <- reactiveValues(toc=NULL)
  
  output$toc_out4 <- renderText({
    
    validate(
      need(input$convert_fDOM4 > 0,
           message = "Click 'Convert fDOM to TOC'")
    )
    
    observeEvent(input$convert_fDOM4,{
      
      toc.out4$toc = 1.86859 + 0.10846*input$fdom4
      
    })
    
    paste0("The estimated TOC level associated with this level of fDOM is ~",round(toc.out4$toc, 1)," mg/L.")
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
