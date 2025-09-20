ui <- function(req) {
  
  tagList( # Added functionality for not losing your settings
    # shinythemes::themeSelector(), # user-defined theme
    # Java to prompt the students to click a button
    # Java script https://community.rstudio.com/t/keeping-track-of-idle-time-during-app-usage/1735
    tags$script("
              (function() {
  var timeoutWarningMsecs = 12 * 60 * 1000;
  var idleTimer;

  function onTimeout() {
    alert('Warning: Session is about to time out! Please click a button to prevent losing progress.');
  }

  function startIdleTimer() {
    if (idleTimer) clearTimeout(idleTimer);
    idleTimer = setTimeout(onTimeout, timeoutWarningMsecs);
  }

  $(document).on('shiny:message shiny:inputchanged', startIdleTimer);

})();"),
    tags$style(type = "text/css", "text-align: justify"),
    tags$head(tags$link(rel = "shortcut icon", href = "macroeddi_ico_green.ico")), # Add icon for web bookmarks
    tags$head(includeHTML(("google-analytics.html"))),
    tags$header(
      introBox(
        img(src = "eddie_banner_2020_test.png", height = 100,
            width = 1544, top = 5),
        data.step = 1,
        data.intro = help_text["welcome", 1]
      )
    ),
    fluidPage(
      column(11,
             br(),
             p(tags$b("Teaching materials associated with this module can be found at ",
                      tags$a(href="https://serc.carleton.edu/eddie/teaching_materials/modules/module10.html", 
                             "https://serc.carleton.edu/eddie/teaching_materials/modules/module10.html.", target="_blank"))),
             h2(tags$b("Module 10: Exploring Tradeoffs in Water Quality Management Using Environmental Data"))
      ),
      column(1, align = "right",
             br(),
             introBox(
               actionButton("help", label = "Help", icon = icon("question-circle"))
             )
      )
    ),
    navbarPage(position = "static-top", id = "maintab",
               tags$header(
                 fluidRow(
                 )
               ),
               # 1. Introduction ----
               tabPanel(introBox(tab_names["mtab1", 2],
                                 data.step = 2,
                                 data.intro = help_text["tab_nav1", 1]
                                 ),
               value = "mtab1",
               introjsUI(), # must include in UI
               withMathJax(), # NEEDS to be here for rendering eqn's in data.table
               
               tags$style(".btn-file {
             background-color:#98CAB2;
             border-color: #2E4F84;
             }

             .progress-bar {
             background-color: #2E4F84;
             }"),
               # Change progress bar color
               tags$style(paste0("
               .irs-grid-text { font-size: 10pt; }
                                   .irs-bar,
.irs-bar-edge,
.irs-single,
.irs-grid-pol {
  background: ", slider_col, ";
  border-color: ", slider_col, ";
}")),
               includeCSS("www/slider_cols.css"),
               tags$style(HTML("
               .irs-bar {
                        border-color: transparent;
                        background-color: transparent;
                        }
                        #first {
                        border: 4px double red;
                        }
                        #13a_graz {
                        margin-bottom: 10px;
                        }
                        #bla_border {
                        border: 2px solid black;
                        }
                        #bla_border2 {
                        border: 1px solid black;
                        box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
                        }
                        #txt_j {
                        text-align: justify;
                        }
                        #txt_c {
                        text-align: center;
                        }
                        #txt_l {
                        text-align: left;
                        }
                        #ackn {
                        color: gray;
                        font-size: 12px
                        }
                        #pheno img {
                        transition:transform 0.25s ease;
                        max-width: 100%; width: 100%; height: auto
                        }
                        #nextBtn1:hover {
                        background-color: yellow;
                        }
                        #dl_btn {
                        width:290px
                        }
                        #pheno:hover img{
    -webkit-transform:scale(1.5);
    transform:scale(1.5);
}
                        #wh_link a {
                        color: #FFFFFF
                        }
                        #q6_tab {
                        'border':'1px solid #ddd'
                        }
                        .box.box-solid.box-primary>.box-header {

                }

                .box.box-solid.box-primary{

                background:#C1E4E2
                }
                .box.box-solid.box-success{

                background: #F5F094;
                }
                .box.box-solid.box-info{

                background: #DDE4E1;
                }
                .box.box-solid.box-warning>.box-header {

                }

                .box.box-solid.box-warning{

                background:#D6AB9C
                }
                        ")),
               introBox(
                 fluidRow(
                   column(6,
                          #* Module text ====
                          introBox(data.step = 10,
                          data.intro = help_text["thank_you", 1],
                          h2("Exploring Tradeoffs in Water Quality Management Using Environmental Data")
                          ),
                          h3("Focal question"),
                          h4(tags$b(tags$i("How can we use environmental data to inform our understanding of the tradeoffs involved in water management decision-making?"))),
                          h3("Summary"),
                          p("Many water management decisions come with tradeoffs. One important example of such a decision is the use of chlorine in the drinking water treatment process. Chlorination is an important disinfection step in water treatment and is needed to protect water consumers from harmful pathogens (such as bacteria). However, when there are high amounts of organic matter in the raw water, chlorination can result in the formation of potentially cancer-causing disinfection byproducts. Environmental sensor data on water quality conditions, such as organic matter measurements from drinking water reservoirs, can help inform water management decision-making and reduce the risk of unintended consequences due to use of chlorine in water treatment."),
                          p("In this module, you will explore organic matter data collected from drinking water reservoirs and learn how to interpret these data to inform your decision-making about chlorination during drinking water treatment."),
                          h3("Learning Outcomes"),
                          tags$line(),
                          tags$ul(
                            tags$li(id = "txt_j", module_text["LO1", ]),
                            tags$li(id = "txt_j", module_text["LO2", ]),
                            tags$li(id = "txt_j", module_text["LO3", ]),
                            tags$li(id = "txt_j", module_text["LO4", ])
                          )
                   ),
                   column(6, 
                          br(), br(), br(),
                          img(src = "mod10_conceptual_figure.png", height = "100%",
                              width = "100%")
                   )
                 )
               ),
               hr(),
               fluidRow(
                              column(11,  
                                     introBox(data.step = 4, data.intro = help_text["workflow", 1],
                                       h3("Workflow for this module")
                                     ),
                                         box(id = "box1", width = 12, status = "success", solidHeader = TRUE,
                                             fluidRow(
                                             column(11, offset = 1, h4(tags$b(module_text["workflow1", ]))))),
                                         br(),br(),br(),
                                     fluidRow(
                                       column(12, offset = 1,
                                              introBox(data.step = 5, data.intro = help_text["videos", 1],
                                              HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/T1-k7VYwsHg" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                              )
                                              )
                                     ),
                                         br(),
                                         box(id = "box1", width = 12, status = "success", solidHeader = TRUE,
                                             fluidRow(
                                               column(11, offset = 1, h4(tags$b(module_text["workflow2", ]))))),
                                         br(),br(),br(),br(),br(),
                                     fluidRow(
                                       column(12, offset = 1,
                                              HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/T1-k7VYwsHg" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                       )
                                     ),
                                     br(),
                                         box(id = "box1", width = 12, status = "success", solidHeader = TRUE,
                                             fluidRow(
                                               column(11, offset = 1, h4(tags$b(module_text["workflow4", ]))))),
                                         br(),br(),br(),br(),
                                     tags$style(type="text/css", "#stud_dl {background-color:#98CAB2;color: white}"),
                                     wellPanel(
                                       fluidRow(
                                         column(6, align = "center", offset = 3,
                                                downloadButton(outputId = "stud_dl", label = "Download Student Handout")
                                         )
                                       )
                                     ),
                                         box(id = "box1", width = 12, status = "success", solidHeader = TRUE,
                                             fluidRow(
                                               column(11, offset = 1, h4(tags$b(module_text["workflow5", ]))))),
                                         br(),br(),br(),
                                         box(id = "box1", width = 12, status = "success", solidHeader = TRUE,
                                             fluidRow(
                                               column(11, offset = 1, h4(tags$b(module_text["workflow6", ]))))),
                                         br(),br(),br(),
                                         box(id = "box1", width = 12, status = "success", solidHeader = TRUE,
                                             fluidRow(
                                               column(11, offset = 1, h4(tags$b(module_text["workflow7", ]))))),
                                         br(),br(),br()
                              )
               ), 
               hr(),
               fluidRow(
                 column(4,
                        h3("Introductory presentation"),
                        p(tags$i("Click through the slides to review some of the main points from the introductory presentation to help you answer the questions below.")),
                        p(tags$b("What are disinfection byproducts?")),
                        tags$ul(
                          tags$li(module_text["dbp_definition", ])
                        ),
                        p(tags$b(" How do disinfection byproducts form?")),
                        tags$ul(
                          tags$li(module_text["dbp_formation", ])
                        ),
                        p(tags$b("How can environmental data help us avoid the formation of disinfection byproducts?")),
                        tags$ul(
                          tags$li(module_text["environmental_data", ])
                        ),
                 ),
                 column(8, offset = 0, align = "center",
                        h3("Key Slides",
                           align = "center"),
                        h5("Click the arrows to navigate through the slides", align = "center"),
                        wellPanel(
                          introBox(data.step = 6, data.intro = help_text["slides", 1],
                          slickROutput("slides", width = "700px", height = "525px")
                          )
                        )
                 )
               ),
               hr(),
               fluidRow(
                 column(10, align = "left",
                        box(id = "box1", width = 10, status = "primary",
                            solidHeader = TRUE,
                            fluidRow(
                              column(8, offset = 1,
                                     h3("Let's begin..."),
                                     p(id = "txt_j", "Open your Canvas quiz or Word document. Then, answer the following questions in the Canvas quiz or Word document."),
                                     introBox(
                                       h3(tags$b("Think about it!")),
                                       p(tags$b(quest["q1", 1])),
                                       tags$ul(
                                         tags$li(id = "txt_j", quest["q1a", ]),
                                         tags$li(id = "txt_j", quest["q1b", ]),
                                         tags$li(id = "txt_j", quest["q1c", ]),
                                       ),
                                       p(tags$b(quest["q2", 1])),
                                       tags$ul(
                                         tags$li(id = "txt_j", quest["q2a", ]),
                                         tags$li(id = "txt_j", quest["q2b", ]),
                                         tags$li(id = "txt_j", quest["q2c", ])
                                       ),
                                       p(tags$b(quest["q3", 1])),
                                       tags$ul(
                                         tags$li(id = "txt_j", quest["q3a", ]),
                                         tags$li(id = "txt_j", quest["q3b", ]),
                                         tags$li(id = "txt_j", quest["q3c", ]),
                                         tags$li(id = "txt_j", quest["q3d", ])
                                       ),
                                       data.step = 8, data.intro = help_text["questions", 1]
                                     )
                              )
                            )
                        )
                 )
               ),
               hr(),
               fluidRow(
                 column(6,
                        h3("Data sources"),
                        p(HTML(paste0('This module will introduce how to use high-frequency water quality data to inform drinking water management using data from  ', a(href = "https://www.ltreb-reservoirs.org/", "Virginia Reservoirs LTREB sites", target = "_blank"), ", which are drinking water supply reservoirs located in southwest Virginia and owned and operated by the Western Virginia Water Authority.")))
                 ),
                 column(6, align = "center",
                        a(
                          href = "https://www.ltreb-reservoirs.org/",
                          img(src = "ltreb.png", title = "Virginia Reservoirs LTREB logo", height = "80%",
                              width = "80%"), target = "_blank"
                        ),
                        a(
                          href = "https://www.westernvawater.org/",
                          img(src = "wvwa.png", title = "Western Virginia Water Authority logo", height = "80%",
                              width = "80%"), target = "_blank"
                        )
                 )
               )
               ),
               
               # 2. Activity A ----
               tabPanel(title = tab_names["mtab2", 2], value = "mtab2",
                        img(src = "eddie_banner_2020_test.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity A - Explore how disinfection byproducts can be formed during chlorination"),
                                           p(module_text["act_A", ])
                                 )
                          ),
                          column(12,
                                 box(id = "box1", width = 10, status = "success",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              introBox(
                                                h3("Objective 1: Understand factors affecting DBP formation and drinking water thresholds for DBPs"))
                                       )
                                     )
                                 )
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(4,
                                 h3("Disinfection byproducts"),
                                 p(tags$i("Watch the video and click through the slides to understand what disinfection byproducts are and their regulatory thresholds.")),
                                 br(),
                                 box(id = "box12", width = 12, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1, align = "left",
                                              h4("Video"),
                                              HTML('<iframe width="280" height="157" src="https://www.youtube.com/embed/T1-k7VYwsHg" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                              h4("Questions"),
                                              p(tags$b(quest["q4", 1])),
                                              tags$ul(
                                                tags$li(id = "txt_j", quest["q4a", ]),
                                                tags$li(id = "txt_j", quest["q4b", ]),
                                                tags$li(id = "txt_j", quest["q4c", ]),
                                                tags$li(id = "txt_j", quest["q4d", ]),
                                                tags$li(id = "txt_j", quest["q4e", ]),
                                                tags$li(id = "txt_j", quest["q4f", ]),
                                                tags$li(id = "txt_j", quest["q4g", ])
                                              ),
                                              p(tags$b(quest["q5", 1])),
                                              tags$ul(
                                                tags$li(id = "txt_j", quest["q5a", ]),
                                                tags$li(id = "txt_j", quest["q5b", ]),
                                                tags$li(id = "txt_j", quest["q5c", ])
                                              ),
                                              p(tags$b(quest["q6", 1])),
                                              tags$ul(
                                                tags$li(id = "txt_j", quest["q6a", ]),
                                                tags$li(id = "txt_j", quest["q6b", ]),
                                                tags$li(id = "txt_j", quest["q6c", ])
                                              )
                                       )
                                     )
                                 )
                          ),
                          column(8, offset = 0, align = "center",
                                 h3("Disinfection byproduct formation and regulatory thresholds",
                                    align = "center"),
                                 h5("Click the arrows to navigate through the slides", align = "center"),
                                 wellPanel(
                                   slickROutput("dbp_formation_thresholds_slides", width = "700px", height = "525px")
                                 )
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(12,
                                 box(id = "box1", width = 10, status = "success",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              introBox(
                                                h3("Objective 2: Explore tradeoffs in chlorination vs. DBP formation"))
                                       )
                                     )
                                 )
                          )
                        ),
                        fluidRow(
                          column(4,
                                 h3("Tradeoffs"),
                                 p(tags$i("Watch the video and click through the slides to understand tradeoffs operators may encounter between removing harmful microbes from drinking water and risking formation of DBPs.")),
                                 br(),
                                 box(id = "box12", width = 12, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1, align = "left",
                                              h4("Video"),
                                              HTML('<iframe width="280" height="157" src="https://www.youtube.com/embed/T1-k7VYwsHg" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                              h4("Questions"),
                                              p(tags$b(quest["q7", 1])),
                                              tags$ul(
                                                tags$li(id = "txt_j", quest["q7a", ]),
                                                tags$li(id = "txt_j", quest["q7b", ]),
                                                tags$li(id = "txt_j", quest["q7c", ]),
                                                tags$li(id = "txt_j", quest["q7d", ])
                                              ),
                                              p(tags$b(quest["q8", 1])),
                                              tags$ul(
                                                tags$li(id = "txt_j", quest["q8a", ]),
                                                tags$li(id = "txt_j", quest["q8b", ]),
                                                tags$li(id = "txt_j", quest["q8c", ]),
                                                tags$li(id = "txt_j", quest["q8d", ])
                                              ),
                                              p(tags$b(quest["q9", 1])),
                                              p(tags$i("Hint: Follow the steps in the slide deck and video to solve the problem. Round your calculations to the nearest hundredth.")),
                                              tags$ul(
                                                tags$li(id = "txt_j", quest["q9a", ]),
                                                tags$li(id = "txt_j", quest["q9b", ]),
                                                tags$li(id = "txt_j", quest["q9c", ])
                                              )
                                       )
                                     )
                                 )
                          ),
                          column(8, offset = 0, align = "center",
                                 h3("Tradeoffs between chlorination vs. formation of DBPs",
                                    align = "center"),
                                 h5("Click the arrows to navigate through the slides", align = "center"),
                                 wellPanel(
                                   slickROutput("dbp_tradeoffs_slides", width = "700px", height = "525px")
                                 )
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(12,
                                 h2("Next step"),
                                 h4("You will select a focal reservoir, visualize raw water quality data from that reservoir, and learn how these data can be related to DBP formation risk."),
                                 
                          )
                        )
               ),
               # 6. Activity B ----
               tabPanel(title = tab_names["mtab3", 2], value = "mtab3",
                        img(src = "eddie_banner_2020_test.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity B - Explore environmental data that can indicate the presence of DBP precursors"),
                                           p(module_text["act_B", ])
                                 )
                          ),
                          column(12,
                                 box(id = "box1", width = 10, status = "success",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              introBox(
                                                h3("Objective 3: Select and learn about a focal drinking water reservoir"))
                                       )
                                     )
                                 )
                          )
                        ),
                        hr(),
                        fluidRow(
                          #** Choose site ----
                          column(4,
                                 h4("Site Names"),
                                 p("Select a site in the table to highlight on the map"),
                                 conditionalPanel("input.row_num > 25",
                                                  selectizeInput("row_num", "Select row",
                                                                 choices = 1:nrow(sites_df),
                                                                 options = list(
                                                                   placeholder = 'Please select a row',
                                                                   onInitialize = I('function() { this.setValue(""); }'))
                                                  )
                                 ),
                                 DTOutput("table01", fill = TRUE),
                                 fluidRow(
                                   column(12,
                                          wellPanel(
                                            h4(tags$b("About Site")),
                                            textOutput("site_info")
                                          )
                                   )
                                 )
                          ),
                          #** Site map ----
                          column(4,
                                 h4("Map of Virginia Reservoir LTREB sites"),
                                 wellPanel(
                                   leafletOutput("ltrebmap")
                                 )
                          ),
                          #** Site photo ----
                          column(4,
                                 h4("Site photo"),
                                 wellPanel(
                                   imageOutput("site_photo"),
                                   p(id = "txt_j", module_text["site_photo", ])
                                 )
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(12, align = "left",
                                 box(id = "box3", width = 10, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(5, offset = 1,
                                              h3("Questions"),
                                              p(tags$b(quest["q10", 1])),
                                              p(tags$b(quest["q11", 1])),
                                              p(tags$b(quest["q12", 1])),
                                              tags$ul(
                                                tags$li(id = "txt_j", quest["q12a", ]),
                                                tags$li(id = "txt_j", quest["q12b", ]),
                                                tags$li(id = "txt_j", quest["q12c", ]),
                                                tags$li(id = "txt_j", quest["q12d", ])
                                              ),
                                              p(tags$b(quest["q13", 1])),
                                              p(tags$b(quest["q14", 1]))
                                       ),
                                       column(5, 
                                              h3(""),
                                              p("Virginia's Water Quality Assessment Guidance Manual gives the following guidance on water quality evaluation using a trophic state index (TSI), which may be calculated from Secchi depth (SD), chlorophyll-a (CA) in the top 1 meter of the water column, or total phosphorus (TP) in the top 1 meter of the water column:"),
                                              p(tags$em("A trophic state index value of 60 or greater for any one of the 3 indices will indicate that nutrient enrichment from anthropogenic sources are adversely interfering, directly or indirectly, with the designated uses. A TSI value of 60 corresponds to a CA concentration of 20 ug/l, a SD of 1 meter, and a TP concentration of 48 ug/l.")),
                                              p(tags$b(quest["q15", 1]))
                                       )
                                     )
                                 )
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(12,
                                 h4("Now we will visualize high-frequency data from your chosen reservoir and explore how these data can be related to potential DBP precursors."))
                        ),
                        hr(),
                        fluidRow(
                          column(8,
                                 h3("A note on reading and interpreting graphs"),
                                 p("Please watch the video on the right for a refresher course on reading and interpreting graphs. This may help you to answer the questions about water temperature data as well as other questions throughout the module.")
                          ),
                          column(4,
                                 h4("Video: Reading and interpreting graphs"),
                                 HTML('<iframe width="280" height="157" src="https://www.youtube.com/embed/AhnwYmHvHSc?si=r0LzHH-t8fAE3Lt9" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(12,
                                 box(id = "box1", width = 10, status = "success",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              introBox(
                                                h3("Objective 4: View and interpret organic matter data from your focal reservoir"))
                                       )
                                     )
                                 )
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(4,
                                 h3("Fluorescent dissolved organic matter (fDOM)"),
                                 p(tags$i("Watch the video and click through the slides to understand how fDOM data relate to possible DBP formation. The information in the presentation is also summarized in text below to help you answer the questions.")),
                                 br(),
                                 box(id = "box12", width = 12, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              h4("Video"),
                                              HTML('<iframe width="280" height="157" src="https://www.youtube.com/embed/T1-k7VYwsHg" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                              h4("Questions"),
                                              p(tags$b(quest["q16", 1])),
                                              tags$ul(
                                                tags$li(id = "txt_j", quest["q16a", ]),
                                                tags$li(id = "txt_j", quest["q16b", ]),
                                                tags$li(id = "txt_j", quest["q16c", ]),
                                                tags$li(id = "txt_j", quest["q16d", ])
                                              ),
                                              p(tags$b(quest["q17", 1])),
                                              tags$ul(
                                                tags$li(id = "txt_j", quest["q17a", ]),
                                                tags$li(id = "txt_j", quest["q17b", ]),
                                                tags$li(id = "txt_j", quest["q17c", ]),
                                                tags$li(id = "txt_j", quest["q17d", ])
                                              ),
                                              p(tags$b(quest["q18", 1])),
                                              tags$ul(
                                                tags$li(id = "txt_j", quest["q18a", ]),
                                                tags$li(id = "txt_j", quest["q18b", ]),
                                                tags$li(id = "txt_j", quest["q18c", ]),
                                                tags$li(id = "txt_j", quest["q18d", ])
                                              )
                                       )
                                     )
                                 )
                          ),
                          column(8, offset = 0, 
                                 h3("Using fluorescent dissolved organic matter as an indicator of DBP precursors",
                                    align = "center"),
                                 h5("Click the arrows to navigate through the slides", align = "center"),
                                 wellPanel(
                                   slickROutput("fdom_slides", width = "700px", height = "525px")
                                 ),
                                 p(tags$b("What is fDOM?")),
                                 tags$ul(
                                   tags$li(module_text["fDOM", ])
                                 ),
                                 p(tags$b("How do we measure fDOM?")),
                                 tags$ul(
                                   tags$li(module_text["fDOM_measure", ])
                                 ),
                                 p(tags$b("How is fDOM related to DBPs?")),
                                 tags$ul(
                                   tags$li(module_text["fDOM_DBPs", ])
                                 )
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(4,
                                 h3("Plot fDOM data"),
                                 p("Click the button below to plot fDOM data at your chosen reservoir site."),
                                 actionButton("plot_fDOM", "Plot high-frequency fDOM data"),
                                 br(),br(),
                                 box(id = "box12", width = 12, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              h4("Questions"),
                                              p(tags$b(quest["q19", 1])),
                                              p(tags$b(quest["q20", 1])),
                                              p(tags$b(quest["q21", 1]))
                                       )
                                     )
                                 )
                          ),
                          column(8,
                                 wellPanel(
                                   introBox(data.step = 7, data.intro = help_text["plots", 1],
                                   plotlyOutput("fDOM_plot")
                                   )
                                 )
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(6,
                                 h3("Converting fDOM to TOC"),
                                 p("We have developed a relationship between fDOM (QSU) and TOC (mg/L) for your focal reservoir."),
                                 p("This allows us to assess the possible levels of DBP precursors in the raw water in terms of TOC."),
                                 p("You can input an fDOM reading in QSU into the box below, click 'Convert', and see the corresponding TOC level in mg/L."),
                                 numericInput( 
                                   "fdom", 
                                   "fDOM (QSU)", 
                                   value = 17.5, 
                                   min = 0.1, 
                                   max = 30,
                                   step = 0.1
                                 ),
                                 actionButton("convert_fDOM", "Convert fDOM to TOC"),
                                 br(),br(),
                                 wellPanel(
                                   textOutput("toc_out")
                                 ),
                                 br(),br(),
                                 p("After completing Q.23, if you would like to learn more about how the EPA regulates TOC, you may download and read the rule below."),
                                 h4("Download and read the U.S. EPA Stage 1 Disinfectants and Disinfection Byproducts Rule"),
                                 tags$style(type="text/css", "#rule_dl {background-color:#98CAB2;color: white}"),
                                 wellPanel(
                                   fluidRow(
                                     column(6, align = "center", offset = 3,
                                            downloadButton(outputId = "rule_dl", label = "Download EPA Rule")
                                     )
                                   )
                                 )
                                 
                          ),
                          column(6,
                                 box(id = "box12", width = 12, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              h4("Questions"),
                                              p(tags$b(quest["q22", 1])),
                                              p(tags$b(quest["q23", 1])),
                                              img(src = "EPA_TOC_rule.png", height = "100%",
                                                  width = "100%"),
                                              br(),br()
                                       )
                                     )
                                 )
                                 )
                        ),
                        hr(),
                        fluidRow(
                          column(12,
                                h2("Next step"),
                                h4("You will complete a case study and use high-frequency fDOM data to make water treatment decisions to minimize the risk of DBP formation."),

                          )
                        )
               ),
               # 7. Activity C ----
               tabPanel(title = tab_names["mtab4", 2], value = "mtab4",
                        img(src = "eddie_banner_2020_test.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity C - Use environmental data to inform water treatment decisions"),
                                           p(module_text["act_C", ])
                                 )
                          ),
                        column(12,
                               box(id = "box1", width = 10, status = "success",
                                   solidHeader = TRUE,
                                   fluidRow(
                                     column(10, offset = 1,
                                            introBox(
                                              h3("Objective 6: Use fluorescent dissolved organic matter data to make coagulation decisions"))
                                     )
                                   )
                               )
                        )
               ),
              hr(),
              fluidRow(
                column(6,
                       box(id = "box12", width = 12, status = "warning",
                           solidHeader = TRUE,
                           fluidRow(
                             column(10, offset = 1,
                                    h3("Operation scenario"),
                                    h4("Decide: should I increase my coagulation time to prevent potential DBP formation during treatment?"),
                                    p("You are operating a reservoir water treatment plant that has experienced high levels of TOC in the filtered water in the past. While there have not been DBPs detected at the treatment plant or in the distribution system to date, your supervisor is concerned about the risk of DBP formation due to high TOC levels that are sometimes observed in both the raw and filtered water."),
                                    p("Because TOC samples are only taken once per month, your supervisor recommends that you monitor the daily fDOM data collected from the raw water to determine whether an increase in coagulation time is needed to mitigate the risk of DBP formation."),
                                    h4("Your objective is to determine whether to increase coagulation time to ensure you meet the regulatory limit for TOC in the filtered water (and therefore hopefully avoid formation of DBPs)."),
                                    p("Previous operators at this reservoir have found that raw water concentrations > XX mg/L of TOC have often led to exceedance of TOC guidelines in the filtered water."),
                                    p("View the fDOM data during three different times of the year (winter, spring, and summer) and use the fDOM to TOC converter to decide whether you should increase coagulation time to reduce DBP formation risk.")
                             )
                           )
                       )
                ),
                column(6,
                       img(src = "SHRtreatmentPlant.png", height = "90%", id = "bla_border",
                           width = "90%", tags$style("border: solid 2px black;")),
                       p(tags$em("Spring Hollow Water Treatment Facility, Roanoke County, VA")),
                       p(tags$em("Photo credit: Western Virginia Water Authority"))
                )
              ),
              hr(),
              fluidRow(
                column(12,
                       h3("Use the fDOM to TOC converter to help you make management decisions"),
                       p("We have developed a relationship between fDOM (QSU) and TOC (mg/L) for your reservoir."),
                       p("This allows us to assess the possible levels of DBP precursors in the raw water in terms of TOC."),
                       p("You can input an fDOM reading in QSU into the box below, click 'Convert', and see the corresponding TOC level in mg/L."),
                       numericInput( 
                         "fdom1", 
                         "fDOM (QSU)", 
                         value = 17.5, 
                         min = 0.1, 
                         max = 30,
                         step = 0.1
                       ),
                       actionButton("convert_fDOM1", "Convert fDOM to TOC"),
                       br(),br(),
                       wellPanel(
                         textOutput("toc_out1")
                       )
                       
                )
              ),
              hr(),
              fluidRow(
                column(4,
                       h3("Management decision #1: Winter data"),
                       p("Examine the previous month of fDOM data from the raw water in your reservoir on the right. Then answer the questions below."),
                       box(id = "box12", width = 12, status = "primary",
                           solidHeader = TRUE,
                           fluidRow(
                             column(10, offset = 1,
                                    h4("Questions"),
                                    p(tags$b(quest["q24", 1])),
                                    p(tags$b(quest["q25", 1])),
                                    tags$ul(
                                      tags$li(id = "txt_j", quest["q25a", ]),
                                      tags$li(id = "txt_j", quest["q25b", ]),
                                      tags$li(id = "txt_j", quest["q25c", ]),
                                      tags$li(id = "txt_j", quest["q25d", ])
                                    ),
                                    p(tags$b(quest["q26", 1])),
                                    tags$ul(
                                      tags$li(id = "txt_j", quest["q26a", ]),
                                      tags$li(id = "txt_j", quest["q26b", ])
                                    )
                             )
                           )
                       )
                       ),
                column(8,
                       wellPanel(
                         plotlyOutput("fDOM_plot_dec")
                       )
                       )
              ),
              hr(),
              fluidRow(
                column(4,
                       h3("Management decision #2: Spring data"),
                       p("Examine the previous month of fDOM data from the raw water in your reservoir on the right. Then answer the questions below."),
                       box(id = "box12", width = 12, status = "primary",
                           solidHeader = TRUE,
                           fluidRow(
                             column(10, offset = 1,
                                    h4("Questions"),
                                    p(tags$b(quest["q27", 1])),
                                    p(tags$b(quest["q28", 1])),
                                    tags$ul(
                                      tags$li(id = "txt_j", quest["q28a", ]),
                                      tags$li(id = "txt_j", quest["q28b", ]),
                                      tags$li(id = "txt_j", quest["q28c", ]),
                                      tags$li(id = "txt_j", quest["q28d", ])
                                    ),
                                    p(tags$b(quest["q29", 1])),
                                    tags$ul(
                                      tags$li(id = "txt_j", quest["q29a", ]),
                                      tags$li(id = "txt_j", quest["q29b", ])
                                    )
                             )
                           )
                       )
                ),
                column(8,
                       wellPanel(
                         plotlyOutput("fDOM_plot_var")
                       )
                )
              ),
              hr(),
              fluidRow(
                column(4,
                       h3("Management decision #3: Summer data"),
                       p("Examine the previous month of fDOM data from the raw water in your reservoir on the right. Then answer the questions below."),
                       box(id = "box12", width = 12, status = "primary",
                           solidHeader = TRUE,
                           fluidRow(
                             column(10, offset = 1,
                                    h4("Questions"),
                                    p(tags$b(quest["q30", 1])),
                                    p(tags$b(quest["q31", 1])),
                                    tags$ul(
                                      tags$li(id = "txt_j", quest["q31a", ]),
                                      tags$li(id = "txt_j", quest["q31b", ]),
                                      tags$li(id = "txt_j", quest["q31c", ]),
                                      tags$li(id = "txt_j", quest["q31d", ])
                                    ),
                                    p(tags$b(quest["q32", 1])),
                                    tags$ul(
                                      tags$li(id = "txt_j", quest["q32a", ]),
                                      tags$li(id = "txt_j", quest["q32b", ])
                                    )
                             )
                           )
                       ) 
                ),
                column(8,
                       wellPanel(
                         plotlyOutput("fDOM_plot_inc")
                       )
                )
              ),
              hr(),
              fluidRow(
                column(12,
                       introBox(data.step = 9, data.intro = help_text["finish", 1],
                       h2("Completed Module!"),
                       h3("You have completed the module! Congratulations!"),
                       h4("Please check through the answers in your Canvas quiz before you submit the quiz to your instructor."),
                       h4("You’ve now made operations decisions informed by high-frequency water quality data - well done!")
                       )
                )
              )
               )
    ),
    # Tab navigation buttons ----
    br(), hr(),
    useShinyjs(),
    introBox(
      # h4("Use the buttons below to navigate through the tabs", align = "center"),
      box(width = 12, status = "info",
          solidHeader = TRUE,
          fluidRow(
            
            column(5, align = "center",
                   br(),
                   hover_action_button(
                     inputId = "prevBtn1",
                     label = "< Previous",
                     button_animation = "glow",
                     style = paste0("color: ", nav_txt, "; background-color: ", nav_butt, "; border-color: #00664B; padding:15px; font-size:22px;")
                   ),
                   bsTooltip("prevBtn1", title = "Navigate to previous tab", placement = "left", trigger = "hover"),
                   br(), br()
                   
            ),
            column(2, align = "center",
                   br(),
                   br(), br()
            ),
            column(5, align = "center",
                   br(),
                   use_hover(popback = TRUE),
                   hover_action_button(
                     inputId = "nextBtn1",
                     label = "Next >",
                     button_animation = "glow",
                     style = paste0("color: ", nav_txt, "; background-color: ", nav_butt, "; border-color: #00664B; padding:15px; font-size:22px;")
                   ),
                   bsTooltip("nextBtn1", title = "Navigate to next tab", placement = "right", trigger = "hover"),
                   br(), br()
                   # )
            )
          )
      ), data.step = 3, data.intro = help_text["tab_nav2", 1], data.position = "right"
    ),
    hr(),
    fluidRow(
      column(8, offset = 1,
             br(),
             p(module_text["acknowledgement", ], id = "ackn"),
             p(app_update_txt, id = "ackn")
      )
    )
  )
}

shinyUI(ui)

# end
