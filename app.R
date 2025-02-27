# Set-up ====
#Chargement des packages manquants
packages <- c("shiny", "shinydashboard", "shinythemes", "data.table", "bslib", "shinyglide",
              "leaflet", "tidyverse", "sf", "terra", "fresh", "leaflet.extras", "leafsync",
              "shinyWidgets", "DT", "DBI", "RSQLite", "plotly",
              "viridis", "shinycssloaders", "shinyjs", "shiny.i18n")

packages_manquants <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(packages_manquants) > 0) {
  install.packages(packages_manquants)
}

lapply(packages, library, character.only = TRUE)

options(shiny.error = function() { 
  cat("Shiny app error:\n")
  traceback()
})

## Traduction pour les input et utilisation de i18n ====
i18n <- Translator$new(translation_csvs_path = "translation/",
                       translation_csv_config = "translation/config.yaml")
i18n$set_translation_language("fr") 

translations_traj <- data.frame(
  id = c('TO', 'TI', 'TP'),
  fr = c("Trajectoire optimiste", "Trajectoire intermédiaire", "Trajectoire pessimiste"),
  it = c("Scenario ottimistico", "Scenario intermedio", "Scenario pessimistico"),
  stringsAsFactors = FALSE
)

translations_var <- data.frame(
  id = c('v', 'c'),
  fr = c("Volume", "Contribution annuelle des glaciers"),
  it = c("Volume", "Contributo annuo dei ghiacciai"),
  stringsAsFactors = FALSE
)

translations_file1 <- data.frame(
  id = c('rast1', 'vec1'),
  fr = c("Raster d'épaisseur", "Bassin versant"),
  it = c("Raster dello spessore", "Bacino idrografico"),
  stringsAsFactors = FALSE
)

translations_file2 <- data.frame(
  id = c('rast2', 'vec2'),
  fr = c("Raster d'épaisseur", "Bassin versant"),
  it = c("Raster dello spessore", "Bacino idrografico"),
  stringsAsFactors = FALSE
)

get_initial_language <- function() {
  return("fr")  
}

# User Interface ====
ui <- dashboardPage(

  
  ## Header ====
  dashboardHeader(
    title = HTML("ACLIMO <b><i>GLACIER</i></b>"),
    titleWidth = 250,
    tags$li(
      class = "dropdown",
      selectInput(
        "language_selector",
        NULL,
        choices = c("Français" = "fr", "Italiano" = "it"),
        selected = "fr",
        width = "120px",
        selectize = FALSE  
      ),
      tags$style(HTML("
        #language_selector {
          background-color: #60a7d1;
          color: white;
          border: 0px;
          border-radius: 0;
        }
      ")
      )
    )
  ),
  
  
  ## Sidebar ====
  dashboardSidebar(
    HTML(paste0(
      "<a href='https://www.interreg-alcotra.eu/fr/aclimo' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='https://www.interreg-alcotra.eu/sites/default/files/ACLIMO_logo_def%20%28002%29.png' width = '186'></a>",
      "<p style = 'text-align: center;'><small><a href='https://www.interreg-alcotra.eu/fr/aclimo' target='_blank'>ACLIMO | Alcotra</a></small></p>",
      "<br>"
    )),
    
    width = 250,
    sidebarMenuOutput("sidebar"),
    tags$div(
      style = "
  position: absolute;
  height: 150px;
  bottom: 0;
  left: 0;
  right: 0;
  padding: 2px;
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
  color: white;
  text-align: center;
  ",
      tags$a(
        href = "https://www.ige-grenoble.fr/",
        tags$div(
          tags$img(src = 'IGE logo.png', 
                   style = "width: 70px; height: 70px; object-fit: contain;"),
          style = "background-color: white; 
               width: 70px; 
               height: 70px; 
               border-radius: 50%; 
               display: flex; 
               justify-content: center; 
               align-items: center;"
        )
      ),
      actionLink("creditLink", 
                 label = tags$div(
                   style = "margin-top: 2px; font-size: 0.8em; color: gray;",
                   i18n$t("2025 - Made by")
                 )
      )
    )
  ),
  
  ## Body ====
  dashboardBody(
    
    usei18n(i18n),
    useShinyjs(),
    chooseSliderSkin("Flat"),
    use_theme(shinytheme('yeti')),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style_mainScrollbar.css")
      ),
    tags$script(HTML("$('body').addClass('fixed');")),
    tags$style(HTML("
    .js-irs-0 .irs-single,
    .js-irs-0 .irs-bar-edge,
    .js-irs-0 .irs-bar {background: #4c8bba;}
    .js-irs-0 .irs-single {background: #4c8bba; color: white;}")),
    
    ### Home ====
    tabItems(
      tabItem(tabName = "acc",
              fluidPage(
                fluidRow(
                  div(
                    style = "margin-left: 125px; margin-right: 125px; text-align: justify;",
                    div(
                      style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 10px;",
                      h1(
                        style = "margin: 0;",
                        strong(i18n$t("Le projet ALCOTRA - ACLIMO"))
                      ),
                      tags$a(
                        href = "https://www.interreg-alcotra.eu/fr/aclimo",
                        tags$img(src = 'INTERREG Aclimo logo.png', height = "80px", style = "object-fit: contain;"))
                    ),
                    hr(),
                    br(),
                    uiOutput("pres_aclimo"),
                    br(),
                    br(),
                    div(
                      style = "text-align: center;",
                      p(i18n$t("Logos des principaux contributeurs"))
                    ),
                    br(),
                    div(
                      style = "text-align: center;",
                      div(
                        style = "display: flex; justify-content: center; align-items: center; gap: 20px; max-width: 1200px; margin: 0 auto;",
                        div(
                          style = "display: flex; flex-direction: column; align-items: center; flex: 2;",
                          em(i18n$t("Parcs nationaux partenaires"), style = "font-size: 14px; margin-bottom: 10px;"),
                          div(
                            style = "display: flex; flex-direction: column; gap: 10px;",
                            div(
                              style = "display: flex; justify-content: center; gap: 10px;",
                              tags$a(
                                href = "https://www.mercantour-parcnational.fr/fr",
                                tags$img(src = "PNM logo.jpg", height = "80px", style = "object-fit: contain;")),
                              tags$a(
                                href = "https://www.vanoise-parcnational.fr/fr",
                                tags$img(src = "PNV logo.png", height = "80px", style = "object-fit: contain;")),
                              tags$a(
                                href = "https://www.ecrins-parcnational.fr/",
                                tags$img(src = "PNE logo.png", height = "80px", style = "object-fit: contain;"))
                            ),
                            div(
                              style = "display: flex; justify-content: center; gap: 10px;",
                              tags$a(
                                href = "https://www.pngp.it/",
                                tags$img(src = 'PNGP_partenaire-removebg-preview.png', height = "80px", style = "object-fit: contain;")),
                              tags$a(
                                href = "https://rivieradeifiori.travel/fr/le-parc-naturel-regional-des-alpes-liguriennes/",
                                tags$img(src = 'PNAL_logo-removebg-preview.png', height = "80px", style = "object-fit: contain;")),
                              tags$a(
                                href = "https://www.parcofluvialegessostura.it/",
                                tags$img(src = 'PNGS logo.png', height = "80px", style = "object-fit: contain;"))
                            ),
                            div(
                              style = "display: flex; justify-content: center; gap: 10px; align-items: center;",
                              tags$a(
                                href = "https://www.parchialpicozie.it/fr/",
                                tags$img(src = 'AreeP AC.png', height = "80px", style = "object-fit: contain;")),
                              tags$a(
                                href = "https://www.areeprotettealpimarittime.it/",
                                tags$img(src = 'AreeP AM.svg', height = "80px", style = "object-fit: contain;"))
                            )
                          )
                        ),
                        div(
                          style = "width: 2px; height: 280px; background-color: #cccccc; margin: 0 15px;"
                        ),
                        div(
                          style = "display: flex; flex-direction: column; align-items: center; flex: 1;",
                          em(i18n$t("Laboratoires partenaires"), style = "font-size: 14px; margin-bottom: 10px;"),
                          div(
                            style = "display: flex; flex-direction: column; gap: 20px; justify-content: center; align-items: center;",
                            tags$a(
                              href = "https://www.ige-grenoble.fr/",
                              tags$img(src = 'IGE logo.png', height = "80px", style = "object-fit: contain;")),
                            tags$a(
                              href = "https://www.cesbio.cnrs.fr/",
                              tags$img(src = 'CESBIO_logo-removebg-preview.png', height = "80px", style = "object-fit: contain;"))
                          )
                        )
                      )
                    ),
                    br(),
                    br(),
                    uiOutput("pres_stud_area"),
                    br(),
                    br(),
                    div(
                      style = "display: flex; flex-direction: column; align-items: center; height: 100%; justify-content: center;",
                      img(src = 'carto_PN.png', align = "center", width = "100%"),
                      div(
                        style = "margin-top: 15px; text-align: center;",
                        em(i18n$t('Carte de localisation des parcs nationaux des Ecrins, de la Vanoise et du Grand Paradis'))
                      )
                    )
                    
                  )
                )
              )
      ),
      
      ### Pres data ====
      tabItem(tabName = "data",
              fluidPage(
                fluidRow(
                  div(
                    style = "margin-left: 125px; margin-right: 125px; text-align: justify;",
                    h1(strong(i18n$t("Présentation des données"))),
                    hr(),
                    br(),
                    uiOutput("pres_data"),
                    h3(i18n$t('Visualisation des différentes trajectoires')),
                    hr()
                    ),
                  glide(
                    id = "plotglide",
                    height = "550px",
                    controls_position = "bottom",
                    next_label = paste(i18n$t("Film Tribolazione"), icon("caret-right")),
                    previous_label = paste(icon("caret-left"), i18n$t("Carte interactive")),
                    screen(
                      div(
                        style = "margin-left: 25px; margin-right: 25px; text-align: justify;",
                        sliderInput(
                          inputId = "annee",
                          label = h4(""),
                          min = 2020,
                          max = 2050,
                          value = 2025,
                          step = 1,
                          width = "100%",
                          sep = ''
                        ),
                        br(),
                        withSpinner(uiOutput("mapplotTCK", width = "100%"),
                                    type = 1, color = "darkgreen", color.background = '#ffffff'), 
                        br()
                      )),
                    screen(
                      div(
                        style = "margin-left: 25px; margin-right: 25px; text-align: justify;",
                        br(),
                        h4(i18n$t("Zoom sur le glacier de la Tribolazione")),
                        br(),
                        div(
                          style = "display: flex; flex-direction: column; justify-content: center; align-items: center;",
                          tags$video(src = "film_tribo.mp4", type = "video/mp4", autoplay = TRUE, controls = TRUE, width = "100%", height = "475px"),
                          br()
                        )
                      )
                    )
                  )
                  
                )
              )
      ),
      
      ### Select BV ====
      tabItem(tabName = "select_bv",
              fluidPage(
                h1(strong(i18n$t("Sélection des bassins versants"))),
                br(),
                p(
                  div(
                    style = "margin-left: 125px; margin-right: 125px; text-align: justify;",
                    em(i18n$t("La section Exploration se divise en deux parties. La sélection des bassins versants peut se faire à l'aide des deux déroulants ci-dessous ou en cliquant directement sur la carte interactive. La sélection d'un bassin versant contenant un bassin versant déjà sélectionné supprime automatiquement ce dernier de la sélection."))
                  )
                ),
                br(),
                hr(),
                fluidRow(column(6,
                                pickerInput("select_type",
                                            label = h4(i18n$t("Sélection des parcs nationaux")),
                                            choices = NULL, 
                                            options = list("actions-box" = TRUE),
                                            multiple = TRUE,
                                            selected = "PNV - Coeur")
                ),
                column(6,
                       pickerInput("selected_locations",
                                   label = h4(i18n$t("Sélection des bassins versants")),
                                   choices = "",
                                   options = list("actions-box" = TRUE,
                                                  size = 5),
                                   multiple = TRUE,
                                   selected = "")
                )
                ),
                br(),
                p(icon("triangle-exclamation"),i18n$t("La sélection de tout les bassisns versant peut prendre une dizaine de secondes!")),
                withSpinner(leafletOutput("map", height = "600px"), 
                            type = 1, color = "darkgreen", color.background = '#ffffff'),
                hr(),
                  div(
                    style = "margin-left: 125px; margin-right: 125px; text-align: center;",
                    em(i18n$t("Notez qu'une fonte rapide de la glace augmente la contribution des glaciers à l'hydrologie. Le bouton Suivant permet de visualiser les tables de la sélection."))
                  ),
                br(),
                fluidRow(
                  glide(
                    id = "plotglide",
                    height = "550px",
                    controls_position = "bottom",
                    next_label = paste(i18n$t("Suivant"), icon("caret-right")),
                    previous_label = paste(icon("caret-left"), i18n$t("Précedent")),
                    screen(
                      tags$head(
                        tags$style(HTML("
                        .custom-value-box .small-box {
                        height: 115px !important;
                        display: flex;}
                        .custom-value-box .small-box h1 {
                        font-size: 1.8em;}
                        .custom-value-box .small-box p {
                        font-size: 1em;}"))
                      ),
                      fluidRow(
                        column(
                          width = 6,
                          valueBoxOutput("n_glacBox1", width = NULL)  
                        ),
                        column(
                          width = 6,
                          valueBoxOutput("n_wsBox1", width = NULL)
                        )
                      ),
                      fluidRow(
                        column(
                          width = 4,
                          tags$div(
                            style = "background-color: transparent; text-align: center; height: 115px; display: flex; align-items: center; justify-content: center;",
                            h3(i18n$t("Pour la trajectoire optimiste :"))
                          )
                        ),
                        column(
                          width = 4,
                          tags$div(
                            class = "custom-value-box", # Ajout d'une classe CSS identique pour uniformiser
                            valueBoxOutput("lossBox1_SO", width = NULL)
                          )
                        ),
                        column(
                          width = 4,
                          tags$div(
                            class = "custom-value-box", # Ajout d'une classe CSS identique pour uniformiser
                            valueBoxOutput("waterBox1_SO", width = NULL)
                          )
                        )
                      ),
                      fluidRow(
                        column(
                          width = 4,
                          tags$div(
                            style = "background-color: transparent; text-align: center; height: 115px; display: flex; align-items: center; justify-content: center;",
                            h3(i18n$t("Pour la trajectoire intermédiaire :"))
                          )
                        ),
                        column(
                          width = 4,
                          tags$div(
                            class = "custom-value-box", # Ajout d'une classe CSS identique pour uniformiser
                            valueBoxOutput("lossBox1_00_22", width = NULL)
                          )
                        ),
                        column(
                          width = 4,
                          tags$div(
                            class = "custom-value-box", # Ajout d'une classe CSS identique pour uniformiser
                            valueBoxOutput("waterBox1_00_22", width = NULL)
                          )
                        )
                      ),
                      fluidRow(
                        column(
                          width = 4,
                          tags$div(
                            style = "background-color: transparent; text-align: center; height: 115px; display: flex; align-items: center; justify-content: center;",
                            h3(i18n$t("Pour la trajectoire pessimiste :"))
                          )
                        ),
                        column(
                          width = 4,
                          tags$div(
                            class = "custom-value-box", # Ajout d'une classe CSS identique pour uniformiser
                            valueBoxOutput("lossBox1_10_22", width = NULL)
                          )
                        ),
                        column(
                          width = 4,
                          tags$div(
                            class = "custom-value-box", # Ajout d'une classe CSS identique pour uniformiser
                            valueBoxOutput("waterBox1_10_22", width = NULL)
                          )
                        )
                      )
                    ),
                    screen(
                      fluidRow(
                        tabBox(
                          id = "tabset1",
                          title = i18n$t("Trajectoire optimiste"),
                          width = 12,
                          height = 525,
                          tabPanel(i18n$t('Volume'),
                                   em(i18n$t("* Le volume est exprimé en m^3")),
                                   DT::dataTableOutput("vtableSO")
                          ),
                          tabPanel(i18n$t('Contribution annuelle des glaciers'),
                                   em(i18n$t("* La contribution est exprimé en l/s")),
                                   DT::dataTableOutput("rtableSO"))
                        )
                      )
                    ),
                    screen(
                      fluidRow(
                        tabBox(
                          id = "tabset1",
                          title = p(i18n$t("Trajectoire intermédiaire")),
                          width = 12,
                          height = 525,
                          tabPanel(i18n$t('Volume'),
                                   em(i18n$t("* Le volume est exprimé en m^3")),
                                   DT::dataTableOutput("vtable00_22")
                          ),
                          tabPanel(i18n$t('Contribution annuelle des glaciers'),
                                   em(i18n$t("* La contribution exprimé en l/s")),
                                   DT::dataTableOutput("rtable00_22"))
                        )
                      )
                    ),
                    screen(
                      fluidRow(
                        tabBox(
                          id = "tabset1",
                          title = i18n$t("Trajectoire pessimiste"),
                          width = 12,
                          height = 525,
                          tabPanel(i18n$t('Volume'),
                                   em(i18n$t("* Le volume est exprimé en m^3")),
                                   DT::dataTableOutput("vtable10_22")
                          ),
                          tabPanel(i18n$t('Contribution annuelle des glaciers'),
                                   em(i18n$t("* La contribution exprimé en l/s")),
                                   DT::dataTableOutput("rtable10_22"))
                        )
                      )
                    )
                  )
                )
              )
              
      ),
      
      ### Viz ====
      tabItem("viz",
              uiOutput("viz_content")
      ),
      
      ### Download ====
      tabItem(tabName = "dl",
              fluidPage(
                h1(strong(i18n$t("Téléchargement des données"))),
                br(),
                p(
                  div(
                    style = "margin-left: 125px; margin-right: 125px; text-align: justify;",
                    em(i18n$t("Cette section permet de télécharger toutes les données utilisées par l'application ou bien uniquemant les données séléctionnées.")),
                    br(),
                    em(i18n$t("* Le téléchargement du 'Raster d'épaisseur' permet d'extraire les données d'épaisseur des glaciers pour les trois scénarios. Les scénarios sont indiqués comme préfixe des noms de chaque bande.")),
                    br(),
                    em(i18n$t("* Les données relatives aux bassins versants indiquent la participation de chaque glacier aux bassins versants qu'il intersecte."))
                  )
                ),
                hr(),
                h4(i18n$t("Les données brutes")),
                br(),
                fluidRow(
                  column(8,
                         tabBox(
                           title = i18n$t('Visualisation'),
                           tabPanel(i18n$t('Epaisseur'),
                                    withSpinner(leafletOutput("mapplotTCK_raw", height = "425px", width = "100%"),
                                                type = 1, color = "darkgreen", color.background = '#ffffff')
                           ),
                           tabPanel(i18n$t('Bassin versant'),
                                    withSpinner(leafletOutput("mapplotWS_raw", height = "425px", width = "100%"),
                                                type = 1, color = "darkgreen", color.background = '#ffffff')),
                           width = 16,
                           height = 500)
                  ),
                  column(4,
                         radioGroupButtons(
                           inputId = 'raw_selected', 
                           label = "",
                           choices = setNames(
                             translations_file1$id, 
                             translations_file1[, "fr"]
                           ),
                           selected = "rast1",
                           checkIcon = list(yes = icon("check")),
                           direction = 'vertical',
                           status = 'primary',
                           justified = TRUE
                         ),
                         hidden(
                           div(
                             id = "format_selector",
                             h4(i18n$t("Formats de l'export :")),
                             radioGroupButtons(
                               inputId = 'format_selected', 
                               label = NULL,
                               choices = c("csv",
                                           "gpkg"),
                               selected = "gpkg",
                               checkIcon = list(
                                 yes = icon("check")
                               ),
                               status = 'primary',
                               justified = TRUE
                             )
                           )
                         ),
                         hr(),
                         div(style="display: flex; justify-content: center; align-items: center;",
                             downloadBttn("dl_raw", 
                                          label = p(i18n$t('Téléchargement des données')), 
                                          style = 'fill',
                                          color = 'success',
                                          icon = icon("file-zipper")
                             )
                         ),
                         br(),
                         em(i18n$t("* Les scénarios sont indiqués par des abreviation :")),br(),
                         em(i18n$t("TO pour trajectoire optimiste")),br(),
                         em(i18n$t("TI pour trajectoire intermédiaire")),br(),
                         em(i18n$t("TP pour trajectoire pessimiste"))
                  )
                ),
                hr(),
                h4(i18n$t("Les données sélectionnées")),
                br(),
                uiOutput("conditionalContent")
              )
      ),
      
      ### Guide ====
      tabItem(tabName = "gd",
              fluidPage(
                div(
                  style = "margin-left: 125px; margin-right: 125px; text-align: justify;",
                  h1(strong(i18n$t("Aide à l'utilisation du dashboard"))),
                  hr(),
                  br(),
                  fluidRow(
                    column(7,
                           div(
                             style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 600px;",
                             tags$img(src = 'guide_1.png', width = "100%", style = "object-fit: contain; max-height: 100%;")
                           )
                    ),
                    column(5,
                           div(
                             class = "scrollbar",
                             style="display: flex; flex-direction: column; overflow-y:scroll; height: 600px; padding-right: 10px; box-sizing: content-box;",
                             uiOutput("pres_guide1")
                           )
                    )
                  ),
                  hr(),
                  fluidRow(
                    column(5,
                           div(
                             class = "scrollbar",
                             style="display: flex; flex-direction: column; overflow-y:scroll; height: 600px; padding-right: 10px; box-sizing: content-box;",
                             uiOutput("pres_guide2")
                           )
                    ),
                    column(7,
                           div(
                             style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 600px;",
                             tags$img(src = 'guide_2.png', width = "100%", style = "object-fit: contain; max-height: 100%;")
                           )
                    )
                  ),
                  hr(),
                  fluidRow(
                    column(7,
                           div(
                             style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 600px;",
                             tags$img(src = 'guide_3.png', width = "100%", style = "object-fit: contain; max-height: 100%;")
                           )
                    ),
                    column(5,
                           div(
                             class = "scrollbar",
                             style="display: flex; flex-direction: column; overflow-y:scroll; height: 600px; padding-right: 10px; box-sizing: content-box;",
                             uiOutput("pres_guide3")
                           )
                    )
                  ),
                  div(
                    style="text-align: center;",
                    h4(em(strong(i18n$t("Bonne navigation !"))))
                  )
                )
              )
      )
    )
  )
)


# Server Interface ====
server <- function(input, output, session) {
  
  ## Traduction ====
  
  # Défine la langue princ sur FR
  get_initial_language <- function() {
    initial_lang <- session$input$language_selector %||% "fr"
    return(initial_lang)
  }
  
  fr_it <- reactive({
    req(input$language_selector)
    input$language_selector
  })
  
  # Actualise la langue à afficher en fonction de langage_selector
  observeEvent(input$language_selector, {
    req(input$language_selector)
    
    # maj de i18n
    i18n$set_translation_language(input$language_selector)
    update_lang(input$language_selector, session)
    
    # maj de la langue des inputs
    updateRadioGroupButtons(
      session,
      "selectSce_viz",
      choices = setNames(translations_traj$id, translations_traj[, input$language_selector]),
      selected = input$selectSce_viz %||% "TI",
      checkIcon = list(yes = icon("check")),
      size = "sm",
      justified = TRUE,
      status = "primary"
    )
    
    updateRadioGroupButtons(
      session,
      "vol_c",
      choices = setNames(translations_var$id, translations_var[, input$language_selector]),
      selected = input$vol_c %||% "v",
      checkIcon = list(yes = icon("check")),
      size = "sm",
      justified = TRUE,
      status = "primary"
    )
    
    updateRadioGroupButtons(
      session,
      'raw_selected', 
      choices = setNames(translations_file1$id, translations_file1[, input$language_selector]),
      checkIcon = list(yes = icon("check")),
      status = 'primary',
      justified = TRUE
    )
    
    updateRadioGroupButtons(
      session,
      'select_selected', 
      choices = setNames(translations_file2$id, translations_file2[, input$language_selector]),
      checkIcon = list(yes = icon("check")),
      status = 'primary',
      justified = TRUE
    )
    
  }, ignoreInit = FALSE)
  
  # maj de la langue des markdowns
  output$pres_aclimo <- renderUI({
    req(fr_it())  
    includeMarkdown(paste0("text_markdown/pres_aclimo_", as.character(fr_it()), ".Rmd"))
  })
  
  output$pres_stud_area <- renderUI({
    req(fr_it())  
    includeMarkdown(paste0("text_markdown/pres_stud_area_", as.character(fr_it()), ".Rmd"))
  })
  
  output$pres_data <- renderUI({
    req(fr_it()) 
    includeMarkdown(paste0("text_markdown/pres_data_", as.character(fr_it()), ".Rmd"))
  })
  
  output$pres_guide1 <- renderUI({
    req(fr_it()) 
    includeMarkdown(paste0("text_markdown/pres_guide1_", as.character(fr_it()), ".Rmd"))
  })
  
  output$pres_guide2 <- renderUI({
    req(fr_it())  
    includeMarkdown(paste0("text_markdown/pres_guide2_", as.character(fr_it()), ".Rmd"))
  })
  
  output$pres_guide3 <- renderUI({
    req(fr_it())  
    includeMarkdown(paste0("text_markdown/pres_guide3_", as.character(fr_it()), ".Rmd"))
  })
  
  # maj de la langue de la sidebar
  output$sidebar <- renderMenu({
    sidebarMenu(
      menuItem(i18n$t("Accueil"), tabName = "acc", icon = icon("home")),
      menuItem(i18n$t("Présentation des données"), tabName = "data", icon = icon("database")),
      menuItem(i18n$t("Exploration des données"), tabName = "ex", icon = icon("magnifying-glass"), startExpanded = TRUE,
               menuSubItem(i18n$t("Sélection des bassins versants"), tabName = "select_bv"),
               menuSubItem(i18n$t("Visualisation"), tabName = "viz")),
      menuItem(i18n$t("Téléchargement"), tabName = "dl", icon = icon("download")),
      menuItem(i18n$t("Aide"), tabName = "gd", icon = icon("circle-question"))
    )
  })
  
  # maj de la langue de la sidebar
  observeEvent(input$creditLink, {
    showModal(modalDialog(
      title = i18n$t("Réalisé par"),
      p(i18n$t("Cette application a été développée en 2025 dans le cadre du projet ALCOTRA - ACLIMO.")),
      p(i18n$t("Le développement a été entièrement assuré par Xavier Klee. En collaboration avec Antoine Rabatel et Romain Millan.")),
      p(i18n$t("Contacts :")), 
      HTML("<a href='https://www.linkedin.com/in/xavier-klee-480094272' target='_blank'>
      <i class='fa fa-linkedin fa-lg' style='margin-right: 10px;'></i></a>
      <a href='https://github.com/XavierClef' target='_blank'>
      <i class='fa fa-github fa-lg'></i></a></p>"),
      hr(),
      h4(i18n$t("Références des données :")),
           p(i18n$t("L'épaisseur des glaciers :")),
           HTML("<p>Cook, S. J., Jouvet, G., Millan, R., Rabatel, A., Zekollari, H., & Dussaillant, I. (2023). Committed ice loss in the European Alps until 2050 using a deep-learning
           -aided 3D ice-flow model with data assimilation. Geophysical Research Letters, 50, e2023GL105029. https://doi.org/10.1029/2023GL105029 </p>"),
           p(i18n$t("La limite des glaciers :")),
           HTML("<p>RGI Consortium. (2017). Randolph Glacier Inventory - A Dataset of Global Glacier Outlines. (NSIDC-0770, Version 6). [Data Set]. Boulder, Colorado USA. National
           Snow and Ice Data Center. https://doi.org/10.7265/4m1f-gd79</p>"),
      easyClose = TRUE,
      footer = modalButton(i18n$t("Fermer"))
    ))
  })
  
  ## Lecture des datas =====
  ws_raw <- reactive({
    # Connexion à la base de données
    con <- dbConnect(RSQLite::SQLite(), "data/database_ACLIMO.sqlite")
    on.exit(dbDisconnect(con))
    
    # Charger les données ws_raw
    ws_raw <- st_read(con, "ws")
    return(ws_raw)
  })
  
  outl <- reactive({
    con <- dbConnect(RSQLite::SQLite(), "data/database_ACLIMO.sqlite")
    on.exit(dbDisconnect(con))
    
    # Charger les données outl
    outl <- st_read(con, "outl")
    return(outl)
  })
  
  outl_viz <- reactive({
    con <- dbConnect(RSQLite::SQLite(), "data/database_ACLIMO.sqlite")
    on.exit(dbDisconnect(con))
    
    # Charger les données outl_viz
    outl_viz <- st_read(con, "outl_viz") %>% 
      st_zm(drop = TRUE)
    return(outl_viz)
  })
  
  selected_tck <- reactive({
    tck <- rast("data/Tck_20202050_corr.tif", lyrs=as.character(input$annee))
  })
  
  selected_tckID <- reactive({
    tck <- rast("data/Tck_20202050_corrID.tif", lyrs=as.character(input$annee))
  })
  
  selected_tckIDQ <- reactive({
    tck <- rast("data/Tck_20202050_corrIDQ.tif", lyrs=as.character(input$annee))
  })
  
  ## Génération de la carte de pres_data ====
  output$mapplotTCK <- renderUI({
    
    outl_viz <- outl_viz()
    
    tck <- selected_tck()
    tck[tck == 0] <- NA
    outl_viz <- st_transform(outl_viz, 4326)
    tck <- project(tck, "+init=EPSG:4326")
    tck <- mask(tck, outl_viz)
    
    tckID <- selected_tckID()
    tckID[tckID == 0] <- NA
    outl_viz <- st_transform(outl_viz, 4326)
    tckID <- project(tckID, "+init=EPSG:4326")
    tckID <- mask(tckID, outl_viz)
    
    tckIDQ <- selected_tckIDQ()
    tckIDQ[tckIDQ == 0] <- NA
    outl_viz <- st_transform(outl_viz, 4326)
    tckIDQ <- project(tckIDQ, "+init=EPSG:4326")
    tckIDQ <- mask(tckIDQ, outl_viz)
    
    pal <- colorNumeric(palette = viridis(100), domain = na.omit(values(tck)), na.color = "transparent")
    
    sync(
      list(
        leaflet() %>%
          addProviderTiles("OpenStreetMap") %>%
          addRasterImage(tck, opacity = 0.7, colors = pal, project = TRUE) %>%
          addPolygons(data = outl_viz,
                      color = "black",
                      weight = 2,
                      fillColor = NA,
                      fillOpacity = 0) %>%
          addControl(i18n$t("Trajectoire optimiste"), position = "topright") %>% 
          setView(6.75, 45.33, 11),
        
        leaflet() %>%
          addProviderTiles("OpenStreetMap") %>%
          addRasterImage(tckID, opacity = 0.7, colors = pal, project = TRUE) %>%
          addPolygons(data = outl_viz,
                      color = "black",
                      weight = 2,
                      fillColor = NA,
                      fillOpacity = 0) %>%
          addControl(i18n$t("Trajectoire intermédiaire"), position = "topright") %>% 
          setView(6.75, 45.33, 11),
        
        leaflet() %>%
          addProviderTiles("OpenStreetMap") %>%
          addRasterImage(tckIDQ, opacity = 0.7, colors = pal, project = TRUE) %>%
          addPolygons(data = outl_viz,
                      color = "black",
                      weight = 2,
                      fillColor = NA,
                      fillOpacity = 0) %>%
          addLegend(position = "bottomright",
                    pal = pal,
                    values = na.omit(values(tckIDQ)),
                    title = paste0(i18n$t("Epaisseurs de glace en "), input$annee),
                    opacity = 1) %>%
          addControl(i18n$t("Trajectoire pessimiste"), position = "topright") %>% 
          setView(6.75, 45.33, 11)
      ),
      ncol = 3
    )
  })
  
  ## Sélection des WS ====
  
  # Sélection des WS en fonction des parcs
  ws <- reactive({
    req(ws_raw)
    filter(ws_raw(), parc_nt %in% input$select_type)
  })
  
  observe({
    choices <- unique(ws_raw()$parc_nt)
    updatePickerInput(session, "select_type",
                      choices = choices,
                      selected = "PNV - Coeur"
    )
  })
  
  # Sélection des WS en fonction de des parcs select
  observeEvent(ws(), {
    updatePickerInput(session,
                      inputId = "selected_locations",
                      choices = ws()$nom_sit,
                      selected = selected$groups)
  })
  
  # Valeur de parc par defaut
  observe({
    if (length(input$select_type) == 0) {
      updatePickerInput(session,
                        inputId = "select_type",
                        selected = "PNV - Coeur")
    }
  })
  
  selected_ids <- reactiveValues(ids = vector())
  
  # Création d'une carte pour la sélection et la visualisation de la sélection
  output$map <- renderLeaflet({
    req({NROW(ws_raw()) > 0})
    
    ws_filtered <- ws() %>% 
      filter(nom_sit %in% input$selected_locations)
    
    outl_viz <- outl_viz()
    
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addPolygons(data = ws(),
                  fillColor = "lightblue",
                  fillOpacity = 0.5,
                  color = "black",
                  stroke = TRUE,
                  weight = 1,
                  layerId = ~nom_sit,
                  group = "regions",
                  label = ~nom_sit) %>%
      addPolygons(data = ws_filtered,
                  fillColor = "green",
                  fillOpacity = 0.5,
                  weight = 1,
                  color = "black",
                  stroke = TRUE,
                  layerId = ~wtrshd_,
                  group = ~nom_sit) %>%
      addPolygons(data = outl_viz,
                  fillColor = "lightgray",
                  fillOpacity = 1,
                  color = "black",
                  stroke = TRUE,
                  group = "Glaciers",
                  label = ~glac_name,
                  weight = 1) %>%
      addLayersControl(
        overlayGroups = c("Glaciers"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      hideGroup('Glaciers')
  }) 
  
  proxy <- leafletProxy("map")
  
  selected <- reactiveValues(groups = vector())
  
  sf_use_s2(FALSE)  
  
  # Gestion de la sélection/désélection de la carte de sélection
  observeEvent(input$map_shape_click, {
    req(input$map_shape_click)
  
    # Converion en data.table pour optimiser les traitements
    current_ws <- ws()
    req(current_ws)  
    ws_raw_dt <- as.data.table(ws_raw())
    setkey(ws_raw_dt, nom_sit)
    
    # Vérifie si le clic concerne une région
    if (!is.null(input$map_shape_click$group) && input$map_shape_click$group == "regions") {
      new_polygon_id <- input$map_shape_click$id
      current_ws_raw <- ws_raw()
      req(current_ws_raw)
      
      new_polygon <- current_ws_raw[current_ws_raw$nom_sit == new_polygon_id, , drop = FALSE]
      if (nrow(new_polygon) == 0) return()
      
      contains_relation_found <- FALSE
      
      # Si des polygines sont déjà sélectionnés ...
      if (length(selected$groups) > 0) {
        # ... récupère les polygones sélectionés ...
        selected_polygons <- current_ws_raw[current_ws_raw$nom_sit %in% selected$groups, ]
        # ...et vérifie les intersections avec les autres polygones ...
        intersecting_indices <- st_intersects(new_polygon, selected_polygons, sparse = TRUE)[[1]]
        
        if (length(intersecting_indices) > 0) {
          # Extrait les polygones qui intersectent
          relevant_polygons <- selected_polygons[intersecting_indices, ]
          
          # Vérifie les différentes relations spatiales possibles
          spatial_relations <- list(
            contains = st_contains(new_polygon, relevant_polygons, sparse = TRUE)[[1]],
            contained = st_contains(relevant_polygons, new_polygon, sparse = TRUE),
            overlaps = st_overlaps(new_polygon, relevant_polygons, sparse = TRUE)[[1]]
          )
          
          # Combine toutes les relations trouvées
          has_relation <- unique(c(
            spatial_relations$contains,
            which(lengths(spatial_relations$contained) > 0),
            spatial_relations$overlaps
          ))
          
          if (length(has_relation) > 0) {
            contains_relation_found <- TRUE
            
            # Compare les surfaces des polygones
            new_area <- as.numeric(st_area(new_polygon))
            existing_areas <- as.numeric(st_area(relevant_polygons[has_relation, ]))
            
            # Détermine quels polygones garder en fonction de leur taille
            polygons_to_process <- relevant_polygons$nom_sit[has_relation]
            areas_comparison <- data.table(
              id = polygons_to_process,
              area = existing_areas,
              action = ifelse(existing_areas < new_area, "remove", "keep")
            )
            
            # Retire les polygones plus petits
            to_remove <- areas_comparison[action == "remove", id]
            if (length(to_remove) > 0) {
              selected$groups <- setdiff(selected$groups, to_remove)
              proxy %>% hideGroup(group = to_remove)
              
              if (!(new_polygon_id %in% selected$groups)) {
                selected$groups <- c(selected$groups, new_polygon_id)
                proxy %>% showGroup(group = new_polygon_id)
              }
            }
            
            # Garde les polygones plus grands
            to_keep <- areas_comparison[action == "keep", id]
            for (keep_id in to_keep) {
              if (!(keep_id %in% selected$groups)) {
                selected$groups <- c(selected$groups, keep_id)
                proxy %>% showGroup(group = keep_id)
              }
            }
          }
        }
      }
      
      # Si aucune relation n'est trouvée, ajoute simplement le nouveau polygone
      if (!contains_relation_found && !(new_polygon_id %in% selected$groups)) {
        selected$groups <- c(selected$groups, new_polygon_id)
        proxy %>% showGroup(group = new_polygon_id)
      }
    } else {
      # Si ce n'est pas un clic sur une région, retire le groupe du clic
      if (!is.null(input$map_shape_click$group)) {
        selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
        proxy %>% hideGroup(group = input$map_shape_click$group)
      }
    }
    
    # Met à jour la liste de sélection
    current_ws <- ws()  
    if (!is.null(current_ws) && !is.null(selected$groups)) {
      updatePickerInput(session,
                        inputId = "selected_locations",
                        choices = current_ws$nom_sit,
                        selected = selected$groups)
    }
  })
  
  # Observe les changements dans la liste de sélection
  observeEvent(input$selected_locations, {
    current_ws <- ws()
    req(current_ws)
    
    if(length(input$selected_locations) == length(current_ws$nom_sit)) {
      all_polygons <- current_ws
      n_polygons <- nrow(all_polygons)
      contained_status <- logical(n_polygons) 
      areas <- as.numeric(st_area(all_polygons))
      
      # Projette les polygones pour des calculs plus précis
      all_polygons_projected <- st_transform(all_polygons, 3857)
      
      # Vérifie chaque polygone
      for(i in 1:n_polygons) {
        if(contained_status[i]) next
        
        current_poly <- all_polygons_projected[i,]
        other_polys <- all_polygons_projected[-i,]
        
        # Crée un buffer négatif pour éviter les problèmes de bordure
        current_poly_buffered <- st_buffer(current_poly, -1)
        
        # Vérifie si le polygone est contenu dans d'autres
        if(!st_is_empty(current_poly_buffered) && st_is_valid(current_poly_buffered)) {
          within_checks <- st_within(current_poly_buffered, other_polys, sparse = TRUE)
          containing_indices <- within_checks[[1]]
          
          if(length(containing_indices) > 0) {
            containing_indices_adjusted <- ifelse(containing_indices >= i,
                                                  containing_indices + 1,
                                                  containing_indices)
            
            # Compare les surfaces
            current_area <- areas[i]
            containing_areas <- areas[containing_indices_adjusted]
            
            # Marque comme contenu si trouvé dans un plus grand polygone
            if(any(containing_areas > current_area)) {
              contained_status[i] <- TRUE
            }
          }
        }
      }
      
      # Récupère uniquement les polygones de niveau 1 (non contenus)
      level_1_polygons <- all_polygons$nom_sit[!contained_status]
      
      # Met à jour l'affichage
      proxy %>% hideGroup(group = selected$groups)
      proxy %>% showGroup(group = level_1_polygons)
      
      selected$groups <- level_1_polygons
      
      # Met à jour la liste de sélection
      updatePickerInput(session,
                        inputId = "selected_locations",
                        choices = all_polygons$nom_sit,
                        selected = level_1_polygons)
      
    } else {
      # Gère les ajouts/retraits individuels de la liste
      removed_via_selectInput <- setdiff(selected$groups, input$selected_locations)
      added_via_selectInput <- setdiff(input$selected_locations, selected$groups)
      
      if(length(removed_via_selectInput) > 0) {
        selected$groups <- input$selected_locations
        proxy %>% hideGroup(group = removed_via_selectInput)
      }
      
      if(length(added_via_selectInput) > 0) {
        selected$groups <- input$selected_locations
        proxy %>% showGroup(group = added_via_selectInput)
      }
    }
  }, ignoreNULL = FALSE)
  
  ## Crée des tables relatives à chaque traj ====
  
  # TO
  comp_tableSO <- reactive({
    req(input$selected_locations)
    
    outl <- outl() %>%
      filter(nom_sit %in% input$selected_locations)
    
    outl <- outl %>%
      group_by(glac_nom, nom_sit, true_name) %>%
      summarise(across(matches("TO$"), sum), .groups = "drop")
    
    colnames(outl) <- gsub("_TO$", "", colnames(outl))
    
    outl
  })
  
  # TI
  comp_table00_22 <- reactive({
    req(input$selected_locations)
    
    outl <- outl() %>%
      filter(nom_sit %in% input$selected_locations)
    
    outl <- outl %>%
      group_by(glac_nom, nom_sit, true_name) %>%
      summarise(across(matches("TI$"), sum), .groups = "drop")
    
    colnames(outl) <- gsub("_TI$", "", colnames(outl))
    
    outl
  })
  
  # TP
  comp_table10_22 <- reactive({
    req(input$selected_locations)
    
    outl <- outl() %>%
      filter(nom_sit %in% input$selected_locations)
    
    outl <- outl %>%
      group_by(glac_nom, nom_sit, true_name) %>%
      summarise(across(matches("TP$"), sum), .groups = "drop")
    
    colnames(outl) <- gsub("_TP$", "", colnames(outl))
    
    outl
  })
  
  # Clip des outline en fonction des WS sélectionnés ====
  outl_select <- reactive({
    req(input$selected_locations)
    
    selected_ws <- ws_raw() %>%
      filter(nom_sit %in% input$selected_locations)
    
    outl_utm <- st_transform(outl(), 32632)
    selected_ws_utm <- st_transform(selected_ws, 32632)
    
    intersection_index <- st_intersects(
      outl_utm,
      st_buffer(selected_ws_utm, -100)
    )
    outl_filtered <- outl_utm[lengths(intersection_index) > 0, ]
    
    outl_int <- st_intersection(
      outl_filtered,
      selected_ws_utm
    )
    
    outl_int <- st_transform(outl_int, 4326)
    
  })
  
  ## Tables à afficher avec DT ====
  
  # TO_volume
  output$vtableSO <- DT::renderDataTable({
    req(comp_tableSO())
    
    df_display <- comp_tableSO() %>%
      st_drop_geometry() %>%
      select(-c(true_name, starts_with("c"))) %>% 
      mutate(across(where(is.numeric), round, digits= 2)) %>% 
      rename(Glacier = glac_nom,
             Bassin = nom_sit)
    
    DT::datatable(df_display,
                  selection = 'none',
                  extensions = 'Scroller',
                  options = list(
                    scroller = TRUE,
                    scrollX = TRUE,
                    scrollY = 325))
  })
  
  # TO_runoff
  output$rtableSO <- DT::renderDataTable({
    req(comp_tableSO())
    
    df_display <- comp_tableSO() %>%
      st_drop_geometry() %>%
      select(-c(true_name, starts_with("v"))) %>% 
      mutate(across(where(is.numeric), round, digits= 2)) %>% 
      rename(Glacier = glac_nom,
             Bassin = nom_sit)
    
    DT::datatable(df_display,
                  selection = 'none',
                  extensions = 'Scroller',
                  options = list(
                    scroller = TRUE,
                    scrollX = TRUE,
                    scrollY = 325))
  })
  
  # TI_volume
  output$vtable00_22 <- DT::renderDataTable({
    req(comp_table00_22())
    
    df_display <- comp_table00_22() %>%
      st_drop_geometry() %>%
      select(-c(true_name, starts_with("c"))) %>% 
      mutate(across(where(is.numeric), round, digits= 2)) %>% 
      rename(Glacier = glac_nom,
             Bassin = nom_sit)
    
    DT::datatable(df_display,
                  selection = 'none',
                  extensions = 'Scroller',
                  options = list(
                    scroller = TRUE,
                    scrollX = TRUE,
                    scrollY = 325))
  })
  
  # TI_runoff
  output$rtable00_22 <- DT::renderDataTable({
    req(comp_table00_22())
    
    df_display <- comp_table00_22() %>%
      st_drop_geometry() %>%
      select(-c(true_name, starts_with("v"))) %>% 
      mutate(across(where(is.numeric), round, digits= 2)) %>% 
      rename(Glacier = glac_nom,
             Bassin = nom_sit)
    
    DT::datatable(df_display,
                  selection = 'none',
                  extensions = 'Scroller',
                  options = list(
                    scroller = TRUE,
                    scrollX = TRUE,
                    scrollY = 325))
  })
  
  # TP_volume
  output$vtable10_22 <- DT::renderDataTable({
    req(comp_table10_22())
    
    df_display <- comp_table10_22() %>%
      st_drop_geometry() %>%
      select(-c(true_name, starts_with("c"))) %>% 
      mutate(across(where(is.numeric), round, digits= 2)) %>% 
      rename(Glacier = glac_nom,
             Bassin = nom_sit)
    
    DT::datatable(df_display,
                  selection = 'none',
                  extensions = 'Scroller',
                  options = list(
                    scroller = TRUE,
                    scrollX = TRUE,
                    scrollY = 325)) 
  })
  
  # TP_runoff
  output$rtable10_22 <- DT::renderDataTable({
    req(comp_table10_22())
    
    df_display <- comp_table10_22() %>%
      st_drop_geometry() %>%
      select(-c(true_name, starts_with("v"))) %>% 
      mutate(across(where(is.numeric), round, digits= 2)) %>% 
      rename(Glacier = glac_nom,
             Bassin = nom_sit)
    
    DT::datatable(df_display,
                  selection = 'none',
                  extensions = 'Scroller',
                  options = list(
                    scroller = TRUE,
                    scrollX = TRUE,
                    scrollY = 325))
  })
  
  ## Création des infobox ====
  output$n_glacBox1 <- renderValueBox({
    if(length(input$selected_locations) == 0) {
      valueBox(
        "0",
        i18n$t("Nombre de glaciers"), 
        icon = icon("mountain"),
        color = "light-blue"
      )
    } else {
      req(comp_tableSO())
      valueBox(
        as.character(nrow(comp_tableSO())),
        i18n$t("Nombre de glaciers"), 
        icon = icon("mountain"),
        color = "light-blue"
      )
    }
  })
  
  output$n_wsBox1 <- renderValueBox({
    if(length(input$selected_locations) == 0) {
      valueBox(
        "0",
        i18n$t("Nombre de bassins versant"), 
        icon = icon("chart-area"),
        color = "green"
      )
    } else {
      req(comp_tableSO())
      valueBox(
        as.character(n_distinct(comp_tableSO()$nom_sit)),
        i18n$t("Nombre de bassins versant"), 
        icon = icon("chart-area"),
        color = "green"
      )
    }
  })
  
  output$lossBox1_SO <- renderValueBox({
    if(length(input$selected_locations) == 0) {
      valueBox(
        "0 %",
        i18n$t("de volume perdu entre 2020 et 2050"), 
        icon = icon("cube"),
        color = "red"
      )
    } else {
      req(comp_tableSO())
      
      volume_2020 <- comp_tableSO() %>%
        pull(v.2020)
      
      volume_2050 <- comp_tableSO() %>%
        pull(v.2050)
      
      volume_loss <- round(((sum(volume_2020)/sum(volume_2020)) - (sum(volume_2050)/sum(volume_2020)))*100, digits = 0)
      
      valueBox(
        paste0(as.character(volume_loss), " %"),
        i18n$t("de volume perdu entre 2020 et 2050"), 
        icon = icon("cube"),
        color = "red"
      )
    }
  })
  
  output$waterBox1_SO <- renderValueBox({
    if(length(input$selected_locations) == 0) {
      valueBox(
        "0 l/s",
        i18n$t("Contribution moyenne annuelle de la variation de volume glaciaire"), 
        icon = icon("faucet-drip"),
        color = "yellow"
      )
    } else {
      req(comp_tableSO())
      
      c_2020 <- comp_tableSO() %>%
        pull(c.2021)
      
      c_2049 <- comp_tableSO() %>%
        pull(c.2050)
      
      volume_loss <- round(sum(c_2020) - sum(c_2049), digits = 0)
      
      valueBox(
        paste0(as.character(volume_loss), " l/s"),
        i18n$t("Contribution moyenne annuelle de la variation de volume glaciaire"), 
        icon = icon("faucet-drip"),
        color = "yellow"
      )
    }
  })
  
  output$lossBox1_00_22 <- renderValueBox({
    if(length(input$selected_locations) == 0) {
      valueBox(
        "0 %",
        i18n$t("de volume perdu entre 2020 et 2050"), 
        icon = icon("cube"),
        color = "red"
      )
    } else {
      req(comp_table00_22())
      
      volume_2020 <- comp_table00_22() %>%
        pull(v.2020)
      
      volume_2050 <- comp_table00_22() %>%
        pull(v.2050)
      
      volume_loss <- round(((sum(volume_2020)/sum(volume_2020)) - (sum(volume_2050)/sum(volume_2020)))*100, digits = 0)
      
      valueBox(
        paste0(as.character(volume_loss), " %"),
        i18n$t("de volume perdu entre 2020 et 2050"), 
        icon = icon("cube"),
        color = "red"
      )
    }
  })
  
  output$waterBox1_00_22 <- renderValueBox({
    if(length(input$selected_locations) == 0) {
      valueBox(
        "0 l/s",
        i18n$t("Contribution moyenne annuelle de la variation de volume glaciaire"), 
        icon = icon("faucet-drip"),
        color = "yellow"
      )
    } else {
      req(comp_table00_22())
      
      c_2020 <- comp_table00_22() %>%
        pull(c.2021)
      
      c_2049 <- comp_table00_22() %>%
        pull(c.2050)
      
      volume_loss <- round(sum(c_2020) - sum(c_2049), digits = 0)
      
      valueBox(
        paste0(as.character(volume_loss), " l/s"),
        i18n$t("Contribution moyenne annuelle de la variation de volume glaciaire"), 
        icon = icon("faucet-drip"),
        color = "yellow"
      )
    }
  })
  
  output$lossBox1_10_22 <- renderValueBox({
    if(length(input$selected_locations) == 0) {
      valueBox(
        "0 %",
        i18n$t("de volume perdu entre 2020 et 2050"), 
        icon = icon("cube"),
        color = "red"
      )
    } else {
      req(comp_table10_22())
      
      volume_2020 <- comp_table10_22() %>%
        pull(v.2020)
      
      volume_2050 <- comp_table10_22() %>%
        pull(v.2050)
      
      volume_loss <- round(((sum(volume_2020)/sum(volume_2020)) - (sum(volume_2050)/sum(volume_2020)))*100, digits = 0)
      
      valueBox(
        paste0(as.character(volume_loss), " %"),
        i18n$t("de volume perdu entre 2020 et 2050"), 
        icon = icon("cube"),
        color = "red"
      )
    }
  })
  
  output$waterBox1_10_22 <- renderValueBox({
    if(length(input$selected_locations) == 0) {
      valueBox(
        "0 l/s",
        i18n$t("Contribution moyenne annuelle de la variation de volume glaciaire"), 
        icon = icon("faucet-drip"),
        color = "yellow"
      )
    } else {
      req(comp_table10_22())
      
      c_2020 <- comp_table10_22() %>%
        pull(c.2021)
      
      c_2049 <- comp_table10_22() %>%
        pull(c.2050)
      
      volume_loss <- round(sum(c_2020) - sum(c_2049), digits = 0)
      
      valueBox(
        paste0(as.character(volume_loss), " l/s"),
        i18n$t("Contribution moyenne annuelle de la variation de volume glaciaire"), 
        icon = icon("faucet-drip"),
        color = "yellow"
      )
    }
  })
  
  ## Plot ====
  
  # Crée un df_tidy pour mieux gérer la création de plot avec ggplot2
  df_plot <- reactive({
    req(comp_table())
    
    tidy_table <- comp_table() %>% 
      pivot_longer(
        cols = c(starts_with("v."), starts_with("c.")),
        names_to = "Variable_Year", 
        values_to = "Value"
      ) %>%
      separate(Variable_Year, into = c("Variable", "Year"), sep = "\\.") %>% 
      mutate(
        Variable = case_when(
          (Variable == 'v') ~ "Volume",
          (Variable == 'c') ~ "Contribution annuelle des glaciers"
        )
      ) %>% 
      mutate(Year = as.integer(Year))
    
    as.data.frame(tidy_table)
    
  })
  
  # Actualise les plot en fonction de la traj à afficher
  comp_table <- reactive({
    switch(input$selectSce_viz,
           "TO" = comp_tableSO(),
           "TI" = comp_table00_22(),
           "TP" = comp_table10_22())
  })
  
  # Actualise le slider de la période en fonction de la variable sélectionnée
  observeEvent(input$vol_c, {
    if (input$vol_c == "c") {
      current_values <- input$varPeriode
      updateSliderInput(session,
                        inputId = "varPeriode",
                        min = 2021,
                        max = 2050,
                        value = c(max(2021, current_values[1]), current_values[2]))
    } else {
      updateSliderInput(session,
                        inputId = "varPeriode",
                        min = 2020,
                        max = 2050,
                        value = input$varPeriode)
    }
  })
  
  ### Boite d'info =====
  output$n_glacBox2 <- renderValueBox({
    if(length(input$selected_locations) == 0) {
      valueBox(
        "0",
        i18n$t("Nombre de glaciers"), 
        icon = icon("mountain"),
        color = "light-blue"
      )
    } else {
      req(comp_table())
      valueBox(
        as.character(nrow(comp_table())),
        i18n$t("Nombre de glaciers"), 
        icon = icon("mountain"),
        color = "light-blue"
      )
    }
  })
  
  output$n_wsBox2 <- renderValueBox({
    if(length(input$selected_locations) == 0) {
      valueBox(
        "0",
        i18n$t("Nombre de bassins versants"), 
        icon = icon("chart-area"),
        color = "green"
      )
    } else {
      req(comp_table())
      valueBox(
        as.character(n_distinct(comp_table()$nom_sit)),
        i18n$t("Nombre de bassins versants"), 
        icon = icon("chart-area"),
        color = "green"
      )
    }
  })
  
  output$lossBox2 <- renderValueBox({
    if(length(input$selected_locations) == 0) {
      valueBox(
        "0 %",
        i18n$t("de volume perdu entre 2020 et 2050"), 
        icon = icon("cube"),
        color = "red"
      )
    } else {
      req(comp_table())
      
      volume_2020 <- comp_table() %>%
        pull(v.2020)
      
      volume_2050 <- comp_table() %>%
        pull(v.2050)
      
      volume_loss <- round(((sum(volume_2020)/sum(volume_2020)) - (sum(volume_2050)/sum(volume_2020)))*100, digits = 0)
      
      valueBox(
        paste0(as.character(volume_loss), " %"),
        i18n$t("de volume perdu entre 2020 et 2050"), 
        icon = icon("cube"),
        color = "red"
      )
    }
  })
  
  output$waterBox2 <- renderValueBox({
    if(length(input$selected_locations) == 0) {
      valueBox(
        "0 l/s",
        i18n$t("Contribution moyenne annuelle de la variation de volume glaciaire"), 
        icon = icon("faucet-drip"),
        color = "yellow"
      )
    } else {
      req(comp_table())
      
      c_2020 <- comp_table() %>%
        pull(c.2021)
      
      c_2049 <- comp_table() %>%
        pull(c.2050)
      
      volume_loss <- round(sum(c_2020) - sum(c_2049), digits = 0)
      
      valueBox(
        paste0(as.character(volume_loss), " l/s"),
        i18n$t("Contribution moyenne annuelle de la variation de volume glaciaire"), 
        icon = icon("faucet-drip"),
        color = "yellow"
      )
    }
  })
  
  ### Carte de récap de la sélection ====
  output$mapplotRecap <- renderLeaflet({
    req(outl_select(), ws(), input$selected_locations)
    
    ws_filtered <- subset(ws(), nom_sit %in% input$selected_locations)
    
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$CartoDB.VoyagerOnlyLabels) %>% 
      addPolygons(data = ws_filtered,
                  fillColor = "green",
                  fillOpacity = 0.5,
                  weight = 1,
                  color = "black",
                  stroke = TRUE,
                  layerId = ~wtrshd_,
                  group = ~nom_sit,
                  label = ~nom_sit) %>% 
      addPolygons(data = outl_select(),  
                  fillColor = "#4c8bba",
                  fillOpacity = 1,
                  color = "black",
                  stroke = TRUE,
                  weight = 1,
                  label = ~glac_nom) 
  })
  
  ### Carte du delta d'épaisseur en fonction de la période et de la traj ====
  
  suffix <- reactive({
    switch(input$selectSce_viz,
           "TO" = "",
           "TI" = "ID",
           "TP" = "IDQ")
  })
  
  typeScenar <- reactive({
    switch(input$selectSce_viz,
           "TO" = "Trajectoire optimiste",
           "TI" = "Trajectoire intermédiaire",
           "TP" = "Trajectoire pessimiste")
  })
  
  output$mapplotDelta <- renderLeaflet({
    req(outl_select(), ws(), input$selected_locations, suffix(), typeScenar())
    
    ws_filtered <- subset(ws(), nom_sit %in% input$selected_locations)
    suffix <- suffix()
    typeScenar <- typeScenar()
    outl_select <- outl_select()
    
    tck_path <- if (suffix == "") {
      "data/Tck_20202050_corr.tif"
    } else {
      paste0("data/Tck_20202050_corr", suffix, ".tif")
    }
    
    tck1 <- rast(tck_path, lyrs = as.character(input$varPeriode[1]))
    tck2 <- rast(tck_path, lyrs = as.character(input$varPeriode[2]))
    
    tck_delta <- tck1 - tck2
    outl_select <- st_transform(outl_select, 4326)
    tck_delta <- project(tck_delta, "+init=EPSG:4326")
    tck_delta <- mask(tck_delta, outl_select)
    
    pal <- colorNumeric(palette = magma(100), domain = na.omit(values(tck_delta)), na.color = "transparent")
    
    bbox_list <- as.list(st_bbox(outl_select))
    center_lng <- (bbox_list$xmin + bbox_list$xmax) / 2
    center_lat <- (bbox_list$ymin + bbox_list$ymax) / 2
    
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>% 
      addRasterImage(tck_delta, 
                     colors = pal, 
                     group = "Delta") %>%
      addPolygons(data = ws_filtered,
                  fillColor = NA,
                  fillOpacity = 0,
                  weight = 1,
                  color = "black",
                  stroke = TRUE,
                  layerId = ~wtrshd_,
                  group = ~nom_sit,
                  label = ~nom_sit) %>% 
      addPolygons(data = outl_select,  
                  fillColor = NA,
                  fillOpacity = 0,
                  color = "lightgray",
                  stroke = TRUE,
                  weight = 1,
                  label = ~glac_nom) %>% 
      addLegend(position = "bottomright",
                pal = pal,
                values = na.omit(values(tck_delta)),
                title = paste0(i18n$t("Différence d'épaisseur (m) de glace entre "),input$varPeriode[1],i18n$t(" et "),input$varPeriode[2]),
                opacity = 1) %>%
      addControl(paste0(i18n$t(as.character(typeScenar))), position = "topright") %>%
      addFullscreenControl() %>% 
      setView(lng = center_lng, lat = center_lat, zoom = 11)
  })
  
  ### Génération des différents plots ====
  
  # plot_line
  output$title_plot1 <- renderText({
    if (input$vol_c == "v") {
      i18n$t("Evolution du volume par glacier")
    } else {
      i18n$t("Evolution de la contribution moyenne annuelle par glacier")
    }
  })
  
  output$plot1 <- renderPlotly({
    
    df_filtered <- df_plot() %>%
      filter(Variable == translations_var[translations_var$id == input$vol_c, "fr"]) %>%
      filter(Year >= input$varPeriode[1] & Year <= input$varPeriode[2])
    
    plot <- df_filtered %>%
      ggplot(aes(text = glac_nom,  
                 x = Year, 
                 y = Value,
                 color = true_name,
                 group = true_name)) +
      geom_point(shape = 3) +
      geom_line() +
      labs(
        x = i18n$t("Année"),
        y = if(input$vol_c == "v") {
          i18n$t("Volume (m3)")
        } else {
          i18n$t("Contribution annuelle des glaciers (l/s)")
        }
      ) +
      xlim(input$varPeriode[1], input$varPeriode[2]) +
      theme_classic() +
      theme(
        legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_colour_viridis(discrete = TRUE, option = "H") +
      scale_y_continuous(
        trans = if(input$varLog1 == "Log10") {
          "log10"
        } else {
          "identity"
        }
      )
    
    if (input$facet && length(input$selected_locations) >= 2 && length(input$selected_locations) <= 8) {
      plot <- plot + facet_wrap(~nom_sit)
    }
    
    ggplotly(plot, tooltip = c("text", "x", "y")) %>%
      layout(showlegend = FALSE)%>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                        "select2d", "lasso2d", 
                                        "pan2d", "resetScale2d",
                                        "hoverCompareCartesian"))
  })
  
  # plot_stack
  output$title_plot2 <- renderText({
    if (input$vol_c == "v") {
      i18n$t("Evolution cumulée du volume")
    } else {
      i18n$t("Evolution de la contribution annuelle cumulée")
    }
  })
  
  output$plot2 <- renderPlotly({
    
    df_filtered <- df_plot() %>%
      filter(Variable == translations_var[translations_var$id == input$vol_c, "fr"]) %>%
      filter(Year >= input$varPeriode[1] & Year <= input$varPeriode[2])
    
    plot <- df_filtered %>%
      ggplot(aes(
        text = glac_nom,
        x = Year, 
        y = Value,
        fill = true_name,
        group = true_name
      )) +
      geom_bar(position = "stack", stat = "identity") +
      labs(
        x = i18n$t("Année"),
        y = if (input$vol_c == "v") {
          i18n$t("Volume (m3)")
        } else {
          i18n$t("Contribution annuelle des glaciers (l/s)")
        }
      ) +
      xlim(input$varPeriode[1]-1, input$varPeriode[2]+1) +
      theme_classic() +
      theme(
        legend.position = 'none', 
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_fill_viridis(discrete = TRUE, option = "H") 
    
    if (input$facet && length(input$selected_locations) >= 2 && length(input$selected_locations) <= 8) {
      plot <- plot + facet_wrap(~nom_sit)
    }
    
    ggplotly(plot, tooltip = c("text", "x", "y")) %>%
      layout(showlegend = FALSE) %>%  # Suppression de la légende
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "zoomIn2d", "zoomOut2d",
          "select2d", "lasso2d", 
          "pan2d", "resetScale2d",
          "hoverCompareCartesian"
        )
      )
  })
  
  # plot_trend
  output$title_reg <- renderText({
    if (input$vol_c == "v") {
      i18n$t("Taux d'évolution du volume annuel par glacier")
    } else {
      i18n$t("Taux d'évolution de la contribution moyenne annuelle par glacier")
    }
  })
  
  output$plot_coef <- renderPlotly({
    req(df_plot())
    
    df_reg <- df_plot() %>% 
      filter(Variable == translations_var[translations_var$id == input$vol_c, "fr"]) %>% 
      group_by(nom_sit, glac_nom, true_name) %>% 
      summarise(coef_reg = coef(lm(Value ~ Year))[2], .groups = "drop")
    
    df_mean <- df_reg %>% 
      group_by(nom_sit) %>% 
      summarise(mean_coef = mean(coef_reg))
    
    plot <- df_reg %>% 
      ggplot(aes(x = nom_sit, 
                 y = coef_reg,
                 color = true_name,
                 text = glac_nom, 
                 group = true_name)) +
      geom_hline(data = df_reg, aes(yintercept = mean(coef_reg)), linetype = 'dashed', color = 'black') +
      geom_jitter() +
      labs(
        x = i18n$t("Bassin versant"),
        y = if(input$vol_c == "v") {
          i18n$t("Taux d'évolution annuel moyen du volume (m3/an)")
        } else {
          i18n$t("Taux d'évolution de la contribution annuelle ((l/s)/an)")
        }
      ) +
      theme_classic() +
      theme(
        legend.position = 'none',
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      scale_colour_viridis(discrete = TRUE, option = "H")
    
    if(input$facet && between(length(input$selected_locations), 2, 8)){
      plot <- plot +
        geom_hline(data = df_mean, aes(yintercept = mean_coef), color = "red") +
        theme(
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank()
        ) +
        facet_wrap(~nom_sit, scales = "free_x")
    }
    
    ggplotly(plot, tooltip = c("text", "y")) %>%
      layout(showlegend = FALSE) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                        "select2d", "lasso2d", 
                                        "pan2d", "resetScale2d",
                                        "hoverCompareCartesian"))
  })
  
  # Panel de visualisation ====
  output$viz_content <- renderUI({
    if(length(input$selected_locations) == 0) {
      div(
        class = "custom-alert",
        style = "
    position: fixed;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    font-size: 30px;
    text-align: center;
    padding: 20px;
    z-index: 1000;
    color: orange;
    font-weight: bold;
  ",
        icon("exclamation-triangle", lib = "font-awesome"),
        i18n$t("Veuillez sélectionner au moins un bassin versant.")
      )
    } else {
      tagList(
        h1(strong(i18n$t("Visualisation"))),
        br(),
        p(
          div(
            style = "margin-left: 125px; margin-right: 125px; text-align: justify;",
            em(i18n$t("Cette partie est dédiée à la visualisation de l'évolution des données sélectionnées dans la partie précédente 'Sélection des bassins versants'. Les visualisations peuvent être personnalisées de manière dynamique en modifiant les années à afficher (de 2020 à 2050) et la variable à afficher. Notez que la contribution hydrique annuelle des glaciers est calculée en utilisant des paires d'années. Il ne peut donc exister de valeur de contribution annuelle des glaciers pour 2050"))
          )
        ),
        br(),
        hr(),
        fluidRow(
          h3(i18n$t("I. Sélectionnez la trajectoire à afficher :")),
          radioGroupButtons(
            inputId = "selectSce_viz",
            label = "",
            choices = setNames(
              translations_traj$id, 
              translations_traj[, get_initial_language()] 
            ),
            selected = "TI",
            checkIcon = list(yes = icon("check")),
            size = "sm",
            justified = TRUE,
            status = "primary"
          )
        ),
        h3(i18n$t("Récapitulatif de la sélection")),
        fluidRow(
          column(4,
                 withSpinner(leafletOutput("mapplotRecap"), 
                             type = 1, color = "darkgreen", color.background = '#ffffff')
          ),
          column(8,
                 br(),
                 valueBoxOutput("n_glacBox2", width = 12),
                 valueBoxOutput("n_wsBox2", width = 12),
                 tags$div(
                   class = "custom-value-box", 
                   valueBoxOutput("lossBox2", width = 6),
                   valueBoxOutput("waterBox2", width = 6))
          )
        ),
        hr(),
        h1(i18n$t("Visualisation graphique")),
        fluidRow(
          h3(i18n$t("II. Sélectionnez une variable à afficher")),
          radioGroupButtons(
            inputId = "vol_c",
            label = "",
            choices = setNames(
              translations_var$id, 
              translations_var[, get_initial_language()]
            ),
            selected = "v",
            checkIcon = list(yes = icon("check")),
            size = "sm",
            justified = TRUE,
            status = "primary"
          ),
          br(),
          column(9,
                 div(
                   tags$style(HTML("
                   .irs-bar,
                   .irs-bar-edge {
                   background: #4c8bba !important;
                   }
                   
                   .irs-single {
                   background: #4c8bba !important;
                   color: white !important;
                   }
                   
                   .irs-from, .irs-to, .irs-single {
                   background: #4c8bba !important;
                   }
                   
                   .irs-bar-edge {
                   background: #4c8bba !important;
                   }
                                   ")),
                   style = "display: flex; justify-content: center; align-items: center;",
                   sliderInput(
                     inputId = "varPeriode",
                     label = h4(i18n$t("Sélectionnez une période :"),
                                style = "margin-left: 10px;"),
                     min = 2020,
                     max = 2050,
                     value = c(2025, 2050),
                     step = 1,
                     width = "90%",
                     sep = ''
                   )
                 )
          ),
          br(),
          column(3,
                 column(6,
                        br(),
                        br(),
                        div(
                          style="display: flex; justify-content: center; align-items: center;",
                          switchInput("facet",
                                      label = i18n$t("Par bassin versant"),
                                      size = "mini"
                          )
                        )
                 ),
                 column(6,
                        br(),
                        br(),
                        em(i18n$t("* Uniquement si la sélection compte entre 2 et 8 bassins versants"),
                           style= "color: darkgray; font-size: 14px;")
                 )
          )
        ),
        br(),
        br(),
        fluidPage(
          h3(i18n$t("L'évolution des glaciers à travers le temps")),
          br(),
          fluidRow(
            withSpinner(leafletOutput("mapplotDelta", height = "550px"),
                        type = 1, color = "darkgreen", color.background = '#ffffff')
          ),
          br(),
          fluidRow(
            box(
              dropdownButton(
                h4(i18n$t("Informations :")),
                p(i18n$t("Ce graphique exprime l'évolution individuelle du volume ou de la contribution hydrique par glacier pour la sélection réalisée.")), 
                prettyRadioButtons(
                  inputId = "varLog1",
                  label = h4(i18n$t("Echelle :")),
                  choices = c(i18n$t("Simple"), "Log10")
                ),
                circle = FALSE,
                status = "primary",
                icon = icon("bars"), width = "300px",
                tooltip = tooltipOptions(title = i18n$t("Option/ Information"))
              ),
              card(
                full_screen = TRUE,
                card_header(h4(textOutput("title_plot1")),
                            style = "background-color: white; color: black; padding: 10px; border-radius: 5px;"),
                card_body(
                  class = "p-0",
                  plotlyOutput("plot1", height = "510px"))
              ),
              height = 650,
              width = 4
            ),
            box(
              dropdownButton(
                h4(i18n$t("Informations :")),
                p(i18n$t("Ce graphique exprime l'évolution cumulée du volume ou de la contribution hydrique des glaciers de la sélection réalisée.")), 
                circle = FALSE,
                status = "primary",
                icon = icon("info"), width = "300px",
                tooltip = tooltipOptions(title = i18n$t("Information"))
              ),
              card(
                full_screen = TRUE,
                card_header(
                  h4(textOutput("title_plot2")),
                  style = "background-color: white; color: black; padding: 10px; border-radius: 5px;"),
                card_body(
                  class = "p-0",
                  plotlyOutput("plot2", height = "510px")
                )),
              height = 650,
              width = 4
            ),
            box(
              dropdownButton(
                h4("Informations :"),
                p(i18n$t("Ce graphique représente l'évolution annuelle moyenne du volume ou de la contribution hydrique. L'évolution est calculée en récupérant la pente d'un modèle linéaire classique.")),
                p(HTML(i18n$t("Le trait noir représente la moyenne de la sélection"))),
                p(HTML(i18n$t("Le trait rouge représente la moyenne du bassin versant"))),
                circle = FALSE,
                status = "primary",
                icon = icon("info"), width = "300px",
                tooltip = tooltipOptions(title = i18n$t("Information"))
              ),
              card(
                full_screen = TRUE,
                card_header(h4(textOutput("title_reg")),
                            style = "background-color: white; color: black; padding: 10px; border-radius: 5px;"),
                card_body(
                  class = "p-0",
                  plotlyOutput("plot_coef", height = "505px")
                )
              ),
              height = 650,
              width = 4
            )
          )
        )
      )
    }
  })
  
  ## Gestiuon du téléchargement des datas ====
  
  ## affichage des datas 
  output$mapplotTCK_raw <- renderLeaflet({
  
    tck <- rast("data/Tck_20202050_corr.tif", lyrs='2020')
    tck[tck == 0] <- NA
    
    pal <- colorNumeric(palette = viridis(100), domain = na.omit(values(tck)), na.color = "transparent")
    
    leaflet() %>% 
      addTiles(group = "OpenStreetmap") %>% 
      addRasterImage(tck, 
                     colors = pal, 
                     group = "tck") %>% 
      setView(6.75, 45.33, 10)
    
  })
  
  output$mapplotWS_raw <- renderLeaflet({
    
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>% 
      addPolygons(
        data = outl(),
        fillColor = "lightgray",
        fillOpacity = 0,
        color = "black",
        stroke = TRUE,
        weight = 1,
        group = "Glaciers RGI",
        label = ~nom_sit
      ) %>%
      setView(
        lng = 6.75,
        lat = 45.33,
        zoom = 10
      )
  })
  
  # Si BV sélectionné alors show input
  observe({
    if ("vec1" %in% input$raw_selected) {
      show("format_selector", anim = TRUE, time = 0.5)
    } else {
      hide("format_selector", anim = TRUE, time = 0.5)
    }
  })
  
  # Gestion du téléchargement effectué par le downloadHandler
  output$dl_raw <- downloadHandler(
    
    # Défintion du nom des fichiers
    filename = function() {
      selected_format <- input$format_selected
      if ("rast1" %in% input$raw_selected) {
        return(paste0("ACLIMOglacraw_Tck_", Sys.Date(), ".tif"))
      }
      if ("vec1" %in% input$raw_selected) {
        if (selected_format == "csv") {
          return(paste0("ACLIMOglacraw_Bassins_", Sys.Date(), ".csv"))
        }
        if (selected_format == "gpkg") {
          return(paste0("ACLIMOglacraw_Bassins_", Sys.Date(), ".gpkg"))
        }
      }
    },
    content = function(file) {
      temp_dir <- gsub(":", "-", tempdir())
      
      # Téléchargement du tif
      if ("rast1" %in% input$raw_selected) {
        
        tckSO <- rast("data/Tck_20202050_corr.tif")
        names(tckSO) <- paste0("TO_", 2020:2050)
        
        tckSR <- rast("data/Tck_20202050_corrID.tif")
        names(tckSR) <- paste0("TI_", 2020:2050)
        
        tckSP <- rast("data/Tck_20202050_corrIDQ.tif")
        names(tckSP) <- paste0("TP_", 2020:2050)
        
        tck_comp <- c(tckSO, tckSR, tckSP)
        
        writeRaster(tck_comp, file, filetype = "GTiff", overwrite = TRUE)
        
          # Téléchargement du csv 
      } else if ("vec1" %in% input$raw_selected) {
        if (input$format_selected == "csv") {
          modified_data <- as.data.frame(outl()) %>% 
            select(-c(true_name, geom, interet))
          write.csv(modified_data, file, row.names = FALSE)
          
          # Téléchargement du gpkg 
        } else if (input$format_selected == "gpkg") {
          modified_data <- as.data.frame(outl()) %>% 
            select(-c(true_name, interet))
          st_write(modified_data, file, driver = "GPKG", quiet = TRUE)
        }
      }
    }
  )
  
  # Same mais uniquement pour les données sélectionnées
  output$mapplotTCK_selected <- renderLeaflet({
    
    tck <- rast("data/Tck_20202050_corr.tif", lyrs='2020')
    outl_select <- st_transform(outl_select(), 32632)
    tck <- crop(tck, outl_select)
    tck[tck == 0] <- NA
    
    pal <- colorNumeric(palette = viridis(100), domain = na.omit(values(tck)), na.color = "transparent")
    
    leaflet() %>% 
      addTiles(group = "OpenStreetmap") %>% 
      addRasterImage(tck, 
                     colors = pal, 
                     group = "tck") %>% 
      setView(6.75, 45.33, 10)
    
  })
  
  output$mapplotWS_selected <- renderLeaflet({
    
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>% 
      addPolygons(
        data = outl_select(),
        fillColor = "lightgray",
        fillOpacity = 0,
        color = "black",
        stroke = TRUE,
        weight = 1,
        group = "Glaciers RGI",
        label = ~nom_sit
      ) %>%
      setView(
        lng = 6.75,
        lat = 45.33,
        zoom = 10
      )
  })
  
  observe({
    req(input$selected_locations)
    if (length(input$selected_locations) == 0) {
      hide("contenuConditionnel")
    } else {
      show("contenuConditionnel")
    }
  })
  
  observe({
    if ("vec2" %in% input$select_selected) {
      show("format_selector2", anim = TRUE, time = 0.5)
    } else {
      hide("format_selector2", anim = TRUE, time = 0.5)
    }
  })
  
  output$dl_select <- downloadHandler(
    filename = function() {
      selected_format <- input$format_selected2
      if ("rast2" %in% input$select_selected) {
        return(paste0("ACLIMOglacSelect_Tck_", Sys.Date(), ".tif"))
      }
      if ("vec2" %in% input$select_selected) {
        if (selected_format == "csv") {
          return(paste0("ACLIMOglacSelect_Bassins_", Sys.Date(), ".csv"))
        }
        if (selected_format == "gpkg") {
          return(paste0("ACLIMOglacSelect_Bassins_", Sys.Date(), ".gpkg"))
        }
      }
    },
    content = function(file) {
      temp_dir <- gsub(":", "-", tempdir())
      if ("rast2" %in% input$select_selected) {
        
        tckSO <- rast("data/Tck_20202050_corr.tif")
        names(tckSO) <- paste0("TO_", 2020:2050)
        
        tckSR <- rast("data/Tck_20202050_corrID.tif")
        names(tckSR) <- paste0("TI_", 2020:2050)
        
        tckSP <- rast("data/Tck_20202050_corrIDQ.tif")
        names(tckSP) <- paste0("TP_", 2020:2050)
        
        tck_comp <- c(tckSO, tckSR, tckSP)
        outl_select <- st_transform(outl_select(), 32632)
        tck_comp <- crop(tck_comp, outl_select)
        
        writeRaster(tck_comp, file, filetype = "GTiff", overwrite = TRUE)
      } else if ("vec2" %in% input$select_selected) {
        if (input$format_selected2 == "csv") {
          modified_data <- as.data.frame(outl()) %>%
            filter(nom_sit %in% input$selected_locations) %>% 
            select(-c(true_name, geom)) 
          write.csv(modified_data, file, row.names = FALSE)
        } else if (input$format_selected2 == "gpkg") {
          modified_data <- as.data.frame(outl()) %>%
            filter(nom_sit %in% input$selected_locations) %>% 
            select(-c(true_name)) 
          st_write(modified_data, file, driver = "GPKG", quiet = TRUE)
        }
      } 
    }
  )
  
  # Si des BV sont sélectionnés affiche la partie pour le dl de la sélection
  output$conditionalContent <- renderUI({
    if (length(input$selected_locations) != 0) {
      div(id = "contenuConditionnel",
          fluidRow(
            column(8,
                   tabBox(
                     title = i18n$t('Visualisation'),
                     tabPanel(i18n$t('Epaisseur'),
                              withSpinner(leafletOutput("mapplotTCK_selected", height = "425px", width = "100%"),
                                          type = 1, color = "darkgreen", color.background = '#ffffff')
                     ),
                     tabPanel(i18n$t('Bassin versant'),
                              withSpinner(leafletOutput("mapplotWS_selected", height = "425px", width = "100%"),
                                          type = 1, color = "darkgreen", color.background = '#ffffff')),
                     width = 16,
                     height = 500)
            ),
            column(4,
                   h4(i18n$t('Selection des fichiers :')),
                   radioGroupButtons(
                     inputId = 'select_selected', 
                     label = "",
                     choices = setNames(
                       translations_file2$id, 
                       translations_file2[, get_initial_language()] 
                     ),
                     checkIcon = list(
                       yes = icon("check")
                     ),
                     direction = 'vertical',
                     status = 'primary',
                     justified = TRUE),
                   hidden(
                     div(
                       id = "format_selector2",
                       h4(i18n$t("Formats de l'export :")),
                       radioGroupButtons(
                         inputId = 'format_selected2', 
                         label = NULL,
                         choices = c("csv",
                                     "gpkg"),
                         selected = "gpkg",
                         checkIcon = list(
                           yes = icon("check")
                         ),
                         status = 'primary',
                         justified = TRUE
                       )
                     )
                   ),
                   
                   hr(),
                   div(style="display: flex; justify-content: center; align-items: center;",
                       downloadBttn("dl_select", 
                                    label = i18n$t('Téléchargement des données'), 
                                    style = 'fill',
                                    color = 'success',
                                    icon = icon("file-zipper")
                       )
                   ),
                   br(),
                   em(i18n$t("* Les scénarios sont indiqués par des abreviation :")),br(),
                   em(i18n$t("TO pour trajectoire optimiste")),br(),
                   em(i18n$t("TI pour trajectoire intermédiaire")),br(),
                   em(i18n$t("TP pour trajectoire pessimiste"))
            )
          )
      )
    } else {
      div(
        class = "custom-alert",
        style = "
        position: relative;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        font-size: 30px;
        text-align: center;
        padding: 20px;
        z-index: 1000;
        color: orange;
        font-weight: bold;
      ",
        icon("exclamation-triangle", lib = "font-awesome"),
        i18n$t("Veuillez sélectionner au moins un bassin versant.")
      )
    }
  })
  
}

# RunApp ====
shinyApp(ui, server, options = list(launch.browser = TRUE))


# Code rédigé par Xavier Klee ;-)