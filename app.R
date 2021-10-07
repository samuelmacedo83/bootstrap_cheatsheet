# BOOTSTRAP CHEAT SHEET FOR SHINY ----
# DS4B 202-R ----

# LIBRARIES ----
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(shinythemes)
library(shinyjs)
library(DT)

# USER INTERFACE ----
ui <- fluidPage(
  title = "Bootstrap Cheat Sheet for Shiny",
  
  div(
    class = "container",
    id = "page",
    
    # HEADER ----
    h1(class = "page-header", "Bootstrap Cheat Sheet", tags$small("For Shiny")),
    p(
      class = "lead",
      "This cheat sheet is the first part of ",
      a(
        href = "https://university.business-science.io/",
        target = "_blank", "Expert Shiny Application Development Course "
      ),
      "by Business Science"
    ),
    
    # 1.0 BOOTSTRAP GRID SYSTEM ----
    h2("1.0 Bootstrap Grid System"),
    div(
      class = "container text-center",
      fluidRow(
        column(width = 4, "tenso", class = "bg-primary"),
        column(width = 4, "tenso", class = "bg-warning"),
        column(width = 4, "tenso", class = "bg-danger")
      ),
      fluidRow(
        column(width = 3, "tenso", class = "bg-primary"),
        column(width = 9, "tenso", class = "bg-info")
      )
    ),
    
    hr(), 
    # 2.0 WORKING WITH TEXT ----
    h2("2.0 Working with text"),
    p(class = "lead", "Business Science University helps us learn Shiny"),
    
    fluidRow(
      column(
        width = 6,
        p("We are creating a Bootstrap for Shiny cheat sheet"),
        p(strong("In section 1"), "we learned about the", strong(em("Bootstrap Grid System"))),
        p(tags$mark("In section 2"), " we learned about working with text in ", code("bootstrap"), ".")
      ),
      column(
        width = 6,
        tags$blockquote(
          class = "blockquote-reverse",
          p("When learning data science, consistency is more important than quantity"),
          tags$footer("Quote by", tags$cite(title = "Matt Dancho", "Matt Dancho"))
        )
      )
    ),
    
    hr(),
    
    # 3.0 TEXT ALIGMENT ----
    h2("3.0 Text Aligment"),
    div(
      class = "container",
      id = "text_aligment_1",
      p(class = "text-left text-lowercase", "Left-Aligned Lowercase Text"),
      p(class = "text-center text-uppercase", "Center-Aligned Uppercase Text"),
      p(class = "text-right text-capitalize", "Left-Aligned capitalize text")
    ),
    div(
      class = "container",
      id = "text_aligment_2",
      fluidRow(
        p(class = "text-left text-lowercase", "Left-Aligned Lowercase Text") |> 
          column(width = 4, class = "bg-primary"),
        p(class = "text-center text-uppercase", "Center-Aligned Uppercase Text") |> 
          column(width = 4, class = "bg-success"),
        p(class = "text-right text-capitalize", "Left-Aligned capitalize text") |> 
          column(width = 4, class = "bg-info")      
      )
    ), 
    
    hr(),
    # 4.0 LISTS ----
    h2("4.0 Lists"),
    tags$ul(
      tags$li("Item 1"),
      tags$li("Item 2"),
      tags$li("Item 3"),
      tags$li("Item 4")
    ),
    tags$ol(
      tags$li("Item 1"),
      tags$li("Item 2"),
      tags$li("Item 3"),
      tags$li("Item 4")
    ),
    tags$ul(
      class = "list-inline",
      tags$li("Item 1"),
      tags$li("Item 2"),
      tags$li("Item 3"),
      tags$li("Item 4")
    ),
    
    hr(),
    # 5.0 CONTEXTUAL COLORS & BACKGROUNDS ----
    h2("5.0 Contextual Colors & Backgrounds"),
    p(class = "text-primary", "Hello R"),
    p(class = "text-success", "Hello R"),
    p(class = "text-info", "Hello R"),
    p(class = "text-warning", "Hello R"),
    p(class = "text-danger", "Hello R"),
    
    p(class = "bg-primary", "Hello R"),
    p(class = "bg-success", "Hello R"),
    p(class = "bg-info", "Hello R"),
    p(class = "bg-warning", "Hello R"),
    p(class = "bg-danger", "Hello R")
  ),
  
  hr(),
  # 6.0 BUTTONS ----
  h2("6.0 Buttons"),
  h3("Contextual Buttons"),
  div(
    class = "container",
    a(href = "https://www.business-science.io/", class = "btn btn-default", "Go to Business Science"),
    a(href = "https://www.business-science.io/", class = "btn btn-primary", "Go to Business Science"),
    a(href = "https://www.business-science.io/", class = "btn btn-success", "Go to Business Science"),
    a(href = "https://www.business-science.io/", class = "btn btn-info", "Go to Business Science"),
    a(href = "https://www.business-science.io/", class = "btn btn-warning", "Go to Business Science"),
    a(href = "https://www.business-science.io/", class = "btn btn-danger", "Go to Business Science")
  ),
  
  br(),
 
  h3("Sizing Buttons"),
  div(
    class = "container",
    a(href = "https://www.business-science.io/", class = "btn btn-default btn-lg", "Go to Business Science"),
    a(href = "https://www.business-science.io/", class = "btn btn-primary btn-md", "Go to Business Science"),
    a(href = "https://www.business-science.io/", class = "btn btn-success btn-sm", "Go to Business Science"),
    a(href = "https://www.business-science.io/", class = "btn btn-info btn-xg", "Go to Business Science")
  ), 
  
  h3("Shiny Buttons"),
  div(
    class = "container",
    shiny::actionButton(
      inputId = "btn_1",
      label = "Shinu=y Button - Click Me!",
      class = "btn-primary btn-lg",
      icon = icon("sync", class = "fa-1x")
    ),
    shiny::textOutput(outputId = "btn_1_txt")
  ),

  hr(),
  # 7.0 IMAGES ----
  h2("7.0 Images"),
  div(
    class = "container",
    column(
      width = 4,
      img(
        class = "thumbnail img-responsive",
        src = "business-science-logo.png",
        style = "width:200px;"
      )
     ),
    column(
      width = 4,
      img(
        class = "img-rounded img-responsive",
        src = "matt-pic.jpg",
        style = "width:200px;"
      )
    ),
    column(
      width = 4,
      img(
        class = "img-circle img-responsive",
        src = "matt-pic.jpg",
        style = "width:200px;"
      )
    )
  ),
  
  hr(),
  # 8.0 THUMBNAILS ----
  h2("8.0 Thumnails"),
  fluidRow(
    column(
      width = 4,
      div(
        class = "thumbnail text-center",
        #style = "padding: 20px",
        img(
          class = "img-rounded img-responsive",
          src = "matt-pic.jpg"
        ),
        h3("Thumbnail Label"),
        p("Text about this thumbnail"),
        a(class = "btn btn-primary btn-sm", href = "#", "Learn More!")
      )
    )
  ),
  
  hr(),
  # 9.0 NAVBARS ----
  h2("9.0 Navbars"),
  shiny::navbarPage(
    title = "Business Science",
    inverse = TRUE,
    collapsible = TRUE,
    tabPanel(
      title = "What is Shiny?",
      value = "page_1",
      h1("What is Shiny", tags$small("A framework for building web apps in R")),
      p("All of the cool features do Shiny")
    ),
    tabPanel(
      title = "What is Bootstrap?",
      value = "page_2",
      h1("What is Bootstrap", tags$small("A popular web framework that extends HTML and CSS")),
      p("All of the cool features do Bootstrap")
    ),
    navbarMenu(
      title = "Using Shiny and Bootstrap",
      tabPanel(title = "Make Plots"),
      tabPanel(title = "Add Shiny Components"),
      "----",
      tabPanel(title = "More Info")
    )
  ),
  
  hr(),
  # 10.0 NAVS ----
  h1("10.0 Navs"),
  h3("Tabset Panel"),
  tabsetPanel(
    id = "tabset_1",
    type = "tabs",
    tabPanel(title = "Shiny", h3("What is Shiny?"), p("Shiny is awesome!")),
    tabPanel(title = "Bootstrap", h3("What is Bootstrap?"), p("Bootstrap is awesome!"))
  ),
  
  br(),
 
  h3("Navlist Panel"),
  navlistPanel(
    id = "tabset_1",
    tabPanel(title = "Shiny", h3("What is Shiny?"), p("Shiny is awesome!")),
    tabPanel(title = "Bootstrap", h3("What is Bootstrap?"), p("Bootstrap is awesome!"))
  ), 
  
  hr(),
  # 11.0 Sidebar Layout ----
  h2("11.0 Sidebar Layout"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      p("UI Elements Go Here"),
      shiny::dateRangeInput(inputId = "date_range_1", label = "Enter a date range")
    ), 
    mainPanel = mainPanel("Plot elements and analysis")
  ),
  
  hr(),
  # 12.0 JUMBOTRON ----
  h2("Jumbotron"),
  div(
    class = "jumbotron",
    style = "background:url('business-science-logo.png');background-size:cover;",
    div(
      class = "container",
      style = "background-color: #9999999c;",
      h1("Learning Shiny"),
      p("It1s your solution for building web application with ", code("R")),
      "learn more" |> a(class = "btn btn-lg btn-default", href = "#") |> p()
    ),
  ),
  
  hr(),
  # 13.0 PANELS ----
  h2("13.0 Panels"),
  
  div(
    class = "panel panel-primary",
    div(
      class = "panel-heading",
      h3("Chart Title - Plot of MPG vs WT")
    ),
    div(
      class = "panel-body bg-info",
      p("Insert Chart"),
      plotlyOutput(outputId = "mtcars")
    ),
    div(
      class = "panel-footer",
      style = "background-color: #d9edf7;",
      p("Footer - Caption: This is a plot of vehicle weight vs fuel efficient") |> tags$small()
    )
  ),
    
  hr(),
  # 14.0 MOBILE ----
  h2("14.0 Mobile"),
  fluidRow(
    class = "hidden-xs", 
    div(
      class = "jumbotron",
      h1("Learning Shiny"),
      p(class = "lead", "Will help you distribute interactive data products?"),
      a(class = "btn btn-primary btn-lg", href = "#", "Learn More!")
    )
  ),
  fluidRow(
    class = "hidden-sm hidden-md hidden-lg", 
    div(
      class = "thumbnail text-center",
      img(class = "img-responsive", style = "width: 200px;", src = "business-science-logo.png"),
      h3("Learning Shiny"),
      p(class = "lead", "Will help you distribute interactive data products?"),
      a(class = "btn btn-primary btn-sm", href = "#", "Learn More!")
    )
  ),
  
  hr(),
  # 15.0 CSS & THEMES ----
  h2("15.0 CSS & Themes"),
  fluidPage(
    theme = shinytheme("flatly"),
    tags$head(
      #tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "my_style.css")
      
    )
  ),
  
  hr(),
  # 16.0 ----
  h2("16.0 JavaScript"),
  useShinyjs(),
  
  fluidRow(
    column(
      width = 4,
      class = "well",
      
      # Toggle Button
      shiny::actionButton(inputId = "toggle_form", label = "Toggle form"),
 
      # Controls
      div(
       id = "controls",
       br(),
       shiny::textInput(
         inputId = "first_name",
         label = "First Name",
         placeholder = "Enter your first name:"
        ),
       shiny::textInput(
         inputId = "email",
         label = "Email",
         placeholder = "Email"
       ),
       shiny::actionButton(inputId = "submit_form", label = "Submit")
      ) |> shinyjs::hidden(),
      
      # Thank you
      div(
        id = "thank_you",
        br(),
        div(
          class = "alert alert-success",
          role = "alert",
          p("Thank you!", shiny::actionButton(inputId = "close_alert", label = "X", class = "pull-right btn-xs"))
        )
      ) |> shinyjs::hidden()
    ),
    column(
      width = 8,
      p("Placeholder for DT"),
      DTOutput(outputId = "new_user_dt")
    )
  ),
  
  
  
  div(style = "height: 400px")
)

server <- function(input, output, session) {
  counter <<- 0
  btn_1_click <- eventReactive(input$btn_1, {
    counter <<- counter + 1
    return(TRUE)
  })
  
  output$btn_1_txt <- renderText({
    if(btn_1_click()) {
      str_glue("You clicked me {counter} times!")
    }
  })
  
  # PLot mtcars
  output$mtcars <- renderPlotly({
    g <- mtcars |> 
      ggplot(aes(wt, mpg)) + 
      geom_point()
    
    ggplotly(g)
  })
  
  # 16.0 ShinyJS ----
  shinyjs::onclick(id = "toggle_form", {
    shinyjs::toggle(id = "controls", anim = TRUE)
  })
  
  observe({
    shinyjs::toggleState(id = "submit_form", condition = {
      !(input$first_name == "") && !(input$email == "")
    })
  })
  
  new_user_tbl <- eventReactive(input$submit_form, {
    new_user_tbl <- tibble(
      first_name = input$first_name,
      email = input$email,
      timestamp = lubridate::now()
    )
  })
  
  output$new_user_dt <- renderDataTable({
    new_user_tbl() |> datatable()
  })
  
  observeEvent(input$submit_form, {
    shinyjs::toggle(id = "thank_you", anim = TRUE, animType = "fade", time = 0.25)
  })
  
  shinyjs::onclick(id = "close_alert", {
    shinyjs::toggle(id = "thank_you", anim = TRUE, animType = "fade", time = 0.25)
  })
}

shinyApp(ui = ui, server = server)















