#setwd("/Users/setegn/Documents/Denmark")
#setwd("C:/Users/AlemuS/OneDrive - AgResearch/Documents/DenmarkPaper/Genomic-selection-in-broiler-main")
# Load required libraries
library(shiny)
library(DT)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(tidyr)
library(lattice)
library(gganimate)
library(png)
library(shinydashboard)
library(gifski)
library(forcats)
library(tidyverse)
library(shinyalert)
library(ggthemes)
library(shinyjs)
library(shiny)
library(plotly)
# Set working directory


# Read data
dat <- read.table('datpaper1.txt', header = TRUE)

# Rename columns
names(dat)[3] <- 'PG'

# Define ordered factors
dat$PMG <- ordered(dat$PMG, levels = c("0%MG", "25%MG", "50%MG", "75%MG", "100%MG"))
dat$PRG <- ordered(dat$PRG, levels = c("0%RG", "25%RG", "50%RG", "75%RG", "100%RG"))
dat$PG <- ordered(dat$PG, levels = c("2.5%geno", "5%geno", "10%geno", "20%geno"))
dat$Trait<- as.factor(dat$Trait)

dat[,5:6]<- round(dat[,5:6],3)
# Round columns 5 and 6 to 3 decimal places


# Create factor levels for PMG, PRG, and PG
dat2 <- dat %>%
  mutate(PMG = factor(PMG, labels = c("0", "25", "50", "75", "100"))) %>%
  mutate(PRG = factor(PRG, labels = c("0", "25", "50", "75", "100"))) %>%
  mutate(PG = factor(PG, labels = c("2.5", "5", "10", "20")))

# Define color palettes
color <- c("green", "black", "red", "blue", "violet")
color2 <- c("green", "red", "blue", "violet")

# Define key lists
mykey <- list(space = 'top', 
              columns = 5, 
              text = list(as.character(unique(dat$PMG)), col = color), 
              points = list(pch = 1, col = color))

mykey2 <- list(space = 'top', 
               columns = 4, 
               text = list(as.character(unique(dat$PG)), col = color2), 
               points = list(pch = 1, col = color2))

mykey3 <- list(space = 'top', 
               columns = 5, 
               text = list(as.character(unique(dat$PRG)), col = color), 
               points = list(pch = 1, col = color))

# Set theme
theme_set(theme_bw())

# Create dashboard header
dashHeader <- dashboardHeader(title = 'Genomic selection in broiler',
                              titleWidth = 450)

# Create dashboard sidebar
dashSidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = 'Plots from R and Javascript',
             tabName = 'onfarm',
             icon = icon('dashboard')),
    menuItem(text = 'Animated plot',
             tabName = 'animated',
             icon = icon('dashboard'))
  )
)

# Create dashboard body
dashBody <- dashboardBody(
  useShinyjs(),
  list(
    actionButton(inputId = "showh", label = "Show hidden text"),
    actionButton(inputId = "hideh", label = "Hide text"),
    br(),
    hidden(tags$div(id = "txt", style = 'color:blue;',
                    list(
                      helpText("This app is built from the project: Optimizing Genomic Selection in Broilers. The App displays the rate of genetic gain for aggregate breeding value, body weight, and feed intake for broiler schemes. The Tab in the App enables the user to choose which traits and which view to display. Furthermore, there are three interactive types of plots with animated or static type.  More importantly, the App is scalable and repeatable."),
                      hr()
                    )
    ))
  ),
  tabItems(
    tabItem(
      tabName = 'onfarm',
      fluidRow(
        box(width = 3,
            collapsible = TRUE,
            title = 'Controls',
            status = 'success',
            solidHeader = TRUE,
            selectInput('view', 'Which view you want to see:',
                        choices = c("Xaxis_PG", "Xaxis_PRG", "Xaxis_PMG"),
                        selected = 'Xaxis_PG'),
            selectInput("trait", "Which trait you want to see :",
                        choices = levels(dat[, 9]),
                        selected = levels(dat[, 9])[1])
        ),
        tabBox(width = 9,
               tabPanel(title = 'GGplot',
                        plotOutput("plot0")),
               tabPanel(title = 'plotly',
                        plotlyOutput("plot1")),
               tabPanel(title = 'Three Dimensional scatterplot',
                        plotlyOutput('plot2'))
        )
      )
    ),
    tabItem(
      tabName = 'animated',
      fluidRow(
        box(width = 4,
            collapsible = TRUE,
            title = 'Controls',
            status = 'primary',
            solidHeader = TRUE,
            selectInput("trait2", "Which trait you want to see :",
                        choices = levels(dat[, 9]),
                        selected = levels(dat[, 9])[1]),
            selectInput("pg2", "Choose Percent genotyped :",
                        choices = levels(dat[, 3]),
                        selected = levels(dat[, 3])[1])
        ),
        tabBox(width = 8,
               tabPanel(title = 'Animated_point_plot',
                        plotlyOutput('plot5')),
               tabPanel(title = 'Animated_line_plot',
                        plotlyOutput('plot4')),
               tabPanel(title = 'Fast_animation',
                        imageOutput("plot3"))
        )
      )
    )
  )
)

# Create dashboard UI
ui <- dashboardPage(
  header = dashHeader,
  sidebar = dashSidebar,
  body = dashBody,
  title = "Investigating different genotyping strategy for genomic selection in broiler"
)


server <- function(input, output) {
  
  observeEvent(input$showh, show("txt")) # show() is shiny js function, pass the element/widget ID as the argument
  
  observeEvent(input$hideh, hide("txt"))
  
  data <- reactive({
    dat[dat[, 9] %in% input$trait, ]
  })
  
  output$plot0 <- renderPlot({
    req(nrow(data()) > 0)
    
    dataf <- data()
    
    maxval = dataf$value + dataf$sd
    minval = dataf$value - dataf$sd
    limits = aes(ymax = maxval, ymin = minval)
    
    if (input$view == 'Xaxis_PG') {
      dat2 = dataf
      pg = as.numeric(dat2$PG)
      dat2$PG <- round(ifelse(pg == 1, 2.5, ifelse(pg == 2, 5, ifelse(pg == 3, 10, 20))), 1)
      dat2$PG = as.numeric(as.character(dat2$PG))
      
      plots = ggplot(dat2, aes(y = value, x = PG, color = PRG)) +
        facet_grid(. ~ PMG) +
        geom_point() +
        geom_line() +
        scale_x_continuous(breaks = dat2$PG) +
        theme_hc() +
        xlab('percentage of gentyped') +
        ylab(input$trait) +
        theme(axis.text = element_text(size = 6))
    }
    
    if (input$view == 'Xaxis_PRG') {
      dat2 = dataf
      pg = as.numeric(dat2$PRG)
      dat2$PRG <- ifelse(pg == 1, 0, ifelse(pg == 2, 25, ifelse(pg == 3, 50, ifelse(pg == 4, 75, 100))))
      dat2$PRG = as.numeric(as.character(dat2$PRG))
      
      plots = ggplot(dat2, aes(y = value, x = PRG, color = PG)) +
        facet_grid(. ~ PMG) +
        geom_point() +
        geom_line() +
        scale_x_continuous(breaks = dat2$PRG) +
        theme_bw() +
        xlab('\npercentage of random gentyped') +
        ylab(input$trait) +
        theme(axis.text = element_text(size = 6, angle = 10, hjust = 1))
    }
    
    if (input$view == 'Xaxis_PMG') {
      dat2 = dataf
      pg = as.numeric(dat2$PMG)
      dat2$PMG <- ifelse(pg == 1, 0, ifelse(pg == 2, 25, ifelse(pg == 3, 50, ifelse(pg == 4, 75, 100))))
      dat2$PMG = as.numeric(as.character(dat2$PMG))
      
      plots = ggplot(dat2, aes(y = value, x = PMG, color = PRG)) +
        facet_grid(. ~ PG) +
        geom_point() +
        geom_line() +
        scale_x_continuous(breaks = dat2$PMG) +
        xlab('percentage of male genotyped') +
        ylab(input$trait) +
        theme_stata()
    }
    
    plots
  })
  
  output$plot1 <- renderPlotly({
    req(nrow(data()) > 0)
    
    dataf <- data()
    maxval = dataf$value + dataf$sd
    minval = dataf$value - dataf$sd
    limits = aes(ymax = maxval, ymin = minval)
    dat2 = dataf
    
    if (input$view == 'Xaxis_PG') {
      pg = as.numeric(dat2$PG)
      dat2$PG <- round(ifelse(pg == 1, 2.5, ifelse(pg == 2, 5, ifelse(pg == 3, 10, 20))), 1)
      dat2$PG = as.numeric(as.character(dat2$PG))
      
      plots = ggplot(dat2, aes(y = value, x = PG, color = PRG)) +
        facet_grid(. ~ PMG) +
        geom_point() +
        geom_line() +
        scale_x_continuous(breaks = dat2$PG) +
        theme_bw() +
        xlab('\npercentage of gentyped') +
        ylab(input$trait) +
        theme(axis.text = element_text(size = 4, angle = 10, hjust = 1), panel.spacing.x = unit(0, "cm"), axis.title = element_text(size = 10, face = "bold"))
    }
    
    if (input$view == 'Xaxis_PRG') {
      pg = as.numeric(dat2$PRG)
      dat2$PRG <- ifelse(pg == 1, 0, ifelse(pg == 2, 25, ifelse(pg == 3, 50, ifelse(pg == 4, 75, 100))))
      dat2$PRG = as.numeric(as.character(dat2$PRG))
      
      plots = ggplot(dat2, aes(y = value, x = PRG, color = PG)) +
        facet_grid(. ~ PMG) +
        geom_point() +
        geom_line() +
        scale_x_continuous(breaks = dat2$PRG) +
        theme_bw() +
        xlab('\npercentage of random gentyped') +
        ylab(input$trait) +
        theme(axis.text = element_text(size = 6, angle = 10, hjust = 1), panel.spacing.x = unit(0, "cm"), axis.title = element_text(size = 6, face = "bold"))
    }
    
    if (input$view == 'Xaxis_PMG') {
      pg = as.numeric(dat2$PMG)
      dat2$PMG <- ifelse(pg == 1, 0, ifelse(pg == 2, 25, ifelse(pg == 3, 50, ifelse(pg == 4, 75, 100))))
      dat2$PMG = as.numeric(as.character(dat2$PMG))
      
      plots = ggplot(dat2, aes(y = value, x = PMG, color = PRG)) +
        facet_grid(. ~ PG) +
        geom_point() +
        geom_line() +
        scale_x_continuous(breaks = dat2$PMG) +
        xlab('\npercentage of male genotyped') +
        ylab(input$trait) +
        theme(legend.position = "bottom", panel.spacing.x = unit(0, "cm"), axis.text = element_text(size = 6, angle = 10, hjust = 1),
              axis.title = element_text(size = 8, face = "bold"))
    }
    
    ggplotly(plots) %>%
      layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
  })
  
  pd <- position_dodge(0.1)
  
  data <- reactive({
    dat[dat[, 9] %in% input$trait, ]
  })
 
  
  
 
  
  output$plot2 <- renderPlotly({
    req(nrow(data()) > 0)
    dat2 <- data()
    req(!is.null(dat2$PRG), !is.null(dat2$PG), !is.null(dat2$PMG), !is.null(dat2$value))

    p1 <- plot_ly()  # Default empty plot


    if (input$view == 'Xaxis_PG') {
      print(head(dat2))
      p1 <- plot_ly(dat2, x = ~PG, y = ~PRG, z = ~value, color = ~PMG, type = 'scatter3d', mode = 'markers') %>%
        layout(scene = list(xaxis = list(title = "PRG"), yaxis = list(title = "PG"), zaxis = list(title = input$trait)))
    }

    if (input$view == 'Xaxis_PRG') {
      print(head(dat2,10))
      p1 <- plot_ly(dat2, x = ~PRG, y = ~PMG, z = ~value, color = ~PG, type = 'scatter3d', mode = 'markers') %>%
        layout(scene = list(xaxis = list(title = "PRG"), yaxis = list(title = "PMG"), zaxis = list(title = input$trait)))
    }

    if (input$view == 'Xaxis_PMG') {
      print(head(dat2,10))
      p1 <- plot_ly(dat2, x = ~PMG, y = ~PG, z = ~value, color = ~PRG, type = 'scatter3d', mode = 'markers') %>%
        layout(scene = list(xaxis = list(title = "PRG"), yaxis = list(title = "PG"), zaxis = list(title = input$trait)))
    }
    
    
    
    return(p1)
  })




 
  
  
  datani <- reactive({
    #dat %>% filter( input$trait)
    
    dat[dat[,9]%in% input$trait2 & dat[,3]%in%input$pg2,]
    
  })
  
  output$plot3 <- renderImage({
    req(nrow(datani()) > 0)
    dat2<-datani()
    pg=as.numeric(dat2$PRG)
    dat2$PRG<- ifelse(pg==1,0, ifelse(pg==2,25,ifelse(pg==3,50,ifelse(pg==4,75,100))))
    dat2$PRG=as.numeric(as.character(dat2$PRG))
    
    outfile <- tempfile(fileext='.gif')
    #dataa<-datsa()
    # now make the animation
    p<-ggplot(dat2, aes(x=PRG, y=value, group=factor(PMG),color=factor(PMG))) +
      geom_line() +
      scale_color_viridis_d() + 
      geom_point(aes(group = seq_along(PRG))) +
      transition_reveal(PRG)
    
    anim_save("outfile.gif", animate(p)) # New
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif',
         width = 400,
         height = 300,
         alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
  output$plot4 <- renderPlotly({
    req(nrow(datani()) > 0)
    dat2<-datani()
    pg=as.numeric(dat2$PRG)
    
    dat2$PRG<- ifelse(pg==1,0, ifelse(pg==2,25,ifelse(pg==3,50,ifelse(pg==4,75,100))))
    dat2$PRG=as.numeric(as.character(dat2$PRG))
    dat2$PMG <- ordered(dat2$PMG, levels = c("0%MG", "25%MG", "50%MG", "75%MG","100%MG"))
    newdat<- dat2
    newdat2= dat2%>% complete(PG,PRG,fill=list(value=0))
    cumulative_launches <- dat2 %>%
      
      split(f = .$PRG) %>%
      accumulate(., ~bind_rows(.x, .y)) %>%
      bind_rows(.id = "frame")
    
    # Create the cumulative animation
    cumulative_launches %>%
      plot_ly(x = ~PRG, y = ~value, color = ~PMG) %>%add_lines(frame = ~as.numeric(frame)) 
    # %>% add_markers(frame = ~as.numeric(frame))
    
  })
  
  output$plot5 <- renderPlotly({
    req(nrow(dat2) > 0)
    dat2<-datani()
    
    pg=as.numeric(dat2$PRG)
    dat2$PRG<- ifelse(pg==1,0, ifelse(pg==2,25,ifelse(pg==3,50,ifelse(pg==4,75,100))))
    dat2$PRG=as.numeric(as.character(dat2$PRG))
    dat2$PMG <- ordered(dat2$PMG, levels = c("0%MG", "25%MG", "50%MG", "75%MG","100%MG"))
    fig <- dat2 %>%
      plot_ly(
        x = ~PRG, 
        y = ~value, 
        ids = ~PMG, 
        color = ~PMG, 
        frame = ~PRG, 
        text = ~PMG, 
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers'
      )
    fig
  })
}

shinyApp(ui = ui, server = server)

