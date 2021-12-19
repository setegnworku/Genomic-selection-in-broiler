setwd("/Users/setegn/Documents/Denmark")
library(shiny)
library(DT)
library(ggplot2)
library(shinythemes)
#setwd('/home/user/Documents/Denmarkpaper/paper1/rshiny/paperwithJust')
dat<- read.table('datpaper1.txt',h=T)
names(dat)[3]<- 'PG'
dat$PMG <- ordered(dat$PMG, levels = c("0%MG", "25%MG", "50%MG", "75%MG","100%MG"))
dat$PRG <- ordered(dat$PRG, levels = c("0%RG", "25%RG", "50%RG", "75%RG","100%RG"))
dat$PG <- ordered(dat$PG, levels = c("2.5%geno", "5%geno", "10%geno", "20%geno"))
library(dplyr)
library(tidyr)
ssin<-dat
dat[,5:6]<- round(dat[,5:6],3)
dat2 <- dat %>%  mutate(PMG = factor(PMG, labels= c("0","25","50","75","100")  )) %>% 
  mutate(PRG = factor(PRG, labels= c("0","25","50","75","100")  )) %>%  
  mutate(PG = factor(PG, labels= c("2.5","5","10","20") ))


library(shiny)
library(ggplot2)
library(plotly)
library(DT)
suppressMessages(library("ggplot2"))


library(lattice)
color=c("green", "black", "red","blue","violet")
color2=c("green", "red","blue","violet")
mykey <- list(space = 'top', 
              columns = 5, 
              text = list(as.character(unique(dat$PMG)), col = color), 
              points = list(pch=1, col = color))

mykey2=list(space = 'top', 
            columns = 4, 
            text = list(as.character(unique(dat$PG)), col = color2), 
            points = list(pch=1, col = color2)) 

mykey3=list(space = 'top', 
            columns = 5, 
            text = list(as.character(unique(dat$PRG)), col = color), 
            points = list(pch=1, col = color)) 






library(ggplot2)
library(gganimate)
theme_set(theme_bw())
library(png)
library(shinydashboard)
library(gifski)
library(shinythemes)
library(plotly)
library(shinythemes)
library(forcats)
library(shiny)
library(shinyjs)
library(ggplot2)
library(plotly)
library(DT)
library(shinythemes)
library(tidyverse)
library(shinyalert)

#setwd('/home/user/Documents/Denmarkpaper/paper1/rshiny')
dat<- read.table('datpaper1.txt',h=T)
names(dat)[3]<- 'PG'
dat$PMG <- ordered(dat$PMG, levels = c("0%MG", "25%MG", "50%MG", "75%MG","100%MG"))
dat$PRG <- ordered(dat$PRG, levels = c("0%RG", "25%RG", "50%RG", "75%RG","100%RG"))
dat$PG <- ordered(dat$PG, levels = c("2.5%geno", "5%geno", "10%geno", "20%geno"))
dat$Trait<- as.factor(dat$Trait)
ssin<-dat
dat[,5:6]<- round(dat[,5:6],3)
#dat=dat2
dashHeader=dashboardHeader(title = 'Genomic selection in brolier',
                           titleWidth = 450
                           
                           
)




dashSidebar=dashboardSidebar(
  
  sidebarMenu(
    menuItem(text='Plots from R and Javascript',
             tabName = 'onfarm',
             icon=icon('dashboard')),
    menuItem(text='Animated plot',
             tabName = 'animated',
             icon=icon('dashboard'))
  ))
dashBody=dashboardBody(
  useShinyjs(),
  # useShinyalert(),
  list(
    actionButton(inputId = "showh", label = "Show hidden text"),
    actionButton(inputId = "hideh", label = "Hide text"),
    br(),
    hidden(tags$div(id="txt", style='color:blue;', list(helpText("This app is built from the project: Optimizing Genomic Selection in Broilers. The App displays the rate of genetic gain for aggregate breeding value, body weight and feed intake for broiler schemes. The Tab in the App enable the user to choose which traits and which view to display. Furthermore, there are three interactive types of plots with annimated or static type.  More importantly the App is scalable and repeatable."),hr())))),
  
  tabItems(
    tabItem(
      tabName='onfarm',
      fluidRow(
        box( width=3,
             collapsible = TRUE,
             title='Controls',
             status='success',solidHeader = TRUE,
             
             selectInput('view', 'Which view you want to see:',choices = c('View_one','View_two','View_three'),selected='View_one'),
             selectInput("trait", "Which trait you want to see :",
                         choices = levels(dat[,9]),selected=levels(dat[,9])[1])),
        
        tabBox( width=9,
                tabPanel(title='GGplot',  
                         plotOutput("plot0")),
                
                tabPanel(title='plotly',  
                         plotlyOutput("plot1")),
                tabPanel(title='Latticeplot',
                         plotOutput('plot2'))))
      
    ) 
    
    
    
    
    ,
    
    tabItem(
      tabName='animated',
      fluidRow(
        box( width=4,
             collapsible = TRUE,
             title='Controls',
             status='primary',solidHeader = TRUE,
             
             selectInput("trait2", "Which trait you want to see :",
                         choices = levels(dat[,9]),selected=levels(dat[,9])[1]),
             selectInput("pg2", "Choose Percent genotyped :",
                         choices = levels(dat[,3]),selected=levels(dat[,3])[1])),
        tabBox( width=8,
                
                tabPanel(title='Animated_point_plot',
                         plotlyOutput('plot5')),
                
                tabPanel(title='Animated_line_plot',
                         plotlyOutput('plot4'))   ,
                
                tabPanel(title='Fast_animation',
                         imageOutput("plot3"))
        )
        
      )
      
    )
  )
  
  
  
)


#)
#)



ui<- dashboardPage(
  header= dashHeader,
  sidebar=dashSidebar,
  body=dashBody,
  title="Investigating different genotyping strategy for genomic selction in brolier"
)





library(shinyjs)
server <- function(input, output) {
  
  library(shinyjs)
  observeEvent(input$showh,
               show("txt")) # show() is shiny js function, pass the element/widget ID as the argument
  
  observeEvent(input$hideh,
               hide("txt"))
  
  data <- reactive({
    #dat %>% filter( input$trait)
    
    dat[dat[,9]%in% input$trait,]
    
  })
  
  ################################ First plot type
  library(ggthemes)
  output$plot0 <- renderPlot({
    req(nrow(data()) > 0)
    
    dataf<- data()
    
    maxval=dataf$value  +dataf$sd
    minval=dataf$value  - dataf$sd
    limits=aes(ymax = maxval, ymin=minval)
    if ( input$view=='View_one'){
      dat2=dataf
      pg=as.numeric(dat2$PG)
      dat2$PG<- round(ifelse(pg==1,2.5, ifelse(pg==2,5,ifelse(pg==3,10,20))),1)
      dat2$PG=as.numeric(as.character(dat2$PG))
      plots= ggplot(dat2,aes(y=value,x=PG,color=PRG))+facet_grid(. ~ PMG)+ geom_point() +
        geom_line() +scale_x_continuous(breaks = dat2$PG)+
        theme_hc()+xlab('percentage of gentyped')+ylab(input$trait)+theme(axis.text = element_text(size = 6))
      
      #plots=ggplot(dataf, aes(fill=PRG, y=value, x=PG))+ geom_col(position= position_dodge(width=0.9))+geom_errorbar(limits, position= position_dodge(width=0.9), width=0.4) +
      # theme_bw( )+xlab('percentage of gentyped')+ylab(input$trait)+     facet_grid(.~PMG, scales = "free", space = "free")+theme(legend.position="top", legend.box = "horizontal")
    }
    if( input$view=='View_two') {
      dat2=dataf
      pg=as.numeric(dat2$PRG)
      dat2$PRG<- ifelse(pg==1,0, ifelse(pg==2,25,ifelse(pg==3,50,ifelse(pg==4,75,100))))
      dat2$PRG=as.numeric(as.character(dat2$PRG))
      plots= ggplot(dat2,aes(y=value,x=PRG,color=PG))+facet_grid(. ~ PMG)+ geom_point() +
        geom_line() +scale_x_continuous(breaks = dat2$PRG)+
        theme_economist() + scale_fill_economist()+xlab('percentage of random gentyped')+ylab(input$trait)+theme(axis.text = element_text(size = 6,angle=90,hjust=1))
      
    }
    if (input$view=='View_three'){
      dat2=dataf
      pg=as.numeric(dat2$PMG)
      dat2$PMG<- ifelse(pg==1,0, ifelse(pg==2,25,ifelse(pg==3,50,ifelse(pg==4,75,100))))
      dat2$PMG=as.numeric(as.character(dat2$PMG))
      plots= ggplot(dat2,aes(y=value,x=PMG,color=PRG))+facet_grid(. ~ PG)+ geom_point() +
        geom_line() +
        scale_x_continuous(breaks = dat2$PMG)+xlab('percentage of male genotyped')+ylab(input$trait)
      
      plots=plots+theme_stata()
    }
    
    plots
    
  })
  #############
  
  
  output$plot1 <- renderPlotly({
    
    req(nrow(data()) > 0)
    
    dataf<- data()
    # req(nrow(dataf) > 0)
    maxval=dataf$value  +dataf$sd
    minval=dataf$value  - dataf$sd
    limits=aes(ymax = maxval, ymin=minval)
    dat2=dataf
    if ( input$view=='View_one'){
      pg=as.numeric(dat2$PG)
      dat2$PG<- round(ifelse(pg==1,2.5, ifelse(pg==2,5,ifelse(pg==3,10,20))),1)
      dat2$PG=as.numeric(as.character(dat2$PG))
      # plots=ggplot(dataf, aes(fill=PRG, y=value, x=PG))+ geom_col(position= position_dodge(width=0.9))+geom_errorbar(limits, position= position_dodge(width=0.9), width=0.4) +
      #   theme_bw( )+xlab('percentage of gentyped')+ylab(input$trait)+     facet_grid(.~PMG, scales = "free", space = "free")+theme(legend.position="top", legend.box = "horizontal")
      plots= ggplot(dat2,aes(y=value,x=PG,color=PRG))+facet_grid(. ~ PMG)+ geom_point() +
        geom_line() +
        scale_x_continuous(breaks = dat2$PG)+
        # scale_x_discrete(breaks = dat2$PG)+
        theme_bw( )+xlab('\npercentage of gentyped')+ylab(input$trait)+theme(axis.text = element_text(size = 4,angle=10,hjust=1),panel.spacing.x=unit(0,"cm"),
                                                                             
                                                                             axis.title=element_text(size=10,face="bold"))
      #ggplotly(s)
      
      
    }
    if( input$view=='View_two') {
      pg=as.numeric(dat2$PRG)
      dat2$PRG<- ifelse(pg==1,0, ifelse(pg==2,25,ifelse(pg==3,50,ifelse(pg==4,75,100))))
      dat2$PRG=as.numeric(as.character(dat2$PRG))
      plots= ggplot(dat2,aes(y=value,x=PRG,color=PG))+facet_grid(. ~ PMG)+ geom_point() +
        geom_line() +scale_x_continuous(breaks = dat2$PRG)+
        theme_bw( )+xlab('\npercentage of random gentyped')+ylab(input$trait)+theme(axis.text = element_text(size = 6,angle=10,hjust=1),panel.spacing.x=unit(0,"cm"),
                                                                                    
                                                                                    axis.title=element_text(size=6,face="bold"))
    }
    if (input$view=='View_three'){
      pg=as.numeric(dat2$PMG)
      dat2$PMG<- ifelse(pg==1,0, ifelse(pg==2,25,ifelse(pg==3,50,ifelse(pg==4,75,100))))
      dat2$PMG=as.numeric(as.character(dat2$PMG))
      plots= ggplot(dat2,aes(y=value,x=PMG,color=PRG))+facet_grid(. ~ PG)+ geom_point() +
        geom_line() +
        scale_x_continuous(breaks = dat2$PMG)+xlab('\npercentage of male genotyped')+ylab(input$trait)
      plots=plots+ theme(legend.position = "bottom",panel.spacing.x=unit(0,"cm"),axis.text = element_text(size = 6,angle=10,hjust=1),
                         axis.title=element_text(size=8,face="bold"))
      
    }
    
    # ggplotly(plots,width = (0.95*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]))
    ggplotly(plots)%>%
      layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
    
  })
  pd <- position_dodge(0.1)
  
  
  data2 <- reactive({
    dat[dat[,9]%in% input$trait,]
    
    
  })
  
  output$plot2 <- renderPlot({
    req(nrow(data2()) > 0)
    data <- reactive({
      #dat %>% filter( input$trait)
      
      dat[dat[,9]%in% input$trait,]
      
    })
    
    if ( input$view=='View_one'){
      p1= xyplot(value ~ PRG|PG,layout=c(4,1), group=PMG,data=data(),type="b",lwd=2.5,lty=1,cex=2,pch=1,col=color,key=mykey,ylab=input$trait,scales=list( x=list(cex=0.4)))
    }
    if ( input$view=='View_two'){
      
      p1= xyplot(value ~ PRG|PMG,layout=c(5,1), group=PG,data=data(),type="b",lwd=2.5,lty=1,cex=0.4,pch=1,col=color2,key=mykey2,ylab=input$trait, scales=list( x=list(cex=0.4)))
    }
    
    
    if ( input$view=='View_three'){
      
      p1= xyplot(value ~ PG|PMG,layout=c(5,1), group=PRG,data=data(),type="b",lwd=2.5,lty=1,cex=2,pch=1,col=color,key=mykey3,ylab=input$trait,scales=list( x=list(cex=0.4)))
    }
    
    p1
    
    
  })
  
  
  datani <- reactive({
    #dat %>% filter( input$trait)
    
    dat[dat[,9]%in% input$trait2&dat[,3]%in%input$pg2,]
    
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
    
    
    # p + 
    # geom_point(aes(group = seq_along(week))) +
    # transition_reveal(week)
    
    anim_save("outfile.gif", animate(p)) # New
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif',
         width = 400,
         height = 300,
         alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
  require(tidyverse)
  
  output$plot4<- renderPlotly({
    
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
  
  output$plot5<- renderPlotly({
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



shinyApp(ui,server)








































