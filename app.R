# from AER_app

library(AER)
library(tidyverse)
library(ggplot2)
library(treemapify)
library(shinythemes)
library(tidytext)
library(bslib)
library(maps)
library(leaflet)
library(showtext)

stolaf <- font_add("stolaf", "stolaf-font.otf")
SmithCenter <- read_csv("SmithCenter.csv")
major_population <- read_csv("Majors_Table.csv")
conc_population <- read_csv("Conc_Table.csv")
costs_table <- read_csv("costs22-23.csv")
long_lat <- read_csv("long_lat.csv")

#start: some helper datasets
csv_plot_7_a <- major_population %>%
  select(Major, Population, Faculty) %>%
  group_by(Major, Faculty)%>%
  summarise(n=sum(Population)) %>%
  mutate(n =ifelse(is.na(n), 0, n),
         Type="Major") %>%
  rename(Name="Major")

csv_plot_7_b <- conc_population %>%
  select(Concentration, Population, Faculty) %>%
  group_by(Concentration, Faculty)%>%
  summarise(n=sum(Population)) %>%
  mutate(n =ifelse(is.na(n), 0, n),
         Type="Concentration")%>%
  rename(Name="Concentration")

csv_plot_7 <- csv_plot_7_a %>%
  bind_rows(csv_plot_7_b)



costs_new <- costs_table %>%
  filter(!is.na(Location)) %>%
  filter(`Course ID`!="36010") %>%
  mutate(new_cost_comp = str_remove(cost_comp, "\\$"),
         new_cost_comp = parse_number(new_cost_comp), 
         new_program_cost = str_remove(program_cost, "\\$"),
         new_program_cost = parse_number(new_program_cost),
         new_pocket_cost = str_remove(pocket_cost, "\\$"), 
         new_pocket_cost = parse_number(new_pocket_cost)) 

major_population_plot_1 <- major_population %>%
  filter(Year==2021) %>%
  select(Faculty, Major)

conc_population_plot_1 <- conc_population %>%
  filter(Year==2021) %>%
  select(Faculty, Concentration)





p1_options <- SmithCenter %>%
  group_by(Name) %>%
  summarize() %>%
  filter(!is.na(Name))

p6_options <- SmithCenter %>%
  group_by(Program) %>%
  summarize() %>%
  filter(!is.na(Program))

p2_options <- SmithCenter %>%
  left_join(major_population_plot_1, by=c("Name"="Major")) %>%
  left_join(conc_population_plot_1, by=c("Name"="Concentration")) %>%
  mutate(Faculty = ifelse(Type=="Major", Faculty.x, Faculty.y)) %>%
  select(-Faculty.x, -Faculty.y) %>%
  group_by(Faculty) %>%
  summarize() %>%
  filter(!is.na(Faculty))

maps <- SmithCenter %>%
  unnest_regex(`Program Currently Assigned Country`, `Program Currently Assigned Country`, pattern = ";", to_lower=FALSE)

program_map <- maps %>%
  left_join(long_lat, by= c(`Program Currently Assigned Country`= "Location"))

#end: some helper datasets



#home page
plot8<- fluidPage(
  
  includeMarkdown("include.Rmd")
)


#Past Choices
plot1 <- fluidPage(
  includeMarkdown("Choices1.Rmd"),
  selectInput("n_breaks", label = "Selected Major or Concentration:",
              choices = p1_options[[1]], selected = 20),
  plotOutput("plot1"),
  
  includeMarkdown("Choices2.Rmd"),
  selectInput("study_abroad", label = "Selected Study Abroad Program",
              choices = p6_options[[1]], selected = 20),
  plotOutput("plot6"))

#page: General Trends
plot2 <- fluidPage(
  includeMarkdown("Trend1.Rmd"),
  checkboxInput("type_check_p7","Filter by a specific Faculty",value=FALSE),
  selectInput("n_breaks_p7", label = "If yes, choose a Faculty:",
              choices = p2_options[[1]], selected = 20),
  plotOutput(outputId = "plot7"),
  
  includeMarkdown("Trend2.Rmd"),
  checkboxInput("type_check_p2","Filter by a specific Faculty",value=FALSE),
  selectInput("n_breaks_p2", label = "If yes, choose a Faculty:",
              choices = p2_options[[1]], selected = 20),
  plotOutput(outputId = "plot2"))

#page: Cost Analysis
plot3 <-fluidPage(
  includeMarkdown("Cost1.Rmd"),
  selectInput("cost_category", label = "Selected Cost Category:",
              choices = c("All" = "all",
                          "Pocket Cost" = "pocket_cost",
                          "Program Cost" = "program_cost",
                          "Cost Comparison" = "cost_comp")),
  plotOutput(outputId = "plot3"),

  includeMarkdown("Cost2.Rmd"),  
  selectInput("cost_category2", label = "Selected Cost Category:",
              choices = c("Pocket Cost" = "pocket_cost",
                          "Program Cost" = "program_cost",
                          "Cost Comparison" = "cost_comp")),
  
  plotOutput(outputId = "plot4")
)


#page:Maps
plot5 <- fluidPage(
  includeMarkdown("Map.Rmd"),
  leafletOutput("plot5",height = 1000))



ui <- navbarPage(title = "WOGW: Which Oles Go Where?",
                 theme = bs_theme(bg = "#FFFFFF", fg = "#e4a01b", 
                                  primary = "black",
                                  secondary="black",
                                  bootswatch = "cosmo",
                                  base_font = stolaf,
                                  code_font = stolaf),
                 tabPanel("Past Choices", plot1),
                 
                 
                
                 tabPanel("General Trends", plot2),
                # tabPanel("Cost Analysis",plot3),
                 tabPanel("Maps",plot5),
                tabPanel("About",plot8))

return_status <- function (x) {
  type <- SmithCenter %>%
    filter(Name==x) %>%
    group_by(Type) %>%
    summarise()
  type[[1]]
}

server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    SmithCenter %>%
      filter(Name==input$n_breaks) %>%
      group_by(Program) %>%
      summarize(n=n()) %>%
      slice_max(n, n=10) %>%
      ggplot(aes(x=fct_reorder(Program, n), y=n)) +
      geom_col( 
        fill="#a3d65c", 
        show.legend = FALSE)+
      coord_flip() +
      labs(y="", x="", title =paste("10 Most Popular Study Abroad Programs for", input$n_breaks, return_status(input$n_breaks))) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(hjust=0, size=12),
            axis.ticks.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank()
      ) +
      geom_text(aes(label = n), hjust = -2)
    
    #axis.text.y=element_text(hjust=0)
    #element_text(size=15)
    
  })
  
  output$plot2 <- renderPlot({
    
    if (!input$type_check_p2) {
      SmithCenter %>%
        left_join(major_population_plot_1, by=c("Name"="Major")) %>%
        left_join(conc_population_plot_1, by=c("Name"="Concentration")) %>%
        mutate(Faculty = ifelse(Type=="Major", Faculty.x, Faculty.y)) %>%
        select(-Faculty.x, -Faculty.y) %>%
        filter(!is.na(Faculty), !is.na(Offered))%>%
        group_by(Faculty) %>%
        summarize(n=n(), prop = 1-(sum(Offered)/n), percentage = as.integer(prop*100)) %>%
        ggplot(aes(area = prop, fill = Faculty, label = paste(Faculty, "\n", percentage, "%"))) +
        geom_treemap() +
        geom_treemap_text(colour = "white",
                          place = "centre",
                          size = 15) +
        theme(legend.position = "none") 
      
    }
    else{
      SmithCenter %>%
        left_join(major_population_plot_1, by=c("Name"="Major")) %>%
        left_join(conc_population_plot_1, by=c("Name"="Concentration")) %>%
        mutate(Faculty = ifelse(Type=="Major", Faculty.x, Faculty.y)) %>%
        select(-Faculty.x, -Faculty.y) %>%
        filter(!is.na(Faculty), !is.na(Offered), Faculty==input$n_breaks_p2)%>%
        group_by(Name) %>%
        summarize(n=n(), prop = 1-(sum(Offered)/n), percentage = as.integer(prop*100)) %>%
        ggplot(aes(area = prop, fill = Name, label = paste(Name, "\n", percentage, "%"))) +
        geom_treemap() +
        geom_treemap_text(colour = "white",
                          place = "centre",
                          size = 15) +
        theme(legend.position = "none")
    }
    
    
    
  })
  
  output$plot3 <- renderPlot({
    if(input$cost_category == "all") {
      costs_new %>%
        select(Location, program_cost, pocket_cost, new_cost_comp) %>%
        mutate_at(vars(pocket_cost:program_cost), parse_number) %>%
        group_by(Location) %>%
        summarise(avg_pocket_cost = mean(pocket_cost),
                  avg_program_cost = mean(program_cost), 
                  avg_cost_comp = mean(new_cost_comp)) %>%
        pivot_longer(2:4, names_to = "cost_category", values_to = "value") %>%
        
        ggplot() +
        geom_col(aes(x = Location, y = value, fill = cost_category), position ="dodge")+
        scale_fill_manual(values = c("avg_pocket_cost" = "blue", 
                                     "avg_program_cost" = "light blue", 
                                     "avg_cost_comp" = "#00827e"),
                          labels=c("Average Pocket Cost","Average Program Cost","Average Cost Comparison"),
                          aesthetics = "fill") +
        theme_bw() +
        theme(plot.title = element_text(size=10),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.title.y = element_text(color = "#6295bf"))+
        scale_y_continuous(limits = c(0, 9000),
                           breaks = seq(0,9000,500),
                           labels = c("$0", "$500", "$1000", "$1500", "$2000", 
                                      "$2500","$3000", "$3500","$4000", "$4500",
                                      "$5000", "$5500","$6000", "$6500","$7000",
                                      "$7500","$8000", "$8500","$9000")) +
        coord_flip()+
        labs(y="", x="", title = "Study Abroad Costs Breakdown in USD", fill="Cost Category") + 
        theme(plot.title = element_text(size=17))
    }
    
    else if (input$cost_category == "pocket_cost"){
      costs_new %>%
        select(Location, new_pocket_cost) %>%
        #mutate_at(vars(pocket_cost:program_cost), parse_number) %>%
        #mutate(new_var = parse_number(pocket_cost)) %>%
        group_by(Location) %>%
        summarise(avg_new_var = mean(new_pocket_cost)) %>%
        ggplot() +
        geom_col(aes(x = Location, y = avg_new_var), fill = "blue") +
        theme_bw() +
        theme(plot.title = element_text(size=10),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.title.y = element_text(color = "#6295bf")) +
        scale_y_continuous(limits = c(0, 9000),
                           breaks = seq(0,9000,500),
                           labels = c("$0", "$500", "$1000", "$1500", "$2000", 
                                      "$2500","$3000", "$3500","$4000", "$4500",
                                      "$5000", "$5500","$6000", "$6500","$7000",
                                      "$7500","$8000", "$8500","$9000")) +
        coord_flip() +
        labs(y="", x="", title =paste("St. Olaf Study Abroad Estimated Average Pocket Costs Breakdown by Geographical Region in USD")) +
        theme(plot.title = element_text(size=17))
    }
    
    else if (input$cost_category == "program_cost"){
      costs_new %>%
        select(Location, new_program_cost) %>%
        #mutate_at(vars(pocket_cost:program_cost), parse_number) %>%
        #mutate(new_var = parse_number(program_cost)) %>%
        group_by(Location) %>%
        summarise(avg_new_var = mean(new_program_cost)) %>%
        ggplot() +
        geom_col(aes(x = Location, y = avg_new_var), fill = "light blue") +
        theme_bw() +
        theme(plot.title = element_text(size=10),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.title.y = element_text(color = "#6295bf"))+
        scale_y_continuous(limits = c(0, 9000),
                           breaks = seq(0,9000,500),
                           labels = c("$0", "$500", "$1000", "$1500", "$2000", 
                                      "$2500","$3000", "$3500","$4000", "$4500",
                                      "$5000", "$5500","$6000", "$6500","$7000",
                                      "$7500","$8000", "$8500","$9000")) +
        coord_flip()+
        labs(y="", x="", title =paste("St. Olaf Study Abroad Estimated Average Program Costs Breakdown by Geographical Region in USD"))+
        theme(plot.title = element_text(size=17))
    }
    
    else if (input$cost_category == "cost_comp"){
      costs_new %>%
        select(Location, new_cost_comp) %>%
        #mutate_at(vars(pocket_cost:program_cost), parse_number) %>%
        group_by(Location) %>%
        summarise(avg_new_var = mean(new_cost_comp)) %>%
        ggplot() +
        geom_col(aes(x = Location, y = avg_new_var), fill = "#00827e") +
        theme_bw() +
        theme(plot.title = element_text(size=10),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.title.y = element_text(color = "#6295bf"))+
        scale_y_continuous(limits = c(0, 9000),
                           breaks = seq(0,9000,500),
                           labels = c("$0", "$500", "$1000", "$1500", "$2000", 
                                      "$2500","$3000", "$3500","$4000", "$4500",
                                      "$5000", "$5500","$6000", "$6500","$7000",
                                      "$7500","$8000", "$8500","$9000"))+
        coord_flip()+
        labs(y="", x="", title =paste("St. Olaf Study Abroad Estimated Average Cost Comparison by Geographical Region in USD")) +
        theme(plot.title = element_text(size=17))
    }
  })
  
  output$plot4 <- renderPlot({
    if(input$cost_category2 == "pocket_cost") {
      costs_new %>%
        ggplot(mapping=aes(x = Location, y = parse_number(pocket_cost))) +
        geom_boxplot(outlier.color = "blue") +
        theme_classic() +
        theme(plot.title = element_text(size=10),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.title.y = element_text(color = "#6295bf"))+
        scale_y_continuous(limits = c(0, 15000),
                           breaks = seq(0,15000,500),
                           labels = c("$0", "$500", "$1000", "$1500", "$2000", 
                                      "$2500","$3000", "$3500","$4000", "$4500",
                                      "$5000", "$5500","$6000", "$6500","$7000",
                                      "$7500","$8000", "$8500","$9000", "$9500",
                                      "$10000", "$10500","$11000","$11500",
                                      "$12000", "$12500","$13000", "$13500",
                                      "$14000", "$14500","$15000")) +
        coord_flip() + 
        labs(y="", x="", title =paste("St. Olaf Study Abroad Pocket Cost Breakdown per Program by Geographical Region in USD")) +
        theme(plot.title = element_text(size=17))
    }
    
    else if (input$cost_category2 == "program_cost"){
      costs_new %>%
        ggplot(mapping=aes(x = Location, y = parse_number(program_cost))) +
        geom_boxplot(outlier.color = "blue") +
        theme_classic() +
        theme(plot.title = element_text(size=10),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.title.y = element_text(color = "#6295bf"))+
        scale_y_continuous(limits = c(0, 15000),
                           breaks = seq(0,15000,500),
                           labels = c("$0", "$500", "$1000", "$1500", "$2000", 
                                      "$2500","$3000", "$3500","$4000", "$4500",
                                      "$5000", "$5500","$6000", "$6500","$7000",
                                      "$7500","$8000", "$8500","$9000", "$9500",
                                      "$10000", "$10500","$11000","$11500",
                                      "$12000", "$12500","$13000", "$13500",
                                      "$14000", "$14500","$15000")) +
        coord_flip() + 
        labs(y="", x="", title =paste("St. Olaf Study Abroad Program Cost Breakdown per Program by Geographical Region in USD"))+
        theme(plot.title = element_text(size=17))
    }
    
    else if (input$cost_category2 == "cost_comp"){
      costs_new %>%
        ggplot(mapping=aes(x = Location, y = new_cost_comp)) +
        geom_boxplot(outlier.color = "blue") +
        theme_classic() +
        theme(plot.title = element_text(size=10),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.title.y = element_text(color = "#6295bf"))+
        scale_y_continuous(limits = c(0, 15000),
                           breaks = seq(0,15000,500),
                           labels = c("$0", "$500", "$1000", "$1500", "$2000", 
                                      "$2500","$3000", "$3500","$4000", "$4500",
                                      "$5000", "$5500","$6000", "$6500","$7000",
                                      "$7500","$8000", "$8500","$9000", "$9500",
                                      "$10000", "$10500","$11000","$11500",
                                      "$12000", "$12500","$13000", "$13500",
                                      "$14000", "$14500","$15000")) +
        coord_flip() + 
        labs(y="", x="", title =paste("St. Olaf Study Abroad Cost Comparison per Program by Geographical Region in USD"))+
        theme(plot.title = element_text(size=17))
    }
    
    
  })
  
  set.seed(1000)
  output$plot5 <- renderLeaflet({
    program_map %>%
      group_by(Program, latitude, longitude) %>%
      summarize(n = n()) %>%
      mutate(radius= ifelse(n<=100, 3, ifelse(n<=200, 5, ifelse(n<=300, 7, ifelse(n<=500, 9, n)))))%>%
      leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,) %>%
      # setView(lng = mean(program_map$longitude), lat=mean(program_map$latitude), zoom=0.8) %>%
      addCircleMarkers(lat = ~ jitter(latitude, factor = 10), 
                       lng = ~ jitter(longitude, factor = 10),
                       popup = ~ Program, 
                       radius = ~ radius,
                       weight = 0.3,
                       fillOpacity = 0.7,
                       color = "#242121", 
                       fillColor = "red") %>%
      addMiniMap(
        toggleDisplay = TRUE,
        tiles = providers$Esri.NatGeoWorldMap
      )
    
  })
  
  output$plot6 <- renderPlot({
    SmithCenter %>%
      filter(Program==input$study_abroad) %>%
      group_by(Name) %>%
      summarize(n=n()) %>%
      slice_max(n, n=10) %>%
      ggplot(aes(x=fct_reorder(Name, n), y=n)) +
      geom_col( 
        fill="#a3d65c", 
        show.legend = FALSE)+
      coord_flip() +
      labs(y="", x="", title =paste(input$study_abroad, "-- Breakdown by Major and Concentration")) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(hjust=0, size=12),
            axis.ticks.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank()
      ) +
      geom_text(aes(label = n), hjust = -2)
    
    #axis.text.y=element_text(hjust=0)
    #element_text(size=15)
    
  })
  
  output$plot7 <- renderPlot({
    if (!input$type_check_p7) {
      SmithCenter %>%
        select(Type, Name) %>%
        group_by(Name, Type) %>%
        summarize(number=n()) %>%
        left_join(csv_plot_7, by=c("Name", "Type")) %>%
        rename(Total="n") %>%
        filter(number<Total) %>%
        group_by(Faculty) %>%
        summarise(Total=sum(Total),number=sum(number)) %>%
        mutate(percentage=as.integer(100*number/Total)) %>%
        ggplot(aes(area = percentage, fill = Faculty, label = paste(Faculty, "\n", percentage, "%"))) +
        geom_treemap() +
        geom_treemap_text(colour = "white",
                          place = "centre",
                          size = 15) +
        theme(legend.position = "none")
    }
    else{
      SmithCenter %>%
        select(Type, Name) %>%
        group_by(Name, Type) %>%
        summarize(number=n()) %>%
        left_join(csv_plot_7, by=c("Name", "Type")) %>%
        rename(Total="n") %>%
        filter(number<Total) %>%
        filter(Faculty==input$n_breaks_p7) %>%
        summarise(Total=sum(Total),number=sum(number)) %>%
        mutate(percentage=as.integer(100*number/Total)) %>%
        ggplot(aes(area = percentage, fill = Name, label = paste(Name, "\n", percentage, "%"))) +
        geom_treemap() +
        geom_treemap_text(colour = "white",
                          place = "centre",
                          size = 15) +
        theme(legend.position = "none")
    }
    
    
  })
  
  output$plot8 <- renderText({
    
  })
  
  
}  

shinyApp(ui = ui, server = server)

