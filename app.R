if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")


library(magrittr)
library(rvest)
url <- "https://www.nationsonline.org/oneworld/country_code_list.htm"
iso_codes <- url %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="CountryCode"]') %>%
    html_table()
iso_codes <- iso_codes[[1]][, -1]
iso_codes <- iso_codes[!apply(iso_codes, 1, function(x){all(x == x[1])}), ]
names(iso_codes) <- c("Country", "ISO2", "ISO3", "UN")
head(iso_codes)

library(maps)
library(mapproj)
library(tidyverse)
library(dplyr)
library(plotly)

countries <- read.csv('D:/E-Courses/Data Science with R/Task_for_batch_6/dataset/Country_vaccination_covid/country_vaccinations.csv')
countries1 <-countries%>%select(country,total_vaccinations,vaccines,iso_code)%>%group_by(country)%>%filter(!is.na(total_vaccinations))%>%filter(total_vaccinations==max(total_vaccinations))

library(maps)
library(ggplot2)
world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)
head(world_data)

old_names <- c("Bolivia (Plurinational State of)", "Cabo Verde", "China, Hong Kong Special Administrative Region",
               "China, Macao Special Administrative Region", "Congo", "Democratic People's Republic of Korea",
               "Democratic Republic of the Congo", "Iran (Islamic Republic of)", "Lao People's Democratic Republic",
               "Micronesia (Federated States of)", "Republic of Korea", "Republic of Moldova", "Saint Vincent and the Grenadines",
               "State of Palestine", "Syrian Arab Republic", "The former Yugoslav Republic of Macedonia",
               "United Kingdom of Great Britain and Northern Ireland", "United Republic of Tanzania",
               "United States Virgin Islands", "Venezuela (Bolivarian Republic of)")
new_names <- c("Bolivia", "Cape Verde", "Hong Kong, SAR China", "Macao, SAR China", "Congo (Brazzaville)",
               "Korea (North)", "Congo, (Kinshasa)", "Iran, Islamic Republic of", "Lao PDR", "Micronesia, Federated States of",
               "Korea (South)", "Moldova", "Saint Vincent and Grenadines", "Palestinian Territory", "Syrian Arab Republic (Syria)",
               "Macedonia, Republic of", "United Kingdom", "Tanzania, United Republic of", "Virgin Islands, US", "Venezuela (Bolivarian Republic)")

for (i in 1:length(old_names)){
    countries1$country[countries1$country == old_names[i]] <- new_names[i]
}

old_names <- c("French Southern and Antarctic Lands", "Antigua", "Barbuda", "Saint Barthelemy", "Brunei", "Ivory Coast",
               "Democratic Republic of the Congo", "Republic of Congo", "Falkland Islands", "Micronesia", "UK", 
               "Heard Island", "Cocos Islands", "Iran", "Nevis", "Saint Kitts", "South Korea", "Laos", "Saint Martin",
               "Macedonia", "Pitcairn Islands", "North Korea", "Palestine", "Russia", "South Sandwich Islands",
               "South Georgia", "Syria", "Trinidad", "Tobago", "Taiwan", "Tanzania", "USA", "Vatican", "Grenadines",
               "Saint Vincent", "Venezuela", "Vietnam", "Wallis and Fortuna")
new_names <- c("French Southern Territories", rep("Antigua and Barbuda", 2), "Saint-Barthélemy",
               "Brunei Darussalam", "Côte d'Ivoire", "Congo, (Kinshasa)", "Congo (Brazzaville)", 
               "Falkland Islands (Malvinas)", "Micronesia, Federated States of", "United Kingdom",
               "Heard and Mcdonald Islands", "Cocos (Keeling) Islands", "Iran, Islamic Republic of",
               rep("Saint Kitts and Nevis", 2), "Korea (South)", "Lao PDR", "Saint-Martin (French part)",
               "Macedonia, Republic of", "Pitcairn", "Korea (North)", "Palestinian Territory", "Russian Federation",
               rep("South Georgia and the South Sandwich Islands", 2), 
               "Syrian Arab Republic (Syria)", rep("Trinidad and Tobago", 2), "Taiwan, Republic of China",
               "Tanzania, United Republic of", "United States of America", "Holy See (Vatican City State)",
               rep("Saint Vincent and Grenadines", 2), "Venezuela (Bolivarian Republic)", "Viet Nam", "Wallis and Futuna Islands")

for (i in 1:length(old_names)){
    world_data$region[world_data$region == old_names[i]] <- new_names[i]
}


countries1['ISO3'] <- iso_codes$ISO3[match(countries1$country, iso_codes$Country)]
countries2 <-countries1%>%filter(!is.na(ISO3))
world_data["ISO3"] <- iso_codes$ISO3[match(world_data$region, iso_codes$Country)]
world_data1 <-world_data%>%filter(!is.na(ISO3))

library(reshape2)
countries2_melt <- melt(countries2, id = c("country", "ISO3"), 
                        variable.name = "Indicator", value.name = "Value")
countries2_melt$Value <- as.numeric(countries2_melt$Value)

worldMaps <- function(countries2_melt, world_data1, indicator){
    
    # Function for setting the aesthetics of the plot
    my_theme <- function () { 
        theme_bw() + theme(axis.title = element_blank(),
                           axis.text = element_blank(),
                           axis.ticks = element_blank(),
                           panel.grid.major = element_blank(), 
                           panel.grid.minor = element_blank(),
                           panel.background = element_blank(), 
                           legend.position = "bottom",
                           panel.border = element_blank(), 
                           strip.background = element_rect(fill = 'white', colour = 'white'))
    }
    
    # Select only the data that the user has selected to view
    plotdf <- countries2_melt[countries2_melt$Indicator == indicator ,]
    plotdf <- plotdf[!is.na(plotdf$ISO3), ]
    
    # Add the data the user wants to see to the geographical world data
    world_data1['Indicator'] <- rep(indicator, nrow(world_data1))
    world_data1['Value'] <- plotdf$Value[match(world_data1$ISO3, plotdf$ISO3)]
    
    # Specify the plot for the world map
    library(RColorBrewer)
    library(ggiraph)
    g <- ggplot() + 
        geom_polygon_interactive(data = subset(world_data1, lat >= -60 & lat <= 90), color = 'gray70', size = 0.1,
                                 aes(x = long, y = lat, fill = Value, group = group, 
                                     tooltip = sprintf("%s<br/>%s", ISO3, Value))) + 
        scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white') + 
        my_theme()
    
    return(g)
}

library(shiny)
library(ggiraph)

# Define the UI
ui = fluidPage(
    
    # App title
    titlePanel("Geographical distribution of the vaccines"),
    
    # Sidebar layout with input and output definitions
    sidebarLayout(
        
        # Sidebar panel for inputs 
        sidebarPanel(
            
            # First input: Type of data
            selectInput(inputId = "data_type",
                        label = "Choose the type of data",
                        
                        # Third input (choices depend on the choice for the first and second input)
                        uiOutput("secondSelection")
                        
            ),
            
            # Main panel for displaying outputs
            mainPanel(
                
                # Hide errors
                tags$style(type = "text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"),
                
                # Output: interactive world map
                girafeOutput("distPlot")
                
            )
        )
    )
)

# Define the server
server = function(input, output) {
    
    # Create the interactive world map
    output$distPlot <- renderGirafe({
        ggiraph(code = print(worldMaps(countries2_melt, world_data1, input$data_type, input$indicator)))
    })
    
    # Change the choices for the third selection on the basis of the input to the first and second selections
    output$secondSelection <- renderUI({
        lab <- ifelse(input$data_type == "Childlessness", "age group", "indicator")
        choice_third <- as.list(unique(countries2_melt$Indicator[countries2_melt$DataType == input$data_type ]))
        selectInput(inputId = "indicator", choices = choice_third,
                    label = paste0("Choose the type of ", lab, " you want to explore:"))
    })
}

# Finally, we can run our app by either clicking "Run App" in the top of our RStudio IDE, or by running
shinyApp(ui = ui, server = server)
