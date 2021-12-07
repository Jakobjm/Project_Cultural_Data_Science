library(shiny)
library(pacman)
p_load(tidyverse,
       jsonlite,
       sf,
       raster,
       leaflet,
       shiny,
       leaflet.extras)


      #Defining my functions
#Function  to select a subset of data based on year
get_year_df <- function(df,year){
  temp_df <- df %>% 
    filter(byg026Year %in% year)
  return(temp_df)
}

#A function that adds a new column to the merged districts with counts of new buildings in each districts in that/those year(s) given a range of years, from the intersections data frame and returns the new dataframe
n_new_buildings <- function(year){
  n_new <- rep(0,length(merged_districts$navn))
  intersections_year <- get_year_df(intersections_merged, year)
  for (i in 1:length(merged_districts$navn)){
    n_new[i] <- nrow(filter(intersections_year, navn == merged_districts$navn[i]))
  }
  
  temp <- cbind(merged_districts, n_new)
  return(temp)
}

#function to plot histograms
plot_hist <- function(Year, df){
  temp_df_histogram <- 
    get_year_df(df, Year) %>%
    dplyr::select(byg026Year, byg054AntalEtager) %>% 
    group_by(byg026Year) %>% 
    summarise_at(vars(byg054AntalEtager),funs(mean(., na.rm =TRUE)))
  
  
  ggplot(temp_df_histogram,aes(x = byg026Year, y= byg054AntalEtager))+
    geom_col(fill = "#76EE00", color = "black")+
    labs(x= "Years", y= "Mean heigth in floors")+
    ggtitle(paste(df$city[1]))+
    theme_minimal()
}




#function to flatten it to a data frame and changing the name of a column to get rid of danish letters
flatten <- function(json){
  json <- as.data.frame(json[["BygningList"]])
  names(json)[names(json)=="byg026Opførelsesår"] <- "byg026Year"
  return(json)
}


#function to filter for buildings used for recreational,educational,habitation,health, light- and service industry out discarding other buildings, refer to BBRkodelister20210322 for the complete list of codes

clean_bbr_df <-function(df, year_cutoff){
  df %>% 
    filter(!is.na(byg404Koordinat)) %>% 
    filter(byg404Koordinat != "POINT(0 0)") %>% 
    filter(byg406Koordinatsystem == "5") %>% 
    filter(!is.na(byg026Year)) %>% 
    filter(byg021BygningensAnvendelse %in% 
             c("101","120","121","122","131","132","140","150","160","190","321","322","324","331","332","333","334","339","421","422","429","431","432","433","439","441","532","533")) %>% 
    distinct(id_lokalId,.keep_all = TRUE) %>% 
    filter(byg026Year >= year_cutoff)
}





#Loading and using functions to preprocess BBR data

bbr_cph_data <- fromJSON("https://sciencedata.dk/public/67e2ad2ca642562dacfa6fdf672a1009/clean_BBR_cph_data.json")
bbr_aarhus_data <- fromJSON("https://sciencedata.dk/public/67e2ad2ca642562dacfa6fdf672a1009/clean_BBR_aarhus_data.json")



bbr_cph_data_flat <- bbr_cph_data %>%
  flatten() %>% 
  clean_bbr_df(year_cutoff = 1900) %>% 
  mutate(city = "Copenhagen")%>% 
  st_as_sf(wkt = "byg404Koordinat", crs = 25832)

bbr_aarhus_data_flat <-bbr_aarhus_data %>%
  flatten() %>% 
  clean_bbr_df(1900) %>% 
  mutate(city = "Aarhus")%>% 
  st_as_sf(wkt = "byg404Koordinat", crs = 25832)



merged_bbr_data_flat <- rbind(bbr_cph_data %>%
                                flatten() %>% 
                                clean_bbr_df(year_cutoff = 1900) %>% 
                                mutate(city = "Copenhagen"), bbr_aarhus_data %>%
                                flatten() %>% 
                                clean_bbr_df(1900) %>% 
                                mutate(city = "Aarhus")) %>% 
  st_as_sf(wkt = "byg404Koordinat", crs = 25832)

#loading DIstrict geometry
cph_districts <- st_read("data/cph_districts.geojson")
aarhus_districts <- st_read("data/aarhus_districts.geojson")

cph_districts
aarhus_districts
merged_districts <- rbind(aarhus_districts %>% 
                            dplyr::select(navn = prog_distrikt_navn,geometry) %>% 
                            mutate(city = "Aarhus"),
                          cph_districts %>% 
                            dplyr::select(navn,geometry) %>% 
                            mutate(city = "Copenhagen")
)

#Creating Intersections of BBR data and the District Geometry
intersections_aarhus <- st_intersection(aarhus_districts,bbr_aarhus_data_flat) %>% 
  dplyr::select(id = id_lokalId, navn = prog_distrikt_navn,byg021BygningensAnvendelse,byg026Year,byg054AntalEtager,byg406Koordinatsystem,city,geometry)
intersections_cph <-  st_intersection(cph_districts,bbr_cph_data_flat) %>% 
  dplyr::select(id = id_lokalId,navn, byg021BygningensAnvendelse,byg026Year,byg054AntalEtager,byg406Koordinatsystem,city,geometry)

intersections_merged <- rbind(intersections_aarhus, intersections_cph)

draw_choropleth <- function(year){
  temp_df_choropleth <- n_new_buildings(year = year)
  
  
  #creating a color palette
  pal <- colorNumeric("Greens", domain = NULL)
  
  #creating labels for the choropleth map
  labels <- sprintf(
    "<strong>%s</strong><br/>%g Nye bygninger</sup>",
    temp_df_choropleth$navn, temp_df_choropleth$n_new
  ) %>% lapply(htmltools::HTML)
  
  
  #drawing the choropleth map on top of OpenStreetMaps base map
  choro <- leaflet() %>% 
    addProviderTiles("CartoDB.Positron") %>% 
    addPolygons(data = st_transform(temp_df_choropleth,4326),
                fillColor = pal(temp_df_choropleth$n_new),
                weight = 1,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlightOptions = highlightOptions(
                  weight = 5,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>% 
    addLegend(
      data = temp_df_choropleth,
      position = "bottomright",
      pal = pal,
      values = ~ n_new,
      title = "Antal nye bygninger",
      opacity = 0.5)
  
  
  return(choro)
}




#RShiny implementation 

ui <- fluidPage(
  
  # App title ----
  titlePanel("Visualizing Urban Development in Aarhus and Copenhagen Municipalities"),
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "Year_range",
                  label = "Select a range of years to plot",
                  min = 1900,
                  max = 2021,
                  value = c(1900,2021)
      
    ),
    plotOutput("Histogram1"),
    plotOutput("Histogram2")),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      leafletOutput("Map")
      
    )
    
  )





server <- function(input, output) {
  output$Histogram1 <- renderPlot(plot_hist(c(input$Year_range[1]:input$Year_range[2]), intersections_aarhus))
  output$Histogram2 <- renderPlot(plot_hist(c(input$Year_range[1]:input$Year_range[2]), intersections_cph))
  output$Map <- renderLeaflet(draw_choropleth(c(input$Year_range[1]:input$Year_range[2])))
}

shinyApp(ui = ui, server = server)