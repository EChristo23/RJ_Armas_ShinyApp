#1.Libraries ---------------------------------------------------------------
library(shiny)
library(tidyverse)
library(data.table)
library(scales)
library(leaflet)
library(rgdal)
library(ggthemes)
library(shinythemes)
library(heatmaply)

#2.Data Import -------------------------------------------------------------
data <- fread("input.csv", header = TRUE)
shape <-readRDS("shape_rj.shp")


#3.UI -------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("journal"),
      width = 12,
      column(6,titlePanel("Análise do Número de Armas Apreendidas nos Municípios do RJ", windowTitle = "Armas RJ"),offset = 3),
      br(),
      fluidRow(
      column(5,
             leafletOutput("mymap"), offset = 1),
      column(5,
             plotOutput(outputId = "plot"), 
             offset = 0)),
      br(),
      fluidRow(
      column(8,
             plotOutput(outputId = "plot2"), 
             offset = 2))
)

#4. Server -------------------------------------------------------------
server <- function(input, output) {
  
  #Labels of polygons
  my_labels <-  
    reactive({
      sprintf(
        "<strong>%s</strong><br/>N<span>&#186;</span> de armas apreendidas: %s<br/> População: %s",
        as.character(shape@data %>% pull(Municipio)), 
        as.character(shape@data %>% pull(Armas)), 
        as.character(shape@data %>% pull(Populacao))
      ) %>% lapply(htmltools::HTML)})
  
  #Color palette
  paleta <- colorQuantile("RdYlGn", NULL, n = 7, reverse = TRUE)

  #Leaflet    
  output$mymap <- renderLeaflet({
    leaflet(shape)%>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      setView(lat = -22.3535, lng = -42.4496, zoom = 7)%>%
      addPolygons(
        group = "Populacao",
        layerId = ~Municipio,
        fillColor = ~paleta(shape@data$Populacao),
        weight = 0.4,
        opacity = 1,
        color = "grey",
        dashArray = "1",
        fillOpacity = 0.8,
        highlight = highlightOptions(
          weight = 3,
          opacity = 1,
          color = "white",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE),
          label = my_labels(),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      )%>%
      addPolygons(
        group = "Armas",
        layerId = ~OBJECTID,
        fillColor = ~paleta(shape@data$Armas),
        weight = 0.4,
        opacity = 1,
        color = "grey",
        dashArray = "1",
        fillOpacity = 0.8,
        highlight = highlightOptions(
          weight = 3,
          opacity = 1,
          color = "white",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE),
        label = my_labels(),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      )%>%
      addLayersControl(
        baseGroups = c("Armas", "Populacao"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observeEvent(input$mymap_groups,{
    mymap <- leafletProxy("mymap", data = shape@data)
    mymap %>% clearControls()
    if (input$mymap_groups == 'Armas') {
      mymap %>% addLegend(position="bottomright", 
                          colors = rev(RdYlGn(9)[-c(1,9)]),
                          labels = paste(
                            round(quantile(shape@data$Armas, seq(0,1,1/7)), 0),
                            shift(round(quantile(shape@data$Armas, seq(0,1,1/7)), 0), n = 1L, type = "lead"), 
                            sep = " - "
                          )[-length(quantile(shape@data$Armas, seq(0,1,1/7)))], 
                          title="N<span>&#186;</span> de armas",
                          opacity = 1)
    }
    else if (input$mymap_groups == 'Populacao') {
      mymap %>% addLegend(position="bottomright", 
                          colors = rev(RdYlGn(9)[-c(1,9)]),
                          labels = paste(
                            round(quantile(shape@data$Populacao, seq(0,1,1/7)), 0),
                            shift(round(quantile(shape@data$Populacao, seq(0,1,1/7)), 0), n = 1L, type = "lead"), 
                            sep = " - "
                          )[-length(quantile(shape@data$Populacao, seq(0,1,1/7)))],
                          title="População",
                          opacity = 1)
    }
  })
  
  # Make a barplot or scatterplot depending of the selected Municipality
  my_place <- NULL
  output$plot <- renderPlot({
    my_place <- input$mymap_shape_click
    if(is.null(my_place)){
      par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, paste("Selecione um Município"), cex = 1.6, col = "black")  
    }else{
      data %>%
        filter((!variavel %in% c('pop_munic', 'total')) & ((rank %in% my_place$id)|(Municipio %in% my_place$id)))%>%
        group_by(Arma = variavel)%>%
        summarise(Quantidade = sum(as.integer(count), na.rm =  TRUE))%>%
        ggplot(aes(x = Arma, y = Quantidade, fill = "#CC1616"))+ 
        geom_bar(stat = "identity")+
        xlab("")+
        ylab("Nº de Armas Apreendidas")+
        ggtitle("Tipo de Armas Apreendidas entre 2007 e 2018")+ 
        theme_hc()+
        scale_x_discrete(labels=c("Fabricação Caseira","Carabina","Espingarda","Fuzil","Garrucha", "Garruchão",
                                                  "Metralhadora", "Outros", "Pistola", "Revolver", "Submetralhadora"))+
        scale_fill_manual(values=rep("#CC1616",11))+ 
        geom_text(aes(Arma, Quantidade, label = Quantidade, fill = NULL), vjust=-0.25, color = "black")+
        theme(axis.text.x = 
                element_text(angle = 60, 
                             hjust = 1, 
                             face = "bold",
                             size = 13),
              legend.position = "none",
              plot.title = element_text(
                hjust = 0.5, 
                size = 16
                )
              )
    }
  })

  # Make a line plot of number of weapons aprehended through years
  my_place2 <- NULL
  output$plot2 <- renderPlot({
    my_place2 <- input$mymap_shape_click
    if(is.null(my_place2)){
      par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, paste("Selecione um Município"), cex = 1.6, col = "black")
    }else{
      data %>%
        filter((!variavel %in% c('pop_munic', 'total')) & rank %in% my_place2$id) %>%
        group_by(vano)%>%
        summarise(num = sum(as.numeric(count), na.rm =  TRUE))%>%
        ggplot(aes(x = vano, y = num)) +
        geom_line(alpha = 0.8)+
        ylab("Nº de Armas Apreendidas")+
        ggtitle("Nº de Armas Apreendidas por Ano")+
        geom_smooth(colour = "#CC1616", method = "loess")+
        theme_minimal()+
        scale_x_continuous(name="Ano", breaks = c(2007:2018))+
        theme(axis.text.x = 
                element_text(
                  angle = 0,
                  hjust = 0.5, 
                  face = "bold",
                  size = 12),
              plot.title = 
                element_text(
                  hjust = 0.5, 
                  size = 16
                  )
              )
    }
  })
}

#5. Create a Shiny app object -----------------------------------------------
shinyApp(ui = ui, server = server)
