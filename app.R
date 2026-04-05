# 1. БИБЛИОТЕКИ
library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(geosphere)
library(DT)

# 2. ДАННЫЕ (замените на ваш путь)
route_counts <- read.csv("route_counts.csv")

# 3. UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .title-panel {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 20px;
        border-radius: 10px;
        margin-bottom: 20px;
      }
      .info-box {
        background: #f8f9fa;
        padding: 15px;
        border-radius: 8px;
        border-left: 4px solid #667eea;
        margin-bottom: 15px;
      }
    "))
  ),
  
  div(class = "title-panel",
      h1("Немецкие почтовые открытки", align = "center"),
      p("Визуализация обмена открытками между странами", align = "center")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      div(class = "info-box",
          h4("📅 Фильтр по времени"),
          sliderInput("decade_range", 
                      "Выберите диапазон десятилетий:",
                      min = 1890,
                      max = 2020,
                      value = c(1890, 2020),
                      step = 10,
                      sep = "",
                      animate = animationOptions(interval = 1500))
      ),
      
      div(class = "info-box",
          h4("🎯 Фильтр по странам"),
          selectInput("countries", 
                      "Выберите страну отправления:",
                      choices = c("Все", unique(route_counts$country_from)),
                      selected = "Все")
      ),
      
      div(class = "info-box",
          h4("📊 Настройки отображения"),
          sliderInput("line_width", 
                      "Толщина линий:",
                      min = 1, max = 15, value = 8, step = 1),
          checkboxInput("show_arcs", "Показывать дуги (изогнутые линии)", value = TRUE),
          br(),
          # НОВАЯ КНОПКА ДЛЯ ПУЗЫРЕЙ
          materialSwitch("show_markers", "Показывать маркеры стран", 
                         value = TRUE, status = "primary")
      ),
      
      div(class = "info-box",
          h4("ℹ️ Статистика"),
          textOutput("total_routes"),
          textOutput("total_postcards"),
          textOutput("decade_info")
      )
    ),
    
    mainPanel(
      width = 9,
      leafletOutput("map", height = "650px"),
      br(),
      fluidRow(
        column(6,
               div(class = "info-box",
                   h4("📈 Топ маршрутов между странами"),
                   DTOutput("top_routes")
               )
        ),
        column(6,
               div(class = "info-box",
                   h4("📊 Распределение по десятилетиям"),
                   plotOutput("decade_plot", height = "250px")
               )
        )
      )
    )
  )
)

# 4. SERVER
server <- function(input, output, session) {
  
  # Реактивные данные
  filtered_data <- reactive({
    data <- route_counts
    
    # Фильтр по десятилетиям
    data <- data %>%
      filter(decade >= input$decade_range[1],
             decade <= input$decade_range[2])
    
    # Фильтр по странам
    if(input$countries != "Все") {
      data <- data %>% filter(country_from == input$countries)
    }
    
    return(data)
  })
  
  # Статистика
  output$total_routes <- renderText({
    paste("📌 Маршрутов между странами:", nrow(filtered_data()))
  })
  
  output$total_postcards <- renderText({
    paste("📬 Всего открыток:", sum(filtered_data()$count))
  })
  
  output$decade_info <- renderText({
    paste("📅 Период:", input$decade_range[1], "-", input$decade_range[2])
  })
  
  # Создание карты
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 15, lat = 50, zoom = 4) %>%
      addScaleBar(position = "bottomleft") %>%
      addControl(position = "topright", 
                 html = "<div style='background: white; padding: 5px 10px; border-radius: 5px; font-size: 12px;'>
                         <strong>📬 Толщина линии = количество открыток</strong><br>
                         🟡 1 | 🟠 2 | 🟠🟠 3 | 🔴 4+
                         </div>")
  })
  
  # Обновление карты
  observe({
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      leafletProxy("map") %>% clearShapes()
      return()
    }
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers()  # очищаем маркеры
    
    # Добавляем линии маршрутов между странами
    for(i in 1:nrow(data)) {
      route <- data[i, ]
      
      # Выбираем тип линии (прямая или дуга)
      if(input$show_arcs) {
        # Создаем дугу между точками
        line_coords <- tryCatch({
          gcIntermediate(
            c(route$lon_from, route$lat_from),
            c(route$lon_to, route$lat_to),
            n = 50,
            addStartEnd = TRUE,
            breakAtDateLine = FALSE
          )
        }, error = function(e) {
          # Если дуга не строится, рисуем прямую
          rbind(
            c(route$lon_from, route$lat_from),
            c(route$lon_to, route$lat_to)
          )
        })
      } else {
        # Прямая линия
        line_coords <- rbind(
          c(route$lon_from, route$lat_from),
          c(route$lon_to, route$lat_to)
        )
      }
      
      # Добавляем линию
      leafletProxy("map") %>%
        addPolylines(
          lng = line_coords[,1],
          lat = line_coords[,2],
          weight = route$line_weight * (input$line_width / 8),
          color = route$color,
          opacity = 0.7,
          label = paste(
            "📬", route$count, "открыток\n",
            "🗺️", route$country_from, "→", route$country_to, "\n",
            "📅", route$years
          ),
          group = "routes"
        )
    }
    
    # ДОБАВЛЯЕМ МАРКЕРЫ ТОЛЬКО ЕСЛИ ВКЛЮЧЕНА НАСТРОЙКА
    if(input$show_markers) {
      # Добавляем маркеры стран (центры стран)
      countries_points <- data %>%
        select(country = country_from, lon = lon_from, lat = lat_from, count) %>%
        bind_rows(
          data %>%
            select(country = country_to, lon = lon_to, lat = lat_to, count)
        ) %>%
        group_by(country, lon, lat) %>%
        summarise(
          total_postcards = sum(count),
          .groups = 'drop'
        )
      
      leafletProxy("map") %>%
        addCircleMarkers(
          data = countries_points,
          lng = ~lon,
          lat = ~lat,
          radius = ~sqrt(total_postcards) * 3,
          color = "#4A90E2",
          fillOpacity = 0.6,
          label = ~paste(
            "🗺️", country, "\n",
            "📬", total_postcards, "открыток"
          ),
          group = "countries"
        )
    }
  })
  
  # Топ маршрутов
  output$top_routes <- renderDT({
    filtered_data() %>%
      arrange(desc(count)) %>%
      head(10) %>%
      select(decade, country_from, country_to, count) %>%
      rename(
        `Десятилетие` = decade,
        `Из страны` = country_from,
        `В страну` = country_to,
        `Кол-во открыток` = count
      ) %>%
      datatable(options = list(
        pageLength = 5,
        dom = 'Bfrtip'
      ))
  })
  
  # График распределения по десятилетиям
  output$decade_plot <- renderPlot({
    filtered_data() %>%
      group_by(decade) %>%
      summarise(total = sum(count)) %>%
      ggplot(aes(x = decade, y = total)) +
      geom_bar(stat = "identity", fill = "#667eea", alpha = 0.7) +
      geom_text(aes(label = total), vjust = -0.5) +
      scale_x_continuous(breaks = seq(1890, 2020, 10)) +
      theme_minimal() +
      labs(x = "Десятилетие", y = "Количество открыток") +
      theme(plot.title = element_text(hjust = 0.5))
  })
}

# 5. ЗАПУСК
shinyApp(ui = ui, server = server)