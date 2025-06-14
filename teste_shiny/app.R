library(shiny)
library(tidyverse)
library(lubridate)
library(geobr)
library(sf)
library(tmap)
library(BlandAltmanLeh)
library(readxl)
library(irr)
library(tseries)

# ===== Carregamento dos dados =====
dados <- read.csv("PM2.5_diario_2023.csv")
dados$Data <- ymd(dados$Date)
dados$mes <- month(dados$Data)
dados$ano <- year(dados$Data)
dados$wday <- weekdays(dados$Data)
dados$UF <- substr(dados$Cod,1,2)
rs <- subset(dados, UF == 43)

rsmes <- rs %>%
  group_by(Cod, mes) %>%
  summarise(pm2.5 = mean(PM2.5, na.rm = TRUE), .groups = "drop")

rsano <- rsmes %>%
  group_by(Cod) %>%
  summarise(pm2.5 = mean(pm2.5, na.rm = TRUE), .groups = "drop")

rsUF <- rsmes %>%
  group_by(mes) %>%
  summarise(pm2.5 = mean(pm2.5, na.rm = TRUE), .groups = "drop")

rs.donkelar <- read_excel("dados_completos_consolidado_donkelar.xlsx") %>%
  filter(SIGLA_UF == 43)

rs.donk.reduzido <- rs.donkelar %>%
  select(Mes, Media_PM25) %>%
  rename(mes = Mes, pm2.5 = Media_PM25) %>%
  mutate(fonte = "Von Donkelar")

rsano$fonte <- "CAMS"
rs.donk.ano <- rs.donkelar %>%
  select(CD_MUN, Mes, Media_PM25) %>%
  mutate(fonte = "Von Donkelar")

rsmes$fonte <- "CAMS"

rs.agregado <- bind_rows(rsmes, rs.donk.reduzido)
rs.agregado.2 <- left_join(rsano, rs.donk.ano, by = c("Cod" = "CD_MUN"))

# ========== UI ==========
ui <- fluidPage(
  titlePanel("Concentração Média de PM2,5 no RS - CAMS vs Von Donkelar"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Análises comparativas de PM2.5"),
      checkboxInput("viewMap", "Mostrar mapas interativos", value = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Normalidade", 
                 verbatimTextOutput("jarque"),
                 plotOutput("hist"),
                 plotOutput("qq")
        ),
        tabPanel("Dispersão",
                 plotOutput("dispersao")
        ),
        tabPanel("Boxplots",
                 plotOutput("box_cams"),
                 plotOutput("box_donk"),
                 plotOutput("box_ambos")
        ),
        tabPanel("Série Temporal",
                 plotOutput("serie_temporal")
        ),
        tabPanel("Bland-Altman",
                 plotOutput("bland1"),
                 plotOutput("bland2"),
                 verbatimTextOutput("teste_pareado"),
                 verbatimTextOutput("icc_res")
        ),
        tabPanel("Mapas",
                 conditionalPanel("input.viewMap == false", plotOutput("mapa_cams"), plotOutput("mapa_donk")),
                 conditionalPanel("input.viewMap == true", tmapOutput("mapa_interativo"))
        )
      )
    )
  )
)

# ========== Server ==========
server <- function(input, output, session) {
  
  output$jarque <- renderPrint({
    jarque.bera.test(rsmes$pm2.5)
  })
  
  output$hist <- renderPlot({
    hist(rsmes$pm2.5, breaks = 30, main = "Histograma", xlab = "PM2.5")
  })
  
  output$qq <- renderPlot({
    qqnorm(rsmes$pm2.5)
    qqline(rsmes$pm2.5, col = "red")
  })
  
  output$dispersao <- renderPlot({
    ggplot(rs.agregado.2, aes(x = pm2.5, y = Media_PM25)) +
      geom_point(color = "#1f77b4", size = 3, alpha = 0.8) +
      geom_smooth(method = "lm", se = FALSE, color = "darkred") +
      labs(title = "Comparação de PM2.5: CAMS vs. Von Donkelar",
           x = "PM2.5 - CAMS (µg/m³)",
           y = "PM2.5 - Von Donkelar (µg/m³)") +
      theme_minimal()
  })
  
  output$box_cams <- renderPlot({
    ggplot(rsmes, aes(x = factor(mes), y = pm2.5)) +
      geom_boxplot() +
      labs(title = "Boxplot CAMS")
  })
  
  output$box_donk <- renderPlot({
    ggplot(rs.donk.reduzido, aes(x = factor(mes), y = pm2.5)) +
      geom_boxplot() +
      labs(title = "Boxplot Von Donkelar")
  })
  
  output$box_ambos <- renderPlot({
    ggplot(rs.agregado, aes(x = factor(mes), y = pm2.5, fill = fonte)) +
      geom_boxplot(position = position_dodge(0.8)) +
      scale_fill_manual(values = c("CAMS" = "#1f77b4", "Von Donkelar" = "#ff7f0e")) +
      labs(title = "Boxplot por mês - comparação entre Satélites")
  })
  
  output$serie_temporal <- renderPlot({
    rs.plot <- rs.agregado %>%
      group_by(fonte, mes) %>%
      summarise(media = mean(pm2.5), .groups = "drop")
    
    media.geral <- rs.plot %>%
      group_by(mes) %>%
      summarise(media = mean(media), .groups = "drop") %>%
      mutate(fonte = "Média Geral")
    
    rs.plot.total <- bind_rows(rs.plot, media.geral)
    
    ggplot(rs.plot.total, aes(x = mes, y = media, color = fonte)) +
      geom_line(data = rs.plot, size = 1.2) +
      geom_smooth(data = rs.plot, se = FALSE) +
      geom_smooth(data = media.geral, se = TRUE, size = 1.2, alpha = 0.15, linetype = 8) +
      geom_point(data = rs.plot, size = 2) +
      scale_color_manual(values = c("CAMS" = "red", "Von Donkelar" = "deepskyblue3", "Média Geral" = "green")) +
      scale_x_continuous(breaks = 1:12) +
      labs(title = "Série Temporal de PM2.5 por Fonte", x = "Mês", y = "PM2.5 médio (µg/m³)") +
      theme_minimal()
  })
  
  output$bland1 <- renderPlot({
    bland.altman.plot(rs.agregado.2$pm2.5, rs.agregado.2$Media_PM25,
                      main = "Bland-Altman (Todos os meses)", xlab = "Média", ylab = "Diferença")
  })
  
  output$bland2 <- renderPlot({
    rs.agregado.2.mensal <- rs.agregado.2 %>%
      group_by(Cod) %>%
      summarise(mensal.cams = mean(pm2.5),
                mensal.VonDonk = mean(Media_PM25), .groups = "drop")
    
    bland.altman.plot(rs.agregado.2.mensal$mensal.cams, rs.agregado.2.mensal$mensal.VonDonk,
                      main = "Bland-Altman (Média por município)", xlab = "Média", ylab = "Diferença")
  })
  
  output$teste_pareado <- renderPrint({
    rs.agregado.2.mensal <- rs.agregado.2 %>%
      group_by(Cod) %>%
      summarise(mensal.cams = mean(pm2.5),
                mensal.VonDonk = mean(Media_PM25), .groups = "drop")
    
    t.test(rs.agregado.2.mensal$mensal.cams, rs.agregado.2.mensal$mensal.VonDonk,
           paired = TRUE, var.equal = FALSE)
  })
  
  output$icc_res <- renderPrint({
    rs.agregado.2.mensal <- rs.agregado.2 %>%
      group_by(Cod) %>%
      summarise(mensal.cams = mean(pm2.5),
                mensal.VonDonk = mean(Media_PM25), .groups = "drop")
    
    icc(data.frame(rs.agregado.2.mensal$mensal.cams, rs.agregado.2.mensal$mensal.VonDonk),
        model = "twoway", type = "agreement")
  })
  
  # ===== Mapas corrigidos =====
  
  output$mapa_cams <- renderPlot({
    map <- read_sf("RS_Municipios_2024.shp")
    rsano$Cod <- as.character(rsano$Cod)
    map <- left_join(map, rsano, by = c("CD_MUN" = "Cod"))
    map <- st_make_valid(map)
    
    tm_shape(map) +
      tm_polygons(
        fill = "pm2.5",
        fill.scale = tm_scale(values = "Greens"),
        fill.legend = tm_legend(title = "PM2.5 CAMS")
      ) +
      tm_borders()
  })
  
  output$mapa_donk <- renderPlot({
    map <- read_sf("RS_Municipios_2024.shp")
    rs.donk.ano <- rs.donkelar %>%
      group_by(CD_MUN) %>%
      summarise(Media_PM25 = mean(Media_PM25, na.rm = TRUE), .groups = "drop")
    rs.donk.ano$CD_MUN <- as.character(rs.donk.ano$CD_MUN)
    
    map <- left_join(map, rs.donk.ano, by = "CD_MUN")
    map <- st_make_valid(map)
    
    tm_shape(map) +
      tm_polygons(
        fill = "Media_PM25",
        fill.scale = tm_scale(values = "Oranges"),
        fill.legend = tm_legend(title = "PM2.5 Von Donkelar")
      ) +
      tm_borders()
  })
  
  output$mapa_interativo <- renderTmap({
    tmap_mode("view")
    map <- read_sf("RS_Municipios_2024.shp")
    rsano$Cod <- as.character(rsano$Cod)
    map <- left_join(map, rsano, by = c("CD_MUN" = "Cod"))
    map <- st_make_valid(map)
    
    tm_shape(map) +
      tm_polygons(
        fill = "pm2.5",
        fill.scale = tm_scale(values = "Greens"),
        fill.legend = tm_legend(title = "PM2.5 CAMS - Interativo")
      ) +
      tm_borders()
  })
  
}

shinyApp(ui, server)
