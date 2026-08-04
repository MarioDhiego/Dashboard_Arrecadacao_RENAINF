# ==============================================================================
# DASHBOARD EXECUTIVO - OBSERVATÓRIO RENAINF (DETRAN-PA)
# SCRIPT COMPLETO (KPIs NATIVOS COM FONTE AMPLIADA E AJUSTADA)
# ==============================================================================

library(shiny)
library(bs4Dash)
library(dplyr)
library(readxl)
library(stringr)
library(scales)
library(echarts4r)

# ==============================================================================
# 1. PROCESSAMENTO DE DADOS (Executado na inicialização)
# ==============================================================================

df_renainf <- read_excel("Banco_RENAINF.xlsx")
linhas_txt <- readLines("orgaos_autuadores.txt", warn = FALSE) 

df_orgaos <- data.frame(texto_bruto = linhas_txt, stringsAsFactors = FALSE) %>%
  filter(str_detect(texto_bruto, "^\\d{6}")) %>%
  mutate(
    Codigo_Orgao = as.numeric(str_extract(texto_bruto, "^\\d{6}")),
    Nome_Orgao = str_trim(str_remove(texto_bruto, "^\\d{6}\\s*-\\s*"))
  ) %>%
  select(Codigo_Orgao, Nome_Orgao) %>%
  distinct(Codigo_Orgao, .keep_all = TRUE)

df_completo <- df_renainf %>%
  left_join(df_orgaos, by = c("Órgão Autuador" = "Codigo_Orgao")) %>%
  mutate(
    Nome_Orgao = ifelse(is.na(Nome_Orgao), paste("CÓDIGO NÃO MAPEADO:", `Órgão Autuador`), Nome_Orgao)
  )

lista_anos <- sort(unique(df_completo$Ano), decreasing = TRUE)

# Foi criado um dicionário para linkar o orgão autuador com a UF correspondente, permitindo o filtro correto no dashboard.
codigos_uf_orgao <- c(
  AC = 101100,
  AL = 102100,
  AM = 103100,
  AP = 104100,
  BA = 105100,
  CE = 106100,
  DF = 107100,
  ES = 108100,
  GO = 109100,
  MA = 110100,
  MT = 111100,
  MS = 112100,
  MG = 113100,
  PA = 114100,
  PB = 115100,
  PR = 116100,
  PE = 117100,
  PI = 118100,
  RJ = 119100,
  RN = 120100,
  RS = 121100,
  RO = 122100,
  RR = 123100,
  SC = 125100,
  SP = 126100,
  SE = 127100,
  TO = 128100
)
lista_ufs <- names(codigos_uf_orgao)
lista_orgaos <- sort(unique(df_completo$Nome_Orgao))
meses_pt <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")

# ==============================================================================
# 2. INTERFACE DO USUÁRIO (UI)
# ==============================================================================

ui <- dashboardPage(
  
  header = dashboardHeader(
    title = dashboardBrand(
      title = "ARRECADAÇÃO - RENAINF",
      color = "primary",
      href = "#"
    ),
    skin = "light",
    status = "white"
  ),
  
  # --- MENU LATERAL (COM ABAS E FILTROS) ---
  sidebar = dashboardSidebar(
    skin = "darkblue",
    elevation = 3,
    
    sidebarMenu(
      id = "menu_principal",
      menuItem("Análise Temporal", tabName = "aba_temporal", icon = icon("calendar-alt")),
      menuItem("Top Órgãos", tabName = "aba_orgaos", icon = icon("trophy")),
      menuItem("Status das Contas", tabName = "aba_status", icon = icon("align-left"))
    ),
    
    hr(style = "border-top: 1px solid #4f5962; margin: 20px 0;"),
    
    div(style = "padding: 0 15px;",
        tags$h6(icon("filter"), " FILTROS GLOBAIS", style = "color: #c2c7d0; font-weight: bold; margin-bottom: 15px; letter-spacing: 1px;"),
        
        selectInput("filtro_ano", "Ano de Referência:", choices = c("Todos", as.character(lista_anos)), selected = "Todos"),
        selectInput("filtro_uf", "UF do Órgão Autuador:", choices = c("Todas", lista_ufs), selected = "Todas"),
        selectizeInput("filtro_orgao", "Órgão Autuador:", choices = c("Todos", lista_orgaos), selected = "Todos")
    )
  ),
  
  body = dashboardBody(
    
    # CSS focado na ampliação das fontes dos valueBoxes originais
    tags$head(tags$style(HTML("
      .small-box h3 { 
        font-size: 32px !important; 
        font-weight: 800 !important; 
        margin-bottom: 4px !important; 
      } 
      .small-box p { 
        font-size: 15px !important; 
        font-weight: 600 !important; 
        letter-spacing: 0.5px;
      }
      .small-box .icon {
        font-size: 60px !important;
      }
    "))),
    
    # ==========================================================================
    # CABEÇALHO GLOBAL (TRÍADE DE KPIs COM DIVISÃO 3 PALETAS: width = 4)
    # ==========================================================================
    fluidRow(
      valueBoxOutput("kpi_receber", width = 4),
      valueBoxOutput("kpi_pagar", width = 4),
      valueBoxOutput("kpi_infracoes", width = 4)
    ),
    
    # ==========================================================================
    # CONTEÚDO DAS ABAS 
    # ==========================================================================
    tabItems(
      
      # --- ABA 1: ANÁLISE TEMPORAL ---
      tabItem(
        tabName = "aba_temporal",
        fluidRow(
          box(
            title = textOutput("titulo_mes", inline = TRUE),
            width = 12,
            status = "primary",
            solidHeader = FALSE,
            maximizable = TRUE,
            echarts4rOutput("grafico_mes", height = "380px"),
            uiOutput("insight_mes")
          )
        ),
        fluidRow(
          box(
            title = textOutput("titulo_ano", inline = TRUE),
            width = 12,
            status = "primary",
            solidHeader = FALSE,
            maximizable = TRUE,
            echarts4rOutput("grafico_ano", height = "380px"),
            uiOutput("insight_ano")
          )
        )
      ),
      
      # --- ABA 2: RANKING DE ÓRGÃOS (TOP 15) ---
      tabItem(
        tabName = "aba_orgaos",
        fluidRow(
          box(
            title = fluidRow(
              column(8, span(icon("trophy"), " Top 15 Órgãos Autuadores (Volume Financeiro Geral)")),
              column(4, checkboxInput("ocultar_detran", "Ocultar DETRAN-PA da visão", value = FALSE))
            ),
            width = 12,
            status = "primary",
            solidHeader = FALSE,
            maximizable = TRUE,
            echarts4rOutput("grafico_orgaos", height = "550px"), 
            uiOutput("insight_orgaos")
          )
        )
      ),
      
      # --- ABA 3: STATUS DAS CONTAS ---
      tabItem(
        tabName = "aba_status",
        fluidRow(
          box(
            title = span(icon("align-left"), " Composição do Status das Contas"),
            width = 12,
            status = "primary",
            solidHeader = FALSE,
            maximizable = TRUE,
            echarts4rOutput("grafico_status", height = "450px"),
            uiOutput("insight_status")
          )
        )
      )
      
    ) # Fim do tabItems
  ) # Fim do dashboardBody
)

# ==============================================================================
# 3. SERVIDOR (Lógica e Reatividade)
# ==============================================================================

server <- function(input, output, session) {
  
  dados_filtrados <- reactive({
    df <- df_completo
    if (input$filtro_ano != "Todos") {
      df <- df %>% filter(Ano == as.numeric(input$filtro_ano))
    }
    #if (input$filtro_uf != "Todas") {
    #  df <- df %>% filter(UF == input$filtro_uf)
    #}
    if (input$filtro_uf != "Todas") {
      codigo_orgao <- codigos_uf_orgao[[input$filtro_uf]]
      if (!is.null(codigo_orgao)) {
        df <- df %>%
          filter(`Órgão Autuador` == codigo_orgao)
      }
    }
    if (input$filtro_orgao != "Todos") {
      df <- df %>% filter(Nome_Orgao == input$filtro_orgao)
    }
    return(df)
  })
  
  formata_moeda <- label_number(prefix = "R$ ", big.mark = ".", decimal.mark = ",", accuracy = 0.01)
  formata_inteiro <- label_number(big.mark = ".", decimal.mark = ",")
  
  # --- RENDERIZAÇÃO DOS KPIs NATIVOS NÍTIDOS E GRANDES ---
  output$kpi_receber <- renderValueBox({ 
    valueBox(
      value = formata_moeda(sum(dados_filtrados()$`Vl.Repasse(R$)`[dados_filtrados()$`Tipo de Contas` == "A Receber"], na.rm = TRUE)), 
      subtitle = "Previsão (A Receber)", 
      color = "success", 
      icon = icon("arrow-up-right-dots")
    ) 
  })
  
  output$kpi_pagar <- renderValueBox({ 
    valueBox(
      value = formata_moeda(sum(dados_filtrados()$`Vl.Repasse(R$)`[dados_filtrados()$`Tipo de Contas` == "A Pagar"], na.rm = TRUE)), 
      subtitle = "Previsão (A Pagar)", 
      color = "danger", 
      icon = icon("arrow-down-left-dots")
    ) 
  })
  
  output$kpi_infracoes <- renderValueBox({ 
    valueBox(
      value = formata_inteiro(sum(dados_filtrados()$Qt.Infrações, na.rm = TRUE)), 
      subtitle = "Total de Autuações", 
      color = "warning", 
      icon = icon("car-burst")
    ) 
  })
  
  # --------------------------------------------------------------------------
  # ABA 1 - GRÁFICO 1: SAZONALIDADE MENSAL
  # --------------------------------------------------------------------------
  output$titulo_mes <- renderText({ paste0("📊 Sazonalidade: Mensal (R$)") })
  
  dados_mes_reativo <- reactive({
    dados_filtrados() %>% filter(!is.na(Mes)) %>% group_by(Mes) %>% summarise(valor = sum(`Vl.Repasse(R$)`, na.rm = TRUE), .groups = "drop") %>% arrange(Mes) %>% mutate(periodo = meses_pt[as.integer(Mes)])
  })
  
  output$grafico_mes <- renderEcharts4r({
    dados <- dados_mes_reativo()
    req(nrow(dados) >= 1) 
    media_val <- mean(dados$valor, na.rm = TRUE)
    
    dados %>% 
      e_charts(periodo) %>% 
      e_line(valor, smooth = TRUE, symbol = "roundRect", symbolSize = 10, itemStyle = list(color = "gray40"), lineStyle = list(width = 4, color = "#1D44B8"), areaStyle = list(opacity = 0.2, color = "#1D44B8")) %>%
      e_mark_point(data = list(type = "max", name = "Máximo"), itemStyle = list(color = "#28A745")) %>%
      e_mark_point(data = list(type = "min", name = "Mínimo"), itemStyle = list(color = "#BA1826")) %>%
      e_mark_line(data = list(type = "average", name = "Média"), itemStyle = list(color = "#FFC107")) %>%
      e_tooltip(trigger = "axis", backgroundColor = "#111111", textStyle = list(color = "#FFFFFF"), formatter = htmlwidgets::JS("function(params){ return params[0].name + '<br/>R$ ' + Number(params[0].value[1]).toLocaleString('pt-BR', {minimumFractionDigits: 2}); }")) %>% 
      e_x_axis(type = "category", axisLabel = list(fontWeight = "bold", fontSize = 12)) %>% 
      e_y_axis(axisLabel = list(fontWeight = "bold", formatter = htmlwidgets::JS("function (value) { if (value >= 1000000) return 'R$ ' + (value / 1000000).toFixed(1).replace('.', ',') + ' M'; if (value >= 1000) return 'R$ ' + (value / 1000).toFixed(1).replace('.', ',') + ' mil'; return value; }"))) %>%
      e_title(subtext = paste0("Média Mensal: ", formata_moeda(media_val)), subtextStyle = list(fontWeight = "bold", fontSize = 13, color = "#6c757d")) %>% e_legend(show = FALSE) %>% e_grid(left = "8%", right = "5%", bottom = "15%") %>% e_locale("pt-Br") %>% e_toolbox_feature(feature = c("saveAsImage"))
  })
  
  output$insight_mes <- renderUI({
    dados <- dados_mes_reativo()
    if(nrow(dados) < 1) return(NULL)
    mes_max <- dados$periodo[which.max(dados$valor)]
    val_max <- max(dados$valor)
    div(style = "background-color:#f8f9fa; padding:15px; border-radius:8px; margin-top:10px; border-left: 5px solid #1D44B8;", tags$h5("📊 Pico de Sazonalidade", style = "font-weight:bold; color: #1D44B8; font-size: 15px;"), tags$p(HTML(paste0("Historicamente, o mês de <strong style='color:#28A745;'>", mes_max, "</strong> acumula o maior volume de repasses, totalizando <strong>", formata_moeda(val_max), "</strong>."))))
  })
  
  # --------------------------------------------------------------------------
  # ABA 1 - GRÁFICO 2: EVOLUÇÃO ANUAL
  # --------------------------------------------------------------------------
  output$titulo_ano <- renderText({ paste0("📈 Histórico Anual (R$)") })
  
  dados_ano_reativo <- reactive({
    dados_filtrados() %>% filter(!is.na(Ano)) %>% group_by(Ano) %>% summarise(valor = sum(`Vl.Repasse(R$)`, na.rm = TRUE), .groups = "drop") %>% arrange(Ano) %>% mutate(ano_str = as.character(Ano))
  })
  
  output$grafico_ano <- renderEcharts4r({
    dados <- dados_ano_reativo()
    req(nrow(dados) >= 2) 
    ultimo_valor <- tail(dados$valor, 1); penultimo <- tail(dados$valor, 2)[1]
    variacao_pct <- ifelse(penultimo == 0 | is.na(penultimo), 0, round(((ultimo_valor - penultimo) / penultimo) * 100, 1))
    periodos <- nrow(dados) - 1; cagr_val <- ifelse(dados$valor[1] == 0, 0, round(((ultimo_valor / dados$valor[1])^(1/periodos) - 1) * 100, 1))
    cor_variacao <- ifelse(variacao_pct >= 0, "#28A745", "#BA1826"); icone_seta <- ifelse(variacao_pct >= 0, "▲", "▼")
    texto_subtitulo <- paste0("Variação: ", icone_seta, " ", format(variacao_pct, nsmall=1, decimal.mark=","), "% | CAGR: ", format(cagr_val, nsmall=1, decimal.mark=","), "% a.a.")
    
    dados %>% e_charts(ano_str) %>% e_line(valor, smooth = TRUE, symbol = "roundRect", symbolSize = 12, itemStyle = list(color = "gray40"), lineStyle = list(width = 4, color = "#1D44B8"), areaStyle = list(opacity = 0.2, color = "#1D44B8")) %>%
      e_mark_point(data = list(list(coord = list(tail(dados$ano_str,1), ultimo_valor), value = formata_moeda(ultimo_valor))), itemStyle = list(color = cor_variacao)) %>%
      e_tooltip(trigger = "axis", backgroundColor = "#111111", textStyle = list(color = "#FFFFFF"), formatter = htmlwidgets::JS("function(params){ return params[0].name + '<br/>R$ ' + Number(params[0].value[1]).toLocaleString('pt-BR', {minimumFractionDigits: 2}); }")) %>% e_x_axis(type = "category", axisLabel = list(fontWeight = "bold", fontSize = 12)) %>% e_y_axis(axisLabel = list(fontWeight = "bold", formatter = htmlwidgets::JS("function (value) { if (value >= 1000000) return 'R$ ' + (value / 1000000).toFixed(1).replace('.', ',') + ' M'; if (value >= 1000) return 'R$ ' + (value / 1000).toFixed(1).replace('.', ',') + ' mil'; return value; }"))) %>%
      e_title(subtext = texto_subtitulo, subtextStyle = list(color = cor_variacao, fontWeight = "bold", fontSize = 13)) %>% e_legend(show = FALSE) %>% e_grid(left = "8%", right = "5%") %>% e_locale("pt-Br") %>% e_toolbox_feature(feature = c("saveAsImage"))
  })
  
  output$insight_ano <- renderUI({
    dados <- dados_ano_reativo()
    if(nrow(dados) < 2) return(NULL)
    ultimo_valor <- tail(dados$valor, 1); penultimo <- tail(dados$valor, 2)[1]
    variacao_pct <- ifelse(penultimo == 0, 0, round(((ultimo_valor - penultimo) / penultimo) * 100, 1))
    tendencia <- ifelse(variacao_pct > 0, "expansão", ifelse(variacao_pct < 0, "retração", "estabilidade"))
    cor_texto <- ifelse(variacao_pct >= 0, "#28A745", "#BA1826")
    div(style = "background-color:#f8f9fa; padding:15px; border-radius:8px; margin-top:10px; border-left: 5px solid #1D44B8;", tags$h5("📊 Desempenho Histórico", style = "font-weight:bold; color: #1D44B8; font-size: 15px;"), tags$p(HTML(paste0("O último ano (", tail(dados$ano_str,1), ") apresentou um cenário de ", tendencia, ", com variação de <strong style='color:", cor_texto, ";'>", format(abs(variacao_pct), nsmall=1, decimal.mark=","), "%</strong> sobre o período anterior."))))
  })
  
  # --------------------------------------------------------------------------
  # ABA 2 - GRÁFICO 3: RANKING TOP 15 ÓRGÃOS AUTUADORES
  # --------------------------------------------------------------------------
  dados_orgaos_reativo <- reactive({
    df_temp <- dados_filtrados()
    if (input$ocultar_detran) {
      df_temp <- df_temp %>% filter(Nome_Orgao != "DETRAN - PA")
    }
    df_temp %>% 
      group_by(Nome_Orgao) %>% 
      summarise(valor = sum(`Vl.Repasse(R$)`, na.rm = TRUE), .groups = "drop") %>% 
      arrange(desc(valor)) %>% 
      head(15) %>% 
      arrange(valor) 
  })
  
  output$grafico_orgaos <- renderEcharts4r({
    dados <- dados_orgaos_reativo()
    req(nrow(dados) > 0)
    
    total_geral <- sum(dados_filtrados()$`Vl.Repasse(R$)`, na.rm = TRUE)
    if (input$ocultar_detran) {
      total_geral <- sum(dados_filtrados()$`Vl.Repasse(R$)`[dados_filtrados()$Nome_Orgao != "DETRAN - PA"], na.rm = TRUE)
    }
    total_js <- formatC(total_geral, format = "f", digits = 2, decimal.mark = ".")
    
    js_label_perc_orgaos <- paste0(
      "function(params) {",
      "  var total = ", total_js, ";",
      "  if(total === 0) return '0%';",
      "  var val = Array.isArray(params.value) ? params.value[0] : params.value;",
      "  return (val / total * 100).toFixed(1).replace('.', ',') + '%';",
      "}"
    )
    
    js_tooltip_orgaos <- paste0(
      "function(params) {",
      "  var total = ", total_js, ";",
      "  var val = Array.isArray(params[0].value) ? params[0].value[0] : params[0].value;",
      "  var p = total === 0 ? '0%' : (val / total * 100).toFixed(1).replace('.', ',') + '%';",
      "  return params[0].name + '<br/>R$ ' + Number(val).toLocaleString('pt-BR', {minimumFractionDigits: 2}) + ' (' + p + ')';",
      "}"
    )
    
    dados %>%
      e_charts(Nome_Orgao) %>%
      e_bar(valor, name = "Repasse Geral", itemStyle = list(borderRadius = c(0, 5, 5, 0), color = "#28A745")) %>%
      e_flip_coords() %>%
      e_labels(position = "right", formatter = htmlwidgets::JS(js_label_perc_orgaos), fontWeight = "bold", fontSize = 12) %>%
      e_tooltip(trigger = "axis", backgroundColor = "#111111", textStyle = list(color = "#FFFFFF"), formatter = htmlwidgets::JS(js_tooltip_orgaos)) %>%
      e_x_axis(axisLabel = list(fontWeight = "bold", formatter = htmlwidgets::JS("function (value) { if (value >= 1000000) return 'R$ ' + (value / 1000000).toFixed(1).replace('.', ',') + ' M'; if (value >= 1000) return 'R$ ' + (value / 1000).toFixed(1).replace('.', ',') + ' mil'; return value; }"))) %>%
      e_y_axis(axisLabel = list(fontWeight = "bold", fontSize = 11)) %>%
      e_legend(show = FALSE) %>%
      e_grid(left = "35%", right = "10%") %>% e_locale("pt-Br") %>% e_toolbox_feature(feature = c("saveAsImage"))
  })
  
  output$insight_orgaos <- renderUI({
    dados <- dados_orgaos_reativo()
    if(nrow(dados) == 0) return(NULL)
    top1_nome <- tail(dados$Nome_Orgao, 1); top1_valor <- tail(dados$valor, 1)
    texto_complementar <- ifelse(input$ocultar_detran, " (excluindo o DETRAN-PA da visão)", "")
    
    div(style = "background-color:#f8f9fa; padding:15px; border-radius:8px; margin-top:10px; border-left: 5px solid #28A745;", 
        tags$h5("📊 Liderança de Arrecadação", style = "font-weight:bold; color: #28A745; font-size: 15px;"), 
        tags$p(HTML(paste0("O órgão líder em repasses no recorte atual", texto_complementar, " é <strong>", top1_nome, "</strong>, operando um total de <strong>", formata_moeda(top1_valor), "</strong>."))))
  })
  
  # --------------------------------------------------------------------------
  # ABA 3 - GRÁFICO 4: BARRAS HORIZONTAIS PERCENTUAIS (STATUS DAS CONTAS)
  # --------------------------------------------------------------------------
  dados_status_reativo <- reactive({
    dados_filtrados() %>%
      group_by(Categoria_Contas) %>%
      summarise(valor = sum(`Vl.Repasse(R$)`, na.rm = TRUE), .groups = "drop") %>%
      arrange(valor) 
  })
  
  output$grafico_status <- renderEcharts4r({
    dados <- dados_status_reativo()
    req(nrow(dados) > 0)
    
    total_geral <- sum(dados$valor, na.rm = TRUE)
    total_js <- formatC(total_geral, format = "f", digits = 2, decimal.mark = ".")
    
    js_label_perc_status <- paste0(
      "function(params) {",
      "  var total = ", total_js, ";",
      "  if(total === 0) return '0%';",
      "  var val = Array.isArray(params.value) ? params.value[0] : params.value;",
      "  return (val / total * 100).toFixed(1).replace('.', ',') + '%';",
      "}"
    )
    
    js_tooltip_status <- paste0(
      "function(params) {",
      "  var total = ", total_js, ";",
      "  var val = Array.isArray(params[0].value) ? params[0].value[0] : params[0].value;",
      "  var p = total === 0 ? '0%' : (val / total * 100).toFixed(1).replace('.', ',') + '%';",
      "  return params[0].name + '<br/>R$ ' + Number(val).toLocaleString('pt-BR', {minimumFractionDigits: 2}) + ' (' + p + ')';",
      "}"
    )
    
    js_formata_eixo <- htmlwidgets::JS("function (value) { if (value >= 1000000) return 'R$ ' + (value / 1000000).toFixed(1).replace('.', ',') + ' M'; if (value >= 1000) return 'R$ ' + (value / 1000).toFixed(1).replace('.', ',') + ' mil'; return value; }")
    
    dados %>%
      e_charts(Categoria_Contas) %>% 
      e_bar(valor, name = "Volume Financeiro", itemStyle = list(borderRadius = c(0, 5, 5, 0), color = "#ffc107")) %>%
      e_flip_coords() %>%
      e_labels(position = "right", formatter = htmlwidgets::JS(js_label_perc_status), fontWeight = "bold", fontSize = 12) %>%
      e_tooltip(trigger = "axis", backgroundColor = "#111111", textStyle = list(color = "#FFFFFF"), formatter = htmlwidgets::JS(js_tooltip_status)) %>%
      e_x_axis(axisLabel = list(fontWeight = "bold", formatter = js_formata_eixo)) %>%
      e_y_axis(axisLabel = list(fontWeight = "bold", fontSize = 11)) %>%
      e_legend(show = FALSE) %>%
      e_grid(left = "25%", right = "15%") %>% 
      e_locale("pt-Br") %>% 
      e_toolbox_feature(feature = c("saveAsImage"))
  })
  
  output$insight_status <- renderUI({
    dados <- dados_status_reativo()
    if(nrow(dados) == 0) return(NULL)
    
    total_geral <- sum(dados$valor, na.rm = TRUE)
    top_categoria <- tail(dados$Categoria_Contas, 1)
    top_valor <- tail(dados$valor, 1)
    percentual <- ifelse(total_geral == 0, 0, (top_valor / total_geral) * 100)
    
    div(style = "background-color:#f8f9fa; padding:15px; border-radius:8px; margin-top:10px; border-left: 5px solid #ffc107;",
        tags$h5("📊 Proporção Majoritária", style = "font-weight:bold; color: #d39e00; font-size: 15px;"),
        tags$p(HTML(paste0("A categoria <strong>", top_categoria, "</strong> representa a maior fatia, somando <strong>", formata_moeda(top_valor), "</strong>, o que equivale a <strong>", format(round(percentual, 1), nsmall=1, decimal.mark=","), "%</strong> do valor.")))
    )
  })
  
}

shinyApp(ui = ui, server = server)