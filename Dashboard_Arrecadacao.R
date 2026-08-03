# ==============================================================================
# DASHBOARD EXECUTIVO - OBSERVATÓRIO RENAINF (DETRAN-PA)
# SCRIPT COMPLETO E DEFINITIVO
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
lista_ufs <- sort(unique(df_completo$UF))
lista_orgaos <- sort(unique(df_completo$Nome_Orgao))
meses_pt <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")

# ==============================================================================
# 2. INTERFACE DO USUÁRIO (UI)
# ==============================================================================

ui <- dashboardPage(
  
  header = dashboardHeader(
    title = dashboardBrand(
      title = "RENAINF | DETRAN-PA",
      color = "primary",
      href = "#"
    ),
    skin = "light",
    status = "white"
  ),
  
  sidebar = dashboardSidebar(
    skin = "dark",
    elevation = 3,
    sidebarMenu(
      id = "menu_principal",
      menuItem("Visão Executiva", tabName = "visao_macro", icon = icon("chart-line"))
    )
  ),
  
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "visao_macro",
        
        # Ajuste visual para garantir leitura perfeita dos números longos nas caixas
        tags$head(tags$style(HTML(".small-box h3 { font-size: 22px; font-weight: bold; } .small-box p { font-size: 13px; font-weight: 600; }"))),
        
        # LINHA DOS 6 KPIs (Simétricos - Tamanho 2 cada)
        fluidRow(
          valueBoxOutput("kpi_receber", width = 2),
          valueBoxOutput("kpi_pagar", width = 2),
          valueBoxOutput("kpi_saldo", width = 2), 
          valueBoxOutput("kpi_infracoes", width = 2),
          valueBoxOutput("kpi_gargalo", width = 2),
          valueBoxOutput("kpi_risco", width = 2) 
        ),
        
        # LINHA DE FILTROS GRANULARES (Logo abaixo dos KPIs)
        fluidRow(
          box(
            title = span(icon("filter"), " Filtros Granulares da Pesquisa"),
            width = 12,
            status = "gray-dark",
            solidHeader = TRUE,
            collapsible = TRUE, 
            
            fluidRow(
              column(width = 3, 
                     selectInput("filtro_ano", "Ano de Referência:", choices = c("Todos", as.character(lista_anos)), selected = "Todos")
              ),
              column(width = 3, 
                     selectInput("filtro_uf", "UF da Infração:", choices = c("Todas", lista_ufs), selected = "Todas")
              ),
              column(width = 6, 
                     selectizeInput("filtro_orgao", "Órgão Autuador:", choices = c("Todos", lista_orgaos), selected = "Todos")
              )
            )
          )
        ),
        
        # GRÁFICO 1: SAZONALIDADE MENSAL (Com botão de maximizar)
        fluidRow(
          box(
            title = textOutput("titulo_mes", inline = TRUE),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            maximizable = TRUE,
            echarts4rOutput("grafico_mes", height = "380px"),
            uiOutput("insight_mes")
          )
        ),
        
        # GRÁFICO 2: EVOLUÇÃO ANUAL (Com botão de maximizar)
        fluidRow(
          box(
            title = textOutput("titulo_ano", inline = TRUE),
            width = 12,
            status = "info",
            solidHeader = TRUE,
            maximizable = TRUE,
            echarts4rOutput("grafico_ano", height = "380px"),
            uiOutput("insight_ano")
          )
        )
      )
    )
  )
)

# ==============================================================================
# 3. SERVIDOR (Lógica e Reatividade)
# ==============================================================================

server <- function(input, output, session) {
  
  # Filtro reativo global
  dados_filtrados <- reactive({
    df <- df_completo
    if (input$filtro_ano != "Todos") {
      df <- df %>% filter(Ano == as.numeric(input$filtro_ano))
    }
    if (input$filtro_uf != "Todas") {
      df <- df %>% filter(UF == input$filtro_uf)
    }
    if (input$filtro_orgao != "Todos") {
      df <- df %>% filter(Nome_Orgao == input$filtro_orgao)
    }
    return(df)
  })
  
  # Formatadores padrão
  formata_moeda <- label_number(prefix = "R$ ", big.mark = ".", decimal.mark = ",", accuracy = 0.01)
  formata_inteiro <- label_number(big.mark = ".", decimal.mark = ",")
  
  # --- RENDERIZAÇÃO DOS 6 KPIS ---
  output$kpi_receber <- renderValueBox({
    total <- sum(dados_filtrados()$`Vl.Repasse(R$)`[dados_filtrados()$`Tipo de Contas` == "A Receber"], na.rm = TRUE)
    valueBox(value = formata_moeda(total), subtitle = "Previsão (A Receber)", color = "success", icon = icon("arrow-up"))
  })
  
  output$kpi_pagar <- renderValueBox({
    total <- sum(dados_filtrados()$`Vl.Repasse(R$)`[dados_filtrados()$`Tipo de Contas` == "A Pagar"], na.rm = TRUE)
    valueBox(value = formata_moeda(total), subtitle = "Provisão (A Pagar)", color = "danger", icon = icon("arrow-down"))
  })
  
  output$kpi_saldo <- renderValueBox({
    saldo <- sum(dados_filtrados()$`Vl.Repasse(R$)`[dados_filtrados()$`Tipo de Contas` == "A Receber"], na.rm = TRUE) - 
      sum(dados_filtrados()$`Vl.Repasse(R$)`[dados_filtrados()$`Tipo de Contas` == "A Pagar"], na.rm = TRUE)
    valueBox(value = formata_moeda(saldo), subtitle = "Balanço Líquido", color = "primary", icon = icon("scale-balanced"))
  })
  
  output$kpi_infracoes <- renderValueBox({
    # CORREÇÃO: Adicionado () após dados_filtrados
    total_inf <- sum(dados_filtrados()$Qt.Infrações, na.rm = TRUE) 
    
    valueBox(
      value = formata_inteiro(total_inf), 
      subtitle = "Volume Autuações", 
      color = "warning", 
      icon = icon("car-burst")
    )
  })
  
  output$kpi_gargalo <- renderValueBox({
    total_gargalo <- sum(dados_filtrados()$`Vl.Repasse(R$)`[dados_filtrados()$Categoria_Contas == "Cobranças Não Aceitas"], na.rm = TRUE)
    valueBox(value = formata_moeda(total_gargalo), subtitle = "Retenção (Não Aceitas)", color = "purple", icon = icon("ban"))
  })
  
  output$kpi_risco <- renderValueBox({
    total_risco <- sum(dados_filtrados()$`Vl.Repasse(R$)`[dados_filtrados()$Categoria_Contas == "Cobranças Vencidas"], na.rm = TRUE)
    valueBox(value = formata_moeda(total_risco), subtitle = "Risco (Vencidas)", color = "orange", icon = icon("calendar-times"))
  })
  
  # ==========================================================================
  # GRÁFICO 1: SAZONALIDADE MENSAL (ACUMULADO)
  # ==========================================================================
  
  output$titulo_mes <- renderText({ paste0("📊 Sazonalidade: Comportamento Histórico Mensal (R$) - UF: ", input$filtro_uf) })
  
  dados_mes_reativo <- reactive({
    dados_filtrados() %>%
      filter(!is.na(Mes)) %>%
      group_by(Mes) %>% 
      summarise(valor = sum(`Vl.Repasse(R$)`, na.rm = TRUE), .groups = "drop") %>%
      arrange(Mes) %>%
      mutate(periodo = meses_pt[as.integer(Mes)])
  })
  
  output$grafico_mes <- renderEcharts4r({
    dados <- dados_mes_reativo()
    req(nrow(dados) >= 1) 
    
    media_val <- mean(dados$valor, na.rm = TRUE)
    media_str <- formata_moeda(media_val)
    texto_subtitulo <- paste0("Visão acumulada histórica | Média Mensal: ", media_str)
    
    dados %>% 
      e_charts(periodo) %>% 
      e_line(valor, smooth = TRUE, symbol = "roundRect", symbolSize = 10, 
             itemStyle = list(color = "gray40"), lineStyle = list(width = 4, color = "#1D44B8"), 
             areaStyle = list(opacity = 0.2, color = "#1D44B8")) %>%
      e_mark_point(data = list(type = "max", name = "Máximo"), itemStyle = list(color = "#28A745")) %>%
      e_mark_point(data = list(type = "min", name = "Mínimo"), itemStyle = list(color = "#BA1826")) %>%
      e_mark_line(data = list(type = "average", name = "Média"), itemStyle = list(color = "#FFC107")) %>%
      e_tooltip(trigger = "axis", backgroundColor = "#111111", textStyle = list(color = "#FFFFFF"),
                formatter = htmlwidgets::JS("function(params){ return params[0].name + '<br/>R$ ' + Number(params[0].value[1]).toLocaleString('pt-BR', {minimumFractionDigits: 2}); }")) %>% 
      e_x_axis(type = "category", axisLabel = list(fontWeight = "bold", fontSize = 12)) %>% 
      e_y_axis(axisLabel = list(fontWeight = "bold", formatter = htmlwidgets::JS("function (value) { if (value >= 1000000) return 'R$ ' + (value / 1000000).toFixed(1).replace('.', ',') + ' M'; if (value >= 1000) return 'R$ ' + (value / 1000).toFixed(1).replace('.', ',') + ' mil'; return value; }"))) %>%
      e_title(subtext = texto_subtitulo, subtextStyle = list(fontWeight = "bold", fontSize = 13, color = "#6c757d")) %>% 
      e_legend(show = FALSE) %>% 
      e_grid(left = "8%", right = "5%", bottom = "15%") %>% 
      e_locale("pt-Br") %>% 
      e_toolbox_feature(feature = c("saveAsImage"))
  })
  
  output$insight_mes <- renderUI({
    dados <- dados_mes_reativo()
    if(nrow(dados) < 1) return(NULL)
    
    mes_max <- dados$periodo[which.max(dados$valor)]
    val_max <- max(dados$valor)
    mes_min <- dados$periodo[which.min(dados$valor)]
    val_min <- min(dados$valor)
    
    div(style = "background-color:#f8f9fa; padding:15px; border-radius:8px; margin-top:10px; border-left: 5px solid #1D44B8;",
        tags$h5("📊 Análise Sazonal de Caixa", style = "font-weight:bold; color: #1D44B8; font-size: 15px;"),
        tags$p(HTML(paste0("Historicamente, o mês de <strong style='color:#28A745;'>", mes_max, "</strong> acumula o maior volume de repasses, totalizando <strong>", formata_moeda(val_max), "</strong>."))),
        tags$p(HTML(paste0("Em contrapartida, <strong style='color:#BA1826;'>", mes_min, "</strong> registra a menor entrada média (<strong>", formata_moeda(val_min), "</strong>), servindo como alerta para planejamento de fluxo de caixa.")))
    )
  })
  
  # ==========================================================================
  # GRÁFICO 2: EVOLUÇÃO HISTÓRICA ANUAL
  # ==========================================================================
  
  output$titulo_ano <- renderText({ paste0("📈 Histórico Anual de Repasses (R$) - UF: ", input$filtro_uf) })
  
  dados_ano_reativo <- reactive({
    dados_filtrados() %>%
      filter(!is.na(Ano)) %>%
      group_by(Ano) %>%
      summarise(valor = sum(`Vl.Repasse(R$)`, na.rm = TRUE), .groups = "drop") %>%
      arrange(Ano) %>%
      mutate(ano_str = as.character(Ano))
  })
  
  output$grafico_ano <- renderEcharts4r({
    dados <- dados_ano_reativo()
    req(nrow(dados) >= 2) 
    
    ultimo_valor  <- tail(dados$valor, 1)
    penultimo <- tail(dados$valor, 2)[1]
    
    variacao_pct  <- ifelse(penultimo == 0 | is.na(penultimo), 0, round(((ultimo_valor - penultimo) / penultimo) * 100, 1))
    periodos <- nrow(dados) - 1
    cagr_val <- ifelse(dados$valor[1] == 0, 0, round(((ultimo_valor / dados$valor[1])^(1/periodos) - 1) * 100, 1))
    
    cor_variacao  <- ifelse(variacao_pct >= 0, "#28A745", "#BA1826") 
    icone_seta <- ifelse(variacao_pct >= 0, "▲", "▼")
    
    var_str <- format(variacao_pct, nsmall = 1, decimal.mark = ",")
    cagr_str <- format(cagr_val, nsmall = 1, decimal.mark = ",")
    texto_subtitulo <- paste0("Variação Anual: ", icone_seta, " ", var_str, "% | Crescimento CAGR: ", cagr_str, "% a.a.")
    
    dados %>% 
      e_charts(ano_str) %>% 
      e_line(valor, smooth = TRUE, symbol = "roundRect", symbolSize = 12, 
             itemStyle = list(color = "gray40"), lineStyle = list(width = 4, color = "#1D44B8"), 
             areaStyle = list(opacity = 0.2, color = "#1D44B8")) %>%
      e_mark_point(data = list(list(coord = list(tail(dados$ano_str,1), ultimo_valor), value = formata_moeda(ultimo_valor))), 
                   itemStyle = list(color = cor_variacao)) %>%
      e_tooltip(trigger = "axis", backgroundColor = "#111111", textStyle = list(color = "#FFFFFF"),
                formatter = htmlwidgets::JS("function(params){ return params[0].name + '<br/>R$ ' + Number(params[0].value[1]).toLocaleString('pt-BR', {minimumFractionDigits: 2}); }")) %>% 
      e_x_axis(type = "category", axisLabel = list(fontWeight = "bold", fontSize = 12)) %>% 
      e_y_axis(axisLabel = list(fontWeight = "bold", formatter = htmlwidgets::JS("function (value) { if (value >= 1000000) return 'R$ ' + (value / 1000000).toFixed(1).replace('.', ',') + ' M'; if (value >= 1000) return 'R$ ' + (value / 1000).toFixed(1).replace('.', ',') + ' mil'; return value; }"))) %>%
      e_title(subtext = texto_subtitulo, subtextStyle = list(color = cor_variacao, fontWeight = "bold", fontSize = 13)) %>% 
      e_legend(show = FALSE) %>% 
      e_grid(left = "8%", right = "5%") %>% 
      e_locale("pt-Br") %>% 
      e_toolbox_feature(feature = c("saveAsImage"))
  })
  
  output$insight_ano <- renderUI({
    dados <- dados_ano_reativo()
    if(nrow(dados) < 2) return(NULL)
    
    ultimo_valor <- tail(dados$valor, 1)
    penultimo <- tail(dados$valor, 2)[1]
    variacao_pct <- ifelse(penultimo == 0, 0, round(((ultimo_valor - penultimo) / penultimo) * 100, 1))
    periodos <- nrow(dados) - 1
    cagr_val <- ifelse(dados$valor[1] == 0, 0, round(((ultimo_valor / dados$valor[1])^(1/periodos) - 1) * 100, 1))
    
    tendencia <- ifelse(variacao_pct > 0, "expansão de repasses", ifelse(variacao_pct < 0, "retração financeira", "estabilidade"))
    cor_texto <- ifelse(variacao_pct >= 0, "#28A745", "#BA1826")
    
    div(style = "background-color:#f8f9fa; padding:15px; border-radius:8px; margin-top:10px; border-left: 5px solid #1D44B8;",
        tags$h5("📊 Avaliação Histórica Anual", style = "font-weight:bold; color: #1D44B8; font-size: 15px;"),
        tags$p(paste0("O fechamento de ", tail(dados$ano_str,1), " registrou o montante de ", formata_moeda(ultimo_valor), ".")),
        tags$p(HTML(paste0("Isso representa uma variação de <strong style='color:", cor_texto, ";'>", format(abs(variacao_pct), nsmall=1, decimal.mark=","), "%</strong> sobre o ano anterior, num cenário de ", tendencia, "."))),
        tags$p(paste0("A Taxa de Crescimento Anual Composta (CAGR) da série é de ", format(cagr_val, nsmall=1, decimal.mark=","), "% a.a."))
    )
  })
}

shinyApp(ui = ui, server = server)