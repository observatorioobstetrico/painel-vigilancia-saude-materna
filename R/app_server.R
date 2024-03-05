#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  mod_documentacao_server("documentacao_1")
  mod_nivel_1_server("nivel_1_1", filtros = filtros, titulo_localidade_aux = titulo_localidade_nivel1)
  mod_bloco_1_server("bloco_1_1", filtros = filtros, titulo_localidade_aux = titulo_localidade_nivel2)
  mod_bloco_2_server("bloco_2_1", filtros = filtros, titulo_localidade_aux = titulo_localidade_nivel2)
  mod_bloco_3_server("bloco_3_1", filtros = filtros, titulo_localidade_aux = titulo_localidade_nivel2)
  mod_bloco_4_server("bloco_4_1", filtros = filtros, titulo_localidade_aux = titulo_localidade_nivel2)
  mod_bloco_5_server("bloco_5_1", filtros = filtros, titulo_localidade_aux = titulo_localidade_nivel2)
  mod_bloco_6_server("bloco_6_1", filtros = filtros, titulo_localidade_aux = titulo_localidade_nivel2)
  mod_nivel_3_server("nivel_3_1", filtros = filtros, titulo_localidade_aux = titulo_localidade_nivel3)


  ##### Criando o objeto reativo que recebe o título da página ####
  titulo_localidade_nivel1 <- eventReactive(input$pesquisar, {
    ano <- input$ano

    texto <- dplyr::case_when(
      input$nivel == "Municipal" ~ glue::glue("({input$municipio}, {ano})"),
      input$nivel == "Estadual" ~ glue::glue("({input$estado}, {input$ano})"),
      input$nivel == "Macrorregião de saúde" ~ glue::glue("({input$macro}, {ano})"),
      input$nivel == "Microrregião de saúde" ~ glue::glue("({input$micro}, {ano})"),
      input$nivel == "Regional" ~ glue::glue("({input$regiao}, {ano})"),
      input$nivel == "Nacional" ~ glue::glue("(Brasil, {ano})")
    )

    tags$b(texto, style = "font-size: 33px")
  }, ignoreNULL = FALSE)

  titulo_localidade_nivel2 <- eventReactive(input$pesquisar, {
    if (length(input$ano2[1]:input$ano2[2]) > 1) {
      ano <- glue::glue("{input$ano2[1]} a {input$ano2[2]}")
    } else {
      ano <- input$ano2[1]
    }

    if (input$comparar == "Não") {
      local1 <- dplyr::case_when(
        input$nivel == "Nacional" ~ "Brasil",
        input$nivel == "Regional" ~ input$regiao,
        input$nivel == "Estadual" ~ input$estado,
        input$nivel == "Macrorregião de saúde" ~ input$macro,
        input$nivel == "Microrregião de saúde" ~ input$micro,
        input$nivel == "Municipal" ~ input$municipio
      )
      texto <- glue::glue("({local1}, {ano})")
    } else {
      local1 <- dplyr::case_when(
        input$nivel == "Nacional" ~ "Brasil",
        input$nivel == "Regional" ~ input$regiao,
        input$nivel == "Estadual" ~ input$estado,
        input$nivel == "Macrorregião de saúde" ~ input$macro,
        input$nivel == "Microrregião de saúde" ~ input$micro,
        input$nivel == "Municipal" ~ input$municipio
      )
      local2 <- dplyr::case_when(
        input$nivel2 == "Nacional" ~ "Brasil",
        input$nivel2 == "Regional" ~ input$regiao2,
        input$nivel2 == "Estadual" ~ input$estado2,
        input$nivel2 == "Macrorregião de saúde" ~ input$macro2,
        input$nivel2 == "Microrregião de saúde" ~ input$micro2,
        input$nivel2 == "Municipal" ~ input$municipio2,
        input$nivel2 == "Municípios semelhantes" ~ "municípios semelhantes"
      )
      texto <- glue::glue("({local1} e {local2}, {ano})")

      tags$b(texto, style = "font-size: 33px")
    }
  }, ignoreNULL = FALSE)

  titulo_localidade_nivel3 <- eventReactive(input$pesquisar, {
    if (length(input$ano2[1]:input$ano2[2]) > 1) {
      ano <- glue::glue("{input$ano2[1]} a {input$ano2[2]}")
    } else {
      ano <- input$ano2[1]
    }

    local1 <- dplyr::case_when(
      input$nivel == "Nacional" ~ "Brasil",
      input$nivel == "Regional" ~ input$regiao,
      input$nivel == "Estadual" ~ input$estado,
      input$nivel == "Macrorregião de saúde" ~ input$macro,
      input$nivel == "Microrregião de saúde" ~ input$micro,
      input$nivel == "Municipal" ~ input$municipio
    )

    tags$b(paste("-", input$indicador, glue::glue("({local1}, {ano})")), style = "font-size: 30px")
  }, ignoreNULL = FALSE)



  output$label_nivel_comp <- renderUI({
    div(
      tags$b(HTML("Nível de análise &nbsp;")),
      if (input$nivel2 == "Municípios semelhantes") {
        shinyWidgets::actionBttn(
          inputId = "botao_agrupamento",
          icon = icon("question"),
          color = "primary",
          style = "material-circle",
          size = "xs"
        )
      }
    )
  })

  observeEvent(input$botao_agrupamento, {
    shinyalert::shinyalert(
      html = TRUE,
      title = "<div style = 'font-size: 25px; color: #656565'> Sobre o agrupamento de municípios semelhantes </div>",
      text = "<div style = 'text-align: justify; text-justify: inter-word;'> Os municípios foram agrupados a partir de seu IDHM e de sua latitude por meio do algoritmo de agrupamento K-médias. Por meio da análise do gráfico do cotovelo e dos índices de Davies-Bouldin, Dunn,
Silhueta e Calinski-Harabasz, o número de grupos adotado foi 3. </div>",
      size = "s",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      type = "info",
      showConfirmButton = TRUE,
      confirmButtonText = "OK",
      confirmButtonCol = "#007bff",
      animation = TRUE,
      immediate = TRUE
    )
  })

  rv <- reactiveValues(input_nivel_anterior = NULL)
  observeEvent(input$nivel, {
    if (length(rv$input_nivel_anterior) == 1) {
      rv$input_nivel_anterior <- c(rv$input_nivel_anterior, input$nivel)
    } else {
      rv$input_nivel_anterior <- c(rv$input_nivel_anterior[length(rv$input_nivel_anterior)], input$nivel)
    }
  })

  observe({
    if (input$nivel == "Municipal") {
      if (rv$input_nivel_anterior[length(rv$input_nivel_anterior) - 1] != "Municipal") {
        updateSelectizeInput(
          inputId = "nivel2",
          choices = c("Nacional", "Regional", "Macrorregião de saúde", "Microrregião de saúde", "Estadual", "Municipal", "Municípios semelhantes")
        )
      }
    }
    if (input$nivel != "Municipal") {
      if (length(rv$input_nivel_anterior) > 1) {
        if (rv$input_nivel_anterior[length(rv$input_nivel_anterior) - 1] == "Municipal") {
          updateSelectizeInput(
            inputId = "nivel2",
            choices = c("Nacional", "Regional", "Macrorregião de saúde", "Microrregião de saúde", "Estadual", "Municipal")
          )
        }
      }
    }
  })

  observeEvent(input$estado_municipio, {
    updateSelectizeInput(
      session,
      inputId = "municipio",
      choices = sort(municipios_choices$municipio[which(municipios_choices$uf == input$estado_municipio)]),
      server = FALSE
    )
  })

  observeEvent(input$estado_micro,{
    updateSelectizeInput(
      session,
      inputId = "micro",
      choices = sort(micro_r_saude_choices$r_saude[which(micro_r_saude_choices$uf == input$estado_micro)]),
      server = FALSE
    )
  })

  observeEvent(input$estado_macro, {
    updateSelectizeInput(
      session,
      inputId = "macro",
      choices = sort(macro_r_saude_choices$macro_r_saude[which(macro_r_saude_choices$uf == input$estado_macro)]),
      server = FALSE
    )
  })

  observeEvent(input$estado_municipio2, {
    updateSelectizeInput(
      session,
      inputId = "municipio2",
      choices = sort(municipios_choices$municipio[which(municipios_choices$uf == input$estado_municipio2)]),
      server = FALSE
    )
  })

  observeEvent(input$estado_micro2, {
    updateSelectizeInput(
      session,
      inputId = "micro2",
      choices = sort(micro_r_saude_choices$r_saude[which(micro_r_saude_choices$uf == input$estado_micro2)]),
      server = FALSE
    )
  })

  observeEvent(input$estado_macro2, {
    updateSelectizeInput(
      session,
      inputId = "macro2",
      choices = sort(macro_r_saude_choices$macro_r_saude[which(macro_r_saude_choices$uf == input$estado_macro2)]),
      server = FALSE
    )
  })

  observeEvent(input$bloco, {
    updateSelectizeInput(
      session,
      inputId = "indicador",
      choices = tabela_indicadores$indicador[which(tabela_indicadores$bloco == input$bloco)],
      server = FALSE
    )

    if (input$bloco == "bloco4") {
      updateSelectizeInput(
        session,
        inputId = "tipo_do_indicador_blocos4_6",
        choices = c(
          "Relacionados aos grupos de Robson e cesariana" = "robson",
          "Relacionados ao deslocamento para o parto" = "deslocamento"
        )
      )
    } else if (input$bloco == "bloco6") {
      updateSelectizeInput(
        session,
        inputId = "tipo_do_indicador_blocos4_6",
        choices = c(
          "Relacionados à mortalidade materna" = "mortalidade",
          "Relacionados à morbidade materna" = "morbidade"
        )
      )
    }
  })


  observeEvent(c(input$tipo_do_indicador_blocos4_6, input$nivel), {
    if (input$tipo_do_indicador_blocos4_6 == "robson") {
      updateSelectizeInput(
        session,
        inputId = "indicador_blocos4_6",
        choices = c(
          tabela_indicadores$indicador[which(startsWith(tabela_indicadores$indicador, "Porcentagem de nascidos vivos") & tabela_indicadores$bloco == "bloco4")],
          tabela_indicadores$indicador[which(startsWith(tabela_indicadores$indicador, "Porcentagem de cesarianas") & tabela_indicadores$bloco == "bloco4")],
          tabela_indicadores$indicador[which(startsWith(tabela_indicadores$indicador, "Contribuição") & tabela_indicadores$bloco == "bloco4")]
        ),
        server = FALSE
      )
    } else if (input$tipo_do_indicador_blocos4_6 == "deslocamento") {
      if (input$nivel %in% c("Municipal", "Estadual")) {
        updateSelectizeInput(
          session,
          inputId = "indicador_blocos4_6",
          choices = tabela_indicadores$indicador[which(tabela_indicadores$bloco == "bloco4_deslocamento")],
          server = FALSE
        )
      } else {
        updateSelectizeInput(
          session,
          inputId = "indicador_blocos4_6",
          choices = tabela_indicadores$indicador[which(tabela_indicadores$bloco == "bloco4_deslocamento" & !base::startsWith(tabela_indicadores$indicador, "Medianas"))],
          server = FALSE
        )
      }
    } else if (input$tipo_do_indicador_blocos4_6 == "mortalidade") {
      updateSelectizeInput(
        session,
        inputId = "indicador_blocos4_6",
        choices = tabela_indicadores$indicador[which(tabela_indicadores$bloco == "bloco6")],
        server = FALSE
      )
    } else if (input$tipo_do_indicador_blocos4_6 == "morbidade") {
      updateSelectizeInput(
        session,
        inputId = "indicador_blocos4_6",
        choices = tabela_indicadores$indicador[which(tabela_indicadores$bloco == "bloco6_morbidade")],
        server = FALSE
      )
    }
  })

  filtros <- eventReactive(input$pesquisar, {
    list(
      ano = input$ano,
      ano2 = input$ano2,
      nivel = input$nivel,
      regiao = input$regiao,
      estado = input$estado,
      estado_macro = input$estado_macro,
      estado_micro = input$estado_micro,
      estado_municipio = input$estado_municipio,
      macro = input$macro,
      micro = input$micro,
      municipio = input$municipio,
      comparar = input$comparar,
      nivel2 = input$nivel2,
      regiao2 = input$regiao2,
      estado2 = input$estado2,
      estado_macro2 = input$estado_macro2,
      estado_micro2 = input$estado_micro2,
      estado_municipio2 = input$estado_municipio2,
      macro2 = input$macro2,
      micro2 = input$micro2,
      municipio2 = input$municipio2,
      bloco = input$bloco,
      indicador = input$indicador,
      pesquisar = input$pesquisar,
      mostrar_referencia = input$mostrar_referencia,
      tipo_do_indicador_blocos4_6 = input$tipo_do_indicador_blocos4_6,
      indicador_blocos4_6 = input$indicador_blocos4_6
    )
  },
  ignoreNULL = FALSE
  )

  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$decimalPoint <- ","
  hcoptslang$thosandsSep <- "."
  options(highcharter.lang = hcoptslang)

  options(reactable.theme = reactable::reactableTheme(
    borderColor = "#dfe2e5",
    stripedColor = "#e5efff",
    highlightColor = "#CDDEFC",
    cellPadding = "8px 12px",
    searchInputStyle = list(width = "100%")
  )
  )

  options(pagedown.remote.maxattempts=60) # number of attempt in total

}



