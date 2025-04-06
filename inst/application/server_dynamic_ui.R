# This file contains the code to create the user interface

output$ui_expand_header <- renderUI({tagList(
  # this provides css information for the regression table to assure proper spacing
  tags$head(
    tags$style(HTML(
      "
      #regression table > thead > tr > th,
      #regression table > tbody > tr > th,
      #regression table > tfoot > tr > th,
      #regression table > thead > tr > td,
      #regression table > tbody > tr > td,
      #regression table > tfoot > tr > td {
      padding:0px 5px;
      }")),
    tags$script(src = "message-handler.js")
  ),

  titlePanel(shiny_title),
  if(!is.null(shiny_abstract)) {
    fluidRow(
      column (12,
              HTML(shiny_abstract),
              p(),
              hr()
      )
    )
  }
)})

udv_row <- function() {
  list(fluidRow(column(6, uiOutput("ui_udv_name")),
                column(6, uiOutput("ui_udv_def"))),
       hr())
}

expand_components <- reactive({
  comp <- components()
  if (length(comp) > 0) {
    ll <- length(comp)
    if (simple_call_mode) expand_components <- vector("list", ll) else
      expand_components <- vector("list", ll + 1)
    lpos <- 1
    html_block_pos <- 1
    for (i in 1:ll) {
      if (names(comp[i]) == "udvars") {
        expand_components[[lpos]] <- list(fluidRow(
          column(6, uiOutput("ui_udv_name")),
          column(6, uiOutput("ui_udv_def"))),
          hr())
        lpos <- lpos + 1
      }

      if(names(comp[i]) == "html_block") {
        expand_components[[lpos]] <- list(fluidRow(
          HTML(shiny_html_blocks[html_block_pos])
        ),
        hr())

        html_block_pos <- html_block_pos + 1
        lpos <- lpos + 1
      }

      if(names(comp[i]) == "sample_selection") {
        if(!server_side_data) {
          expand_components[[lpos]] <- list(fluidRow(
            column (4, uiOutput("ui_sample")),
            column (4, uiOutput("ui_select_ids")),
            column (4, uiOutput("ui_balanced_panel"))
          ),
          hr())
        } else {
          expand_components[[lpos]] <- list(fluidRow(
            column (6, uiOutput("ui_sample")),
            column (6, uiOutput("ui_balanced_panel"))),
            hr())
        }
        lpos <- lpos + 1
      }

      if(names(comp[i]) == "subset_factor") {
        expand_components[[lpos]] <- list(fluidRow(
          column (6, uiOutput("ui_subset_factor")),
          column (6, uiOutput("ui_subset_value"))
        ),
        hr())
        lpos <- lpos + 1
      }

      if(names(comp[i]) == "grouping") {
        expand_components[[lpos]] <- list(fluidRow(
          column (6, uiOutput("ui_group_factor")),
          column (6, uiOutput("ui_outlier_treatment"))
        ),
        hr())
        lpos <- lpos + 1
      }

      if(names(comp[i]) == "bar_chart") {
        expand_components[[lpos]] <- list(fluidRow(
          column (2,
                  uiOutput("ui_bar_chart")
          ),
          column (10, withSpinner(plotOutput("bar_chart")))
        ),

        hr())
        lpos <- lpos + 1
      }

      if(names(comp[i]) == "missing_values") {
        expand_components[[lpos]] <- list(fluidRow(
          column (2,
                  uiOutput("ui_missing_values")
          ),
          column (10, withSpinner(plotOutput("missing_values")))
        ),
        hr())
        lpos <- lpos + 1
      }

      if(names(comp[i]) == "descriptive_table") {
        expand_components[[lpos]] <- list(fluidRow(
          column(2, uiOutput("ui_descriptive_table_left")),
          column(10, align="center", uiOutput("ui_descriptive_table_right"))
        ),

        hr())
        lpos <- lpos + 1
      }

      if(names(comp[i]) == "histogram") {
        expand_components[[lpos]] <- list(fluidRow(
          column(2, uiOutput("ui_histogram")),
          column(10, withSpinner(plotOutput("histogram")))
        ),
        hr())
        lpos <- lpos + 1
      }

      if(names(comp[i]) == "ext_obs") {
        expand_components[[lpos]] <- list(fluidRow(
          column(2, uiOutput("ui_ext_obs")),
          column(10, align="center", tableOutput("ext_obs"))
        ),
        hr())
        lpos <- lpos + 1
      }

      if(names(comp[i]) == "by_group_bar_graph") {
        expand_components[[lpos]] <- list(fluidRow(
          column(2, uiOutput("ui_by_group_bar_graph")),
          column(10,
                 div(
                   style = "position:relative",
                   uiOutput("by_group_bar_graph.ui", height="100%"))
          )
        ),
        hr())
        lpos <- lpos + 1
      }

      if(names(comp[i]) == "by_group_violin_graph") {
        expand_components[[lpos]] <- list(fluidRow(
          column(2, uiOutput("ui_by_group_violin_graph")),
          column(10,
                 div(
                   style = "position:relative",
                   uiOutput("by_group_violin_graph.ui", height="100%"))
          )
        ),
        hr())
        lpos <- lpos + 1
      }

      if(names(comp[i]) == "trend_graph") {
        expand_components[[lpos]] <- list(fluidRow(
          column(2, uiOutput("ui_trend_graph")),
          column(10, withSpinner(plotOutput("trend_graph")))
        ),
        hr())
        lpos <- lpos + 1
      }

      if(names(comp[i]) == "quantile_trend_graph") {
        expand_components[[lpos]] <- list(fluidRow(
          column(2, uiOutput("ui_quantile_trend_graph")),
          column(10, withSpinner(plotOutput("quantile_trend_graph", height="600px")))
        ),
        hr())
        lpos <- lpos + 1
      }

      if(names(comp[i]) == "by_group_trend_graph") {
        expand_components[[lpos]] <- list(fluidRow(
          column(2, uiOutput("ui_by_group_trend_graph")),
          column(10, withSpinner(plotOutput("by_group_trend_graph", height="600px")))
        ),
        hr())
        lpos <- lpos + 1
      }

      if(names(comp[i]) == "corrplot") {
        expand_components[[lpos]] <- list(fluidRow(
          column(2,uiOutput("ui_corrplot")),
          column(10,
                 div(
                   style = "position:relative",
                   uiOutput("corrplot.ui", height="100%"),
                   uiOutput("corrplot_hover_info")
                 ))
        ),
        hr())
        lpos <- lpos + 1
      }

      if(names(comp[i]) == "scatter_plot") {
        expand_components[[lpos]] <- list(fluidRow(
          column(2, uiOutput("ui_scatter_plot")),
          column(10,
                 div(
                   style = "position:relative",
                   withSpinner(plotOutput("scatter_plot",
                                          hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
                                          height="600px")),
                   uiOutput("hover_info")
                 ))
        ),
        hr())
        lpos <- lpos + 1
      }

      if(names(comp[i]) == "regression") {
        expand_components[[lpos]] <- list(fluidRow(
          column(2,
                 uiOutput("ui_regression"),
                 hr(),
                 uiOutput("ui_clustering"),
                 hr(),
                 uiOutput("ui_model")),
          column(10, align="center", htmlOutput("regression"))
        ),
        hr())
        lpos <- lpos + 1
      }
    }
  } else expand_components <- NULL
  expand_components
})

output$ui_expand_components <- renderUI(tagList(expand_components()))

output$download <- downloadHandler(
  filename ="ExPanD.RDS",
  content = function(file) {
    save_uc <- reactiveValuesToList(uc)
    save_uc$config_parsed <- NULL
    if (shiny_store_encrypted) {
      raw <- serialize(save_uc, NULL)
      encrypted <- openssl::aes_cbc_encrypt(raw, key, iv = NULL)
      saveRDS(encrypted, file)
    } else saveRDS(save_uc, file)
  }
)

expand_footer <- reactive({
  expand_footer <- list()
  if(shiny_save_settings_option && length(components()) > 1) {
    expand_footer <- c(
      expand_footer,
      list(
        fluidRow(
          column(6, align="center",
                 downloadButton('download', 'Save Settings'),
                 helpText("Click here to save ExPanD settings to your local environment.")
          ),
          column(6, align="center",
                 fileInput('upload', ''),
                 helpText("Select RDS file to load locally stored settings.")
          )
        ),
        hr()
      )
    )
  }

  if(shiny_export_nb_option && length(components()) > 1) {
    expand_footer <- c(
      expand_footer,
      list(
        fluidRow(
          column(12, align="center",
                 downloadButton('nb_download', 'Export Data and Notebook Code'),
                 helpText("Click here to export a zip file containing data and R notebook code to continue the analysis in R")
          )
        ),
        hr()
      )
    )
  }

  c(expand_footer, list(
    fluidRow(
      column(
        12, align="center",
        HTML("ExPanD based on <a href=https://trr266.github.io/ExPanDaR>",
             "ExPanDaR</a>, <a href=https://twitter.com/JoachimGassen>",
             "Joachim Gassen</a>,",
             "<a href=https://www.wiwi.hu-berlin.de/rewe>",
             "Humboldt-Universität zu Berlin</a> and",
             "<a href=https://www.accounting-for-transparency.de>",
             "TRR 266 Accounting for Transparency</a>, 2025<p>")
      )
    )
  ))
})

output$ui_expand_footer <- renderUI(tagList(expand_footer()))
