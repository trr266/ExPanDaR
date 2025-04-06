create_nb_code_for_component <- function(comp) {
  df <- create_analysis_sample()

  if (comp == "setup") return({
    nb_code <- c(
      "### Setup", " ",
      "```{r setup}",
      "suppressWarnings(suppressMessages({",
      "  library(knitr)",
      "  library(kableExtra)",
      "  library(htmltools)",
      "  library(tidyverse)",
      "  library(scales)",
      "  library(ExPanDaR)",
      "}))",
      "knitr::opts_chunk$set(fig.align = 'center')",
      "```", " ", " "
    )
  })


  if (comp == "create_sample") return({
    nb_code <- c(
      "### Create Sample", " ",
      "This step reads the raw data provided by `ExPanD()` and generates the sample for the analysis.",
      " ",
      "```{r create_sample}",
      "create_sample <- function(df, df_def) {",
      "  # Set infinite numerical variables to NA",
      "  df[, df_def$var_name[df_def$type == \"numeric\"]] <-",
      "    lapply(df[, df_def$var_name[df_def$type == \"numeric\"]],",
      "      function(x) ifelse(is.finite(x), x, NA))",
      " ",
      "  # Delete numerical variables that only contain NAs",
      "  all_na_vars <- sapply(df, function (x) all(is.na(x)))",
      "  df_def <- df_def[!all_na_vars,]",
      "  df <- df[, df_def$var_name]",
      " ",
      "  # Drop observations that are NA in variables that are not allowed to",
      "  df <- df[complete.cases(df[, df_def$var_name[which(df_def$can_be_na == FALSE)]]), ]"
    )

    if ((uc$subset_factor != "Full Sample") & (uc$subset_value != "All"))
      nb_code <- c(nb_code,
                   " ",
                   "  # Subset the analysis as requested in ExPanD()",
                   sprintf('  df <- df[which(df$%s == "%s"), ]', uc$subset_factor, uc$subset_value))

    if (uc$balanced_panel)
      nb_code <- c(nb_code,
                   " ",
                   "  # Balance sample as requested in ExPanD()",
                   '  df <- group_by_at(df, vars(one_of(df_def$var_name[df_def$type  == "cs_id"]))) %>%',
                   '    mutate(nobs = n())',
                   '  max_nobs <- length(levels(as.data.frame(df[, df_def$var_name[df_def$type  == "ts_id"]])[,1]))',
                   '  bal_df <- as.data.frame(select(filter(df, nobs == max_nobs), -nobs))',
                   '  df <- as.data.frame(bal_df)')

    if (uc$outlier_treatment > 1) {
      if (uc$outlier_factor == "None")
        group <- "NULL"
      else
        group <- sprintf('"%s"', df[,uc$outlier_factor])

      nb_code <- c(nb_code,
                   " ",
                   "  # Outlier treatment as requested in ExPanD()",
                   '  nums <- df_def$var_name[df_def$type == "numeric"]')
    }

    if (uc$outlier_treatment == 2)
      nb_code <- c(nb_code, sprintf('  df[, nums] <- treat_outliers(df[, nums], 0.01, FALSE, %s)', group))
    if (uc$outlier_treatment == 3)
      nb_code <- c(nb_code, sprintf('  df[, nums] <- treat_outliers(df[, nums], 0.05, FALSE, %s)', group))
    if (uc$outlier_treatment == 4)
      nb_code <- c(nb_code, sprintf('  df[, nums] <- treat_outliers(df[, nums], 0.01, TRUE, %s)', group))
    if (uc$outlier_treatment == 5)
      nb_code <- c(nb_code, sprintf('  df[, nums] <- treat_outliers(df[, nums], 0.05, TRUE, %s)', group))

    nb_code <- c(nb_code,
                 " ",
                 "  df <- droplevels(df)",
                 "  return(list(df = df, df_def = df_def))",
                 "}",
                 " ",
                 "load(\"ExPanD_nb_data.Rdata\")",
                 " ",
                 "smp_list <- create_sample(nb_df, nb_df_def)",
                 "smp <- smp_list$df",
                 "smp_def <- smp_list$df_def",
                 "```", " ", " ")

    nb_code
  })


  if (comp == "bar_chart") return({
    nb_code <- c(
      "### Bar Chart", " ",
      "```{r bar_chart}",
      "df <- smp"
    )
    if (uc$bar_chart_group_by != "All")
      nb_code <- c(nb_code,
                   sprintf('df <- df[df$%s == "%s", ]',
                           uc$group_factor,
                           uc$bar_chart_group_by))
    nb_code <- c(nb_code,
                 sprintf('df$%s <- as.factor(df$%s)', uc$bar_chart_var1, uc$bar_chart_var1))

    if (uc$bar_chart_var2 != "None")
      nb_code <- c(nb_code,
                   sprintf('df$%s <- as.factor(df$%s)', uc$bar_chart_var2, uc$bar_chart_var2))

    if (uc$bar_chart_var2 != "None" & (!uc$bar_chart_relative)) {
      nb_code <- c(nb_code,
                   sprintf('p <- ggplot(df, aes(x = %s)) +', uc$bar_chart_var1),
                   sprintf('  geom_bar(aes(fill= %s), position = "stack") +', uc$bar_chart_var2),
                   sprintf('  labs(x = "%s", fill = "%s")', uc$bar_chart_var1, uc$bar_chart_var2))
    } else if (uc$bar_chart_var2 != "None") {
      nb_code <- c(nb_code,
                   sprintf('p <- ggplot(df, aes(x = %s)) +', uc$bar_chart_var1),
                   sprintf('  geom_bar(aes(fill = %s), position = "fill") +', uc$bar_chart_var2),
                   sprintf('  labs(x = "%s", fill = "%s", y = "Percent") +', uc$bar_chart_var1, uc$bar_chart_var2),
                   'scale_y_continuous(labels = percent_format())')
    } else {
      nb_code <- c(nb_code,
                   sprintf('p <- ggplot(df, aes(x = %s)) +', uc$bar_chart_var1),
                   sprintf('geom_bar() + labs(x = "%s")', uc$bar_chart_var1))
    }

    if (length(unique(df[,uc$bar_chart_var1])) > 5) {
      if (!anyNA(suppressWarnings(as.numeric(df[, uc$bar_chart_var1])))) {
        nb_code <- c(
          nb_code,
          sprintf('p <- p + scale_x_discrete(breaks = pretty(as.numeric(as.character(df$%s)), n = 10))', uc$bar_chart_var1)
        )
      } else {
        nb_code <- c(
          nb_code,
          "p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))"
        )
      }
    }

    nb_code <- c(nb_code, "p", "```", " ", " ")

    nb_code
  })


  if(comp == "missing_values") return({
    nb_code <- c(
      "### Missing Values", " ",
      "```{r missing_values}",
      "df <- smp"
    )
    if (uc$missing_values_group_by != "All")
      nb_code <- c(nb_code,
                   sprintf('df <- df[df$%s == "%s",]',
                           uc$group_factor,
                           uc$missing_values_group_by))
    nb_code <- c(nb_code,
                 sprintf('prepare_missing_values_graph(df, "%s")', lts_id$name),
                 "```", " ", " ")

    nb_code
  })


  if(comp == "descriptive_table") return({
    nb_code <- c(
      "### Descriptive Statistics", " ",
      "```{r descriptive_statistics}",
      "df <- smp",
      "t <- prepare_descriptive_table(smp)",
      "t$kable_ret  %>%",
      '  kable_styling("condensed", full_width = F, position = "center")',
      "```", " ", " ")

    nb_code
  })


  if(comp == "histogram") return({
    nb_code <- c(
      "### Histogram", " ",
      "```{r histogram}"
    )

    if (uc$hist_group_by == "All")
      nb_code <- c(nb_code,
                   sprintf('var <- as.numeric(smp$%s)', uc$hist_var))
    else
      nb_code <- c(nb_code,
                   sprintf('var <- as.numeric(smp$%s[smp$%s == "%s"])',
                           uc$hist_var, uc$group_factor, uc$hist_group_by))

    nb_code <- c(nb_code,
                 sprintf('hist(var, main="", xlab = "%s", col="red", right = FALSE, breaks= %d)',
                         uc$hist_var, uc$hist_nr_of_breaks),
                 "```", " ", " ")

    nb_code
  })


  if(comp == "ext_obs") return({
    nb_code <- c(
      "### Extreme Observations", " ",
      "```{r extreme_obs}",
      "df <- smp")

    if (uc$group_factor != "None")
      nb_code <- c(nb_code,
                   sprintf('vars <- c("%s", "%s", "%s", "%s")',
                           lcs_id$name, lts_id$name, uc$ext_obs_var, uc$group_factor))
    else if (!cross_sec_data())
      nb_code <- c(nb_code,
                   sprintf('vars <- c("%s", "%s", "%s")',
                           lcs_id$name, lts_id$name, uc$ext_obs_var))
    else
      nb_code <- c(nb_code,
                   sprintf('vars <- c("%s", "%s")',
                           lcs_id$name, uc$ext_obs_var))

    if (uc$ext_obs_period_by != "All")
      nb_code <- c(nb_code,
                   sprintf('df <- df[df$%s == "%s", ]',
                           lts_id$name, uc$ext_obs_period_by))
    if (uc$ext_obs_group_by != "All")
      nb_code <- c(nb_code,
                   sprintf('df <- df[df$%s == "%s", ]',
                           uc$group_factor, uc$ext_obs_group_by))

    nb_code <- c(nb_code,
                 "df <- df[, vars]",
                 "df <- droplevels(df[complete.cases(df), ])",
                 "if (nrow(df) <= 10) {",
                 '  cat("Not enough data to generate table")',
                 "} else {",
                 sprintf('  tab <- prepare_ext_obs_table(df, var = "%s")', uc$ext_obs_var),
                 "  tab$kable_ret %>%",
                 "    kable_styling()",
                 "}",
                 "```", " ", " ")

    nb_code
  })


  if(comp == "by_group_bar_graph") return({
    nb_code <- c(
      "### By Group Bar Graph", " ",
      "```{r by_group_bar_graph}",
      "df <- smp")

    if (uc$bgbg_stat == "q25")
      nb_code <- c(nb_code,
                   'q25 <- function(x, na.rm) {quantile(x, 0.25, na.rm)}')
    if (uc$bgbg_stat == "q75")
      nb_code <- c(nb_code,
                   'q75 <- function(x, na.rm) {quantile(x, 0.75, na.rm)}')

    if (uc$bgbg_group_by != "All") {
      nb_code <- c(nb_code,
                   sprintf('df <- df[df$%s == "%s", ]',
                           uc$group_factor, uc$bgbg_group_by))
    }

    nb_code <- c(nb_code,
                 sprintf('prepare_by_group_bar_graph(df, "%s", "%s", %s, %s)$plot +',
                         uc$bgbg_byvar, uc$bgbg_var, uc$bgbg_stat, uc$bgbg_sort_by_stat),
                 sprintf('  ylab("%s %s")', uc$bgbg_stat, uc$bgbg_var),
                 "```", " ", " ")

    nb_code
  })


  if(comp == "by_group_violin_graph") return({
    nb_code <- c(
      "### By Group Violin Graph", " ",
      "```{r by_group_violin_graph}",
      "df <- smp")

    if (uc$bgvg_group_by != "All") {
      nb_code <- c(nb_code,
                   sprintf('df <- df[df$%s == "%s", ]',
                           uc$group_factor, uc$bgvg_group_by))
    }

    nb_code <- c(nb_code,
                 sprintf('prepare_by_group_violin_graph(df, "%s", "%s", %s)',
                         uc$bgvg_byvar, uc$bgvg_var, uc$bgvg_sort_by_stat),
                 "```", " ", " ")

    nb_code
  })


  if(comp == "trend_graph") return({
    nb_code <- c(
      "### Trend Graph", " ",
      "```{r trend_graph}",
      "df <- smp")

    vars <- c(uc$trend_graph_var1, uc$trend_graph_var2, uc$trend_graph_var3)
    vars <- vars[which(!vars %in% "None")]

    if (uc$trend_graph_group_by != "All") {
      nb_code <- c(nb_code,
                   sprintf('df <- df[df$%s == "%s", ]',
                           uc$group_factor, uc$trend_graph_group_by))
    }

    nb_code <- c(nb_code,
                 sprintf('prepare_trend_graph(df, "%s", c("%s"))$plot',
                         lts_id$name,
                         paste(vars, collapse = '", "')),
                 "```", " ", " ")

    nb_code
  })


  if(comp == "quantile_trend_graph") return({
    nb_code <- c(
      "### Quantile Trend Graph", " ",
      "```{r quantile_trend_graph}",
      "df <- smp")

    quantiles <- as.numeric(uc$quantile_trend_graph_quantiles)

    if (uc$quantile_trend_graph_group_by != "All") {
      nb_code <- c(nb_code,
                   sprintf('df <- df[df$%s == "%s", ]',
                           uc$group_factor, uc$quantile_trend_graph_group_by))
    }

    nb_code <- c(nb_code,
                 sprintf('prepare_quantile_trend_graph(df, "%s", c(%s), "%s")$plot',
                         lts_id$name, paste(quantiles, collapse = ", "),
                         uc$quantile_trend_graph_var),
                 "```", " ", " ")

    nb_code
  })


  if(comp == "by_group_trend_graph") return({
    nb_code <- c(
      "### By Group Trend Graph", " ",
      "```{r by_group_trend_graph}",
      "df <- smp")

    if (uc$bgtg_group_by != "All") {
      nb_code <- c(nb_code,
                   sprintf('df <- df[df$%s == "%s", ]',
                           uc$group_factor, uc$bgtg_group_by))
    }

    nb_code <- c(nb_code,
                 sprintf('prepare_by_group_trend_graph(df, "%s", "%s", "%s")$plot',
                         lts_id$name, uc$bgtg_byvar, uc$bgtg_var),
                 "```", " ", " ")

    nb_code
  })


  if(comp == "corrplot") return({
    nb_code <- c(
      "### Correlation Graph", " ",
      "```{r corrplot}",
      "df <- smp")

    if (uc$corrplot_group_by != "All") {
      nb_code <- c(nb_code,
                   sprintf('df <- df[df$%s == "%s", ]',
                           uc$group_factor, uc$corrplot_group_by))
    }

    vars <- which(sapply(df, function(x) all(is.numeric(x) | is.logical(x))))
    vars <- vars[vars %in% which(!sapply(df, function(x) all(is.na(x))))]
    vars <- vars[vars %in% which(!sapply(df, function(x) (all(duplicated(x)[-1]))))]

    nb_code <- c(nb_code,
                 sprintf('ret <- prepare_correlation_graph(df[, c(%s)])', paste(vars, collapse = ", ")),
                 "```", " ", " ")

    nb_code
  })


  if(comp == "scatter_plot") return({
    nb_code <- c(
      "### Scatter Plot", " ",
      "```{r scatter_plot}",
      "df <- smp")

    if (uc$scatter_group_by != "All") {
      nb_code <- c(nb_code,
                   sprintf('df <- df[df$%s == "%s", ]',
                           uc$group_factor, uc$scatter_group_by))
    }

    vars <- c(lcs_id$name, lts_id$name,
              uc$scatter_x, uc$scatter_y, uc$scatter_color, uc$scatter_size)
    vars <- vars[which(!vars %in% "None")]

    nb_code <- c(nb_code,
                 sprintf('df <- df[, c("%s")]', paste(vars, collapse = '", "')),
                 "df <- df[complete.cases(df), ]")

    scatter_df <- df[, vars]
    scatter_df <- scatter_df[complete.cases(scatter_df), ]

    if (uc$scatter_color %in% lfactor$name)
      nb_code <- c(nb_code,
                   sprintf('df$%s <- as.factor(df$%s)', uc$scatter_color, uc$scatter_color))
    if (uc$scatter_sample & (nrow(scatter_df) > 1000)) {
      nb_code <- c(nb_code,
                   "set.seed(42)",
                   "df <- sample_n(df, 1000)")
    }

    parm_str <- sprintf('"%s", "%s"', uc$scatter_x, uc$scatter_y)
    if (uc$scatter_color != "None") parm_str <- paste0(parm_str, sprintf(', color = "%s"', uc$scatter_color))
    if (uc$scatter_size != "None") parm_str <- paste0(parm_str, sprintf(', size = "%s"', uc$scatter_size))
    if (uc$scatter_loess) parm_str <- paste0(parm_str, ", loess = 1")

    nb_code <- c(nb_code,
                 sprintf('prepare_scatter_plot(df, %s)', parm_str),
                 "```", " ", " ")

    nb_code
  })


  if(comp == "regression") return({
    nb_code <- c(
      "### Regresssion Table", " ",
      "```{r regression}",
      "df <- smp")

    vars <- c(uc$reg_y, uc$reg_x, uc$reg_fe1, uc$reg_fe2, uc$reg_by)
    vars <- vars[!vars %in% "None"]

    nb_code <- c(nb_code,
                 sprintf('df <- df[, c("%s")]', paste(vars, collapse = '", "')),
                 "df <- df[complete.cases(df), ]")


    if (uc$reg_by != "None")
      nb_code <- c(nb_code,
                   sprintf('df$%s <- as.factor(df$%s)', uc$reg_by, uc$reg_by))
    if (!uc$reg_y %in% c(lnumeric$name, llogical$name))
      nb_code <- c(nb_code,
                   sprintf('df$%s <- as.factor(df$%s)', uc$reg_y, uc$reg_y))

    nb_code <- c(nb_code, "df <- droplevels(df)")


    feffect <- ""
    if (uc$reg_fe1 != "None") {
      feffect <- uc$reg_fe1
      if (uc$reg_fe2 != "None") feffect <- c(feffect, uc$reg_fe2)
    } else if (uc$reg_fe2 != "None") feffect <- uc$reg_fe2
    cluster <- ""
    if (uc$cluster == 2) cluster <- uc$reg_fe1
    if (uc$cluster == 3) cluster <- uc$reg_fe2
    if (uc$cluster == 4) cluster <- c(uc$reg_fe1, uc$reg_fe2)
    cluster <- cluster[!cluster %in% "None"]
    reg_by <- ifelse(uc$reg_by != "None", uc$reg_by, "")


    parm_str <- sprintf('df, dvs = "%s", idvs = c("%s")',  uc$reg_y, paste(uc$reg_x, collapse = '", "'))
    if (feffect[1] != "") parm_str <- paste0(parm_str,
                                          sprintf(', feffects = c("%s")', paste(feffect, collapse = '", "')))
    if (cluster[1] != "") parm_str <- paste0(parm_str,
                                          sprintf(', clusters = c("%s")', paste(cluster, collapse = '", "')))

    if (reg_by != "") parm_str <- paste0(parm_str,
                                         sprintf(', byvar = "%s"', uc$reg_by))

    parm_str <- paste0(parm_str, sprintf(', models = "%s"', uc$model))

    nb_code <- c(nb_code,
                 sprintf('t <- prepare_regression_table(%s)', parm_str),
                 "HTML(t$table)",
                 "```", " ", " ")

    nb_code
  })


  if(comp == "end_note") return({
    nb_code <- c(
      "### Note", " ",
      "This Notebook has been automatically generated using the [ExPanDaR](https://trr266.github.io/ExPanDaR) package.", " "
    )
  })

  return("")
}


output$nb_download <- downloadHandler(
  filename = "ExPanD_nb.zip",
  content = function(file) {
    nb <- c(
      "---",
      sprintf("title: %s", shiny_title),
      "output: html_notebook",
      "---", " ", " "
    )

    if (!is.null(shiny_abstract)) {
      nb <- c(nb,
              "### Abstract",
              shiny_abstract, " ", " "
      )
    }

    comp <- components()

    nb_blocks <- c("setup", "create_sample",
                   names(comp[!names(comp) %in%
                                c("sample_selection", "subset_factor",
                                  "grouping", "udvars")]),
                   "end_note")
    pos_html_blocks <- 0

    for (blk in nb_blocks) {
      if (blk == "html_block") {
        pos_html_blocks <- pos_html_blocks + 1
        nb <- c(nb, shiny_html_blocks[pos_html_blocks], rep(" ", 2))
      } else {
        comp_code <- create_nb_code_for_component(blk)
        if(length(comp_code) <= 1) {
          stop(sprintf('Encountered unknown notebook block: "%s"', blk))
        } else nb <- c(nb, comp_code)
      }
    }

    write(nb, file = "ExPanD_nb_code.Rmd", sep = "\n")

    nb_df <- create_ca_sample()
    nb_df_def <- cas_definition
    if (length(uc$udvars) != 0) {
      nb_df <- cbind(nb_df, udv_sample)
      nb_df_def <- rbind(nb_df_def, udv_definition)
    }
    nb_df_def <- nb_df_def[!nb_df_def$var_name %in% uc$delvars,]
    nb_df <- nb_df[, as.character(nb_df_def$var_name)]

    save(nb_df, nb_df_def, file = "ExPanD_nb_data.Rdata")
    zip::zipr(file, files = c("ExPanD_nb_code.Rmd", "ExPanD_nb_data.Rdata"))
  }
)
