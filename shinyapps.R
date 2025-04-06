# Code to generate the shinyapps.io applications
# Deploy on MacOS (seems to be less prone to stupid locale and enconding issues)
devtools::install_github("trr266/ExPanDaR")

# --- expand -------------------------------------------------------------------

library(ExPanDaR)

ExPanD(export_nb_option = TRUE)

# expand_r3
ExPanD(
  df = russell_3000, df_def = russell_3000_data_def,
  config_list = ExPanD_config_russell_3000,
  abstract = paste(
    "The data for this sample has been collected from Google Finance",
    "and Yahoo Finance."
  ),
  export_nb_option = TRUE
)

# if you need a new config file - Save from within ExPanD and then

if (FALSE) {
  ExPanD_config_russell_3000 <- readRDS("~/Downloads/ExPanD.RDS")
  save(
    ExPanD_config_russell_3000, file = "data/ExPanD_config_russell_3000.RData",
    version = 2
  )
}

# --- expand_wb ----------------------------------------------------------------

ExPanD(
  worldbank, df_def = worldbank_data_def, var_def = worldbank_var_def,
  config_list = ExPanD_config_worldbank, df_name = "World Bank Data",
  abstract = paste(
    "The data for this sample has been collected using the",
    "<a href=https://data.worldbank.org>World Bank API</a>."
  ),
  export_nb_option = TRUE
)

# --- expand_fuel_economy ------------------------------------------------------

# See https://joachim-gassen.github.io/2019/04/customize-your-interactive-eda-explore-the-fuel-economy-of-the-u.s.-car-market/
# for more info

# The following two chuncks borrow
# from the raw data code of the
# fueleconomy package by Hadley Wickham,
# See: https://github.com/hadley/fueleconomy

library(tidyverse)
library(ExPanDaR)

if(!file.exists("vehicles.csv")) {
        tmp <- tempfile(fileext = ".zip")
        download.file("http://www.fueleconomy.gov/feg/epadata/vehicles.csv.zip",
                      tmp, quiet = TRUE)
        unzip(tmp, exdir = ".")
}

raw <- read.csv("vehicles.csv", stringsAsFactors = FALSE)
countries <- read.csv("https://joachim-gassen.github.io/data/countries.csv",
                      stringsAsFactors = FALSE)

vehicles <- raw %>%
        mutate(car = paste(make, model, trany),
               mpg_hwy = ifelse(highway08U > 0, highway08U, highway08),
               mpg_city = ifelse(city08U > 0, city08U, city08)) %>%
        left_join(countries) %>%
        select(car, make, country, trans = trany,
               year,
               class = VClass, drive = drive, fuel = fuelType,
               cyl = cylinders, displ = displ,
               mpg_hwy, mpg_city) %>%
        filter(drive != "",
               year > 1985,
               year < 2020) %>%
        mutate(fuel = case_when(
                fuel == "CNG" ~ "gas",
                fuel == "Gasoline or natural gas" ~ "hybrid_gas",
                fuel == "Gasoline or propane" ~ "hybrid_gas",
                fuel == "Premium and Electricity" ~ "hybrid_electro",
                fuel == "Premium Gas or Electricity" ~ "hybrid_electro",
                fuel == "Premium Gas and Electricity" ~ "hybrid_electro",
                fuel == "Regular Gas or Electricity" ~ "hybrid_electro",
                fuel == "Electricity" ~ "electro",
                fuel == "Diesel" ~ "diesel",
                TRUE ~ "gasoline"
        ),
        class = case_when(
                grepl("Midsize", class) ~ "Normal, mid-size",
                grepl("Compact", class) ~ "Normal, compact",
                grepl("Small Station Wagons", class) ~ "Normal, compact",
                grepl("Large Cars", class) ~ "Normal, large",
                grepl("Minicompact", class) ~ "Normal, sub-compact",
                grepl("Subcompact", class) ~ "Normal, sub-compact",
                grepl("Two Seaters", class) ~ "Two Seaters",
                grepl("Pickup Trucks", class) ~ "Pickups",
                grepl("Sport Utility Vehicle", class) ~ "SUVs",
                grepl("Special Purpose Vehicle", class) ~ "SUVs",
                grepl("Minivan", class) ~ "(Mini)vans",
                grepl("Vans", class) ~ "(Mini)vans"
        ),
        drive = case_when(
                grepl("4-Wheel", drive) ~ "4-Wheel Drive",
                grepl("4-Wheel", drive) ~ "4-Wheel Drive",
                grepl("All-Wheel", drive) ~ "4-Wheel Drive",
                grepl("Front-Wheel", drive) ~ "Front-Wheel Drive",
                grepl("Rear-Wheel", drive) ~ "Rear-Wheel Drive"
        ),
        trans = case_when(
                grepl("Automatic", trans) ~ "Automatic",
                grepl("Manual", trans) ~ "Manual"
        )) %>%
        na.omit()

df_def <- data.frame(
        var_name = names(vehicles),
        var_def = c("Make, model and transition type indentifying a unique car in the data",
                    "Make of car",
                    "Country where car producing firm is loacted",
                    "Transition type (automatic or manual)",
                    "Year of data",
                    "Classification type of car (simplified from orginal data)",
                    "Drive type of car (Front Wheel, Rear Wheel or 4 Wheel)",
                    "Fuel type (simplified from orginal data)",
                    "Number of engine cylinders",
                    "Engine displacement in liters",
                    "Highway miles per gallon (MPG). For electric and CNG vehicles this number is MPGe (gasoline equivalent miles per gallon).",
                    "City miles per gallon (MPG). For electric and CNG vehicles this number is MPGe (gasoline equivalent miles per gallon)."),
        type = c("cs_id", rep("factor", 3), "ts_id", rep("factor", 3), rep("numeric", 4))
)

html_blocks <- c(
        paste("<div class='col-sm-12'>",
              "By default, this display uses all data from car makes with more",
              "than 100 cars in the 'fueleconomy.gov' database.",
              "Above, you can limit the analysis to cars from a certain make,",
              "class, country, fuel type or other factor present in the data.",
              "</div>"),
        paste("<div class='col-sm-12'>",
              "In the display above, remove the check mark to see the absolute",
              "number of cars included in the data each year.",
              "Also, change the additional factor to see how the distribution",
              "of cars across countries, transition types, etc. changes over time",
              "</div>"),
        paste("<div class='col-sm-12'>",
              "In the two tables above, you can assess the distributions of the",
              "four numerical variables of the data set. Which car has the",
              "largest engine of all times?",
              "</div>"),
        paste("<div class='col-sm-12'>",
              "Explore the numerical variables across factors. You will see,",
              "not surprisingly, that fuel economy varies by car class.",
              "Does it also vary by drive type?",
              "</div>"),
        paste("<div class='col-sm-12'>",
              "The above panels contain good news. Fuel economy has",
              "increased over the last ten years. See for yourself:",
              "Has the size of engines changed as well?",
              "</div>"),
        paste("<div class='col-sm-12'>",
              "The scatter plot documents a clear link between engine size",
              "and fuel economy in term of miles per gallon.",
              "Below, you can start testing for associations.",
              "</div>"),
        paste("<div class='col-sm-12'>",
              "Probably, you will want to test for some associations that",
              "require you to construct new variables. No problem. Just enter the",
              "variable definitions above. Some ideas on what to do:",
              "<ul><li>Define country dummies (e.g., country == 'US') to see",
              "whether cars from certain countries are less fuel efficient than others.</li>",
              "<li>Define a dummy for 4-Wheel drive cars to assess the penalty",
              "of 4-Wheel drives on fuel economy.</li>",
              "<li>If you are from a metric country, maybe your are mildly annoyed",
              "by the uncommon way to assess fuel economy via miles per gallon.",
              "Fix this by defining a liter by 100 km measure",
              "(hint: 'l100km_hwy := 235.215/mpg_hwy').</li></ul>",
              "</div>"),
        paste("<div class='col-sm-12'>",
              "Above, you can play around with certain regression parameters.",
              "See how robust coefficients are across car classes by estimating",
              "the models by car class ('subset' option).",
              "Try a by year regression to assess the development of fuel economy",
              "over time. <br> <br>",
              "If you like your analysis, you can download a zipfile containing",
              "the data and an R notebook reporting the analysis. Alternatively,",
              "you can store the ExPanD configuration and reload it at a later",
              "stage.",
              "</div>")
)

cl <- list(
        ext_obs_period_by = "2019",
        bgbg_var = "mpg_hwy",
        bgvg_var = "mpg_hwy",
        scatter_loess = FALSE,
        delvars = NULL,
        scatter_size = "cyl",
        bar_chart_relative = TRUE,
        reg_x = c("cyl", "displ", "trans"),
        scatter_x = "displ",
        reg_y = "mpg_hwy",
        scatter_y = "mpg_hwy",
        bgvg_byvar = "class",
        quantile_trend_graph_var = "mpg_hwy",
        bgtg_var = "mpg_hwy",
        bgtg_byvar = "class",
        bgbg_byvar = "country",
        scatter_color = "country", bar_chart_var2 = "class",
        ext_obs_var = "mpg_hwy",
        trend_graph_var1 = "mpg_hwy",
        trend_graph_var2 = "mpg_city",
        sample = "vehicles"
)

abstract <- paste(
        "This interactive display features the",
        "<a href=https://www.fueleconomy.gov/>",
        "fuel economy data provided by the U.S. Environmental Protection Agency.</a>",
        "It allows you to explore the fuel economy of cars in the U.S. market",
        "across time and other dimensions.",
        "<br>&nbsp;<br>",
        "It is based on the 'ExPanD' display provided by the",
        "<a href=https://trr266.github.io/ExPanDaR>'ExPanDaR' package</a>.",
        "Click <a href=https://jgassen.shinyapps.io/expand>here</a> to explore your",
        "own data with 'ExPanD'.",
        "<br>&nbsp;<br>",
        "Otherwise: Scroll down and start exploring!"
)

ExPanD(vehicles, df_def = df_def, config_list = cl,
       title = "Explore the Fuel Economy of Cars in the U.S. Market",
       abstract = abstract,
       components = c(subset_factor = TRUE,
                      html_block = TRUE,
                      bar_chart = TRUE,
                      html_block = TRUE,
                      descriptive_table = TRUE,
                      ext_obs = TRUE,
                      html_block = TRUE,
                      by_group_bar_graph = TRUE,
                      by_group_violin_graph = TRUE,
                      html_block = TRUE,
                      trend_graph = TRUE,
                      quantile_trend_graph = TRUE,
                      by_group_trend_graph = TRUE,
                      html_block = TRUE,
                      scatter_plot = TRUE,
                      html_block = TRUE,
                      udvars = TRUE,
                      html_block = TRUE,
                      regression = TRUE,
                      html_block = TRUE),
       html_blocks = html_blocks,
       export_nb_option = TRUE
)


# --- expand_gapminder ---------------------------------------------------------

library(ExPanDaR)
library(gapminder)
data(gapminder)

df_def <- data.frame(
        var_name = names(gapminder),
        var_def = c("Name of the country",
                    "Continent where country is located",
                    "Year of data",
                    "Life expectancy in years at birth",
                    "Population in million",
                    "Gross Domestic Product (GDP) per capita"),
        type = c("cs_id", "factor", "ts_id", rep("numeric", 3)),
        stringsAsFactors = FALSE
)

gapminder$pop <- gapminder$pop / 1e6

clist <- list(
        scatter_x = "gdpPercap",
        scatter_y = "lifeExp",
        scatter_size = "pop",
        scatter_color = "continent",
        scatter_loess = TRUE,
        scatter_sample = FALSE,

        reg_y = "lifeExp",
        reg_x = "gdpPercap",
        reg_fe1 = "country",
        reg_fe2 = "year",
        cluster = "4" # No this is hard to guess 1: none, 2: first FE, 3: second FE, 4: both FE
)

html_blocks <- c(
        paste('<div class="col-sm-2"><h3>Variation of life expectancy',
              "across regions and income levels</h3></div>",
              '<div class="col-sm-10">',
              "<p>&nbsp;</p>As you see below, life expectancy varies widely",
              "across countries and continents. One potential reason for this",
              "variation is the difference in income levels across countries.",
              "This association is visualized by the",
              "<a href=https://en.wikipedia.org/wiki/Preston_curve>",
              "Preston Curve</a> that you also find below.",
              "</div>"),
        paste('<div class="col-sm-2"><h3>Transform variables</h3></div>',
              '<div class="col-sm-10">',
              "The Preston Curve is far from",
              "linear. Maybe you can come up with a transformation",
              "of GDP per capita that makes the association",
              "a little bit more well behaved?",
              "Use the dialog below to define a transformed",
              "measure of GDP per capita and assess its association",
              "with life expectancy in the scatter plot above.",
              "</div>"),
        paste('<div class="col-sm-2"><h3>Assess Robustness</h3></div>',
              '<div class="col-sm-10">',
              "You see below that the linear regression coefficient",
              "for GDP per capita is <i>negative</i>",
              "and signficant in a panel model with country and year",
              "fixed effects.",
              "Does this also hold when you use a log-transformed version",
              "of GDP per capita?",
              "</div>")
)

ExPanD(df = gapminder,
       title = "Explore the Preston Curve",
       abstract = paste("This interactive display uses 'gapminder' data to",
                        "let you explore the Preston Curve. Scroll down and enjoy!"),
       components = c(descriptive_table = TRUE,
                      html_block = TRUE,
                      by_group_violin_graph = TRUE,
                      scatter_plot = TRUE,
                      html_block = TRUE,
                      udvars = TRUE,
                      html_block = TRUE,
                      regression = TRUE),
       df_def = df_def,
       config_list = clist,
       html_blocks = html_blocks)


