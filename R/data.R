#' Annual Financial Accounting and Stock Return Data for a Sample of Russell 3000 Firms (2013-2016)
#'
#' Data collected from Google Finance and Yahoo finance using the package \code{tidyquant}.
#'
#' @docType data
#'
#' @usage data(russell_3000)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @keywords data datasets acccounting
#'
#' @source Has been collected using the \code{tidyquant::tq_get} function family in Summer 2017.
#'  The code to generate this data is available in the
#'  \href{https://github.com/trr266/ExPanDaR}{github repository} of this package.
#'  As the Google Finance API providing financial statement data is currently unavailable,
#'  the data cannot be replicated by running the code.
#'  Use in scientific studies is not advised without prior cleaning/checking.
#'
#' @examples
#' data(russell_3000)
#' prepare_missing_values_graph(russell_3000, ts_id = "period")
#'
"russell_3000"

#' Data Definitions for \code{russell_3000} Data Set
#'
#' A data frame containing variable definitions for the \code{russell_3000} data set.
#' The data definitions can be passed to \link{ExPanD} via the
#' \code{df_def} parameter.
#'
#' Data definitions are provided by the package maintainer and are somewhat superficial
#' to make them both, short and informative. User discretion is advised when using this
#' data outside of its didactic purpose.
#'
#' @docType data
#'
#' @usage data(russell_3000_data_def)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @examples
#' data(russell_3000)
#' data(russell_3000_data_def)
#' data(ExPanD_config_russell_3000)
#' \dontrun{
#'   ExPanD(russell_3000, df_def = russell_3000_data_def, config_list = ExPanD_config_russell_3000)
#' }
"russell_3000_data_def"


#' Default Configuration to use with ExPanD and the Russell 3000 Data Set
#'
#' List to use as a \code{list_config} parameter when starting \link{ExPanD}.
#'
#' @docType data
#'
#' @usage data(ExPanD_config_russell_3000)
#'
#' @format An object of class \code{"list"}.
#'
#' @examples
#' data(russell_3000)
#' data(russell_3000_data_def)
#' data(ExPanD_config_russell_3000)
#' \dontrun{
#'   ExPanD(russell_3000, df_def = russell_3000_data_def, config_list = ExPanD_config_russell_3000)
#' }
"ExPanD_config_russell_3000"

#' A Snapshot of Macroeconomic Data as Provided by the World Bank API (1960 - 2018)
#'
#' Data collected from the World Bank API using the package \code{wbstats}.
#'
#' @docType data
#'
#' @usage data(worldbank)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @keywords data datasets macroeconomic
#'
#' @source Has been collected using the \code{wbstats::wb()} function
#'  from the World Bank API in Dec 2020.
#'  The code to generate this data is available in the
#'  \href{https://github.com/trr266/ExPanDaR}{github repository} of this package.
#'  Use in scientific studies is not advised without prior cleaning/checking.
#'
#' @examples
#' data(worldbank)
#' prepare_missing_values_graph(worldbank, ts_id = "year")
#'
#' data(worldbank_data_def)
#' data(worldbank_var_def)
#' data(ExPanD_config_worldbank)
#' \dontrun{
#'   ExPanD(worldbank, df_def = worldbank_data_def,
#'     var_def = worldbank_var_def, config_list = ExPanD_config_worldbank)
#' }
#'
"worldbank"

#' Data Definitions for \code{worldbank} Data Set
#'
#' A data frame containing variable definitions for the \code{worldbank} data set.
#' The data definitions can be passed to \code{\link{ExPanD}} via the
#' \code{df_def} parameter.
#'
#' Data definitions are as provided by the World Bank API and
#' the code to generate them is available in the
#'  \href{https://github.com/trr266/ExPanDaR}{github repository}
#'  of this package.
#'
#' @docType data
#'
#' @usage data(worldbank_data_def)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @examples
#' data(worldbank)
#' data(worldbank_data_def)
#' data(worldbank_var_def)
#' data(ExPanD_config_worldbank)
#' \dontrun{
#'   ExPanD(worldbank,df_def = worldbank_data_def,
#'     var_def = worldbank_var_def, config_list = ExPanD_config_worldbank)
#' }
"worldbank_data_def"

#' Variable Definitions to Construct an Analysis Sample Based on the \code{worldbank} Data Set
#'
#' A data frame containing variable definitions that can be passed to \code{\link{ExPanD}}
#' via the var_def parameter.
#'
#' @docType data
#'
#' @usage data(worldbank_var_def)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @examples
#' data(worldbank)
#' data(worldbank_data_def)
#' data(worldbank_var_def)
#' data(ExPanD_config_worldbank)
#' \dontrun{
#'   ExPanD(worldbank, df_def = worldbank_data_def,
#'     var_def = worldbank_var_def, config_list = ExPanD_config_worldbank)
#' }
"worldbank_var_def"

#' Default Configuration to Use with ExPanD and the \code{worldbank} Data Set
#'
#' List to use as a \code{list_config} parameter when starting \link{ExPanD}.
#'
#' @docType data
#'
#' @usage data(ExPanD_config_worldbank)
#'
#' @format An object of class \code{"list"}.
#'
#' @examples
#' data(worldbank)
#' data(worldbank_data_def)
#' data(worldbank_var_def)
#' data(ExPanD_config_worldbank)
#' \dontrun{
#'   ExPanD(worldbank, df_def = worldbank_data_def,
#'     var_def = worldbank_var_def, config_list = ExPanD_config_worldbank)
#' }
"ExPanD_config_worldbank"
