#'Visualize airport delays
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_label
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_blank
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
#' @importFrom dplyr arrange
#' @importFrom dplyr distinct
#' @import nycflights13
#' @importFrom methods new
#' @export visualize_airport_delays

visualize_airport_delays <- function () {
  
  flights_origin <- select(nycflights13::flights, origin, dep_delay)
  flights_destination <- select(nycflights13::flights, dest, arr_delay)
  
  #add new column faa to origin and destination
  flights_origin2<- mutate(flights_origin, faa = origin)
  flights_destination2<- mutate(flights_destination, faa = dest)
  airports_treated <- select(nycflights13::airports, name, faa, lat, lon)

  joined_data_ori <-left_join(flights_origin2, airports_treated, by = "faa")
  joined_data_dest <-left_join(flights_destination2, airports_treated, by = "faa")
  
  #remove negative values(not delayed)
  joined_data_ori2 <- filter(joined_data_ori, dep_delay >0 )
  joined_data_dest2 <- filter(joined_data_dest, arr_delay >0 )
  
  #delay mean
  group_ori_mean <- joined_data_ori2 %>%
    group_by(origin) %>%
    summarise(avg_delay = mean(dep_delay) )
  
  group_dest_mean <- joined_data_dest2 %>% group_by(dest) %>% summarise(avg_delay =mean(arr_delay) )
  
  # join info again to get airport info (and exclude dep_delay)
  origin_delays <- arrange((as.data.frame(distinct(left_join(group_ori_mean, select(joined_data_ori2, - dep_delay), by = "origin")))),desc(avg_delay) )
  
  # join info again to get airport info (and exclude arr_delay)
  destination_delays <-distinct(left_join(group_dest_mean, select(joined_data_dest2, - arr_delay), by = "dest"))
  destination_delays_head<-head(arrange(destination_delays,desc(avg_delay) ))
}