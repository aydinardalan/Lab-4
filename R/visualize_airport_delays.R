#'Visualize airport delays
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom utils head
#' @importFrom methods new
#' @export

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

p1<- ggplot(data = as.data.frame(origin_delays)) + 
  geom_point(mapping=aes(x=lat, y=lon)) + 
  labs(title = "Flights Delay - Origin",
       x="Latitude",
       y="Longitude")  +
  theme_minimal() + 
  geom_label(mapping=aes(x=lat, y=lon), label=c(origin_delays)[[3]]
  ) 

p2<- ggplot(data= as.data.frame(origin_delays), aes(x= paste(c(origin_delays)[[3]]," - " ,c(origin_delays)[[4]]), y=avg_delay )) +
  geom_bar(stat="identity", fill="grey")+
  geom_text(aes(label=round(c(origin_delays)[[2]],2)), vjust=-0.3, size=3.5) + 
  theme_minimal() + labs(title = "Flights Delay - Origin",
                         x="Airports",
                         y="Average Delay, in minutes") 

p3<- ggplot(data = as.data.frame(destination_delays_head)) + 
  geom_point(mapping=aes(x=lat, y=lon)) + 
  labs(title = "Top 6 Flights Delayed - Destination",
       x="Latitude",
       y="Longitude")  +
  theme_minimal() + 
  geom_label(mapping=aes(x=lat, y=lon), label=c(destination_delays_head)[[3]]
  ) 

p4<- ggplot(data= as.data.frame(destination_delays_head), aes(x= c(destination_delays_head)[[3]], y=avg_delay )) +
  geom_bar(stat="identity", fill="grey")+
  geom_text(aes(label=round(c(destination_delays_head)[[2]],2)), vjust=-0.3, size=3.5) + 
  theme_minimal() + labs(title = " Top 6 Flights Delayed - Destination",
                         x="Airports",
                         y="Average Delay, in minutes") 

plist<-list(p1,p2,p3,p4)
return(plist)
}

utils::globalVariables(c("name", "faa", "lat", "lon", "avg_delay", "origin","dep_delay","dest","arr_delay"))