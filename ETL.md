ETL
================

Download the raw data
---------------------

We create a directory `~/Raw` to hold our working files This is a convention I've adopted for no good reason. Then we download the zipfile and unpack it.

``` r
dir.create("~/Raw", recursive = TRUE)
```

    ## Warning in dir.create("~/Raw", recursive = TRUE): '/home/znmeb/Raw' already
    ## exists

``` r
url <- 
  "https://s3.amazonaws.com/biketown-tripdata-public/BiketownPublicTripData201804.zip"
destfile <- "~/Raw/BiketownPublicTripData201804.zip"
download(url, destfile, quiet = TRUE)
unzip(destfile, exdir = "~/Raw", overwrite = TRUE)
```

Concatenate the files
---------------------

This is straightforward - we create an empty tibble, read each file with `read_csv` and append it to the tibble with `bind_rows`.

``` r
csv_files <- list.files("~/Raw/PublicTripData", full.names = TRUE) %>% 
  grep(pattern = ".csv", value = TRUE)
biketown_raw <- tibble()

for (file in csv_files) {
  biketown_raw <- biketown_raw %>% bind_rows(read_csv(
    file,
    progress = FALSE,
    col_types = cols(
      .default = col_character(),
      StartLatitude = col_double(),
      StartLongitude = col_double(),
      StartDate = col_date(format = "%m/%d/%Y"),
      StartTime = col_time(format = "%H:%M"),
      EndLatitude = col_double(),
      EndLongitude = col_double(),
      EndDate = col_date(format = "%m/%d/%Y"),
      EndTime = col_time(format = "%H:%M"),
      Distance_Miles = col_double())))
}
```

Clean the data
--------------

The raw data has a lot of NAs; some whole rows are totally empty. So we get rid of those rows.

``` r
biketown_cleaned <- biketown_raw %>% 
  filter(
    !is.na(StartLatitude),
    !is.na(EndLatitude),
    !is.na(Duration),
    !is.na(Distance_Miles))
```

We want to do some sanity checking, so we compute the trip durations in hours and the average velocity in miles per hour.

``` r
biketown_cleaned <- biketown_cleaned %>% mutate(
  duration_hours = as.numeric(
    as.duration(hms(Duration)), "hours"),
  mph = Distance_Miles / duration_hours)
```

Get rid of outliers
-------------------

Inspecting the data, there are some points that are clearly bogus. For one thing, nine trips end in the future - '2080-01-05'! Here's what they look like.

``` r
long_trips <- biketown_cleaned %>% 
  top_n(20, duration_hours) %>%
  mutate(days = duration_hours / 24) %>% 
  select(RouteID, duration_hours, days, StartDate, EndDate) %>% 
  arrange(desc(days))
options(tibble.print_max = 20, tibble.print_min = 20)
long_trips
```

    ## # A tibble: 20 x 5
    ##    RouteID duration_hours     days StartDate  EndDate   
    ##    <chr>            <dbl>    <dbl> <date>     <date>    
    ##  1 2475238       550617.  22942.   2017-03-14 2080-01-05
    ##  2 2596071       550041.  22918.   2017-04-07 2080-01-05
    ##  3 2674282       549764.  22907.   2017-04-18 2080-01-05
    ##  4 3580600       547725.  22822.   2017-07-12 2080-01-05
    ##  5 3897287       547186.  22799.   2017-08-04 2080-01-05
    ##  6 3972142       547049.  22794.   2017-08-10 2080-01-05
    ##  7 4433814       546260.  22761.   2017-09-11 2080-01-05
    ##  8 4498302       546155.  22756.   2017-09-16 2080-01-05
    ##  9 4577375       546002.  22750.   2017-09-22 2080-01-05
    ## 10 2719516          191.      7.97 2017-04-25 2017-05-03
    ## 11 4611006           97.2     4.05 2017-09-25 2017-09-29
    ## 12 3240778           96.9     4.04 2017-06-17 2017-06-21
    ## 13 4296715           87.5     3.64 2017-09-01 2017-09-05
    ## 14 1478741           81.3     3.39 2016-08-19 2016-08-22
    ## 15 1429170           70.4     2.93 2016-08-11 2016-08-14
    ## 16 2700033           68.8     2.87 2017-04-22 2017-04-25
    ## 17 3445706           65.6     2.73 2017-07-02 2017-07-05
    ## 18 1711196           60.8     2.53 2016-09-18 2016-09-21
    ## 19 5085413           56.8     2.36 2017-11-05 2017-11-08
    ## 20 4079752           54.9     2.29 2017-08-17 2017-08-20

I'll have more to say about that 8-day trip (RouteID 2719516) later. For now, just filter out the trips that end in the future.

``` r
biketown_cleaned <- biketown_cleaned %>% filter(EndDate != '2080-01-05')
```

Now let's look at the distances. Or rather, the velocities. Were there trips that required superhuman pedaling?

``` r
high_velocities <- biketown_cleaned %>% 
  top_n(20, mph) %>% 
  select(RouteID, mph, Distance_Miles, duration_hours) %>% 
  arrange(desc(mph))
high_velocities
```

    ## # A tibble: 20 x 4
    ##    RouteID     mph Distance_Miles duration_hours
    ##    <chr>     <dbl>          <dbl>          <dbl>
    ##  1 5039933 50909.         5246.           0.103 
    ##  2 4298322 40811.         5249.           0.129 
    ##  3 1819628 32137.         5249.           0.163 
    ##  4 5941795 30625.         5249.           0.171 
    ##  5 3124476 26172.         5249.           0.201 
    ##  6 2309921 19927.         5247.           0.263 
    ##  7 1374716 17998.         5250.           0.292 
    ##  8 3903517 16807.         5247.           0.312 
    ##  9 1376084  5945.         5253.           0.884 
    ## 10 3113184   733.         5258.           7.17  
    ## 11 1362610   470.         2587.           5.50  
    ## 12 5231953   251.            4.53         0.0181
    ## 13 2712719    79.1        1430.          18.1   
    ## 14 4091034    65.9           1.41         0.0214
    ## 15 2302115    64.4           1.77         0.0275
    ## 16 5052003    57.4           1.18         0.0206
    ## 17 1722700    56.4           1.05         0.0186
    ## 18 3164907    49.9           1.76         0.0353
    ## 19 1341803    49.3           1.63         0.0331
    ## 20 3317091    45.2           4.21         0.0931

Those distances around 5250 miles are very suspicious. There are 5280 feet in a mile; perhaps the bike was measuring feet instead of miles, or somebody left a divide out in some code. Who knows?

What about that trip of 1429.65 miles? I'd buy a burst of 79 MPH downhill, but sustained over 18 hours? Nope! A side note on that trip - it starts at Director Park in downtown Portland and ends - wait for it - in the Pacific Ocean, west of Vancouver Island, British Columbia, Canada. Nope!

I'll buy a burst of 66 MPH in a minute and a half. So we'll insist that trips average under 70 MPH.

``` r
biketown_cleaned <- biketown_cleaned %>% filter(mph < 70)
```

Make up hub names for the ones that have "NA"
---------------------------------------------

Some of the trips don't start or end at a hub. They'll have "NA" for a hub name. So we create a hub name from the longitude and latitude and use that.

``` r
.start_coords <- paste(
  "GPS(",
  biketown_cleaned$StartLongitude,
  ",",
  biketown_cleaned$StartLatitude,
  ")", sep = ""
)
.end_coords <- paste(
  "GPS(",
  biketown_cleaned$EndLongitude,
  ",",
  biketown_cleaned$EndLatitude,
  ")", sep = ""
)
biketown_cleaned$StartHub <- ifelse(
  is.na(biketown_cleaned$StartHub),
  .start_coords,
  biketown_cleaned$StartHub
)
biketown_cleaned$EndHub <- ifelse(
  is.na(biketown_cleaned$EndHub),
  .end_coords,
  biketown_cleaned$EndHub
)
```

Make table of unique end points
-------------------------------

The geospatial processing we want to do on this dataset requires a list of the end points and their coordinates. This chunk creates that table.

``` r
.start_hubs <- biketown_cleaned %>% select(
  hub = StartHub,
  longitude = StartLongitude,
  latitude = StartLatitude
) %>% unique()
.end_hubs <- biketown_cleaned %>% select(
  hub = EndHub,
  longitude = EndLongitude,
  latitude = EndLatitude
) %>% unique()
biketown_hubs <- bind_rows(.start_hubs, .end_hubs) %>% unique()

biketown_hubs_sp <- SpatialPointsDataFrame(
    coords = select(biketown_hubs, longitude, latitude),
    data = biketown_hubs,
    proj4string = CRS("+init=epsg:4326")
  )
biketown_hubs_sf <- st_as_sf(biketown_hubs_sp)
```
