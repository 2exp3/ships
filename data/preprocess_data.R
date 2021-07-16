here::i_am("preprocess_data.R")

library(here)
library(data.table)
library(fst)
library(glue)
library(magrittr)
library(purrr)
library(dplyr)
library(bit64)
library(stringr)
library(lubridate)
library(ggthemes)

data_path = here::here("ships.fst")

if(file.exists(data_path)){
  ships = read_fst(data_path, as.data.table = TRUE)
}else{
  temp = tempfile(fileext = ".zip")
  download.file("https://drive.google.com/u/0/uc?id=1IeaDpJNqfgUZzGdQmR6cz2H3EQ3_QfCV&export=download", 
                mode = "wb", destfile =  temp)
  out = unzip(temp, exdir = tempdir())
  ships = fread(file = out)
  write_fst(ships, data_path)
}

## Vessel information from AIS signal----
# Details about datatypes:
  # https://www.marinetraffic.com/blog/information-transmitted-via-ais-signal/
  # http://emsa.europa.eu/cise-documentation/cise-data-model-1.5.3/model/guidelines/687507181.html
  # https://en.wikipedia.org/wiki/Automatic_identification_system

## Static data ----
  # SHIP_ID - ship’s unique identifier | MMSI (9-digit number) may change if ownership changes
  # FLAG - ship’s flag
  # LENGTH - ship’s length in meters
  # SHIPNAME - ship’s name | may change if ownership changes
  # SHIPTYPE - ship’s type
  # WIDTH - ship’s width in meters
  # DWT - ship’s deadweight in tones

## Dynamic data ----
  # Automatic
    # LAT - ship’s latitude  [+/-10 meters]
    # LON - ship’s longitude
    # SPEED - ship’s speed in knots (in 1/10 knot steps, >=102.2knot is 1022, NA is 1023)
    # COURSE - ship’s course as angle 
    # HEADING - ship’s compass direction (NA is 511)
    # DATETIME - date and time of the observation
    # Date - date extracted from DATETIME
    # week_nb - week number extracted from date
    # ship_type - ship’s type from SHIPTYPE
    # port - current port assigned based on the ship’s location
    # is_parked - indicator whether the ship is moving or not
  
  # Manual
    # DESTINATION - ship’s destination (reported by the crew)
    # PORT - current port reported by the vessel

# Data Pre-processing ----
# The unique identifier for a vessel is its ID.
# The unique identifier for a record for a given vessel is DATETIME
# set colors for each ship_type
type_colors = c(ggthemes_data$few$colors$Medium$value[-1],"black")

ships %>% 
  glimpse

ships_unique = 
  ships %>% 
  mutate(ID = as.character.integer64(SHIP_ID),# int64 to char to avoid issues with int64 class
         time = as_datetime(DATETIME)) %>%  # to datetime format
  distinct(ID, time, .keep_all = TRUE) %>%     # remove repeated records (duplicates) with same DATETIME by ID
  select(-c(SHIP_ID, DATETIME))

# The main interest is in whether a ship moved and the distance it sailed in that movement. 
# There might be 3 cases related to vessel movement:
  # -it only has 1 record, thus its movement is undefined because there is unsufficient data | UNDEFINED
  # -it is parked across all its records, thus there was likely no "intentional" ship movement | PARKED
  # -it moved one or more times across its records. These are the main focus here | MOVED
#  I create a new var (move_type) indicating these categories 

undefined = 
  ships_unique %>% 
  count(ID) %>% 
  filter(n == 1) %>% 
  pull(ID)

parked=
  ships_unique %>% 
  filter(!ID%in%undefined) %>% 
  group_by(ID) %>% 
  summarize(mpark = mean(is_parked)) %>% 
  filter(mpark==1) %>% 
  pull(ID)

ships_unique_move=
  ships_unique %>% 
  mutate(move_type = 
           case_when(ID %in% undefined ~ "undefined",
                     ID %in% parked ~ "parked",
                     TRUE ~ "moved"),
         type_color = factor(ship_type, labels = type_colors ),
         speed_kt = SPEED/10) # speed in knots
         


# As now the "parked" category has no relevant information regarding movement, 
# I will summarize each parked vessel records in one row (this is already the case for the undefined category).
# There is an extra challenge, since some vessel's static info is not actually static
# (some likely due to signal errors, when one version is much less frequent than the other, see Appendix at the end of script)
# A parsimonious criterion is to keep the most recent record, after fixing SHIPNAME typos 
# (keeping the most frequent fixed SHIPNAME)

parked_multiple_names =
  ships_unique_move %>% 
  filter(move_type=="parked") %>% 
  distinct(ID, SHIPNAME) %>% 
  count(ID) %>% 
  filter(n>1) %>% 
  pull(ID)  


ships_unique_move %>%
  filter(ID %in% parked_multiple_names) %>% 
  count(ID, SHIPNAME) 


ships_not_moving = 
  ships_unique_move %>% 
  filter(move_type!="moved") %>%
  mutate(fixed_name =
           case_when(SHIPNAME=="BBAS" ~ "ODYS",
                     TRUE ~ SHIPNAME)) %>% 
  group_by(ID) %>% 
  slice(which.max(time)) 


# Within the "moved" category, I am only interested in the records where the ship was actually moving 
# This might be across all records, but if not, I have to include the "start" and "end" of movement records 
# delimited by "is_parked" to account for any movement that may have happened in-between these records
# Again, I have to fix some names with errors

moving_multiple_names =
  ships_unique_move %>% 
  filter(move_type=="moved") %>% 
  distinct(ID, SHIPNAME) %>% 
  count(ID) %>% 
  filter(n>1) %>% 
  pull(ID)  


fixed_names = 
  ships_unique_move %>%
  filter(ID %in% moving_multiple_names) %>% 
  count(ID, SHIPNAME) %>%
  group_by(ID) %>% 
  slice(which.max(n)) %>% 
  ungroup %>% 
  mutate(fixed_name = 
           str_replace_all(SHIPNAME, "^(\\.)", "") %>% str_trim(.,"both")) %>% 
  select(ID, fixed_name)

# Compute the distance between consecutive records of the ships that are moving
get_distance = function(LAT1, LAT2, LON1, LON2){
  # Use Haversine formula to compute distance between 2 pairs of coordinates in meters
  p = pi/180
  a = 0.5 - cos((LAT2 - LAT1) * p)/2 + cos(LAT1 * p) * cos(LAT2 * p) * (1 - cos((LON2 - LON1) * p))/2
  return (12742 * asin(sqrt(a)) * 1000)
}

ships_moving =
  ships_unique_move %>% 
  filter(move_type =="moved")  %>%
  group_by(ID) %>%
  arrange(time, .by_group = TRUE) %>% # make sure records are ordered chronologically
  mutate(before    = is_parked - lag(is_parked),
         after     = is_parked - lead(is_parked),
         is_moving = before==1|after==1|is_parked==0) %>%
  filter(is_moving == TRUE) %>%
  left_join(.,fixed_names, .by= "ID") %>% 
  mutate(fixed_name = case_when(is.na(fixed_name) ~ SHIPNAME, TRUE ~ fixed_name),
         distance   = get_distance(LAT, lag(LAT), LON, lag(LON)), # Compute the distance between consecutive records of the ships that are moving
         obs_id     = 1:n()) %>% 
  ungroup()
  
# Although some ships had is_parked = 0 state, they are actually not moving (distance < 10 across all records)
false_moving =   
  ships_moving %>% 
  group_by(ID) %>% 
  summarize(m_dist = mean(distance < 10, na.rm=TRUE)) %>% # because AIS signal has an error of ~ +/- 10m
  filter(m_dist == 0)

ships_not_moving_false = 
  ships_moving %>% 
  mutate(move_type = case_when(ID%in%false_moving$ID ~ "parked",
                               TRUE ~ move_type)) %>% 
  filter(move_type == "parked") %>% 
  group_by(ID) %>% 
  slice(which.max(time)) %>% 
  bind_rows(ships_not_moving, .)


# Summarize moving ships with longest distance record and previous record 
# (I will use this data for the map) and join with non-moving ships
moving_ships_coords = 
  ships_moving %>% 
  mutate(move_type = case_when(ID%in%false_moving$ID ~ "parked",
                               TRUE ~ move_type)) %>% 
  filter(move_type != "parked") %>%
  group_by(ID) %>% 
  arrange(desc(distance), desc(time),.by_group = TRUE) %>%  # set longest distance (if more than 1 row, then most recent) as first row
  slice(c(which(obs_id == (obs_id[1] - 1) ),1)) %>%  # important: 2nd row is the end point
  mutate(
    type_color = as.character(type_color),
    type_color = replace(type_color, 1, "darkgrey")
    ) 


all_ships = 
  moving_ships_coords %>% 
  bind_rows(., ships_not_moving_false) %>% 
  select(ID, fixed_name, LAT, LON, distance, COURSE, 
         speed_kt, FLAG, LENGTH, WIDTH, DWT, 
         ship_type, type_color, port, time, move_type) 

# compute ranks for plot
ranks = 
  all_ships %>% 
  group_by(ID) %>% 
  slice(which.max(time)) %>% 
  group_by(ship_type) %>% 
  mutate(across(c(LENGTH, WIDTH, DWT, distance, speed_kt),
                ~rank(.x, na.last = "keep"),
                .names = "{col}_rank"),
         across(ends_with("rank"),
                ~round(.x)),
         across(ends_with("rank"),
                ~max(.x, na.rm = TRUE),
                .names = "{col}_max")
  ) %>% 
  ungroup() %>% 
  select(c(ends_with(c("rank","max")), ID))


ships_data = 
  all_ships %>%
  left_join(.,ranks, by = "ID") %>% 
  mutate(across(where(is.character), ~str_replace(.x, "NaN", "NA")),
         across(where(is.numeric), ~replace(.x, is.infinite(.x), NA)),
         distance = round(distance))


write_fst(ships_data, here::here("ships_data.fst"))






## Data "Oddities" ----
# Although MMSI is a 9-digit number, this database seems to have records of other ID types, or maybe corrupt MMSI IDs.
# In fact, no ID has 9-digits and most of the ship IDs have between 5 and 6 digits. 
# Nonetheless, this does not allow me to filter out data (since there is no clear criterion)
ships_unique %>%  
  distinct(ID) %>% # 1210 unique IDs
  mutate(n_digits = nchar(ID) ) %>% 
  group_by(n_digits) %>%
  summarize(N = n())

# It is reasonable to think that static data should be fairly constant for each ship ID, however:
# 1 ship "oceanograf" changed from tug to fishing on 2016-12-16. This is odd, but it may happen (as ships change ownership/purpose/refaction)
ships_unique %>% 
  filter(ID == ships_unique %>% 
           distinct(ID, ship_type) %>%
           count(ID) %>%
           filter(n>1) %>%
           pull(ID)) %>% 
  group_by(ship_type) %>% 
  summarize(date = range(time))

# 1 ship changed flag (and also name (ARGO to C)) on 2016-12-16. Again, this is odd, but it may happen (as ships change ownership)
ships_unique %>% 
  filter(ID== ships_unique %>% 
           distinct(ID, FLAG) %>%
           count(ID) %>%
           filter(n>1) %>% 
           pull(ID)) %>% 
  group_by(FLAG) %>% 
  summarize(dates = range(time))

# Structural info (lengt, width, deadweight) also changes in some cases (There is missing data for LENGTH, WIDTH, DWT)
# some seem like rounding artifacts (e.g. ID 151751), others are very different (e.g. ID 316784)
# But as date ranges overlap, this are very likely signal errors, 
ships_unique %>% 
  distinct(ID, LENGTH, WIDTH, DWT) %>%
  count(ID) %>%
  filter(n>1) %>% 
  pull(ID) -> id_LWDWT

ships_unique %>% 
  filter(ID %in% id_LWDWT) %>%
  group_by(ID,LENGTH, WIDTH, DWT) %>% 
  summarize(dates = range(time)) %>% 
  as.data.frame()

# Some ship IDs have more than one name. 
id_multiple_names =
  ships_unique %>% 
  distinct(ID, SHIPNAME) %>% 
  count(ID) %>% 
  filter(n>1) %>% 
  pull(ID)  

ships_unique %>%
  filter(ID %in% id_multiple_names) %>% 
  count(ID, SHIPNAME)

# This may be the actual case, but in some cases it seems these changes are unintentional
# some names have exra characters (e.g. .WLA-311), some have "typos" (e.g. WXA A SZCZESCIA)
# some have completely different names (e.g., ARGO->C which might have happened on 2016-12-16)
ships_unique %>% 
  filter(SHIP_ID==3653787) %>% 
  group_by(SHIPNAME) %>% 
  summarize(rn = range(time))


# most data around 1-2  or 120secs between records
# This is because vessels that are anchored or moving slowly transmit less frequently than those that are moving faster or are maneuvering
ships_unique %>% 
  filter(ID%in%parked)%>% 
  group_by(ID) %>% 
  summarise(
    sdSPEED = sd(SPEED), 
    sdLAT = sd(LAT),
    sdLON = sd(LON)) %>% 
  summary()