library(data.table)
library(tidyverse)
library(readxl)
library(lubridate)

##### Reading each file with sample IDs that was created each time when using pXRF #####
xrf_id_22aug <-read_excel(
  "data/York_Grab_pXRF_IDs/York_Grab_pXRF_SampleID_22Aug19.xlsx",
  sheet = "22augID")

xrf_id_23aug <-read_excel(
  "data/York_Grab_pXRF_IDs/York_Grab_pXRF_SampleID_23Aug19.xlsx",
  sheet = "23augID")

xrf_id_26aug <-read_excel(
  "data/York_Grab_pXRF_IDs/York_Grab_pXRF_SampleID_26Aug19.xlsx",
  sheet = "26augID")

xrf_id_27aug <-read_excel(
  "data/York_Grab_pXRF_IDs/York_Grab_pXRF_SampleID_27Aug19_gp.xlsx",
  sheet = "27augID")

xrf_id_29aug <-read_excel(
  "data/York_Grab_pXRF_IDs/York_Grab_pXRF_SampleID_29Aug19.xlsx",
  sheet = "29augID")
#------------------------------------------------------------------------------------#

##### Join all sample IDs files into "all_id" #####
all_id <- bind_rows(xrf_id_22aug, xrf_id_23aug, xrf_id_26aug, xrf_id_27aug, xrf_id_29aug)

all_id %>%
  mutate(Trunc = str_to_upper(str_replace(Trunc, "^1", "01"))) %>% 
  rename(TRUNC_ID = Trunc) %>% 
  mutate(Date = as_date(Date),
         `SCANNING_CODE` = paste0(month(Date, label = T, abbr = T),
                                  " ",
                                  day(Date),
                                  "-",
                                  `Reading_No`)) %>% 
  select(TRUNC_ID, GENPRINT, SCANNING_CODE, STEM_ID, SUBSAMPLE_ID, SUBSAMPLE, Remarks) %>% 
  distinct(TRUNC_ID, GENPRINT, SCANNING_CODE, STEM_ID, SUBSAMPLE_ID, SUBSAMPLE, Remarks) %>% 
  filter(is.na(Remarks), !is.na(GENPRINT)) ->
  all_id_select

  view(all_id_select)

  write_csv(all_id_select, "data/clean_data/all_pxrf_id.csv")
  
#####################################################################################################  
# Another way of creating new column heading: Adding a new column "reading_id" with the number
# format "000" and concatenating with date
# all_id_select %>% mutate(SCANNING_CODE = str_pad(Reading_No, 3, "left", "0")) %>% 
# unite(reading_id, Date, reading_id, sep = "-") %>% 
#  select(reading_id, SUBSAMPLE_ID, STEM_ID, GENPRINT, SUBSAMPLE) %>% 
 

#-----------------------------------------------------------------------------------------------#
## temp <- data.table::fread("data/pXRF_Data_original/chemistry-803819-2019-08-22-16-49-44.csv")

## temp %>%
##  rename(Reading_No = `Reading #`)
#-----------------------------------------------------------------------------------------------#
#####################################################################################################
  
  
##### Reading each file with pXRF chemistry data #####

xrf_files <- list.files(path = "data/pXRF_Data_original/", pattern = "chemistry", full.names = T)
xrf_files

xrf_list <- lapply(xrf_files, data.table::fread)

names(xrf_list) <- list.files(path = "data/pXRF_Data_original/", pattern = "chemistry")

xrf_raw <- data.table::rbindlist(xrf_list, idcol = T, fill = TRUE)

names(xrf_raw)

# Removing the objects "xrf_files" abd "xrf_list" from the Global Environment
rm(xrf_files, xrf_list)


nrow(distinct(xrf_raw, `Reading #`, Date)) #looks like there are duplicate records in different
# excel files; we can ensure they are true duplicates, rather than label duplications, 
# by using distinct() on the date frame once the .id is dropped.

xrf_raw%>%
  mutate(Date = as_date(Date),
         SCANNING_CODE = paste0(month(Date, label = T, abbr = T),
                                " ",
                                day(Date),
                                "-",
                                `Reading #`))%>%
  select(SCANNING_CODE, ends_with("Concentration"), ends_with("Error1s")) %>% # Selecting only the columns containing "Concentration" and "Errors1s"
  distinct()-> # removes true duplicates
  xrf_data
view(xrf_data)

write_csv(xrf_data, "data/clean_data/all_pxrf_chem.csv")

rm(xrf_raw)

######################################################################################################
#------------------------------------------------------------------------------------#
# chem_xrf_22aug <- read_csv("data/pXRF_Data/chemistry-803819-2019-08-22-16-49-44.csv")

# chem_xrf_222aug <- read_csv("data/pXRF_Data/chemistry-803819-2019-08-22-12-33-41.csv")

# chem_xrf_23aug <- read_csv("data/pXRF_Data/chemistry-803819-2019-08-23-12-46-47.csv")

# chem_xrf_26aug <- read_csv("data/pXRF_Data/chemistry-803819-2019-08-26-16-11-38.csv")

# chem_xrf_27aug <- read_csv("data/pXRF_Data/chemistry-803819-2019-08-27-16-23-10.csv")

# chem_xrf_29aug <- read_csv("data/pXRF_Data/chemistry-803819-2019-08-29-14-24-17.csv")

#------------------------------------------------------------------------------------#

##### Join all pXRF chemistry files into "all_chem" #####
# all_chem <- bind_rows(chem_xrf_22aug, chem_xrf_23aug, chem_xrf_26aug, chem_xrf_27aug, chem_xrf_29aug)

# str(all_chem)


# all_chem %>%
# select(Reading_No, Date, PConcentration, P_Error1s) %>% 
# separate(col = Date, into = c("day", "month", "year"), sep = "/") %>% 
# mutate(reading_id = str_pad(Reading_No, 3, "left", "0")) %>% 
#  unite(reading_id, year, month, day, reading_id, sep = "-") %>% 
#  select(reading_id, PConcentration, P_Error1s) %>% 
#  write_csv("data/clean_data/all_pxrf_phosphorus.csv")
#######################################################################################################   

all_pxrf_id <- read_csv("data/clean_data/all_pxrf_id.csv")
all_pxrf_chem <- read_csv("data/clean_data/all_pxrf_chem.csv")

genotype_chem <- left_join(all_pxrf_id, all_pxrf_chem, by = "SCANNING_CODE")

genotype_chem %>% 
  view () %>% 
  write_csv("data/clean_data/all_gen_pxrf_chem.csv")

all_gen_pxrf_chem <- read_csv("data/clean_data/all_gen_pxrf_chem.csv")
hi_York_data <- read_csv("data/York_hi/hi_York_data.csv")

str(genotype_chem)
str(hi_York_data)

gen_chem_york_hi <- left_join(all_gen_pxrf_chem, hi_York_data, by = "STEM_ID")

gen_chem_york_hi %>% 
  write_csv("data/clean_data/gen_chem_york_hi.csv")
#-----------------------------------------------------------------------------------------------------#
# Reading data file for first graphical visualisation
gen_chem_york_hi <- read_csv("data/clean_data/gen_chem_york_hi.csv")
view(gen_chem_york_hi)

# Graph 1: First data visualisation shows 2 outliers of harvest index (hi), which needs to be removed
ggplot(data = gen_chem_york_hi,
       mapping = aes(x = `P Concentration`,
                     y = harvest_index,
                     colour = SUBSAMPLE
                     )) +
  geom_point() +
  geom_smooth(method = "lm", size = 0.5)

# Removing the 2 hi outliers
good_data <- gen_chem_york_hi %>% # Removing negative values for harvest index
  filter(harvest_index > 0)

# Good data in "good_data_york_pxrf.csv
good_data %>% 
  write_csv("data/clean_data/good_data_york_pxrf.csv")  

# Graph 2: P concentration vs harvest index
hi_Pconc <- ggplot(data = good_data,
                   mapping = aes(x = harvest_index,
                                 y = `P Concentration`,
                                 colour = SUBSAMPLE
                                 )) +
  geom_point() +
  geom_smooth(method = "lm", size = 0.5)

hi_Pconc +
  labs(title = "Harvest index against phosphorus concentration in shoots and grains",
       caption = "Data from gen_chem_york_hi",
       x = "Harvest Index",
       y = "P Concentration (ppm)") +
  geom_point()



