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
#-----------------------------------------------------------------------------------------------------#
# Reading data file for first graphical visualisation
gen_chem_york_hi <- read_csv("data/clean_data/gen_chem_york_hi.csv")
view(gen_chem_york_hi)

# Graph 1: First data visualisation shows 2 outliers of harvest index (hi), which needs to be removed
data_check <- ggplot(data = gen_chem_york_hi,
       mapping = aes(x = `P Concentration`,
                     y = harvest_index,
                     colour = SUBSAMPLE
                     )) +
  geom_point() +
  geom_smooth(method = "lm", size = 0.5, se = FALSE)

ggsave("results/graph1_data_check.jpg", 
       plot = data_check,
       width = 20,
       height = 10,
       units = "cm")

#####---------- Removing the 2 hi outliers -----------#####
good_data <- gen_chem_york_hi %>% # Removing negative values for harvest index
  filter(harvest_index > 0)

# Good data in "good_data_york_pxrf.csv
good_data %>% 
  write_csv("data/clean_data/good_data_york_pxrf.csv")  

# Graph 2: P concentration in shoots and grains vs harvest 
hi_Pconc <- ggplot(data = good_data,
                   mapping = aes(x = harvest_index,
                                 y = `P Concentration`,
                                 colour = PAP,
                                 shape = SUBSAMPLE
                                 )) +
  geom_point() +
  geom_smooth(method = "lm", size = 0.5, se = FALSE)

graph2_hi_Pconc <- hi_Pconc +
  labs(title = "Phosphorus concentration in straw and grains vs harvest index",
       caption = "Data from good_data_york_pxrf.csv",
       x = "Harvest Index",
       y = "P Concentration (ppm)") +
  geom_point()

ggsave("results/graph2_hi_Pconc.jpg", 
       plot = graph2_hi_Pconc,
       width = 20,
       height = 10,
       units = "cm")

# Graph 3: P concentration in shoots and grains vs biomass
biomass_Pconc <- ggplot(data = good_data,
                   mapping = aes(x = biomass,
                                 y = `P Concentration`,
                                 colour = PAP,
                                 shape = SUBSAMPLE
                   )) +
  geom_point() +
  geom_smooth(method = "lm", size = 0.5, se = FALSE)

graph3_biomass_Pconc <- biomass_Pconc +
  labs(title = "Phosphorus concentration in straw and grains vs biomass",
       caption = "Data from good_data_york_pxrf.csv",
       x = "Biomass (Kg)",
       y = "P Concentration (ppm)") +
  geom_point()

ggsave("results/graph3_biomass_Pconc.jpg", 
       plot = graph3_biomass_Pconc,
       width = 20,
       height = 10,
       units = "cm")

# Graph 4: P concentration in shoots and grains vs grain yield
yield_Pconc <- ggplot(data = good_data,
                        mapping = aes(x = yield,
                                      y = `P Concentration`,
                                      colour = PAP,
                                      shape = SUBSAMPLE
                        )) +
  geom_point() +
  geom_smooth(method = "lm", size = 0.5, se = FALSE)

graph4_yield_Pconc <- yield_Pconc +
  labs(title = "Phosphorus concentration in straw and grains vs grain yield",
       caption = "Data from good_data_york_pxrf.csv",
       x = "Yield (Kg)",
       y = "P Concentration (ppm)") +
  geom_point()

ggsave("results/graph4_yield_Pconc.jpg", 
       plot = graph4_yield_Pconc,
       width = 20,
       height = 10,
       units = "cm")

#-----------------------------------------------------------------------------------------------------#
# Create 2 dataframes from good-data: One for with values for straw and the other one for grain
straw_p <- good_data %>% 
  select(STEM_ID, GENOTYPE, SUBSAMPLE, `P Concentration`, PAP, yield) %>% 
  filter(SUBSAMPLE =="Straw") %>% 
  view()

grain_p <- good_data %>% 
  select(STEM_ID, SUBSAMPLE, `P Concentration`, PAP) %>% 
  filter(SUBSAMPLE =="Grain") %>% 
  view()

straw_grain_p <- left_join(straw_p, grain_p, by = "STEM_ID") %>%
  rename(straw_pconc = `P Concentration.x`, 
         grain_pconc = `P Concentration.y`) %>% 
  view() %>% 
  write_csv("data/clean_data/straw_grain_p.csv")

# Graph 5: P in shoots vs P in grains
straw_grain_p <- read_csv("data/clean_data/straw_grain_p.csv")

straw_grain <- ggplot(data = straw_grain_p,
                      mapping = aes(x = grain_pconc,
                                    y = straw_pconc,
                                    colour = PAP.x
                                    )) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", size = 0.5, se = FALSE)

graph5_P_straw_grain <- straw_grain +
  labs(title = "Amount of P in straw vs amount of P in grains",
       caption = "Data from straw_grain_p",
       x = "Grains",
       y = "Straw") +
  geom_point(alpha = 0.2)

ggsave("results/graph5_P_straw_grain.jpg", 
       plot = graph5_P_straw_grain,
       width = 20,
       height = 10,
       units = "cm")

#-----------------------------------------------------------------------------------------------------#
  

