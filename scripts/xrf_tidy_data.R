library(tidyverse)
library(readxl)

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
view(all_id)
str(all_id)


##### Select only where Remarks is equal to "NA" and GENPRINT not equal to "NA" #####
all_id_select <- all_id %>% 
  select(Trunc, SUBSAMPLE_ID, STEM_ID, GENPRINT, Date, Reading_No, Mode, Remarks) %>% 
  filter(is.na(Remarks), !is.na(GENPRINT)) %>% 
  view  


##### Adding a new column "reading_id" with the number format "000" and concatenating with date
all_id_select %>% mutate(reading_id = str_pad(Reading_No, 3, "left", "0")) %>% 
  unite(reading_id, Date, reading_id, sep = "-") %>% 
  select(reading_id, SUBSAMPLE_ID, STEM_ID, GENPRINT) %>% 
  write_csv("data/clean_data/all_pxrf_id.csv")


##### Reading each file with pXRF chemistry data #####
chem_xrf_22aug <- read_csv("data/pXRF_Data/chemistry-803819-2019-08-22-16-49-44.csv")

chem_xrf_23aug <- read_csv("data/pXRF_Data/chemistry-803819-2019-08-23-12-46-47.csv")

chem_xrf_26aug <- read_csv("data/pXRF_Data/chemistry-803819-2019-08-26-16-11-38.csv")

chem_xrf_27aug <- read_csv("data/pXRF_Data/chemistry-803819-2019-08-27-16-23-10.csv")

chem_xrf_29aug <- read_csv("data/pXRF_Data/chemistry-803819-2019-08-29-14-24-17.csv")

#------------------------------------------------------------------------------------#

##### Join all pXRF chemistry files into "all_chem" #####
all_chem <- bind_rows(chem_xrf_22aug, chem_xrf_23aug, chem_xrf_26aug, chem_xrf_27aug, chem_xrf_29aug)

str(all_chem)


all_chem %>%
  select(Reading_No, Date, PConcentration, P_Error1s) %>% 
  separate(col = Date, into = c("day", "month", "year"), sep = "/") %>% 
  mutate(reading_id = str_pad(Reading_No, 3, "left", "0")) %>% 
  unite(reading_id, year, month, day, reading_id, sep = "-") %>% 
  select(reading_id, PConcentration, P_Error1s) %>% 
  write_csv("data/clean_data/all_pxrf_phosphorus.csv")
   

all_pxrf_id <- read_csv("data/clean_data/all_pxrf_id.csv")
all_pxrf_phosphorus <- read_csv("data/clean_data/all_pxrf_phosphorus.csv")

genotype_phosphorus <- left_join(all_pxrf_id, all_pxrf_phosphorus, by = "reading_id")

genotype_phosphorus %>% 
  view() %>% 
  write_csv("data/clean_data/all_gen_phosphorus.csv")



