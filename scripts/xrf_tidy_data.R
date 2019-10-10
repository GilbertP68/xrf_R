library(tidyverse)
library(readxl)

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

all_id <- bind_rows(xrf_id_22aug, xrf_id_23aug, xrf_id_26aug, xrf_id_27aug, xrf_id_29aug)
view(all_id)
str(all_id)

all_id_select <- all_id %>% 
  select(Trunc, GENPRINT, Date, Reading_No, Mode, Remarks) %>% 
  filter(is.na(Remarks), !is.na(GENPRINT)) %>% 
  view  

  view(all_id_select)

  
mutate(reading_id = str_pad(Reading_No, 3, "left", "0")) %>% 
  unite(reading_id, Date, reading_id, sep = "-") %>% 
  group_by(Trunc)

view(all_id_select)



