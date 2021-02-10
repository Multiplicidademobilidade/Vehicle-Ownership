# Cars, motorcycles and bicycles ownership by dwelling and race in Brazil
# Source: POF 2018 - Pesquisa de Or�amentos Familiares (Family Budget Survey)
# primary data available also at https://www.ibge.gov.br/

library(openxlsx)
library(dplyr)
library(tidyr)

# 1 - original data
  # import orignal data from POF - IBGE;
  # only useful variables were English renamed:
  # "STATE_COD", "SAMPLE_AREA_COD", "URBAN_RURAL","SAMPLE_UNIT_COD", "DWELLING_NUMBER","WEIGHT"

RESIDENT <- 
  read.fwf("MORADOR.txt",
    widths = c(
      2, 4, 1, 9, 2, 1, 2, 2, 1, 2, 2, 4, 3, 1, 1,
      1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1,
      2, 1, 2, 14, 14, 10
    ),
    na.strings = c(" "),
    col.names = c(
      "UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
      "SAMPLE_UNIT_COD", "DWELLING_NUMBER", "NUM_UC",
      "COD_INFORMANTE", "V0306", "V0401",
      "V04021", "V04022", "V04023", "V0403",
      "V0404", "V0405", "V0406", "V0407",
      "V0408", "V0409", "V0410", "V0411",
      "V0412", "V0413", "V0414", "V0415",
      "V0416", "V041711", "V041712", "V041721",
      "V041722", "V041731", "V041732", "V041741",
      "V041742", "V0418", "V0419", "V0420",
      "V0421", "V0422", "V0423", "V0424",
      "V0425", "V0426", "V0427", "V0428",
      "V0429", "V0430", "ANOS_ESTUDO", "PESO",
      "PESO_FINAL", "RENDA_TOTAL"
    ),
    dec = "."
  )

INVENTORY <-
  read.fwf("INVENTARIO.txt",
    widths = c(2, 4, 1, 9, 2, 1, 2, 2, 7, 2, 2, 4, 1, 14, 14, 10),
    na.strings = c(" "),
    col.names = c(
      "UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
      "SAMPLE_UNIT_COD", "DWELLING_NUMBER", "NUM_UC", "QUADRO", "SEQ",
      "V9001", "V9005", "V9002", "V1404", "V9012", "PESO",
      "PESO_FINAL", "RENDA_TOTAL"
    ),
    dec = "."
  )

DWELLING <-
  read.fwf("DOMICILIO.txt",
    widths = c(
      2, 4, 1, 9, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 14, 14
    ),
    na.strings = c(" "),
    col.names = c(
      "STATE_COD", "SAMPLE_AREA_COD", "URBAN_RURAL",
      "SAMPLE_UNIT_COD", "DWELLING_NUMBER", "V0201", "V0202",
      "V0203", "V0204", "V0205", "V0206", "V0207",
      "V0208", "V0209", "V02101", "V02102",
      "V02103", "V02104", "V02105", "V02111",
      "V02112", "V02113", "V0212", "V0213",
      "V02141", "V02142", "V0215", "V02161",
      "V02162", "V02163", "V02164", "V0217",
      "V0219", "V0220", "V0221", "PESO",
      "WEIGHT"
    ),
    dec = "."
  )
#Labeling dwelling
#contain state name, state_acronym, large region in Brazil, and
  # sample_area_type (State_Capital, State_Capital_MetropolitanArea;
  # State_non_Capital_MetropolitanArea;Rural
LABELS<- read.csv("SAMPLE_AREA_LABELS.csv", sep=';', encoding="UTF-8")



# 2 - compute race data by dwelling
# name races
RESIDENT <- RESIDENT %>%
  mutate(RACE = if_else(V0405 == 1, "WHITE",
    if_else(V0405 == 2, "BLACK",
      if_else(V0405 == 3, "YELLOW",
        if_else(V0405 == 4, "BROWN",
          if_else(V0405 == 5, "INDIGENOUS", "MISS")
        )
      )
    )
  ))

# compute total residents
RESIDENT <- RESIDENT %>% add_count(SAMPLE_UNIT_COD, DWELLING_NUMBER, name = "TOTAL_RESIDENT")

# compute residents races by dwelling
DWELLING_RACE <- RESIDENT %>%
  group_by(SAMPLE_UNIT_COD, DWELLING_NUMBER, TOTAL_RESIDENT, RACE) %>%
  summarise(n = n()) %>%
  pivot_wider(
    names_from = RACE,
    values_from = n,
    values_fill = list(n = 0)
  )

DWELLING_RACE <- DWELLING_RACE %>%
  mutate(FAMILY_RACE = if_else(TOTAL_RESIDENT == BLACK + BROWN, "ONLY_BLACK_BROWN",
    if_else(TOTAL_RESIDENT == WHITE, "ONLY_WHITE", "OTHER_COMBINATION")
  ))



# 3 - compute vehicle data by dwelling

# filter vehicles from inventory
VEHICLES <- INVENTORY %>%
  filter(V9001 == "1403001" | V9001 == "1403101" | V9001 == "1403201")

# name vehicles
VEHICLES <- VEHICLES %>%
  mutate(TYPE_VEHICLE = if_else(V9001 == "1403001", "CAR",
    if_else(V9001 == "1403101", "MOTO", "BIKE")
  ))

# compute vehicles by dwelling
DWELLING_VEHICLE <- VEHICLES %>%
  group_by(SAMPLE_UNIT_COD, DWELLING_NUMBER, TYPE_VEHICLE) %>%
  summarise(n = sum(V9005)) %>%
  pivot_wider(
    names_from = TYPE_VEHICLE,
    values_from = n,
    values_fill = list(n = 0)
  )



# 4 - join and weight data

# join

DWELLING <- select(
  DWELLING, 
  "SAMPLE_AREA_COD", # 
  "URBAN_RURAL", # 1 urban, 2 rural
  "SAMPLE_UNIT_COD", # sample unid code
  "DWELLING_NUMBER", # dwelling number
  "WEIGHT" # weight
  ) 

DWELLING <- left_join(DWELLING,LABELS, by = "SAMPLE_AREA_COD")
DWELLING <- left_join(DWELLING, DWELLING_RACE, by = c("SAMPLE_UNIT_COD", "DWELLING_NUMBER"))
DWELLING <- left_join(DWELLING, DWELLING_VEHICLE, by = c("SAMPLE_UNIT_COD", "DWELLING_NUMBER"))
DWELLING <- DWELLING %>%
  mutate_all(replace_na, 0)

# weight variables
DWELLING <- DWELLING %>%
  mutate_at(
    .funs = list(to_w = ~ . * WEIGHT),
    .vars = c("CAR", "MOTO", "BIKE")
  )

# save
saveRDS(DWELLING, "FINAL_DATABASE.rds")
write.xlsx(DWELLING, "./FINAL_DATABASE.xlsx")
