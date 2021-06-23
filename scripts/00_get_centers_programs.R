

# load used packages
library(package = "tidyverse")


# read contents of pdf file with programs

lines <- pdftools::pdf_text("docs/Listado_de_Carreras_mayo_2019.pdf") %>%
  sub("b- *\n *learning", "b-learning", ., ignore.case = TRUE) %>%
  strsplit(split = "\n") %>%
  unlist() %>%
  print()




# grams
lines %>%
  iconv(to = "ASCII//TRANSLIT") %>%
  tolower() %>%
  strsplit(split ="[^a-z-]") %>%
  unlist() %>%
  table() %>%
  sort(decreasing = TRUE)




content <- tibble(line = lines) %>%
  slice(-(1:10)) %>%
  filter(
    !grepl("UNIDAD ACADÉMICA Y CARRERA", line),
    !grepl("/", line),
    !grepl("contin", line, ignore.case = TRUE)
  ) %>%
  mutate(
    line = line %>%
      gsub(" +", " ", .) %>%
      sub("^ ", "", .),
    all_caps = line %>%
      iconv(to = "ASCII//TRANSLIT") %>%
      gsub("[A-Z]", "", .) %>%
      nchar() %>%
      magrittr::divide_by(nchar(line)) %>%
      magrittr::subtract(1, .),
    center = if_else(
      all_caps > 0.75,
      line,
      NA_character_
    ) %>%
      zoo::na.locf(na.rm = FALSE),
    program = if_else(
      all_caps > 0.75,
      NA_character_,
      line
    )
  ) %>%
  print(n = 45)



# check distribution of CAPS percent
# clearly two groups, >75% caps vs <
content %>%
  ggplot() +
  geom_histogram(
    aes(x = all_caps)
  ) +
  scale_y_continuous(trans = "log")



# Explore organized content
content %>%
  DT::datatable(
    options = list(pageLength = 1000)
  )



carreras_usac <- content %>%
  slice(1:(grep("Observaciones", line) - 1)) %>%
  filter(!is.na(program)) %>%
  mutate(
    escuela = if_else(
      grepl("facultad|escuela", center, ignore.case = TRUE),
      center,
      NA_character_
    ),
    center = case_when(
      grepl("médicas|psicol", center, ignore.case = TRUE) ~ "Centro Universitario Metropolitano -CUM -, Guatemala",
      grepl("facultad|escuela", center, ignore.case = TRUE) ~ "Campus central -Central-, Guatemala",
      TRUE ~ center
    )
  ) %>%
  transmute(
    nombre_corto = sub("[^,]+[ -]+([^ ,-]+).*", "\\1", center),
    departamento = sub(".+, *(.+)", "\\1", center) %>%
      stringr::str_to_title(),
    centro = center %>%
      sub(" *-.+", "", .) %>%
      stringr::str_to_title() %>%
      factor(., levels = unique(.)),
    escuela = escuela %>%
      stringr::str_to_title() %>%
      factor(., levels = unique(.)),
    carrera = program
  ) %>%
  select(departamento, centro, nombre_corto, escuela, carrera) %>%
  arrange(centro, escuela) %>%
  print()




write_csv(carreras_usac, "data/carreras_usac.csv")




# avoid overwriting
if(!file.exists("data/centros_usac.csv")){
  carreras_usac %>%
    mutate(
      centro = factor(centro, levels = unique(centro))
    ) %>%
    count(centro, nombre_corto, departamento) %>%
    mutate(
      lugar = NA_character_,
      long = NA_real_,
      lat = NA_real_
    ) %>%
    select(-n) %>%
    print() %>%
    write_csv("data/centros_usac.csv", na = "")
}

# End of script
