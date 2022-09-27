# Librerias ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(broom)

# Idea original -----------------------------------------------------------

#https://tkoomar.github.io/post/2020-05-02-tutorial-iterate-lm/

# Cargar datos ------------------------------------------------------------

datos_temp_0_variables <- read_excel("data/Data_Qh_0.xlsx")

# Funcion lm() ------------------------------------------------------------

lm_caller <- function(variable_objetivo, covariables, data){
  formula <- as.formula(paste0(variable_objetivo, " ~ ", covariables))
  output <- lm(formula, data = data)
  return(output)
}

# Lista variables ---------------------------------------------------------

cationes <- c("ca", "k", "mg", "na")

lista_variables <-
  crossing(
    variable_1 = colnames(datos_temp_0_variables), 
    variable_2 = colnames(datos_temp_0_variables), 
  ) %>%
  filter(!variable_1 == variable_2) %>%
  filter(variable_1 < variable_2) %>% 
  filter(!(variable_1 %in% cationes & variable_2 %in% cationes)) %>%  
  mutate(covariables = paste0(variable_1, "+", variable_2)) 


# Definir variables -------------------------------------------------------

variable_objetivo <- data.frame(variable_objetivo = c('cec', 'om', 'wsa'))

covariables <- data.frame(covariables = lista_variables$covariables %>% dput())

# Formulas ----------------------------------------------------------------

formulas <- crossing(variable_objetivo, covariables) %>% 
  filter(!str_detect(covariables, 'om|cec|wsa'))

# Modelacion --------------------------------------------------------------

lista_modelos <- mutate(formulas,
                        modelos_lm = map2(variable_objetivo, covariables, function(variable_objetivo, covariables) {
                          lm_caller(
                            variable_objetivo = variable_objetivo,
                            covariables = covariables,
                            data = datos_temp_0_variables
                          )
                        }),
                        resumen = purrr::map(modelos_lm, ~broom::glance(.x)),
                        coeficientes = purrr::map(modelos_lm, ~broom::tidy(.x))
                        ) 
# Extraer coeficientes ----------------------------------------------------

ecuaciones_modelos <-
lista_modelos %>% 
  unnest(coeficientes) %>% 
  group_by(variable_objetivo, covariables) %>% 
  summarise(intercepto = estimate[1],
            coeficiente_1 = estimate[2],
            coeficiente_2 = estimate[3]) %>% 
  separate(covariables, c("variable_1", "variable_2"), remove = FALSE) %>% 
  mutate(ecuacion = paste0(variable_objetivo, " = ", 
                           round(coeficiente_1,3), " ", variable_1, " + ", round(coeficiente_2,3)," ", variable_2))


# Modelos + ecuaciones ----------------------------------------------------

lista_modelos_ecuaciones <- left_join(lista_modelos,ecuaciones_modelos)
  
# filtrar significantes y con R alto --------------------------------------

# ejemplo: solo significativos (p <0.05 y R sobre 0.8)

modelos_filtrados <-
  lista_modelos_ecuaciones %>% 
  unnest(resumen) %>% 
  filter(p.value < 0.05 & r.squared > 0.7) 

# Graficar ----------------------------------------------------------------

plot_data <- modelos_filtrados %>%
  group_by(variable_objetivo) %>% 
  slice_max(r.squared, n = 2) %>% #aca deje los 2 R2 mas altos
  mutate(residual = map(modelos_lm, broom::augment)) %>%
  dplyr::select(variable_objetivo, covariables,residual,ecuacion, r.squared) %>%
  unnest(residual) %>%
  pivot_longer(cols = c(-starts_with("."), -variable_objetivo, -covariables, -ecuacion, -r.squared), 
               names_to = 'name', 
               values_to ='actual'
  ) %>%
  filter(variable_objetivo == name) %>%
  nest(data = -variable_objetivo) %>%
  mutate(plot = map2(data, variable_objetivo,  ~(
    ggplot(.x, aes(x = actual, y = .fitted, height = 0)) +
      geom_point() +
      facet_wrap(~covariables, scales = 'free') + 
      labs(title = .y, x = "measured", y = "estimated")+
      geom_text(
        size    = 5,
        mapping = aes(x = Inf, y = -Inf, label = paste0(ecuacion, ", R² =", round(r.squared,3))),
        hjust   = 1,
        vjust   = -1,
        check_overlap = TRUE,
        fontface = "italic")
  )))

patchwork::wrap_plots(plot_data$plot) & theme_bw()