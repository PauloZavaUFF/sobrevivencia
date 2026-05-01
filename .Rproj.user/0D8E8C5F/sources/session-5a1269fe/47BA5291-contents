library(readxl)
library(dplyr)
library(ggplot2)
library(knitr)
library(survival)
library(ggsurvfit)
library(muhaz)
library(survRM2)

base=read_xlsx(path = "transplante.xlsx") 
glimpse(base)

#Categorigas

##### ventilator_support######


tabela <- base |>
  mutate(
    ventilator_support = if_else(ventilator_support == 1, "Sim", "Não")
  ) |>
  count(ventilator_support, name = "Contagem") |>
  mutate(
    Proporcao = (Contagem / sum(Contagem)) *100
  )

# adicionando linha de total
tabela_total <- tabela |>
  bind_rows(
    tibble(
      ventilator_support = "Total",
      Contagem = sum(tabela$Contagem),
      Proporcao = sum(tabela$Proporcao)
    )
  )

# exibindo com kable
tabela_total |>
  kable(
    col.names = c("Ventilação Mecânica", "Contagem", "Proporção"),
    digits = 2
  )

base |>
  mutate(
    ventilator_support = if_else(ventilator_support == 1, "Sim", "Não")
  ) |>
  ggplot(aes(x = ventilator_support)) + 
  geom_bar(fill="skyblue") +
  labs(
    x = "Uso de ventilação mecânica antes do transplante",
    y = "Contagem"
  ) +
  theme_classic()

##### ecmo_support ######

tabela <- base |>
  mutate(
    ecmo_support = if_else(ecmo_support == 1, "Sim", "Não")
  ) |>
  count(ecmo_support, name = "Contagem") |>
  mutate(
    Proporcao = (Contagem / sum(Contagem)) *100
  )

# adicionando linha de total
tabela_total <- tabela |>
  bind_rows(
    tibble(
      ecmo_support = "Total",
      Contagem = sum(tabela$Contagem),
      Proporcao = sum(tabela$Proporcao)
    )
  )

# exibindo com kable
tabela_total |>
  kable(
    col.names = c("ECMO", "Contagem", "Proporção"),
    digits = 2
  )


base |>
  mutate(
    ecmo_support = if_else(ecmo_support == 1, "Sim", "Não")
  ) |>
  ggplot(aes(x = ecmo_support)) + 
  geom_bar(fill="skyblue") +
  labs(
    x = "Uso ECMO do transplante",
    y = "Contagem"
  ) +
  theme_classic()

##### donor_recipient_blood_mismatch #####

base |>
  mutate(
    ventilator_support = if_else(donor_recipient_blood_mismatch == 1, "Não", "Sim")
  ) |>
  ggplot(aes(x = donor_recipient_blood_mismatch)) + 
  geom_bar(fill="skyblue") +
  labs(
    x = "Incompatibilidade sanguínea?",
    y = "Contagem"
  ) +
  theme_classic()

#### previous_surgery #####

base |>
  mutate(
    ventilator_support = if_else(previous_surgery == 1, "Sim", "Não")
  ) |>
  ggplot(aes(x = previous_surgery)) + 
  geom_bar(fill="skyblue") +
  labs(
    x = "Fez cirigia antes?",
    y = "Contagem"
  ) +
  theme_classic()

##### quantitativas ####

#####recipient_age #####

tabela_resumo <- base |>
  summarise(
    Min= min(recipient_age, na.rm = TRUE),
    Média = mean(recipient_age, na.rm = TRUE),
    `1º Quartil` = quantile(recipient_age, 0.25, na.rm = TRUE),
    Mediana = median(recipient_age, na.rm = TRUE),
    `3º Quartil` = quantile(recipient_age, 0.75, na.rm = TRUE),
    Moda = as.numeric(names(sort(table(recipient_age), decreasing = TRUE)[1])),
    Max= max(recipient_age, na.rm = TRUE)
  )

tabela_resumo |>
  kable(digits = 2)

base |> 
  ggplot(aes(y=recipient_age))+
  geom_boxplot(fill="skyblue")+
  labs(y= "Idade do Receptor")+
  theme_classic(base_size = 16)

##### donor_age#####

tabela_resumo <- base |>
  summarise(
    Min= min(donor_age, na.rm = TRUE),
    Média = mean(donor_age, na.rm = TRUE),
    `1º Quartil` = quantile(donor_age, 0.25, na.rm = TRUE),
    Mediana = median(donor_age, na.rm = TRUE),
    `3º Quartil` = quantile(donor_age, 0.75, na.rm = TRUE),
    Moda = as.numeric(names(sort(table(donor_age), decreasing = TRUE)[1])),
    Max= max(donor_age, na.rm = TRUE)
  )

tabela_resumo |>
  kable(digits = 2)

base |> 
  ggplot(aes(y=donor_age))+
  geom_boxplot(fill="skyblue")+
  labs(y= "Idade do Doador")+
  theme_classic(base_size = 16)

###### weight_kg #####

tabela_resumo <- base |>
  summarise(
    Min= min(weight_kg, na.rm = TRUE),
    Média = mean(weight_kg, na.rm = TRUE),
    `1º Quartil` = quantile(weight_kg, 0.25, na.rm = TRUE),
    Mediana = median(weight_kg, na.rm = TRUE),
    `3º Quartil` = quantile(weight_kg, 0.75, na.rm = TRUE),
    Moda = as.numeric(names(sort(table(weight_kg), decreasing = TRUE)[1])),
    Max= max(weight_kg, na.rm = TRUE)
  )

tabela_resumo |>
  kable(digits = 2)

base |> 
  ggplot(aes(y=weight_kg))+
  geom_boxplot(fill="skyblue")+
  labs(y= "Peso do Receptor")+
  theme_classic(base_size = 16)


##### creatinine####

tabela_resumo <- base |>
  summarise(
    Min= min(creatinine, na.rm = TRUE),
    Média = mean(creatinine, na.rm = TRUE),
    `1º Quartil` = quantile(creatinine, 0.25, na.rm = TRUE),
    Mediana = median(creatinine, na.rm = TRUE),
    `3º Quartil` = quantile(creatinine, 0.75, na.rm = TRUE),
    Moda = as.numeric(names(sort(table(creatinine), decreasing = TRUE)[1])),
    Max= max(creatinine, na.rm = TRUE)
  )

tabela_resumo |>
  kable(digits = 2)

base |> 
  ggplot(aes(y=creatinine))+
  geom_boxplot(fill="skyblue")+
  labs(y= "Nível de Creatinina")+
  theme_classic(base_size = 16)

###### bilirubin ####

tabela_resumo <- base |>
  summarise(
    Min= min(bilirubin, na.rm = TRUE),
    Média = mean(bilirubin, na.rm = TRUE),
    `1º Quartil` = quantile(bilirubin, 0.25, na.rm = TRUE),
    Mediana = median(bilirubin, na.rm = TRUE),
    `3º Quartil` = quantile(bilirubin, 0.75, na.rm = TRUE),
    Moda = as.numeric(names(sort(table(bilirubin), decreasing = TRUE)[1])),
    Max= max(bilirubin, na.rm = TRUE)
  )

tabela_resumo |>
  kable(digits = 2)

base |> 
  ggplot(aes(y=bilirubin))+
  geom_boxplot(fill="skyblue")+
  labs(y= "Nível de Bilirrubina")+
  theme_classic(base_size = 16)

###### ischemic_time_hours #####

tabela_resumo <- base |>
  summarise(
    Min= min(ischemic_time_hours, na.rm = TRUE),
    Média = mean(ischemic_time_hours, na.rm = TRUE),
    `1º Quartil` = quantile(ischemic_time_hours, 0.25, na.rm = TRUE),
    Mediana = median(ischemic_time_hours, na.rm = TRUE),
    `3º Quartil` = quantile(ischemic_time_hours, 0.75, na.rm = TRUE),
    Moda = as.numeric(names(sort(table(ischemic_time_hours), decreasing = TRUE)[1])),
    Max= max(ischemic_time_hours, na.rm = TRUE)
  )

tabela_resumo |>
  kable(digits = 2)

base |> 
  ggplot(aes(y=ischemic_time_hours))+
  geom_boxplot(fill="skyblue")+
  labs(y= "Tempo de Isquemia  (Horas)",
       x = NULL)+
  theme_classic(base_size = 16)



#################

base= base |> 
  rename(tempos=observed_time_years,
         cens=event) 

ekm1<- survfit2(data=base,
                formula = Surv(tempos,cens)~ventilator_support,
                conf.type = "log-log", 
                conf.int=0.95)


ggsurvfit(ekm1, linewidth = 1.2) +
  add_confidence_interval() +
  add_censor_mark()+
  add_risktable() +
  scale_ggsurvfit() +
  labs(x = "Tempo (minutos)",
       y = "Sobrevivência estimada") +
  theme_classic(base_size=16)

ggsurvfit(ekm, linewidth = 1.2) +
  add_confidence_interval() +
  add_censor_mark()+
  add_risktable() +
  scale_ggsurvfit() +
  labs(
    x = "Tempo (semanas)",
    y = "Sobrevivência estimada"
  ) +
  theme_classic(base_size = 16)



base= base |> 
  rename(tempos=observed_time_years,
         cens=event)

base <- base |>
  mutate(
    ventilator_support2 = factor(
      ventilator_support,
      levels = c(0, 1),
      labels = c("Não", "Sim")
    ),
    ecmo_support2=factor(
      ecmo_support,
      levels = c(0, 1),
      labels = c("Não", "Sim")
    ),
    donor_recipient_blood_mismatch2 = factor(
      donor_recipient_blood_mismatch,
      levels = c(0, 1),
      labels = c("Compatível", "Incompatívo")
    ),
    previous_surgery2= factor(
      previous_surgery,
      levels = c(0, 1),
      labels = c("Não", "Sim")
    )
  )

A=survdiff(Surv(tempos,cens)~ventilator_support2, data=base)

B=logrank_test(Surv(tempos,cens)~factor(ventilator_support2),type="Peto-Peto", data = base)

