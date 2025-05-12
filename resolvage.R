df<- read.csv2("C:/Users/Masspeclab/Downloads/plasma_formate_2025-05-12.csvooooooooooooooooooooooo.csv")
min_cols <- grep("Min", colnames(df),value = TRUE,fixed=TRUE)
mean_cols <- grep("Mean", colnames(df),value = TRUE,fixed=TRUE)
max_cols <- grep("Area", colnames(df),  value = TRUE,fixed=TRUE)

df_min <- df %>%
  select(Compound, all_of(min_cols)) %>%
  pivot_longer(-Compound, names_to = "Measure", values_to = "Value") %>%
  mutate(Date = str_remove(Measure, "Min_"), Stat = "Min")

df_mean <- df %>%
  select(Compound, all_of(mean_cols)) %>%
  pivot_longer(-Compound, names_to = "Measure", values_to = "Value") %>%
  mutate(Date = str_remove(Measure, "Mean_"), Stat = "Mean")

df_max <- df %>%
  select(Compound, all_of(max_cols)) %>%
  pivot_longer(-Compound, names_to = "Measure", values_to = "Value") %>%
  mutate(Date = str_remove(Measure, "Area_"), Stat = "Max")

# Regroupement pour courbes indépendantes
df_all <- bind_rows(df_min, df_mean, df_max) %>%
  mutate(
    Date = ymd(Date),
    Value = as.numeric(str_replace(as.character(Value), ",", "."))
  ) 

df_all<-df_all[order(df_all$Date),]
df_all_c<-df_all[df_all$Compound==df_all$Compound[1],]
plot_ly(df_all_c, x = ~Date, y = ~Value, color = ~Stat, type = 'scatter', mode = 'lines+markers') %>%
  layout(
    title = "Tendances Min / Moyenne / Max (Aires)",
    yaxis = list(title = "Aire moyenne"),
    xaxis = list(title = "Date"),
    legend = list(orientation = "h", x = 0.1, y = 1.1)
  )


min_cols <- grep("Min", colnames(df), value = TRUE,fixed=TRUE)
mean_cols <- grep("Mean", colnames(df),  value = TRUE,fixed=TRUE)
max_cols <- grep("Area", colnames(df),  value = TRUE,fixed=TRUE)

# min_cols <- grep("^Min_", names(df), value = TRUE)
# mean_cols <- grep("^Mean_", names(df), value = TRUE)
# max_cols <- grep("^Area_", names(df), value = TRUE)

validate(need(length(c(min_cols, mean_cols, max_cols)) > 0, "❌ Aucune colonne Min_, Mean_ ou Area_ détectée."))

# Mise au format long
df_min <- df %>%
  select(Compound, all_of(min_cols)) %>%
  pivot_longer(-Compound, names_to = "Measure", values_to = "Value") %>%
  mutate(Date = str_remove(Measure, "Min_"), Stat = "Min")

df_mean <- df %>%
  select(Compound, all_of(mean_cols)) %>%
  pivot_longer(-Compound, names_to = "Measure", values_to = "Value") %>%
  mutate(Date = str_remove(Measure, "Mean_"), Stat = "Mean")

df_max <- df %>%
  select(Compound, all_of(max_cols)) %>%
  pivot_longer(-Compound, names_to = "Measure", values_to = "Value") %>%
  mutate(Date = str_remove(Measure, "Area_"), Stat = "Max")

# Regroupement pour courbes indépendantes
df_all <- bind_rows(df_min, df_mean, df_max) %>%
  mutate(
    Date = ymd(Date),
    Value = as.numeric(str_replace(as.character(Value), ",", "."))
  ) 
# %>%
#   group_by(Date, Stat) %>%
#   summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")

# Affichage
plot_ly(df_all, x = ~Date, y = ~Value, color = ~Stat, type = 'scatter', mode = 'lines+markers') %>%
  layout(
    title = "Tendances Min / Moyenne / Max (Aires)",
    yaxis = list(title = "Aire moyenne"),
    xaxis = list(title = "Date"),
    legend = list(orientation = "h", x = 0.1, y = 1.1)
  )
