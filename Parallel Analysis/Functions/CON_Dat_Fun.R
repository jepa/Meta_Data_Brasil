# Function to extract information from CONAPESCA webpage #

Conapesca_Data <- function(x){

D1 <- x %>% 
  group_by(`Nombre Comun`,
           Entidad,
           Oficina,
           Origen
  ) %>% 
  summarise(
    Min=min(Año), 
    Max=max(Año) 
  )

D2 <- x %>% 
  group_by(`Nombre Comun`,
           Entidad,
           Oficina,
           Origen,
           Año
  ) %>% 
  summarise(
    n =n()
  ) %>% 
  group_by(`Nombre Comun`,
           Entidad,
           Oficina,
           Origen
  ) %>% 
  summarise(
    n =n()
  )

N <- data.frame(D2$n)

D3 <- D1 %>% 
  bind_cols(N)

DF <- D3 %>% 
  mutate(Nombre_I = paste(Origen,"(Peso Vivo) de",`Nombre Comun`,"en",Oficina)) %>% 
  mutate(Nombre_II = paste(Origen,"(Peso Desemb.) de",`Nombre Comun`,"en",Oficina)) %>% 
  mutate(Nombre_III = paste("Valor de",Origen,"de",`Nombre Comun`,"en",Oficina)) %>% 
  mutate(Key_I = paste("Peso; Vivo; Captura;", `Nombre Comun`,Oficina, sep=";"))%>% 
  mutate(Key_II = paste("Peso; Desembarcado; Captura;", `Nombre Comun`,Oficina, sep=";"))%>% 
  mutate(Key_III = paste("Valor; Ganancia; Captura;", `Nombre Comun`,Oficina, sep=";"))


return(
  write.csv(DF,"CON_OUT.csv")
       )

}
