# Esse é o script. Aqui nós escrevemos nosso código.

imdb <- readRDS("aula-r-base/data/dados_imdb.rds")
imdb_modificado <- imdb %>% 
  count(director_name, sort = T) %>% 
  na.omit() %>% 
  head(8) %>%
  mutate(director_name = fct_reorder(director_name, n, fun = max, .desc = T))

ggplot(imdb_modificado) +
  geom_bar(mapping = aes(x = director_name, y = n), stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Exercício: no fim de cada aula, tente compreender cada linha do código acima.



# Usamos o atalho 'Ctrl + Enter' para rodar o código de uma linha.
# O código é rodado no Console. É onde o R mora dentro do RStudio.

30 / 4

# Podemos criar 'objetos' usando o operador assign: <-


obj1 <- 1

obj2 <- -1

obj3 <- "a"

# Objetos são "nomes" que guardam um valor.
# Eles ficam gravados na aba "Environment".

obj1

obj2

obj3

# Você pode usar quase qualquer nome como objeto. Algumas exceções

## Não pode começar com números ou símbolos.
1x <- 1 
_x <- 1

## O uso de alguns nomes não é recomendado.

T
F
pi
letters

## Alguns nomes você não vai conseguir usar.

TRUE
FALSE
NA
NULL
if
else

  

# Gráficos ficam armazenados na aba "Plots".

hist(rnorm(100))



# A aba "Help" mostra a documentação de uma função.

?mean

help(sd)
