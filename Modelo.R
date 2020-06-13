######################################################
# Carregando bibliotecas

library(dplyr)
library(caret) #this package has the createDataPartition function

######################################################
# Carregando Dados

train <- read.csv("C:/Users/kaique/Downloads/train.csv")
test <- read.csv("C:/Users/kaique/Downloads/test.csv")

######################################################
# Limpando NA e selecionando variáveis

summary(train)

train = train %>% select( TP_SEXO, NU_IDADE,
                   NU_NOTA_CN, NU_NOTA_CH, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO,
                   NU_NOTA_COMP1, NU_NOTA_COMP2, NU_NOTA_COMP3, NU_NOTA_COMP4)

NU_INSCRICAO = test %>% select( NU_INSCRICAO )

test = test %>% select( TP_SEXO, NU_IDADE,
                          NU_NOTA_CN, NU_NOTA_CH, NU_NOTA_LC, NU_NOTA_REDACAO,
                        NU_NOTA_COMP1, NU_NOTA_COMP2, NU_NOTA_COMP3, NU_NOTA_COMP4)


N_MT = train %>% select(NU_NOTA_MT) %>% pull()
N_CN = train %>% select(NU_NOTA_CN) %>% pull()
N_CH = train %>% select(NU_NOTA_CH) %>% pull()
N_LC = train %>% select(NU_NOTA_LC) %>% pull()
N_R  = train %>% select(NU_NOTA_REDACAO) %>% pull()

M_MT = N_MT %>% mean(.,na.rm = T)
M_CN = N_CN %>% mean(.,na.rm = T)
M_CH = N_CH %>% mean(.,na.rm = T)
M_LC = N_LC %>% mean(.,na.rm = T)
M_R  = N_R  %>% mean(.,na.rm = T)

p = N_MT %>% is.na() %>% which ()
N_MT[p] = M_MT

p = N_CN %>% is.na() %>% which ()
N_CN[p] = M_CN

p = N_CH %>% is.na() %>% which ()
N_CH[p] = M_CH

p = N_LC %>% is.na() %>% which ()
N_LC[p] = M_LC

p = N_R %>% is.na() %>% which ()
N_R[p] = M_R

# Concatenando Banco de dados tratado
train = train  %>%  
  select(TP_SEXO, NU_IDADE, NU_NOTA_COMP1, NU_NOTA_COMP2, NU_NOTA_COMP3, NU_NOTA_COMP4 ) %>% 
  mutate(N_MT = N_MT) %>% 
  mutate(N_CN = N_CN) %>% 
  mutate(N_CH = N_CH) %>% 
  mutate(N_LC = N_LC) %>% 
  mutate(N_R = N_R)

######################################################
# Separando dados de treino e teste
train_index = createDataPartition(train$TP_SEXO,p=0.7,list=FALSE)

train_train = train[train_index,]
train_test = train[-train_index,]

# Treinando o modelo
glm1 = glm(N_MT ~ TP_SEXO + NU_IDADE +
           N_CH + N_CN + N_LC +
           NU_NOTA_COMP1 + NU_NOTA_COMP2 + NU_NOTA_COMP3 + NU_NOTA_COMP4,
    data = train)


#train_test[8] = predict(glm1, newdata = train_test, type = 'response')

######################################################
# Tratando banco de dados de teste para aplicar o modelo

N_CN = test %>% select(NU_NOTA_CN) %>% pull()
N_CH = test %>% select(NU_NOTA_CH) %>% pull()
N_LC = test %>% select(NU_NOTA_LC) %>% pull()
N_R  = test %>% select(NU_NOTA_REDACAO) %>% pull()

p = N_CN %>% is.na() %>% which ()
N_CN[p] = M_CN

p = N_CH %>% is.na() %>% which ()
N_CH[p] = M_CH

p = N_LC %>% is.na() %>% which ()
N_LC[p] = M_LC

p = N_R %>% is.na() %>% which ()
N_R[p] = M_R

# Concatenando banco de dados de teste
test = test  %>%  
  select(TP_SEXO, NU_IDADE, NU_NOTA_COMP1, NU_NOTA_COMP2, NU_NOTA_COMP3, NU_NOTA_COMP4) %>% 
  mutate(N_CN = N_CN) %>% 
  mutate(N_CH = N_CH) %>% 
  mutate(N_LC = N_LC) %>% 
  mutate(N_R = N_R)


######################################################
# Aplicando o modelo treinado nos dados de teste
NU_NOTA_MT = predict(glm1, newdata = test, type = 'response')


######################################################
# Exportando resultados para submissão
answer = cbind(NU_INSCRICAO, NU_NOTA_MT)

write.csv(answer,"C:/Users/kaique/Downloads/answer.csv", row.names = FALSE)