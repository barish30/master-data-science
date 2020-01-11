roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
}

# 1. Generar combinaciones de tres elementos de entre los siguientes símbolos: 
# diamonds (DD), sevens (7), triple bars (BBB), double bars (BB), single bars (B), 
# cherries (C), and zeroes (0). 

#simbolos posibles:
{DD, BBB, BB, B, 7, C, 0}

#Probabilidad:
{0.03(DD), 0.06(BBB), 0.1(BB), 0.25(B), 0.003(7), 0.01(C), 0.52(0)}

#Creamos una función con estos datos:
get_symbols <- function(){
  wheel<-c('DD','7','BBB','BB','B','C','0')
  sample(wheel, size = 3, replace=TRUE, prob =c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}

symbols<-get_symbols()

symbols<-c("7","7","7")
all(symbols == symbols[1])



#Asignamos condiciones:

# symbols<-c("7","7","7")
same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
# symbols<-c("BB","BBB","B")
bars <-all(symbols %in% c("B", "BB", "BBB")

if(same){
  print('Your prize>10e')
}else if(bars){
  print('Your prize=5e')
}else{
  print('Your prize=0e')}


#Agregamos a nuestra condición los distintos premios:

score<-function(symbols){
  
  same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
  bars <- all(symbols %in% c("B", "BB", "BBB"))
             
   if(same & symbols[1]!=0){
     payout<-c('DD'=100, '7'=80, 'BBB'=40, 'BB'=25, 'B'=10, 'C'=10)
     prize<-unname(payout[symbols[1]])
     }else if(bars){
       prize<-5
       }else{
         cherries<- sum(symbols=='C')
         if(cherries==2){
           prize<-5
           }else if(cherries==1){
             prize<-2
             }else{
               prize<-0
             }
         }
   diamonds<-sum(symbols=='DD')
   prize<-prize*2^diamonds
   return(prize)
   }
score(symbols)

play<-function(){
  symbols<-get_symbols()
  print(symbols)
  print(score(symbols))
}


wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
combos <- expand.grid(wheel, wheel, wheel, stringsAsFactors = FALSE)
View(combos)

prob <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06, 
          "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52)

combos$prob1 <- prob[combos$Var1]
combos$prob2 <- prob[combos$Var2]
combos$prob3 <- prob[combos$Var3]
head(combos, 3)

combos$prob <- combos$prob1 * combos$prob2 * combos$prob3
sum(combos$prob)

for (i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3])
  combos$prize[i] <- score(symbols)
}
head(combos)

sum(combos$prize * combos$prob)