library(ggplot2)
library(readxl)


points.fee <- function(fee, third.quart, goal.fee, k = 1/6, weight){
  goal.fee <- goal.fee + 1.2 # нужно вывести формулу для второго слагаемого
  
  middle <- (goal.fee + third.quart) / 2
  
  
  
  sigmoid <- function(x, a = 0, b = 1){
    return(1/(1+exp(-(x+a)*b)))
  }
  
  lin.function <- function(x, k, b){
    return(k*x+b)
  }
  
  result.sigmoind <- sigmoid(x = fee, a = -middle, b = 4/((goal.fee - third.quart)/2))
  result.lin <- lin.function(x = fee, k = k, b = - middle*k)

  result <- ifelse(fee < middle,
                   result.sigmoind,
                   result.sigmoind + result.lin)
  
  return(result * weight)
}

points.price <- function(price.rate, half.life = 5, weight){
  result <- 1/(price.rate/half.life+1)
  return(result * weight)
}

x <- seq(0, 12, by = 0.1)
plotdata <- rbind(data.frame(x = x,
                             y = points.fee(x, third.quart = 3, goal.fee = 4, weight = 1),
                             type = "fee"),
                  data.frame(x = x,
                             y = points.price(price.rate = x, half.life = 5, weight = 1),
                             type = "price"))
plotdata$type <- as.factor(plotdata$type)
ggplot(data = plotdata, aes(x = x, y = y, col = type))+
  geom_line() +
  ylim(0,6)

weigths <- list(
  price = 0.6,
  YD = 0.1,
  FD = 0.1,
  PM = 0.1,
  fee = 0.1
)

examples <- read_excel("input/example-1.xlsx",sheet=1)
fee.3rd.q <- quantile(x = examples$fee, 0.75)
fee.goal <- 4
examples$price.rate <- (examples$price/min(examples$price) - 1)*100


examples$price.points <- points.price(price.rate = examples$price.rate, half.life = 5,
                                      weight = weigths$price)
examples$fee.points <- points.fee(examples$fee,
                                  third.quart = fee.3rd.q, goal.fee = fee.goal,
                                  weight = weigths$fee)
#--
examples$YD.points <- examples$YD * weigths$YD
examples$FD.points <- examples$FD * weigths$FD
examples$PM.points <- examples$PM * weigths$PM

examples$total.points <- examples$price.points +
  examples$fee.points + examples$YD.points + examples$FD.points + examples$PM.points

# -- обязательные условия --
# В наличии
# Локальность
# Рейтинг = 5*
# CPA
#---
# в примере у меня уже выполнено три первых условия
examples$total.points <- ifelse(examples$CPA == 0, 0, examples$total.points)

# -- порог отбора кандидатов --
# порог должен быть больше веса цены, чтобы предложения не могли только по
# минимальной цене выходить в кандидаты - им нужно что-то ещё
threshold <- weigths$price + weigths$fee/2 # этот порог означает, что оффер
# может быть с минимальной ценой без доп. условий, но с fee равным значению между третьим квартилем
# и целевым fee
candidates.sum.point <- sum(examples$total.points[examples$total.points > threshold])

examples$p.show <- ifelse(examples$total.points > threshold,
                          examples$total.points / candidates.sum.point,
                          0)

write.csv(x = examples, file = "./output/examples.csv")
write.csv(x = examples[, c("id", "CPA", "price", "fee", "YD", "FD", "PM", "p.show")],
          file = "./output/examples_goodlook.csv")


