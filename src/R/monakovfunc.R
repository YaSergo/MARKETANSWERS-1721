pricelevel <- function(price, minprice, monakov.coef){
  # функция рассчитывает линию по цене
  # подробности:
  # https://st.yandex-team.ru/MARKETANSWERS-1247#1464712276000
  
  threshold <- function(minprice, a, b, c){
    return(a * minprice + b + c / minprice)
  }
  

  result <- 9
  for (i in 1:ncol(monakov.coef)){
    level <- threshold(minprice = minprice,
                             a = monakov.coef[1, i],
                             b = monakov.coef[2, i],
                             c = monakov.coef[3, i])
    #print(level)
    if ((price/minprice - 1) < level){
      result <- i
      break
    }
  }
  
  return(result)
}

# pricelevel(101, 100)
