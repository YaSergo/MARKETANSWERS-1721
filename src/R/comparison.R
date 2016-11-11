options(stringsAsFactors = FALSE)

source("./src/R/monakovfunc.R")

offers <- read.csv(file = "~/Downloads/query_result (57).csv", na.strings = "NULL")
# https://st.yandex-team.ru/MARKETINDEXER-5944#1477637440000
delivery.0 <- read.delim(file = "./input/for_analitics.tsv", header = FALSE,
                       col.names = c("offer_id", "shop_id", "model_id", "days_from",
                                     "days_to", "order_before", "delivery_cost"))
# для связки offer_id + shop_id + model_id оставляем только минимальный срок доставки
delivery <- aggregate(days_to ~ offer_id + shop_id + model_id, data = delivery.0, FUN = min)


logical_columns <- c("flags_available", "local", "flags_cpa", "flags_yx_money", "fast_delivery",
                     "flags_inpost_enable", "flags_delivery")

offers[, logical_columns] <- apply(X = offers[, logical_columns], MARGIN = 2, FUN = as.logical)

# сохраняем информацию о том, сколько предложений было у каждой модели до применения фильтров
models.count <- aggregate(binary_price_price ~ model_id, data = offers, FUN = length)
colnames(models.count) <- c("model_id", "count")

# ВОПРОС: определяем минимальную цену до или после основных фильтров???
# определяем минимальную цену для каждой оставшейся модели
models.min.price <- aggregate(binary_price_price ~ model_id, data = offers, FUN = min)
colnames(models.min.price) <- c("model_id", "min_price")

# ОСНОВНЫЕ УСЛОВИЯ, они должны всегда выполняться
offers$cond.main <- ifelse(offers$flags_cpa == TRUE &
                             offers$flags_available == TRUE &
                             offers$local == TRUE &
                             offers$shop_rating == 5,TRUE, FALSE)

offers <- offers[offers$cond.main == TRUE, ]

# проверяем, что нет дублей по offer_id, shop_id и model_id
# должно быть ноль
sum(duplicated(offers[, c("offer_id", "shop_id", "model_id")]))
# сохраняем список shop_id и model_id для выгрузки сроков заказов
# https://st.yandex-team.ru/MARKETANSWERS-1721#1478621382000
# write.csv(x = offers[, c("offer_id", "shop_id", "model_id")],
#           file = "./output/offers.phones.cpa.available.local.rating5.20161028.csv",
#           row.names = FALSE)
write.csv(x = unique(offers$shop_id),
          file = "./output/offers_shop_id.phones.cpa.available.local.rating5.20161109.csv",
          row.names = FALSE, col.names = FALSE)
write.csv(x = unique(offers$model_id),
          file = "./output/offers_model_id.phones.cpa.available.local.rating5.20161109.csv",
          row.names = FALSE, col.names = FALSE)

# Проставляем ценовые линии
monakov.coef <- read.csv2(file = "./input/data4monakovfunc.csv", dec = ".")
offers$pricelevel <- NA
for (i in 1:nrow(offers)){
  offers$pricelevel[i] <- pricelevel(price = offers$binary_price_price[i],
                                     minprice = models.min.price[models.min.price$model_id == offers$model_id[i], "min_price"],
                                     monakov.coef = monakov.coef)
}

# подвязываем информацию о сроках доставки
offers <- merge(x = offers, y = delivery, by = c("shop_id", "model_id", "offer_id"), all.x = TRUE)

# Исключаем информацию о предложениях, к которым не удалось подвязать данные
# offers <- offers[!is.na(offers$days_to), ]

# Считаю быстрой доставкой, если есть возможность доставить сегодня или завтра
offers$fast_delivery <- ifelse(offers$days_to <= 1 & !is.na(offers$days_to), TRUE, FALSE)
# этот флаг не извлекается из dictionaries.offers
# т.к. мы отобрали только AVAILABLE предложения, то считаю и флаг DELIVERY выставленным
offers$flags_delivery <- TRUE

# дополнительные условия

# 1. Возможность предоплаты через ЯД
offers$cond.yx_money <- ifelse(offers$flags_yx_money == TRUE, 1,0)

# 2. Срочная доставка (только для Москвы)
offers$cond.fast_delivery <- ifelse(offers$fast_delivery == TRUE, 1,0)

# 3. Есть доставка в почтомат плюс еще один другой способ доставки
offers$cond.in_post <- ifelse(offers$flags_inpost_enable == TRUE &
                                offers$flags_delivery == TRUE, 1,0)

offers$cond.total <- offers$cond.yx_money +
  offers$cond.fast_delivery + offers$cond.in_post

### ДИСКРЕТНАЯ РЕАЛИЗАЦИЯ ###
offers$discrete.candidate_pl1 <- ifelse(offers$cond.total > 0 & offers$pricelevel <= 1, TRUE, FALSE)
offers$discrete.candidate_pl2 <- ifelse(offers$cond.total > 0 & offers$pricelevel <= 2, TRUE, FALSE)
offers$discrete.candidate_pl3 <- ifelse(offers$cond.total > 0 & offers$pricelevel <= 3, TRUE, FALSE)

table(offers$discrete.candidate_pl1)
table(offers$discrete.candidate_pl2)
table(offers$discrete.candidate_pl3)

library(plyr)
result.df <- ddply(offers , .(model_id), summarise,
                   discrete.candidate_pl1=sum(discrete.candidate_pl1),
                   discrete.candidate_pl2=sum(discrete.candidate_pl2),
                   discrete.candidate_pl3=sum(discrete.candidate_pl3))