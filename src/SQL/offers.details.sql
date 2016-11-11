SELECT
  offers.*,
  shop_details.shop_rating
FROM
(
  SELECT
    binary_ware_md5,
    offer_id,
    shop_id,
    model_id,
    category_id,
    (flags & 8 <> 0) as flags_available,
    IF(priority_regions = 213, True, False) as local,
    -- is_CPA, -- для проверки использования флагов
    (flags & 16777216 <> 0) as flags_cpa,
    binary_price_price,
    IF(binary_price_id = "", "RUR", binary_price_id) as binary_price_id,
    fee,
    (flags & 4 <> 0) as flags_yx_money,
    NULL as fast_delivery, -- необходимо уточнить источник
    (flags & 33554432 <> 0) as flags_inpost_enable,
    (flags & 1048576 <> 0) as flags_delivery,
    delivery_flag
  FROM
    dictionaries.offers
  WHERE
    day = '2016-11-09' AND
    -- только гуру категории
    model_id > 0 AND
    -- мобильные телефоны, смотрим только их чтобы не превысить миллион строк в выгрузке
    category_id = 91491
  --LIMIT 10000 -- для отладки
) offers LEFT JOIN
(
  SELECT shop_id, rating as shop_rating
  FROM dictionaries.shop_ratings
  WHERE day = '2016-11-09'
) shop_details
ON offers.shop_id = shop_details.shop_id
