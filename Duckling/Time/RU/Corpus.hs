-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.RU.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types hiding (add)

corpus :: Corpus
corpus = (testContext {locale = makeLocale RU Nothing}, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext {locale = makeLocale RU Nothing}, testOptions, examples)
  where
    examples =
      [ "ржу нимагу"
      , "мы разделены"
      , "25"
      , "он тут один"
      , "единственный день"
      , "900 рублей потратил"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "сейчас"
             , "прямо сейчас"
             ]
  , examples (datetime (2013, 2, 12, 15, 19, 12) Second)
             [ "15:19:12"
             ]
  , examples (datetime (2013, 2, 12, 15, 19, 0) Minute)
             [ "15:19"
             , "в 15 19"
             , "в 15-19"
             , "в пятнадцать девятнадцать"
             ]
  , examples (datetime (1989, 10, 3, 0, 0, 0) Day)
             [ "3 октября 1989"
             ]
  , examples (datetime (2100, 6, 12, 0, 0, 0) Day)
             [ "12 июня 2100"
             ]
  , examples (datetime (2013, 10, 3, 0, 0, 0) Day)
             [ "3 октября"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Year)
             [ "в 2014"
             , "в 2014 году"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "в 15"
             , "в 15 часов"
             , "три часа пополудни"
             , "в три"
             , "в три дня"
             , "в 3 дня"
             , "сегодня в 15"
             , "сегодня в 3 дня"
             ]
  , examples (datetime (2013, 2, 12, 11, 30, 0) Minute)
             [ "в 11:30"
             , "в 11 30"
             , "11 часов 30 минут"
             , "11 часов 30 мин"
             , "11 часов 30 мин."
             , "в пол двенадцатого"
             , "в полдвенадцатого"
             , "в половину двенадцатого"
             ]
  , examples (datetime (1989, 10, 3, 11, 30, 0) Minute)
             [ "11 часов 30 минут 3 октября 1989"
             , "03.10.89 11:30"
             ]

  , examples (datetime (1900, 1, 1, 0, 0, 0) Day)
             [ "1 января 1900"
             -- , "первого января тысяча девятисотого"
             ]
  , examples (datetime (1905, 1, 1, 0, 0, 0) Year)
             [ "в 1905 году"
             ]
  , examples (datetime (2014, 1, 22, 8, 0, 0) Hour)
             [ "в 8 22 января"
             , "в 8 утра 22 января"
             ]
  -- , examples (datetime (2013, 3, 1, 20, 0, 0) Hour)
  --            [ "в 8 вечера первого числа"
  --            ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ "в 10:30"
             , "в 10 30"
             , "пол одиннадцатого"
             , "полодиннадцатого"
             , "пол одинадцатого"
             , "полодинадцатого"
             ]
  , examples (datetime (2013, 2, 12, 22, 30, 0) Minute)
             [ "в 22:30"
             , "в 22 30"
             , "пол одиннадцатого вечера"
             , "пол одинадцатого вечера"
             ]
  , examples (datetime (2013, 2, 12, 7, 30, 0) Minute)
             [ "в 7:30"
             , "в 7 30"
             , "пол 8"
             , "пол восьмого"
             , "полвосьмого"
             , "в семь тридцать"
             , "семь тридцать утра"
             ]
  , examples (datetime (2013, 2, 12, 19, 45, 0) Minute)
             [ "в 19:45"
             , "в девятнадцать сорок пять"
             , "без четверти 8 вечера"
             , "без четверти восемь вечера"
             , "без 15 восемь вечера"
             ]
  , examples (datetime (2013, 2, 12, 10, 54, 0) Minute)
             [ "в десять пятьдесят четыре"
             ]
  , examples (datetime (1977, 3, 7, 10, 0, 0) Hour)
             [ "7 марта 1977 в 10"
             , "7 марта 1977 в 10 утра"
             ]
  , examples (datetime (2013, 3, 16, 15, 0, 0) Hour)
             [ "16 марта в 15"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Hour)
             [ "в полночь"
             , "в 12 ночи"
             , "ровно в 12 ночи"
             , "ровно в полночь"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Minute)
             [ "в ноль часов ноль минут"
             ]
  , examples (datetime (2013, 2, 13, 0, 30, 0) Minute)
             [ "в 00:30 ночи"
             , "в 00 30 минут"
             , "пол первого ночи"
             , "полпервого ночи"
             ]
  , examples (datetime (2013, 2, 12, 11, 11, 0) Minute)
             [ "в одиннадцать одиннадцать"
             ]
  , examples (datetime (2013, 2, 12, 7, 40, 0) Minute)
             [ "в семь сорок"
             , "в 7:40"
             , "в 7 40 минут"
             , "без двадцати восемь"
             ]
  , examples (datetime (2013, 2, 12, 11, 57, 0) Minute)
             [ "в 11:57"
             , "без трех двенадцать"
             ]
  , examples (datetime (2013, 2, 12, 17, 47, 0) Minute)
             [ "без тринадцати восемнадцать"
             ]
  , examples (datetime (2013, 2, 12, 10, 11, 0) Minute)
             [ "в 10:11"
             , "в 10 11 утра"
             , "10 11 утра"
             , "в 11 минут одиннадцатого"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "завтра"
             -- , "завтрашний день"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "вчера"
             -- , "вчерашний день"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "послезавтра"
             -- , "после завтра" -- todo remove?
             -- , "послезавтрашний день"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "позавчера"
             -- , "позавчерашний день"
             ]
  , examples (datetime (2013, 2, 12, 12, 0, 0) Hour)
             [ "полдень"
             , "12 дня"
             , "12 утра"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Hour)
             [ "в полночь"
             , "полночь"
             ]
  , examples (datetime (2013, 2, 12, 6, 5, 0) Minute)
             [ "в 06:05"
             , "в 6 5"
             , "в шесть 05"
             -- , "на 6 ноль 5 утра"
             ]
  , examples (datetime (2013, 2, 12, 13, 5, 0) Minute)
             [ "в 13:05"
             , "в час 05"
             , "час 05"
             ]
  , examples (datetime (2013, 2, 13, 18, 0, 0) Hour)
             [ "завтра в 18"
             , "следующий день в 6 вечера"
             , "завтра в 6 вечера"
             , "завтра вечером в 6"
             ]
  , examples (datetime (2013, 2, 13, 18, 0, 0) Minute)
             [ "завтра в 18:00"
             -- , "следующий день в 6 00 вечера"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Minute)
             [ "в 3 часа 0 минут"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Hour)
             [ "в час дня"
             , "в час"
             , "в один дня"
             , "в 1 дня"
             , "в 1 час дня"
             , "в 13 дня"
             ]
  , examples (datetime (2013, 2, 12, 16, 20, 0) Minute)
             [ "в 16:20"
             , "20 минут пятого вечера"
             ]
  , examples (datetime (2013, 2, 12, 22, 0, 0) Hour)
             [ "на двадцать два"
             ]
  , examples (datetime (2013, 2, 12, 22, 55, 0) Minute)
             [ "в 22:55"
             , "в 22 55 ночи"
             ]
  , examples (datetime (2013, 2, 12, 5, 55, 0) Minute)
             [ "в 5:55"
             , "без пяти 6 утра"
             , "без 5 6 утра"
             ]
  , examples (datetime (2013, 2, 13, 7, 0, 0) Minute)
             [ "завтра в 7:00"
             -- , "на завтра на 7 00"
             ]
  , examples (datetime (2013, 2, 13, 7, 0, 0) Hour)
             [ "завтра на 7"
             , "завтра в 7"
             , "завтра в 7 часов"
             ]
  , examples (datetime (2013, 2, 13, 7, 20, 0) Minute)
             [ "завтра в 7:20"
             , "завтра утром в 7 20"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "через неделю"
             , "через 1 неделю"
             ]
  , examples (datetime (2013, 2, 12, 4, 31, 0) Second)
             [ "через минуту"
             , "через 1 минуту"
             , "через минутку"
             , "через минуточку"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Minute)
             [ "через час"
             , "через часок"
             , "через часик"
             , "через часочек"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 19, 0, 0, 0)) Second)
             [ "в течение недели"
             , "в течение 1 недели"
             ]
  , examples (datetime (2013, 2, 26, 0, 0, 0) Day)
             [ "через 2 недели"
             , "через две недели"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 26, 0, 0, 0)) Second)
             [ "в течение 2 недель"
             , "в течение двух недель"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Minute)
             [ "в 4:45"
             , "без четверти 5"
             , "без четверти пять"
             ]
  , examples (datetime (2013, 2, 12, 11, 15, 0) Minute)
             [ "в 11:15"
             , "четверть двенадцатого"
             , "15 минут двенадцатого"
             , "15 мин. двенадцатого"
             ]
  , examples (datetime (2013, 2, 12, 4, 40, 0) Second)
             [ "через 10 минут"
             , "через десять минут"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "через 15 минут"
             , "через четверть часа"
             ]
  , examples (datetime (2013, 2, 12, 6, 00, 0) Second)
             [ "через полтора часа"
             , "спустя полтора часа"
             , "полтора часа спустя"
             ]
  , examples (datetime (2013, 3, 12, 0, 0, 0) Day)
             [ "через месяц"
             , "спустя месяц"
             , "месяц спустя"
             ]
  , examples (datetime (2012, 1, 1, 0, 0, 0) Year)
             [ "прошлый год"
             , "в прошлом году"
             , "прошедший год"
             , "предыдущий год"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Year)
             [ "этот год"
             , "текущий год"
             , "ближайший год"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Year)
             [ "в 2014 году"
             , "в следующем году"
             , "в предстоящем году"
             , "в будущем году"
             ]
  , examples (datetime (2013, 2, 17, 4, 30, 0) Minute)
             [ "через 5 суток"
             , "спустя 5 суток"
             , "5 суток спустя"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "на выходных"
             , "в выходные"
             ]
  , examples (datetimeInterval ((2013, 2, 8, 18, 0, 0), (2013, 2, 11, 0, 0, 0)) Hour)
             [ "на прошедших выходных"
             , "в прошлые выходные"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ "в понедельник утром"
             , "утром в понедельник"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 0, 0, 0), (2013, 2, 14, 0, 0, 0)) Day)
             [ "с понедельника по среду"
             , "с понедельника до среды"
             ]
  , examples (datetimeHoliday (2014, 1, 1, 0, 0, 0) Day "Новый год")
             [ "Новый Год"
             ]
  , examples (datetimeHoliday (2014, 1, 7, 0, 0, 0) Day "Рождество")
             [ "Рождество"
             , "Рождеству"
             , "Рождества Христова"
             ]
  ]
