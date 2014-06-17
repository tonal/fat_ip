fat_ip
======

Быстро считаем статистику по логам на Haskell

По мотивам обсуждения в группе [Русский Haskell](https://groups.google.com/forum/?hl=ru#!topic/haskell-russian/Av0cHd8Aiig)

Задача 1
------
В каталоге лежит некторое количество логов.
Интересующие поля расположены в начале записи и разбиты пробелами:

 * Первое поле - IP
 * Третье поле - время, затраченное на обработку или прочерк "-"

Примеры:
```
199.15.233.185 - - [10/Jun/2014:09:30:10 +0400] "GET / HTTP/1.0" 200 16034 "-" "Mozilla/5.0 (Windows NT 6.2; WOW64; rv:27.0) Gecko/20100101 Firefox/27.0" "PHPSESSID=r3s1nudkk4c18cgn36egf117b0; cookie[valuta]=0; cookie[page_rows]=30; cookie[city]=8; cookie[sort_cat_n]=0; cookie[only_cond]=0; cookie[is_prop_open]=0"
157.55.35.84 - - [10/Jun/2014:09:31:06 +0400] "GET / HTTP/1.0" 200 5060 "-" "Mozilla/5.0 (compatible; bingbot/2.0; +http://www.bing.com/bingbot.htm)" "-"
5.255.253.94 - 71611 [10/Jun/2014:09:31:51 +0400] "GET /good/50226 HTTP/1.0" 200 8610 "-" "Mozilla/5.0 (compatible; YandexDirect/3.0; +http://yandex.com/bots)" "-"
188.116.54.12 - 17690 [10/Jun/2014:09:32:37 +0400] "HEAD / HTTP/1.0" 200 - "-" "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.97 Safari/537.11" "-"

```
Представленные реализации:

 * fat_ip.py - простая реализация на Python-е с итераторами
 * fat_ip_bs.hs - реализация с использованием Data.ByteString.Lazy
 * fat_ip_text.hs - реализация с использованием Data.Text.Lazy
 * fat_ip_mt.hs - альтернативная реализация Data.Text.Lazy

Хаскель компилил со следующими параметрами:
```
$ ghc -O3 -Wall --make ИМЯ-ФАЙЛА.hs
```
Характеристики тестовых данных:

 * *Всего логов*: 20
 * *Общее количество строк*: 2189232
 * *Уникальных IP*: 31485
 * *Самый большой файл*: 853535Kб

Примерное время/память при запуске на моей машине:
 * fat_ip.py - 10-11 сек. ~8мб.
 * fat_ip_bs - 4-5 сек. ~200мб.
 * fat_ip_text - 1 мин. 36 сек. ~15мб.

Удалось существенно уменьшить запросы по памяти с помощью следующего трюка:

IP преобразовывается в строгую версию и для неё вызывается copy. Фрагмент ```BS.copy $ BSL.toStrict ip```
функции *line2item*
```haskell
line2item line = case BSL.words line of
  ip : _ : timeStr : _
    | Just (t, "") <- BSL.readInt timeStr -> Just (BS.copy $ BSL.toStrict ip, t)
  _ -> Nothing
```
Без этого память на ByteString ~ 1гб, а на Text ~ 400мб

Задача 2
--------
Слить все логи из указанной директории в один поток, сохраняя временную последовательность записей.

 * merge_logs.py - простая реализация на Python-е с итераторами
 * merge_logs_heap.hs - реализация с использованием Data.ByteString.Lazy + Data.Attoparsec

Примерное время/память при запуске на моей машине:

 * merge_logs.py - 1 мин. 22 сек. <4 мб.
 * merge_logs_heap - 1 мин. 19 сек. Б мб.

Выводы
------
Не всё так однозначно в Hsakell-е с вводом/выводом...
