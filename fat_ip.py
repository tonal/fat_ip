#! /usr/bin/env python
# -*- coding: utf-8 -*-
"""
задача №1159
Epool - парсер логов - самые нагружающие IP
"""
from __future__ import print_function

from collections import Counter
from datetime import datetime
from functools import partial
from itertools import chain, islice
import os
from os.path import join as pjoin

#DIR = 'logs/20140611'
#DIR = 'logs/20140612'
DIR = 'logs/20140613'
CNT = 10

def main():
  files = tuple(open_files(DIR))
  print(len(files))
  ips = Counter()
  for i, item in enumerate(chain(*files), 1):
    ips[item[0]] += item[1]
    if i % 100000 == 0:
      print(i, len(ips))
  print(i, len(ips), sum(ips.itervalues()))
  for v, ip in islice(
    sorted(((v, ip) for ip, v in ips.iteritems()), reverse=True),
    CNT
  ):
    print (ip, v)

def open_files(path):
  for fn in sorted(os.listdir(path)):
    if not fn.endswith('-access_log'):
      continue
    yield read_items(path, fn)

def read_items(path, fn):
  l2i = partial(line2item, fn)
  f = open(pjoin(path, fn))
  return (row for row in (l2i(l) for l in f) if row)

def line2item(fname, line):
  # 199.15.233.185 - - [10/Jun/2014:09:30:10 +0400] "GET / HTTP/1.0" 200 16034 "http://www.azuro.ru/" "Mozilla/5.0 (Windows NT 6.2; WOW64; rv:27.0) Gecko/20100101 Firefox/27.0" "PHPSESSID=r3s1nudkk4c18cgn36egf117b0; cookie[valuta]=0; cookie[page_rows]=30; cookie[city]=8; cookie[sort_cat_n]=0; cookie[only_cond]=0; cookie[is_prop_open]=0"
  # 157.55.35.84 - - [10/Jun/2014:09:31:06 +0400] "GET / HTTP/1.0" 200 5060 "-" "Mozilla/5.0 (compatible; bingbot/2.0; +http://www.bing.com/bingbot.htm)" "-"
  # 5.255.253.94 - 71611 [10/Jun/2014:09:31:51 +0400] "GET /good/50226 HTTP/1.0" 200 8610 "-" "Mozilla/5.0 (compatible; YandexDirect/3.0; +http://yandex.com/bots)" "-"
  # 188.116.54.12 - 17690 [10/Jun/2014:09:32:37 +0400] "HEAD / HTTP/1.0" 200 - "-" "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.97 Safari/537.11" "-"
  # Выделяем нуждные поля
  ip, user, time = line.split(' ', 3)[:3]
  if time == '-':
    return None
  try:
    return (ip, int(time), fname)
  except ValueError:
    return None

if __name__ == '__main__':
  main()
