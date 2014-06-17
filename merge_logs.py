#! /usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function

from datetime import datetime
from functools import partial
from heapq import merge as heap_merge
import os
from os.path import join as pjoin

DIR = 'logs/20140611'

def main():
  files = tuple(open_files(DIR))
  print(len(files))
  for i, item in enumerate(heap_merge(*files), 1):
    #print(i, item2line(item).split(']', 1)[0] + ']')
    #if i == 100:
    #  break
    print (item[-1])

def open_files(path):
  for fn in sorted(os.listdir(path)):
    if not fn.endswith('-access_log'):
      continue
    yield read_items(path, fn)

def read_items(path, fn):
  l2i = partial(line2item, fn)
  f = open(pjoin(path, fn))
  return (l2i(l) for l in f)

def line2item(fname, line):
  ln = line.rstrip()
  # 157.55.33.123 - - [03/May/2014:05:30:02 +0400] "GET /good/77932 HTTP/1.0" 200 14043 "-" "Mozilla ...
  # Выделяем время
  ltime = ln.split('[', 1)[1].split(']', 1)[0].split(' ', 1)[0]
  #03/May/2014:05:30:02
  dt = datetime.strptime(ltime, '%d/%b/%Y:%H:%M:%S')
  #print(dt, fname, ln.split(']', 1)[0] + ']')
  return (dt, fname, ln)

def item2line(item):
  return '%s: %s' % item[1:]

if __name__ == '__main__':
  main()
