#!/usr/bin/python
from datetime import datetime
f = open("log_reverse.txt", "r")

start_time = datetime(2012, 1, 1)
for line in f:
  #print line
  bad_string = " -0300 "
  bad_string_index = line.find(bad_string)
  time_string = line[:bad_string_index]
  fmt = "%a %b %d %H:%M:%S %Y"
  parsed_date = datetime.strptime(time_string, fmt)

  commit = line[bad_string_index + len(bad_string):]
  #print time_string, " <-> ", commit
  #print parsed_date

  words = commit.split()

  if len(words) > 1:
    action = words[1]
    if action == "start":
      print time_string, " <-> ", commit
      start_time = parsed_date
    elif action == "done":
      print time_string, " <-> ", commit
      end_time = parsed_date
      diff = end_time - start_time
      print diff

