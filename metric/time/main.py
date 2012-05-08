#!/usr/bin/python

from datetime import datetime

start_actions = set(["start", "started", "restart", "resume"])
other_actions = set(["commit", ".fuse_hidden*", "branch", "working", ".*.swp", "restart-done", "reduce2d", "fill_histogram", "prefixsum", "binary_search", "fill_mask", "ReduceState", "read_matrix", "max_matrix", "get_threshold", "reduce2d_with_filter", "split", "read_integer", "testing", "still_synchronous", "asynchrony", "matrix", "parallel_scan", "using", "with_filter", "refac", "sequential_sort", "fill_values", "parallel_sort", "parfor", "parsort", "tuple_sorter", "almost", "quit", "merged", "compiles", "files"])
end_actions = set(["done", "pause"])

start_time = datetime(2012, 1, 1)

start_times = {}
total_times = {}

f = open("log_reverse.txt", "r")
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
    index = words[0]
    action = words[1]
    if action in start_actions:
      #print time_string, " <-> ", commit
      start_time = parsed_date
    elif action in end_actions:
      #print time_string, " <-> ", commit
      end_time = parsed_date
      diff = end_time - start_time
      #print diff
      assert(diff.days == 0)
      diff = diff.seconds / 60.0
      if index in total_times:
        total_times[index] += diff
      else:
        total_times[index] = diff
    elif action in other_actions:
      pass
    else:
      print action
      print line
      assert(False)

#print total_times
for key, value in total_times.iteritems():
  words = key.split("-")
  #print words
  assert (len(words) == 2 or len(words) == 3)
  language = words[0]
  problem = words[1]
  if language == "cpp":
    assert (len(words) == 2)
    if problem != "chain":
      words.append("")
  if problem == "refac":
    continue
  if problem == "chain":
    print words
    assert (len(words) == 2)
    words.append("")

  if len(words) != 3:
    print words

