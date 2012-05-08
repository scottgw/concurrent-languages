#!/usr/bin/python

from datetime import datetime
import sys

start_actions = set(["start", "started", "restart", "resume"])
other_actions = set(["commit", ".fuse_hidden*", "branch", "working", ".*.swp", "restart-done", "reduce2d", "fill_histogram", "prefixsum", "binary_search", "fill_mask", "ReduceState", "read_matrix", "max_matrix", "get_threshold", "reduce2d_with_filter", "split", "read_integer", "testing", "still_synchronous", "asynchrony", "matrix", "parallel_scan", "using", "with_filter", "refac", "sequential_sort", "fill_values", "parallel_sort", "parfor", "parsort", "tuple_sorter", "almost", "quit", "merged", "compiles", "files"])
end_actions = set(["done", "pause"])

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
      assert(index not in start_times);
      start_times[index] = parsed_date
    elif action in end_actions:
      #print time_string, " <-> ", commit
      #print index
      assert (index in start_times)
      end_time = parsed_date
      diff = end_time - start_times[index]
      del start_times[index]
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

assert(len(start_times) == 0)

problems = set()
languages = set()
variations = set()
result = {}

#print total_times
for key, value in total_times.iteritems():
  words = key.split("-")
  #print words
  assert (len(words) == 2 or len(words) == 3)
  language = words[0]
  problem = words[1]
  if language == "cpp":
    assert (len(words) == 2)
    words.append("seq")
  if problem == "refac":
    assert(False)
    continue
  if problem == "chain":
    if language != "cpp":
      words.append("par")

  if len(words) != 3:
    print words
    assert(False)

  variation = words[2]

  problems.add(problem)
  languages.add(language)
  variations.add(variation)

  if language not in result:
    result[language] = {}
  
  if problem not in result[language]:
    result[language][problem] = {}

  assert (variation not in result[language][problem])

  result[language][problem][variation] = value

#print result
#print problems
#print languages

########## first table ###############

for variation in variations:
  sys.stdout = open("table-" + variation + ".tex", "w")

  first = True
  print " & ",
  for problem in sorted(problems):
    if not (problem == "chain" and variation == "seq"):
      if not first:
        print " & ", 
      #print problem, "-", variation,
      print problem,
      first = False;
  print " \\\\ \\hline"

  for language in sorted(languages):
    if language == "cpp":
      continue
    print language,
    for problem in sorted(problems):
        if variation in result[language][problem]:
          print " & ",
          print("%.2f" % result[language][problem][variation])
        else:
          if not (problem == "chain" and variation == "seq"):
            print " & ---",
    print " \\\\"
