#!/usr/bin/python

from datetime import datetime
import sys
import subprocess
import os

start_actions = set(["start", "started", "restart", "resume"])
other_actions = set(["commit", ".fuse_hidden*", "branch", "working", ".*.swp", "restart-done", "reduce2d", "fill_histogram", "prefixsum", "binary_search", "fill_mask", "ReduceState", "read_matrix", "max_matrix", "get_threshold", "reduce2d_with_filter", "split", "read_integer", "testing", "still_synchronous", "asynchrony", "matrix", "parallel_scan", "using", "with_filter", "refac", "sequential_sort", "fill_values", "parallel_sort", "parfor", "parsort", "tuple_sorter", "almost", "quit", "merged", "compiles", "files"])
end_actions = set(["done", "pause"])

start_times = {}
total_times = {}

problems = set()
languages = set()
variations = ["seq", "par"]
result = {}

def load_data():
  f = open("log_reverse.txt", "r")
  for line in f:
    bad_string = " -0300 "
    bad_string_index = line.find(bad_string)
    time_string = line[:bad_string_index]
    fmt = "%a %b %d %H:%M:%S %Y"
    parsed_date = datetime.strptime(time_string, fmt)
    commit = line[bad_string_index + len(bad_string):]
    words = commit.split()

    if len(words) > 1:
      index = words[0]
      action = words[1]
      if action in start_actions:
        assert(index not in start_times);
        start_times[index] = parsed_date
      elif action in end_actions:
        assert (index in start_times)
        end_time = parsed_date
        diff = end_time - start_times[index]
        del start_times[index]
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

  for key, value in total_times.iteritems():
    words = key.split("-")
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

    if language not in result:
      result[language] = {}

    if problem not in result[language]:
      result[language][problem] = {}

    assert (variation not in result[language][problem])

    result[language][problem][variation] = value

  for problem in problems:
    if problem != "chain":
      result["tbb"][problem]["seq"] += result["cpp"][problem]["seq"]

def output_tables():
  def create_table(table_name, output_value, extra):
    old_stdout = sys.stdout

    for variation in variations:
      sys.stdout = open("../../../ufrgs/meu/chapters/table-%s-%s.tex" % (
        table_name, variation), "w")

      first = True
      print " & ",
      for problem in sorted(problems):
        if not (problem == "chain" and variation == "seq"):
          if not first:
            print " & ", 
          print problem,
          first = False;
      print " \\\\ \\hline"

      for language in sorted(languages):
        if language == "cpp":
          continue
        print language,
        for problem in sorted(problems):
          if problem == "chain" and variation == "seq":
            continue
          output_value(language, problem, variation, extra)
        print " \\\\"

    sys.stdout = old_stdout

  ########## time tables ###############

  def time_table_output(language, problem, variation, extra):
    assert(variation in result[language][problem])
    print " & ",
    print("%.2f" % result[language][problem][variation])

  create_table("time", time_table_output, None)

  ########## LoC-NoW-NoC tables ###############

  extensions = { "chapel" : "chpl", "cilk" : "cilk", "erlang" : "erl",
      "go" : "go", "scoop" : "e", "tbb" : "cc" }

  table_types = {"loc" : "-l", "now" : "-w", "noc" : "-c"}

  def wc_table_output(language, problem, variation, extra):
    extension = extensions[language]
    table_flag = extra["table_flag"]
    cmd = "find ../../%s/%s/%s/ | grep \"\\.%s$\" | xargs cat | wc %s > wc.out" % (
        language, problem, variation, extension, table_flag)
    if problem == "chain" and variation == "par":
      cmd = "find ../../%s/%s/ | grep \"\\.%s$\" | xargs cat | wc %s > wc.out" % (
          language, problem, extension, table_flag)
    os.system(cmd)
    value = open("wc.out", "r").read()
    print " & ", value,

  for table_name, table_flag in table_types.iteritems():
    create_table(table_name,  wc_table_output, {"table_flag": table_flag})


def main():
  load_data()
  output_tables()

if __name__ == "__main__":
  main()
