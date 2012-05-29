import os
import sys

#languages = set(["chapel", "cilk", "erlang", "go", "scoop", "tbb"])
languages = ["cilk"]
#problems = set(["chain", "outer", "product", "randmat", "thresh", "winnow"])
#problems = ["randmat", "thresh"]
problems = ["thresh"]
variations = ["seq", "par"]

def system(cmd):
  assert(os.system(cmd) == 0)

def write_to_file(output, content):
  with open(output, 'w') as f:
    f.write(content)

def append_to_file(output, content):
  with open(output, 'a') as f:
    f.write(content)

def read_from_file(file_name):
  with open(file_name, 'r') as f:
    content = f.read()
  return content

def read_from_file_skipping_first_line(file_name):
  with open(file_name, "r") as f:
    f.readline()
    content = f.read()
  return content

def get_directory(language, problem, variation=""):
  directory = "../../%s/%s" % (language, problem);
  if problem != "chain" and language != "cpp":
    directory += "/%s" % variation;
  return directory

def get_problems_with_variations():
  for problem in sorted(problems):
    for variation in sorted(variations):
      if problem == "chain" and variation == "seq":
        continue
      yield (problem, variation)

ERLANG_MAIN = ("#!/bin/sh\n"
               "cd ~/tudo/tcc/apmc/lucia/metric/perf/%s\n"
               "erl -noshell -s main main -s init stop\n")

def get_all():
  for (problem, variation) in get_problems_with_variations():
    for language in sorted(languages):
      yield (language, problem, variation)

def generate_erlang_main():
  for (problem, variation) in get_problems_with_variations():
    directory = get_directory("erlang", problem, variation)
    output = directory + "/main.sh"
    print output
    write_to_file(output, ERLANG_MAIN % directory)

def make_all():
  for (language, problem, variation) in get_all():
    directory = get_directory(language, problem, variation)
    cmd = "cd %s && make main" % directory
    print cmd
    system(cmd)

inputs = []
input_thresh = []
input_winnow = []

# ===== general =====
#inputs = ["10 10 55", "100 100 666", "100 250 777"] #, "100 1000 888"] #, "100 1000 888"]
#input_thresh = ["55", "66", "77", "55"]
#input_winnow = ["10", "100", "125", "250"]

# ===== chapel =====
#inputs = ["10000 10000 888"]
#input_thresh = ["55"]
#input_winnow = ["250"]

# ===== cilk =====
#inputs = ["10000 100000 888"]
#input_thresh = ["55"]
#input_winnow = ["250"]

# ===== tbb =====
#inputs = ["10000 100000 888"]
#input_thresh = ["55"]
#input_winnow = ["250"]

# ===== go =====
#inputs = ["10000 10000 888"]
#input_thresh = ["55"]
#input_winnow = ["250"]

# ===== erlang =====
#inputs = ["10000 1000 888"]
#input_thresh = ["55"]
#input_winnow = ["250"]

# ===== scoop =====
#inputs = ["4 25000 888"]
#input_thresh = ["55"]
#input_winnow = ["250"]

def create_inputs(problems):
  # TODO: cache input files
  for i in range(len(inputs)):
    cur = inputs[i]
    file_name = "%s%d.in" % (problems[0], i)
    write_to_file(file_name, cur + "\n")

  for i in range(len(inputs)):
    cur = inputs[i]
    data = inputs[i].split()
    file_name = "chain%d.in" % i
    write_to_file(file_name, "%s\n%s\n%s\n%s\n" % (
      data[1], data[2], input_thresh[i], input_winnow[i]))

  for i in range(len(problems) - 1):
    problem = problems[i]
    next_problem = problems[i + 1]
    print problem
    for j in range(len(inputs)):
      input_file = "%s%d.in" % (problem, j)
      output_file = "%s%d.out" % (problem, j)
      next_input_file = "%s%d.in" % (next_problem, j)
      directory = get_directory("cpp", problem)
      cmd = "%s/main < %s > %s" % (
          directory, input_file, output_file);
      #print cmd
      system(cmd)
      cmd = "cp %s %s" % (output_file, next_input_file)
      #print cmd
      system(cmd)
      if next_problem == "thresh":
        append_to_file(next_input_file, input_thresh[j] + "\n")
      if next_problem == "winnow":
        cmd = "cp randmat%d.out %s" % (j, next_input_file)
        #print cmd
        system(cmd)
        content = read_from_file_skipping_first_line(output_file)
        append_to_file(next_input_file, "\n%s\n%s\n" % (
            content, input_winnow))

  for i in range(len(problems) - 1):
    problem = problems[i]
    for j in range(len(inputs)):
      output_file = "%s%d.out" % (problem, j)
      cmd = "rm %s" % output_file
      #print cmd
      system(cmd)

def run_all():
  # TODO: check processor usage
  for (language, problem, variation) in get_all():
    for i in range(len(inputs)):
      time_output = "time-%s-%s-%s-%d.out" % (
          language, problem, variation, i)
      # TODO: refactor variations
      #print time_output
      cmd = ""
      if language == "go":
        cmd += "GOMAXPROCS=4 "
      directory = get_directory(language, problem, variation)
      cmd += "time -a -f %%e -o %s %s/" % (time_output, directory)

      if language == "erlang":
        cmd += "main.sh"
      elif language == "scoop":
        cmd += "main -i"
      else:
        cmd += "main"

      if language == "chapel":
        cmd += " --numLocales=1 --numThreadsPerLocale=2 "
      elif language == "cilk":
        cmd += " --nproc 4 "

      if language != "scoop":
        cmd += " <";

      #cmd += " < %s%d.in > %s-%s-%s-%d.out 2> 2.out 3> 3.out" % (
          #problem, i, language, problem, variation, i)
      cmd += " %s%d.in > /dev/null 1>&0 2>&0" % (
      #cmd += " %s%d.in" % (
      #cmd += " < %s%d.in > 1.out 1>2.out 2>3.out" % (
      #cmd += "main < %s.in > /dev/null 1>&0 2>&0" % (
      #cmd += "main --nproc 2 < %s.in > /dev/null 1>&0 2>&0" % (
      #cmd += "main < %s.in" % (
          problem, i);
      print cmd
      system(cmd)
      value = read_from_file(time_output)
      print value

results = {}
INVALID = 999

def get_results():
  #for (language, problem, variation) in get_all():
    #if not results[problem]:
      #results[problem] = {}
    #if not results[problem][variation]:
      #results[problem][variation] = {}
    #if not results[problem][variation][language]:
      #results[problem][variation][language] = {}

  for problem in sorted(problems):
    results[problem] = {}
    for variation in sorted(variations):
      if problem == "chain" and variation == "seq":
        continue
      results[problem][variation] = {}
      for language in sorted(languages):
        results[problem][variation][language] = {}
        for i in range(len(inputs)):
          results[problem][variation][language][i] = INVALID
          cur = []
          time_output = "time-%s-%s-%s-%d.out" % (
              language, problem, variation, i)
          #print time_output
          f = open(time_output, "r")
          for line in f:
            value = float(line)
            cur.append(value)
          f.close()

          total = 0.0
          for j in range(len(cur)):
            total += cur[j]
          avg = total / len(cur)
          print problem, variation, language, i, avg
          if language == "erlang": # IMPORTANT !!!
            assert(avg > 1)
            avg -= 1
          results[problem][variation][language][i] = avg
  #print results

def output_graphs():
  def create_graph(graph_name, values, max_value, pretty_name):
    old_stdout = sys.stdout

    variation_names = {"seq" : "Sequential", "par" : "Parallel"}
    for i in range(len(inputs)):
      nmax = 0
      for language in sorted(languages):
        for problem in sorted(problems):
          for variation in variations:
            if problem == "chain" and variation == "seq":
              continue
            cur = values[problem][variation][language][i]
            if cur == INVALID:
              continue
            if cur > nmax:
              nmax = cur
      for variation in variations:
        sys.stdout = open("../../../ufrgs/meu/images/graph-%s-%s-%d.perf" % (
          graph_name, variation, i), "w")

        sys.stdout.write("=cluster")
        for language in sorted(languages):
          sys.stdout.write(";" + language)
        print '''
colors=black,yellow,red,med_blue,light_green,cyan
=table
yformat=%g
=norotate
xscale=1
'''
        variation_name = variation_names[variation]
        print "max=%f" % (nmax * 1.5)
        print "ylabel=%s %sexecution time in seconds for input %d" % (variation_name, pretty_name, i)
        for problem in sorted(problems):
          if problem == "chain" and variation == "seq":
            continue
          print problem,
          for language in sorted(languages):
            print("%.2f" % (
              float(values[problem][variation][language][i]))),
          print ""

    sys.stdout = old_stdout

  create_graph("exec-time", results, 60, "")

def main():
#generate_erlang_main()
#make_all()
  #problems = ["randmat", "thresh", "winnow", "outer", "product", "final"]
  problems = ["randmat", "thresh", "final"]
#create_inputs(problems)
  run_all()
  get_results()
  output_graphs()

if __name__ == '__main__':
  main()
