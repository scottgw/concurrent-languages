#!/usr/bin/env python
import os
import math
import time
from rpy import r

from problems import *
from graphs import *
from utils import *


languages = ["chapel", "cilk", "go", "tbb"]
#problems = ["randmat", "thresh", "winnow", "outer", "product", "chain"]
problems = ["randmat","chain"]
#variations = ["seq", "par"]
variations = ["seq","expertpar", "par"]
threads = [1, 2, 3, 4]
output_dir = "output"


inputs = [
    #ProblemInput(20000, 20000, 666, 1, 1),
    #ProblemInput(20000, 20000, 666, 1, 10000),
    ProblemInput(32, 8000, 666, 1, 10000),
  ]

all_values = {}

class Config:
  def __init__ (self):
    self.languages = languages
    self.problems = problems
    self.variations = variations
    self.inputs = inputs
    self.threads = threads
    self.all_values = all_values
    self.output_dir = output_dir

cfg = Config ()

class Data:
  def __init__ (self, avg, std_dev, ci):
    self.avg = avg
    self.std_dev = std_dev
    self.ci = ci

ERLANG_MAIN = ("#!/bin/sh\n"
               "cd ~/lucia/metric/perf/%s\n"
               "erl -noshell +S $1 -s main main is_bench -s init stop\n")


def get_problems_with_variations():
  for problem in sorted(problems):
    for variation in sorted(variations):
      yield (problem, variation)

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
    cmd = "chmod +x %s" % output
    system(cmd)

def make_all():
  for (language, problem, variation) in get_all():
    directory = get_directory(language, problem, variation)
    cmd = "cd %s && make main" % directory
    print cmd
    system(cmd)

def create_inputs():
  for i in range(len(problem_classes)):
    problem = problem_classes[i]
    for j in range(len(inputs)):
      cur = inputs[j]
      file_name = problem.input_file_name(cur)
      write_to_file(file_name, problem.get_input(cur))

def get_time_output(language, problem, variation, i, nthreads):
  return "time-%s-%s-%s-%d-%d.out" % (
      language, problem, variation, i, nthreads)

def is_sequential (variation):
  return  variation.find('seq') >= 0

def is_parallel (variation):
  return  variation.find('par') >= 0


def run_all(redirect_output=True):
  # TODO: check processor usage
  for nthreads in threads:
    for (language, problem, variation) in get_all():
      if is_sequential (variation) and nthreads != threads[-1]: continue
      for i in range(len(inputs)):
        #TODO: get time output file name
        time_output = get_time_output(language, problem, variation, i, nthreads)
        # TODO: refactor variations
        #print time_output
        cmd = ""
        if language == "go" and is_parallel (variation):
          cmd += "GOMAXPROCS=%d " % nthreads

        if is_sequential (variation) or nthreads == 1:
          cmd += "taskset -c 0 "
        else:
          cmd += "taskset -c 0-%d " % (nthreads - 1)

        # using python timing below
        # directory = get_directory(language, problem, variation)
        # cmd += "/usr/bin/time -a -f %%e -o %s %s/" % (time_output,
        #     directory)
        directory = get_directory(language, problem, variation)
        cmd += directory + '/'

        if language == "erlang":
          cmd += "main.sh"
        elif language == "scoop":
          cmd += "main -bench -i"
        else:
          cmd += "main"

        if language == "chapel":
          cmd += " --numLocales=1 --numThreadsPerLocale=%d" % (
              nthreads)
        elif language == "cilk":
          if variation == 'par':
            cmd += " --nproc %d" % nthreads
          else:
            pass # must NOT pass --nproc here (because of --is_bench)
        elif language == 'tbb':
          if variation == 'par':
            cmd += ' --threads %d' % nthreads
        elif language == 'erlang':
          if variation == 'par':
            cmd += ' %d' % nthreads
          else:
            cmd += ' 1'

        if (language == "chapel" or language == "cilk" or language == "tbb"
            or language == 'go'):
          cmd += " --is_bench"

        if language != "scoop":
          cmd += " <";

        cmd += " %s" % (problem_map[problem].input_file_name(inputs[i]));

        if redirect_output:
          cmd += " > /dev/null 1>&0 2>&0"

        print cmd

        t1 = time.time ()
        system(cmd, timeout=True)
        t2 = time.time ()
        tdiff = t2 - t1

        with open (time_output, 'a') as output_file:
          output_file.write (str (tdiff) + '\n')

        print tdiff

results = {}

def get_results():
  for nthreads in threads:
    if nthreads not in results:
      results[nthreads] = {}
    for (language, problem, variation) in get_all():
      if is_sequential (variation) and nthreads != threads[-1]: continue
      if problem not in results[nthreads]:
        results[nthreads][problem] = {}
      if variation not in results[nthreads][problem]:
        results[nthreads][problem][variation] = {}
      if language not in results[nthreads][problem][variation]:
        results[nthreads][problem][variation][language] = {}

      for i in range(len(inputs)):
        time_output = get_time_output(
            language, problem, variation, i, nthreads)
        #print time_output
        cur = read_file_values(time_output)
        ci = r.t_test (cur, **{"conf.level": 0.975})
        data = Data (r.mean (cur), r.sd (cur), ci)

        results[nthreads][problem][variation][language][i] = data

def output_graphs():
  hist_graphs(cfg, results[threads[-1]])
  speedup_lang_var (cfg, results)
  speedup_prob_var (cfg, results)
#  create_graph(cfg, "exec-time", results[threads[-1]], "")
#  create_graph(cfg, "exec-time", results[threads[-1]], "", is_relative=True)
#  speedup_graph_name = 'speedup'
#  create_speedup_graph(cfg, speedup_graph_name, results)
#  create_problem_speedup_graph(cfg, "problem-speedup", speedup_graph_name)
#  create_language_speedup_graph(cfg, "language-speedup", speedup_graph_name)

TOTAL_EXECUTIONS = 3

def main():
  # make_all()
  # create_inputs()
  # for _ in range(TOTAL_EXECUTIONS):
  #   run_all(redirect_output=False)  # TODO: remove outputs
  get_results()
  output_graphs()

if __name__ == '__main__':
  main()
