#!/usr/bin/env python
import os
import math
import time
from rpy import r

from problems import *
from graphs import *
from utils import *
from config import *

ERLANG_MAIN = ("#!/bin/sh\n"
               "cd ~/lucia/metric/perf/%s\n"
               "erl -noshell +S $1 -s main main is_bench -s init stop\n")

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


def fieller (aa, bb):
  mean_a = r.mean (aa)
  mean_b = r.mean (bb)

  q   = mean_a/mean_b

  sem_a = r.sd (aa) / sqrt (len (aa))
  sem_b = r.sd (bb) / sqrt (len (bb))

  t   = r.qt (0.975, len (aa))
  se_q = q * sqrt (sem_a^2/mean_a^2 + sem_b^2/mean_b^2)

  return (q - t * se_q, q + t * se_q)

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

TOTAL_EXECUTIONS = 3

def main():
  make_all()
  create_inputs()
  for _ in range(TOTAL_EXECUTIONS):
    run_all(redirect_output=False)  # TODO: remove outputs

if __name__ == '__main__':
  main()
