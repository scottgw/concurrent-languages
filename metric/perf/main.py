import os
import math
import time

from stats import *
from problems import *
from graphs import *
from utils import *


languages = ["chapel", "cilk", "go", "tbb"]
#problems = ["randmat", "thresh", "winnow", "outer", "product", "chain"]
problems = ["randmat","chain"]
#variations = ["seq", "par"]
variations = ["seq","expertpar", "par"]
threads = [1, 2, 3, 4]
ALPHA = 0.001
GRAPH_SIZE = 700
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
    self.alpha = ALPHA
    self.output_dir = output_dir
    self.graph_size = GRAPH_SIZE

cfg = Config ()

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
  return  variation == "seq" or variation == "expertseq"

def run_all(redirect_output=True):
  # TODO: check processor usage
  for nthreads in threads:
    for (language, problem, variation) in get_all():
      if is_sequential (variation) and nthreads != threads[-1]: continue
      for i in range(len(inputs)):
        #TODO: get time output file name
        time_output = get_time_output(
            language, problem, variation, i, nthreads)
        # TODO: refactor variations
        #print time_output
        cmd = ""
        if language == "go" and (variation == 'par' or variation == 'expertpar'):
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
INVALID = 999

def get_results():
  for nthreads in threads:
    if nthreads not in results:
      results[nthreads] = {}
      all_values[nthreads] = {}
    for (language, problem, variation) in get_all():
      if is_sequential (variation) and nthreads != threads[-1]: continue
      if problem not in results[nthreads]:
        results[nthreads][problem] = {}
        all_values[nthreads][problem] = {}
      if variation not in results[nthreads][problem]:
        results[nthreads][problem][variation] = {}
        all_values[nthreads][problem][variation] = {}
      if language not in results[nthreads][problem][variation]:
        results[nthreads][problem][variation][language] = {}
        all_values[nthreads][problem][variation][language] = {}

      for i in range(len(inputs)):
        results[nthreads][problem][variation][language][i] = INVALID
        time_output = get_time_output(
            language, problem, variation, i, nthreads)
        #print time_output
        cur = read_file_values(time_output)
        all_values[nthreads][problem][variation][language][i] = cur

        if (len(cur) == 0):
          continue

        total = 0.0
        for j in range(len(cur)):
          total += cur[j]
        avg = total / len(cur)
        print nthreads, problem, variation, language, i, avg
        #if language == "erlang": # IMPORTANT !!!
          #assert(avg > 1)
          #avg -= 1
        results[nthreads][problem][variation][language][i] = avg
  #print results
  #print all_values


ttest_res = {}

def test_significance_speedup():
  for nthreads in threads:
    if nthreads == 1:
      continue
    for (language, problem, variation) in get_all():
      if is_sequential (variation): continue

      for i in range(len(inputs)):
        values = all_values[nthreads][problem][variation][language][i]
        single = all_values[threads[-1]][problem]['seq'][language][i]
        pvalue = ttest(single, values, ALPHA)

        if not nthreads in ttest_res['spseq']:
          ttest_res['spseq'][nthreads] = {}
        if not problem in ttest_res['spseq'][nthreads]:
          ttest_res['spseq'][nthreads][problem] = {}
        if not variation in ttest_res['spseq'][nthreads][problem]:
          ttest_res['spseq'][nthreads][problem][variation] = {}
        if not language in ttest_res['spseq'][nthreads][problem][variation]:
          ttest_res['spseq'][nthreads][problem][variation][language] = {}
        ttest_res['spseq'][nthreads][problem][variation][language][i] = pvalue

        previous = all_values[nthreads - 1][problem][variation][language][i]
        pvalue = ttest(previous, values, ALPHA)

        if not nthreads in ttest_res['spprev']:
          ttest_res['spprev'][nthreads] = {}
        if not problem in ttest_res['spprev'][nthreads]:
          ttest_res['spprev'][nthreads][problem] = {}
        if not variation in ttest_res['spprev'][nthreads][problem]:
          ttest_res['spprev'][nthreads][problem][variation] = {}
        if not language in ttest_res['spprev'][nthreads][problem][variation]:
          ttest_res['spprev'][nthreads][problem][variation][language] = {}
        ttest_res['spprev'][nthreads][problem][variation][language][i] = pvalue

  for t in ['spseq', 'spprev']:
    for nthreads in threads:
      if nthreads == 1:
        continue
      for (language, problem, variation) in get_all():
        if variation == 'seq': continue

        for i in range(len(inputs)):
          pvalue = ttest_res[t][nthreads][problem][variation][language][i]
          passed = pvalue <= ALPHA
          if not passed:
            print 'nth%d:%s:%s:%s:in%d NOT passed %s' % (
                nthreads, problem, variation, language, i, t)

  pretty = {'spseq' : 'sequential', 'spprev' : 'previous parallel'}

def test_significance():
  ttest_types = ['spseq', 'spprev', 'seq', 'par']
  for t in ttest_types:
    ttest_res[t] = {}

  test_significance_speedup()

  print '\n\n*** sequential execution time ***\n\n'
  for i in range(len(inputs)):
    for problem in problems:
      print '\n### %s ###' % problem
      for la in languages:
        print '%s is better than:' % la, 
        values = all_values[threads[-1]][problem]['seq'][la][i]
        for lb in languages:
          if la != lb:
            other = all_values[threads[-1]][problem]['seq'][lb][i]
            pvalue = ttest(other, values, ALPHA)

            if 'seq' not in ttest_res:
              ttest_res['seq'] = {}
            if problem not in ttest_res['seq']:
              ttest_res['seq'][problem] = {}
            if la not in ttest_res['seq'][problem]:
              ttest_res['seq'][problem][la] = {}
            if lb not in ttest_res['seq'][problem][la]:
              ttest_res['seq'][problem][la][lb] = {}
            ttest_res['seq'][problem][la][lb][i] = pvalue

            passed = pvalue <= ALPHA
            if passed:
              print ' %s' % lb, 
        print ''

  print '\n\n*** parallel execution time ***\n\n'
  for i in range(len(inputs)):
    for problem in problems:
      print '\n### %s ###' % problem
      for la in languages:
        print '%s is better than:' % la, 
        values = all_values[threads[-1]][problem]['par'][la][i]
        for lb in languages:
          if la != lb:
            other = all_values[threads[-1]][problem]['par'][lb][i]
            pvalue = ttest(other, values, ALPHA)

            if 'par' not in ttest_res:
              ttest_res['par'] = {}
            if problem not in ttest_res['par']:
              ttest_res['par'][problem] = {}
            if la not in ttest_res['par'][problem]:
              ttest_res['par'][problem][la] = {}
            if lb not in ttest_res['par'][problem][la]:
              ttest_res['par'][problem][la][lb] = {}
            ttest_res['par'][problem][la][lb][i] = pvalue

            passed = pvalue <= ALPHA
            if passed:
              print ' %s' % lb, 
        print ''

  pretty = {'seq' : 'sequential'
            , 'par' : 'parallel'
            , 'expertseq' : 'expert sequential'
            , 'expertpar' : 'expert parallel'
            }


def output_graphs():
  read_table()
  # create_graphs2(cfg,results[threads[-1]])
  speedup_lang_var (cfg, results)  
#  create_graph(cfg, "exec-time", results[threads[-1]], "")
#  create_graph(cfg, "exec-time", results[threads[-1]], "", is_relative=True)
#  speedup_graph_name = 'speedup'
#  create_speedup_graph(cfg, speedup_graph_name, results)
#  create_problem_speedup_graph(cfg, "problem-speedup", speedup_graph_name)
#  create_language_speedup_graph(cfg, "language-speedup", speedup_graph_name)

TOTAL_EXECUTIONS = 3

def calculate():
  # float(results[threads[-1]][problem]['par'][language][0])))
  nt = threads[-1]
  i = 0
  variations = ['par']
  nmax = {}
  nmin = {}
  local_problems = []
  for problem in problems:
    if problem != 'product':
      local_problems.append(problem)

  for problem in local_problems:
    nmax[problem] = {}
    nmin[problem] = {}
    for variation in variations:
      nmax[problem][variation] = 0
      nmin[problem][variation] = 999999999999
      for language in languages:
        nmax[problem][variation] = max(nmax[problem][variation],
            results[nt][problem][variation][language][i])
        nmin[problem][variation] = min(nmin[problem][variation],
            results[nt][problem][variation][language][i])
  calc = {}
  nsum = 0.0
  #for problem in local_problems:
    #print problem, 
  #print ''
  for language in languages:
    #print '%s:' % language, 
    num = 0.0
    den = 0.0
    for problem in local_problems:
      for variation in variations:
        #print ' %f (%f) ' % (
            #results[nt][problem][variation][language][i],
            #results[nt][problem][variation][language][i] / float(
                #nmin[problem][variation]))
        num += (results[nt][problem][variation][language][i] / (
            float(nmin[problem][variation])))
        den += 1
    #print ''
    #print '%f / %f = %f' % (num, float(den), num / float(den))
    calc[language] = num / float(den)
    nsum += calc[language]

  vec = []
  for language in languages:
    #vec.append((calc[language] / nsum, language))
    vec.append((calc[language], language))

  print 'time to execute (par %d)' % nt

  vec.sort()
  for x in vec:
    (val, lang) = x
    print '%s : %f' % (lang, val)



def main():
#  make_all()
#  create_inputs()
#  for _ in range(TOTAL_EXECUTIONS):
#    run_all(redirect_output=False)  # TODO: remove outputs
  get_results()
  output_graphs()
  # system('xmessage " ALL DONE " -nearmouse -timeout 1')
  # raw_input("done! press enter to continue...")
  # system('cd %s && make' % output_dir)

if __name__ == '__main__':
  main()
