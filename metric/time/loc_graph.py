#!/usr/bin/env python
import csv

import os.path
from os.path import abspath, normpath

import subprocess

import rpy2.robjects as robjects
import rpy2.robjects.lib.ggplot2 as ggplot2
from rpy2.robjects.packages import importr
from rpy2.robjects import FloatVector, StrVector, IntVector, DataFrame


bargraph_dir = os.path.abspath(".")
pretty_names = {"seq"      : "Sequential",
                "par"      : "Parallel",
                "expertseq": "Sequential (expert)",
                "expertpar": "Parallel (expert)"
                }

langs = ["go", "erlang", "chapel", "cilk", "tbb", "scoop"]

# on the left we have the name of the CLOC language, on the right
# how we should interpret it for ourselves.
# there may be some overlap, as C++ could go to TBB or Cilk Plus, for example.
cloc_names = {"Go": ["go"],
              "Erlang": ["erlang"],
              "C": ["chapel", "cilk"],
              "C++": ["cilk", "tbb"],
              "Eiffel": ["scoop"]
              }


loc_file = "cloc.csv"

# this command line is intended to be run from the 'metric/perf' directory
cloc_cmd_line = 'cloc --csv --force-lang="C++",cilk --force-lang="C",chpl --by-file --skip-uniqueness --read-lang-def=eiffel_cloc_def.txt --quiet --out=' + loc_file + ' --exclude-dir=cpp,metric --exclude-lang=make,Python ../../'

def main():
  subprocess.check_call (cloc_cmd_line, shell=True)
  for lang in langs:
    with open (loc_file, 'r') as csvfile:
      contents = csv.reader (csvfile)
      loc_graph (contents, lang)

def loc_graph (csv, lang):
  print (lang)
  r = robjects.r
  varis = []
  probs = []
  locs  = []

  sums = {}

  # skip the header
  csv.next()

  for row in csv:
    csvlang = row [0]
    path = row [1]
    loc  = row [4]
    (pathlang, task, var) = break_path (path)

    if pathlang == lang and csvlang in cloc_names and lang in cloc_names [csvlang]:

      if (task, var) not in sums.keys():
        sums [(task,var)] = int (loc)
      else:
        sums [(task,var)] = sums [(task,var)] + int (loc)

  for (task, var) in sums.keys():
    loc = sums [(task,var)]
    varis.append (pretty_names [var])
    probs.append (task)
    locs.append (loc)
  r.pdf ('compare-expert-loc-' + lang + '.pdf')
  df = robjects.DataFrame({'Variation': StrVector (varis),
                           'Problem': StrVector (probs),
                           'Lines' : IntVector (locs),
                           })

  print (df)
  gp = ggplot2.ggplot (df)

  pp = gp + \
      ggplot2.aes_string (x='Problem', y='Lines', fill='Variation') + \
      ggplot2.geom_bar (position='dodge', stat='identity') 
  pp.plot ()
  r['dev.off']()


def break_path (path):
  parts = normpath (path).split(os.sep)
  # we take 2 3 and 4 to account for the ../../ beginning,
  # we only want the language, problem and variation
  return (parts [2], parts [3], parts [4])


if __name__ == '__main__':
  main ()

