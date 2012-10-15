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

languages = ["chapel", "cilk", "erlang", "go", "scoop", "tbb"]
#languages = ["chapel", "cilk", "go", "tbb"]
problems = ["randmat", "thresh", "winnow", "outer", "product", "chain"]
variations = ["seq", "par", "expertseq", "expertpar"]

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
  #for lang in langs:
  #  with open (loc_file, 'r') as csvfile:
  #    contents = csv.reader (csvfile)
  #    loc_graph (contents, lang)
  results = get_results ()
  hist_varis (results)
  hist_varis_diff (results)
  hist_langs (results)
  
def get_results ():
  results = {}
  for lang in languages:
    with open (loc_file, 'r') as csvfile:
      contents = csv.reader (csvfile)
  
      # skip the header
      contents.next()

      for row in contents:
        csvlang = row [0]
        path = row [1]
        loc  = row [4]
        (pathlang, prob, var) = break_path (path)

        if pathlang == lang and csvlang in cloc_names and lang in cloc_names [csvlang]:
          if (lang, prob, var) not in results.keys():
            results [(lang, prob, var)] = int (loc)
          else:
            results [(lang, prob, var)] = results [(lang, prob, var)] + int (loc)
  return results

def hist_varis (results):
  r = robjects.r

  for variation in variations:
    langs = []
    probs = []
    locs  = []
    for (lang, prob, var) in results.keys():
      if var == variation:
        loc = results [(lang, prob, var)]
        langs.append (lang)
        probs.append (prob)
        locs.append (loc)
    r.pdf ('compare-loc-' + variation + '.pdf')
    df = robjects.DataFrame({'Language': StrVector (langs),
                             'Problem': StrVector (probs),
                             'Lines' : IntVector (locs),
      })
    
    #print (df)
    gp = ggplot2.ggplot (df)
  
    pp = gp + \
        ggplot2.aes_string (x='Problem', y='Lines', fill='Language') + \
        ggplot2.geom_bar (position='dodge', stat='identity') 
    pp.plot ()
    r['dev.off']()

def hist_varis_diff (results):
  r = robjects.r

  for (standard, expert) in [('seq', 'expertseq'), ('par', 'expertpar')]:
    langs = []
    probs = []
    diffs  = []
    for lang in languages:
      for prob in problems:
        loc = results [(lang, prob, standard)]
        loc_expert = results [(lang, prob, expert)]
        diff = (float(loc_expert) / float(loc) - 1) * 100

        langs.append (lang)
        probs.append (prob)
        diffs.append (diff)

    r.pdf ('compare-loc-diff-' + standard + '.pdf')
    df = robjects.DataFrame({'Language': StrVector (langs),
                             'Problem': StrVector (probs),
                             'Difference' : IntVector (diffs),
      })
    
    #print (df)
    gp = ggplot2.ggplot (df)
  
    pp = gp + \
        ggplot2.aes_string (x='Problem', y='Difference', fill='Language') + \
        ggplot2.geom_bar (position='dodge', stat='identity') 
    pp.plot ()
    r['dev.off']()

def hist_langs (results):
  r = robjects.r

  for language in languages:
    varis = []
    probs = []
    locs  = []
    for (lang, prob, var) in results.keys():
      if lang == language:
        loc = results [(lang, prob, var)]
        varis.append (pretty_names [var])
        probs.append (prob)
        locs.append (loc)
    r.pdf ('bargraph-loc-' + language + '.pdf')
    df = robjects.DataFrame({'Variation': StrVector (varis),
                             'Problem': StrVector (probs),
                             'Lines' : IntVector (locs),
      })
    
    #print (df)
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

