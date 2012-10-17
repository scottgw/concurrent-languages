#!/usr/bin/env python
import csv

import os.path
from os.path import abspath, normpath

import subprocess

import rpy2.robjects as robjects
import rpy2.robjects.lib.ggplot2 as ggplot2
from rpy2.robjects.packages import importr
from rpy2.robjects import FloatVector, StrVector, IntVector, DataFrame

def ggplot2_options ():
  return ggplot2.opts (**{'axis.title.x' : ggplot2.theme_blank(),
                          'axis.title.y' : ggplot2.theme_text(family = 'serif', face = 'bold', size = 15, angle=90, vjust=0.2),
                          'axis.text.x' : ggplot2.theme_text(family = 'serif', size = 15),
                          'axis.text.y' : ggplot2.theme_text(family = 'serif', size = 15),
                          'legend.title' : ggplot2.theme_text(family = 'serif', face = 'bold', size = 15),
                          'legend.text' : ggplot2.theme_text(family = 'serif', size = 15),
    })

  
bargraph_dir = os.path.abspath(".")
pretty_varis = {"seq"      : "Sequential",
                "par"      : "Parallel",
                "expertseq": "Sequential (expert)",
                "expertpar": "Parallel (expert)"
                }
pretty_langs = {"chapel"   : "Chapel",
                "cilk"     : "Cilk",
                "erlang"   : "Erlang",
                "go"       : "Go",
                "scoop"    : "SCOOP",
                "tbb"      : "TBB"
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
  results = get_results ()
  bargraph_variation (results)
  bargraph_variation_norm (results)
  bargraph_variation_diff (results)
  bargraph_language (results)
  
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

def bargraph_variation (results):
  r = robjects.r

  for variation in variations:
    langs = []
    probs = []
    locs  = []
    for (lang, prob, var) in results.keys():
      if var == variation:
        loc = results [(lang, prob, var)]
        langs.append (pretty_langs [lang])
        probs.append (prob)
        locs.append (loc)
    r.pdf ('bargraph-loc-var-' + variation + '.pdf')
    df = robjects.DataFrame({'Language': StrVector (langs),
                             'Problem': StrVector (probs),
                             'Lines' : IntVector (locs),
      })
    
    #print (df)
    gp = ggplot2.ggplot (df)
  
    pp = gp + \
        ggplot2.aes_string (x='Problem', y='Lines', fill='Language') + \
        ggplot2.geom_bar (position='dodge', stat='identity') + \
        ggplot2_options () + \
        robjects.r('ylab("Lines of Code")')
    pp.plot ()
    r['dev.off']()

def bargraph_variation_norm (results):
  r = robjects.r

  for variation in variations:
    langs = []
    probs = []
    locs  = []
    for problem in problems:
      results_filtered = { key: results[key] for key in [ (lang, problem, variation) for lang in languages ] }
      loc_min = min (results_filtered.values())
      
      for (lang, prob, var) in results_filtered.keys():
        loc_norm = (float (results_filtered [(lang, prob, var)])) / float(loc_min)
        langs.append (pretty_langs [lang])
        probs.append (prob)
        locs.append (loc_norm)

    r.pdf ('bargraph-loc-var-norm-' + variation + '.pdf')
    df = robjects.DataFrame({'Language': StrVector (langs),
                             'Problem': StrVector (probs),
                             'Lines' : FloatVector (locs),
      })
    
    #print (df)
    gp = ggplot2.ggplot (df)
  
    pp = gp + \
        ggplot2.aes_string (x='Problem', y='Lines', fill='Language') + \
        ggplot2.geom_bar (position='dodge', stat='identity') + \
        ggplot2_options () + \
        robjects.r('ylab("Lines of Code (normalized to smallest)")')
    pp.plot ()
    r['dev.off']()

def bargraph_variation_diff (results):
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

        langs.append (pretty_langs [lang])
        probs.append (prob)
        diffs.append (diff)

    r.pdf ('bargraph-loc-diff-' + standard + '.pdf')
    df = robjects.DataFrame({'Language': StrVector (langs),
                             'Problem': StrVector (probs),
                             'Difference' : IntVector (diffs),
      })
    
    #print (df)
    gp = ggplot2.ggplot (df)
  
    pp = gp + \
        ggplot2.aes_string (x='Problem', y='Difference', fill='Language') + \
        ggplot2.geom_bar (position='dodge', stat='identity') + \
        ggplot2_options () + \
        robjects.r('ylab("Lines of code difference (in percent)")')
    pp.plot ()
    r['dev.off']()

def bargraph_language (results):
  r = robjects.r

  for language in languages:
    varis = []
    probs = []
    locs  = []
    for (lang, prob, var) in results.keys():
      if lang == language:
        loc = results [(lang, prob, var)]
        varis.append (pretty_varis [var])
        probs.append (prob)
        locs.append (loc)
    r.pdf ('bargraph-loc-lang-' + language + '.pdf')
    df = robjects.DataFrame({'Variation': StrVector (varis),
                             'Problem': StrVector (probs),
                             'Lines' : IntVector (locs),
      })
    
    #print (df)
    gp = ggplot2.ggplot (df)
  
    pp = gp + \
        ggplot2.aes_string (x='Problem', y='Lines', fill='Variation') + \
        ggplot2.geom_bar (position='dodge', stat='identity') + \
        ggplot2_options () + \
        robjects.r('ylab("Lines of Code")')
    pp.plot ()
    r['dev.off']()
    
def break_path (path):
  parts = normpath (path).split(os.sep)
  # we take 2 3 and 4 to account for the ../../ beginning,
  # we only want the language, problem and variation
  return (parts [2], parts [3], parts [4])

if __name__ == '__main__':
  main ()

