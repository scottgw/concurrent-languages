#!/usr/bin/env python
from problems import *
from utils import *
from config import *

import rpy2.robjects as robjects
import rpy2.robjects.lib.ggplot2 as ggplot2
from rpy2.robjects.packages import importr
from rpy2.robjects import FloatVector, StrVector, IntVector, DataFrame


bargraph_dir = os.path.abspath("../time/graph")
pretty_names = {"seq"      : "Sequential",
                "par"      : "Parallel",
                "expertseq": "Expert sequential",
                "expertpar": "Expert parallel"
                }

def main():
  results = get_results()

  hist_graphs(cfg, results[threads[-1]])
  speedup_lang_var (cfg, results)
  speedup_prob_var (cfg, results)

def get_results():
  results = {}
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
        cur = read_file_values(time_output)

        results[nthreads][problem][variation][language][i] = cur
  return results

# fieller's method for calculating confidence intervals
# for ratios of means.
def fieller (aa, bb):
  mean_a = r.mean (aa)
  mean_b = r.mean (bb)

  q   = mean_a/mean_b

  sem_a = r.sd (aa) / sqrt (len (aa))
  sem_b = r.sd (bb) / sqrt (len (bb))

  t   = r.qt (0.975, len (aa))
  se_q = q * sqrt (sem_a**2/mean_a**2 + sem_b**2/mean_b**2)

  return (q - t * se_q, q + t * se_q)


def hist_graphs (cfg, values):
  r = robjects.r
  for var in cfg.variations:
    # each variation gets plot
    avgs = []
    ses = []

    langs = []
    probs = []

    for prob in cfg.problems:
      # aggregate by problems
      for lang in cfg.languages:
        # each problem displays a list of language times for that problem
        data = FloatVector (values[prob][var][lang][0])
        
        langs.append (lang)
        probs.append (prob)
        avgs.append (r['mean'] (data)[0])

        t_result = r['t.test'] (data, **{"conf.level": 0.975}).rx ('conf.int')[0]
        ses.append ((t_result[1] - t_result[0])/2)

    r.pdf ('bargraph-time-' + var + '.pdf')

    df = robjects.DataFrame({'Language': StrVector (langs),
                             'Problem': StrVector (probs),
                             'Time' : FloatVector (avgs),
                             'SE' : FloatVector (ses)
                             })

    limits = ggplot2.aes (ymax = 'Time + SE', ymin = 'Time - SE')
    dodge = ggplot2.position_dodge (width=0.9)
    gp = ggplot2.ggplot (df)

    pp = gp + \
        ggplot2.aes_string (x='Problem', y='Time', fill='Language') + \
        ggplot2.geom_bar (position='dodge', stat='identity') + \
        ggplot2.geom_errorbar (limits, position=dodge, width=0.25)
 
    pp.plot ()
    r['dev.off']()


# produce a graph for each langauge/variation that shows 
# the speed up for each problem

# produce a graph for each problem/variation that shows all languages
# speedup
def speedup_prob_var (cfg, values):
  for var in cfg.variations:
    if var.find ('par') >= 0:
      for prob in cfg.problems:
        selector = lambda lang: lambda thread:\
            values [thread][prob][var][lang][0]
        line_plot (cfg, var, prob, 'Language', cfg.languages, selector)

# produce a graph for each language/variation that shows all problems
# speedups
def speedup_lang_var (cfg, values):
  for var in cfg.variations:
    if var.find ('par') >= 0:
      for lang in cfg.languages:
        selector = lambda prob: lambda thread:\
            values [thread][prob][var][lang][0]
        line_plot (cfg, var, lang, 'Problem', cfg.problems, selector)


# Function to plot lines for a given dataset. Here these
# datasets will be either per-problem or per-language.
def line_plot (cfg, var, control, change_name, changing, selector):
  r = robjects.r

  r.pdf ('speedup-' + var + '-' + control + '.pdf')

  speedups = []
  thrds = []
  changes = []

  for c in changing:
    sel     = selector (c)
    thr_1_data = sel(1)

    for n in cfg.threads:
      m1 = r.mean (FloatVector (sel(1)))[0]
      mn = r.mean (FloatVector (sel(n)))[0]
      speedups.append (m1 / mn)
      thrds.append (n)
      changes.append (c)

  df = DataFrame ({'Speedup': FloatVector (speedups),
                   'Threads': IntVector (thrds),
                   change_name: StrVector (changes)
                   })

  gg = ggplot2.ggplot (df)
  pp = gg + ggplot2.geom_line() + \
      ggplot2.aes_string(x='Threads', y='Speedup', 
                         group=change_name, color=change_name)

  pp.plot()

  r['dev.off']()

if __name__ == '__main__':
  main ()

