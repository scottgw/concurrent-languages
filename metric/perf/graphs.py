#!/usr/bin/env python
from problems import *
from utils import *
from config import *

import rpy2.robjects as robjects
import rpy2.robjects.lib.ggplot2 as ggplot2
#ggplot2.theme_set(ggplot2.theme_bw ())
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

bargraph_dir = os.path.abspath("../time/graph")
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

def main():
  results = get_results()

  bargraph_language (cfg, results[threads[-1]])
  bargraph_variation(cfg, results[threads[-1]])
  bargraph_variation_diff (cfg, results[threads[-1]])
  speedup_lang_var (cfg, results, 'fastest') # p1, seq, fastest
  speedup_prob_var (cfg, results, 'fastest') # p1, seq, fastest
  mem_usage_graph (cfg)

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

def mem_usage_graph (cfg):
  r = robjects.r
  varis = []
  langs = []
  probs = []
  mems  = []
  for var in cfg.variations:
    for lang in cfg.languages:
      for prob in cfg.problems:
        mem_filename = get_mem_output (lang, prob, var)
        with open (mem_filename, 'r') as mem_file:
          mem = mem_file.readline()
          mems.append (float (mem))
        varis.append (pretty_varis [var])
        langs.append (pretty_langs [lang])
        probs.append (prob)

  # memory usage is a simple histogram with all information in one graph.
  r.pdf ('bargraph-memusage.pdf')
  df = robjects.DataFrame({'Language': StrVector (langs),
                           'Problem': StrVector (probs),
                           'Variation' : StrVector (varis),
                           'Mem' : FloatVector (mems)
                           })

  gp = ggplot2.ggplot (df)

  # we rotate the x labels to make sure they don't overlap
  pp = gp  +\
      ggplot2.opts (**{'axis.text.x': ggplot2.theme_text (angle = 90, hjust=1)}) + \
      ggplot2.aes_string (x='Problem', y='Mem', fill='Language') + \
      ggplot2.geom_bar (position='dodge', stat='identity') + \
      ggplot2.facet_wrap ('Variation') + \
      ggplot2_options () + \
      robjects.r('ylab("Memory usage (in bytes)")')# + \
#      ggplot2.scale_y_log10()
  pp.plot ()
  r['dev.off']()

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

def bargraph_language (cfg, values):
  r = robjects.r
  for lang in cfg.languages:
    times = []
    varss = []
    probs = []
    ses   = []

    for prob in cfg.problems:
      for var in cfg.variations:
        # we use the pretty names to make the 
        varss.append (pretty_varis [var])
        probs.append (prob)

        data = FloatVector (values[prob][var][lang][0])
        times.append (r['mean'] (data)[0])

        t_result = r['t.test'] (data, **{"conf.level": 0.975}).rx ('conf.int')[0]
        ses.append ((t_result[1] - t_result[0])/2)

    r.pdf ('bargraph-executiontime-lang-' + lang + '.pdf')
    df = robjects.DataFrame({'Variation': StrVector (varss),
                             'Problem': StrVector (probs),
                             'Time' : FloatVector (times),
                             'SE' : FloatVector (ses)
                             })

    limits = ggplot2.aes (ymax = 'Time + SE', ymin = 'Time - SE')
    dodge = ggplot2.position_dodge (width=0.9)

    gp = ggplot2.ggplot (df)

    pp = gp + \
        ggplot2.aes_string (x='Problem', y='Time', fill='Variation') + \
        ggplot2.geom_bar (position='dodge', stat='identity') + \
        ggplot2.geom_errorbar (limits, position=dodge, width=0.25) + \
        ggplot2_options () + \
        robjects.r('ylab("Execution time (in seconds)")') 
    pp.plot ()
    r['dev.off']()


def bargraph_variation (cfg, values):
  r = robjects.r
  for var in cfg.variations:
    # each variation gets plot
    avgs = []
    ses = []

    # normalized values
    navgs = []
    nses = []

    langs = []
    probs = []

    for prob in cfg.problems:
      # aggregate by problems
      lavgs = []
      lses = []
      for lang in cfg.languages:
        # each problem displays a list of language times for that problem
        data = FloatVector (values[prob][var][lang][0])
        
        langs.append (pretty_langs [lang])
        probs.append (prob)
        mean = r['mean'] (data)[0]
        lavgs.append (mean)

        t_result = r['t.test'] (data, **{"conf.level": 0.975}).rx ('conf.int')[0]
        lses.append ((t_result[1] - t_result[0])/2)
        
      avgs.extend (lavgs)
      ses.extend (lses)
        
      lmin = min (lavgs)
      navgs.extend ([la/lmin for la in lavgs])
      nses.extend ([ls/lmin for ls in lses])


    df = robjects.DataFrame({'Language': StrVector (langs),
                             'Problem': StrVector (probs),
                             'Time' : FloatVector (avgs),
                             'SE' : FloatVector (ses),
                             'NormTime' : FloatVector (navgs),
                             'NormSE' : FloatVector (nses),
                             'TimeLabel' : StrVector ([str(round(time, 1)) + "s" for time in avgs])
                             })

    # plot histogram of actual times
    r.pdf ('bargraph-executiontime-var-' + var + '.pdf')


    limits = ggplot2.aes (ymax = 'Time + SE', ymin = 'Time - SE')
    dodge = ggplot2.position_dodge (width=0.9)
    gp = ggplot2.ggplot (df)

    pp = gp + \
        ggplot2.aes_string (x='Problem', y='Time', fill='Language') + \
        ggplot2.geom_bar (position='dodge', stat='identity') + \
        ggplot2.geom_errorbar (limits, position=dodge, width=0.25) + \
        ggplot2_options () + \
        robjects.r('ylab("Execution time (in seconds)")')
 
    pp.plot ()

    # plot histogram of times normalized with respect to fastest time for a problem
    r.pdf ('bargraph-executiontime-var-norm-' + var + '.pdf')

    limits = ggplot2.aes (ymax = 'NormTime + NormSE', ymin = 'NormTime - NormSE')
    dodge = ggplot2.position_dodge (width=0.9)
    gp = ggplot2.ggplot (df)

    pp = gp + \
        ggplot2.aes_string (x='Problem', y='NormTime', fill='Language') + \
        ggplot2.geom_bar (position='dodge', stat='identity') + \
        ggplot2.geom_errorbar (limits, position=dodge, width=0.25) +\
        ggplot2_options () + \
        robjects.r('ylab("Execution time (normalized to fastest)")')
        #ggplot2.geom_text(data=df,
        #                  mapping = ggplot2.aes_string (x='Problem', 
        #                                                y='NormTime + NormSE + 0.1', 
        #                                                label='TimeLabel')
 
    pp.plot ()
    r['dev.off']()

def bargraph_variation_diff (cfg, values):
  r = robjects.r

  for (standard, expert) in [('seq', 'expertseq'), ('par', 'expertpar')]:
    langs = []
    probs = []
    diffs  = []
    for lang in cfg.languages:
      for prob in cfg.problems:
        data = FloatVector (values[prob][standard][lang][0])
        data_expert = FloatVector (values[prob][expert][lang][0])

        mean = r['mean'] (data)[0]
        mean_expert = r['mean'] (data_expert)[0]
        diff = (float(mean_expert) / float(mean) - 1) * 100

        langs.append (pretty_langs [lang])
        probs.append (prob)
        diffs.append (diff)

    r.pdf ('bargraph-executiontime-diff-' + standard + '.pdf')
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
        robjects.r('ylab("Execution time difference (in percent)")')
    pp.plot ()
    r['dev.off']()


# produce a graph for each langauge/variation that shows 
# the speed up for each problem

# produce a graph for each problem/variation that shows all languages
# speedup
def speedup_prob_var (cfg, values, basis):
  for var in cfg.variations:
    if var.find ('par') >= 0:
      for prob in cfg.problems:
        base_selector = lambda lang:\
          values [cfg.threads[-1]][prob][var.replace ('par','seq')][lang][0]

        selector = lambda lang: lambda thread:\
            values [thread][prob][var][lang][0]
        line_plot (cfg, var, prob, 'Language', cfg.languages, selector, base_selector, basis)

# produce a graph for each language/variation that shows all problems
# speedups
def speedup_lang_var (cfg, values, basis):
  for var in cfg.variations:
    if var.find ('par') >= 0:
      for lang in cfg.languages:
        base_selector = lambda prob:\
          values [cfg.threads[-1]][prob][var.replace ('par','seq')][lang][0]

        selector = lambda prob: lambda thread:\
            values [thread][prob][var][lang][0]
        line_plot (cfg, var, lang, 'Problem', cfg.problems, selector, base_selector, basis)


# Function to plot lines for a given dataset. Here these
# datasets will be either per-problem or per-language.
def line_plot (cfg, var, control, change_name, changing, selector, base_selector, basis):
  r = robjects.r

  r.pdf ('speedup-' + var + '-' + control + '.pdf')

  speedups = []
  thrds = []
  changes = []

  for n in cfg.threads:
    speedups.append (n)
    thrds.append (n)
    changes.append ('ideal')

  for c in changing:
    sel  = selector (c)
    # sequential base
    base = robjects.r.mean (FloatVector (base_selector(c)))[0]
    # base with p = 1
    base_p1 = robjects.r.mean (FloatVector (sel(1)))[0]
    # use fastest sequential program
    if basis == 'fastest' and base_p1 < base:
      base = base_p1
    elif basis == 'seq':
      pass
    elif basis == 'p1':
      base = base_p1
      
    for n in cfg.threads:
      mn = r.mean (FloatVector (sel(n)))[0]
      speedups.append (base / mn)
      # plot slowdowns
      #speedups.append (-mn/base)#(base / mn)
      thrds.append (n)
      changes.append (c)

  df = DataFrame ({'Speedup': FloatVector (speedups),
                   'Threads': IntVector (thrds),
                   change_name: StrVector (changes)
                   })
  ideal_changing = ['ideal']
  ideal_changing.extend (changing)
  legendVec = IntVector (range (len (ideal_changing)))
  legendVec.names = StrVector (ideal_changing)

  gg = ggplot2.ggplot (df)

  pp = gg + \
      ggplot2.geom_line() + ggplot2.geom_point() +\
      ggplot2.aes_string(x='Threads', y='Speedup', 
                         group=change_name, color=change_name, 
                         shape=change_name) +\
      ggplot2.scale_shape_manual(values=legendVec) + \
      ggplot2_options () + \
      ggplot2.opts (**{'axis.title.x' : ggplot2.theme_text(family = 'serif', face = 'bold', size = 15, vjust=-0.2)}) + \
      robjects.r('ylab("Speedup")') + \
      robjects.r('xlab("Cores")')

      # ggplot2.xlim (min(threads), max(threads)) + ggplot2.ylim(min(threads), max(threads)) +\
  pp.plot()

  r['dev.off']()

if __name__ == '__main__':
  main ()

