#!/usr/bin/env python
from rpy import r 

from problems import *
from utils import *
from config import *

r.library ("gplots")
r.library ("ggplot2")

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
#  create_graph(cfg, "exec-time", results[threads[-1]], "")
#  create_graph(cfg, "exec-time", results[threads[-1]], "", is_relative=True)
#  speedup_graph_name = 'speedup'
#  create_speedup_graph(cfg, speedup_graph_name, results)
#  create_problem_speedup_graph(cfg, "problem-speedup", speedup_graph_name)
#  create_language_speedup_graph(cfg, "language-speedup", speedup_graph_name)


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
        #print time_output
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
  se_q = q * sqrt (sem_a^2/mean_a^2 + sem_b^2/mean_b^2)

  return (q - t * se_q, q + t * se_q)


def hist_graphs (cfg, values):
  for var in cfg.variations:
    # each variation gets plot
    avgs = []
    cis = []

    for prob in cfg.problems:
      # aggregate by problems
      for lang in cfg.languages:
        # each problem displays a list of language times for that problem
        data = values[prob][var][lang][0]
        
        avgs.append (r.mean (data))
        cis.append (r.t_test (data, **{"conf.level": 0.975}))

    def lang_matrix (v):
      return r.matrix (v, len (cfg.languages), len (cfg.problems))

    counts = lang_matrix (avgs)

    # extract lower and upper bounds of confidence interval
    lowers, uppers = zip (*map(lambda ci: ci ['conf.int'], cis))    
    lower_bound = lang_matrix (lowers)
    upper_bound = lang_matrix (uppers)

    r.pdf ('bargraph-time-' + var + '.pdf')
    # r.barplot2 (counts, beside=True, names=cfg.problems, xpd=False, 
    #             legend=cfg.languages,
    #             **{"plot.grid" : True, "plot.ci": True, 
    #                "ci.l": lower_bound, "ci.u": upper_bound})

    # r.title (main = pretty_names [var] + ' execution time (seconds)')
    r.ggplot (counts, 
              r.aes (x="dose", y="len", fill="supp") +
              r["geom_bar"] (position=r["position_dodge"]()))
    r.dev_off()


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
        line_plot (cfg, var, prob, cfg.languages, selector)

# produce a graph for each language/variation that shows all problems
# speedups
def speedup_lang_var (cfg, values):
  for var in cfg.variations:
    if var.find ('par') >= 0:
      for lang in cfg.languages:
        selector = lambda prob: lambda thread:\
            values [thread][prob][var][lang][0]
        line_plot (cfg, var, lang, cfg.problems, selector)


# Function to plot lines for a given dataset. Here these
# datasets will be either per-problem or per-language.
def line_plot (cfg, var, control, changing, selector):
  r.pdf ('speedup-' + var + '-' + control + '.pdf')
  r.plot (cfg.threads, cfg.threads, type='l',
          yaxp = (0, max(cfg.threads), 8),
          ylim=(0, max(cfg.threads)*1.15))

  r.lines (cfg.threads, cfg.threads)

  for c in changing:
    cluster = []
    sel     = selector (c)
    thr_1_data = sel(1)

    for n in cfg.threads:
      cluster.append (r.mean (sel(1)) / r.mean (sel(n)))
    r.lines (cfg.threads, cluster, type='o', pch=changing.index (c))

  r.legend ("topright", None, legend=changing, pch=range(len(changing)))
  r.dev_off()

if __name__ == '__main__':
  main ()

