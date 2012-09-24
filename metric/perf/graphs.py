from rpy import r 

from problems import *
from utils import *

r.library ("gplots")

bargraph_dir = os.path.abspath("../time/graph")
pretty_names = {"seq" : "Sequential"
                   ,"par" : "Parallel"
                   ,"expertseq": "Expert sequential"
                   , "expertpar": "Expert parallel"
                   }

def hist_graphs (cfg, values):
  for var in cfg.variations:
    # each variation gets plot
    avgs = []
    cis = []

    for prob in cfg.problems:
      # aggregate by problems
      for lang in cfg.languages:
        # each problem displays a list of language times for that problem
        v = values[prob][var][lang][0]
        avgs.append (v.avg)
        cis.append (v.ci)

    def lang_matrix (v):
      return r.matrix (v, len (cfg.languages), len (cfg.problems))

    counts = lang_matrix (avgs)

    # extract lower and upper bounds of confidence interval
    lowers, uppers = zip (*map(lambda ci: ci ['conf.int'], cis))    
    lower_bound = lang_matrix (lowers)
    upper_bound = lang_matrix (uppers)

    r.pdf ('bargraph-time-' + var + '.pdf')
    r.barplot2 (counts, beside=True, names=cfg.problems, xpd=False, 
                legend=cfg.languages,
                **{"plot.grid" : True, "plot.ci": True, 
                   "ci.l": lower_bound, "ci.u": upper_bound})

    r.title (main = pretty_names [var] + ' execution time (seconds)')
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
    thread1 = sel(1).avg
    for thread in cfg.threads:
      cluster.append (thread1 / sel(thread).avg)
    r.lines (cfg.threads, cluster, type='o', pch=changing.index (c))

  r.legend ("topright", None, legend=changing, pch=range(len(changing)))
  r.dev_off()
