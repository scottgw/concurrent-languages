from rpy import r 

from problems import *
from utils import *

r.library ("gplots")

bargraph_dir = os.path.abspath("../time/graph")
variation_names = {"seq" : "Sequential"
                   ,"par" : "Parallel"
                   ,"expertseq": "Expert sequential"
                   , "expertpar": "Expert parallel"
                   }

def create_graphs2 (cfg, values):
  for var in cfg.variations:
    # each variation gets plot
    avgs = []
    std_devs = []
    for prob in cfg.problems:
      # aggregate by problems
      for lang in cfg.languages:
        # each problem displays a list of language times for that problem
        v = values[prob][var][lang][0]
        std_devs.append (v.std_dev)
        avgs.append (v.avg)

    def lang_matrix (v):
      return r.matrix (v, len (cfg.languages), len (cfg.problems))

    counts = lang_matrix (avgs)

    zipped = zip (avgs, std_devs)

    lower_bound = lang_matrix ([(av - std/2) for (av, std) in zipped])
    upper_bound = lang_matrix ([(av + std/2) for (av, std) in zipped])

    r.pdf ('bargraph-time-' + var + '.pdf')
    r.barplot2 (counts, beside=True, names=cfg.problems, xpd=False, 
                legend=cfg.languages,
                **{"plot.grid" : True, "plot.ci": True, 
                   "ci.l": lower_bound, "ci.u": upper_bound})

    r.title (main = 'Time for ' + var + ' in seconds')
    r.dev_off()


# produce a graph for each langauge/variation that shows 
# the speed up for each problem

# produce a graph for each problem/variation that shows all languages
# speedup
def speedup_prob_var (cfg, values):
  for var in cfg.variations:
    if var.find ('par') >= 0:
      for prob in cfg.problems:
        r.pdf ('speedup-' + var + '-' + prob + '.pdf')
        r.plot (cfg.threads, cfg.threads, type='l',
                yaxp = (0, max(cfg.threads), 8),
                ylim=(0, max(cfg.threads)*1.15))

        r.lines (cfg.threads, cfg.threads)
        for lang in cfg.languages:
          cluster = []
          thread1 = values[1][prob][var][lang][0].avg
          for thread in cfg.threads:
            cluster.append (thread1 / values[thread][prob][var][lang][0].avg)
          r.lines (cfg.threads, cluster, type='o',
                   pch=cfg.languages.index (lang))
        r.legend ("topright", None, legend=cfg.languages, 
                  pch=range(len(cfg.languages)))
        r.dev_off()

# produce a graph for each language/variation that shows all problems
# speedups
def speedup_lang_var (cfg, values):
  for var in cfg.variations:
    if var.find ('par') >= 0:
      for lang in cfg.languages:
        r.pdf ('speedup-' + var + '-' + lang + '.pdf')
        r.plot (cfg.threads, cfg.threads, type='l',
                yaxp = (0, max(cfg.threads), 8),
                ylim=(0, max(cfg.threads)*1.15))

        r.lines (cfg.threads, cfg.threads)
        for prob in cfg.problems:
          cluster = []
          thread1 = values[1][prob][var][lang][0].avg
          for thread in cfg.threads:
            cluster.append (thread1 / values[thread][prob][var][lang][0].avg)
          r.lines (cfg.threads, cluster, type='o',
                   pch=cfg.problems.index (prob))
        r.legend ("topright", None, legend=cfg.problems, 
                  pch=range(len(cfg.problems)))
        r.dev_off()
