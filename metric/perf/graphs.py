from rpy import r 

from stats import *
from problems import *
from utils import *

r.library ("gplots")

INVALID = 999
bargraph_dir = os.path.abspath("../time/graph")

def create_graphs2 (cfg, values):
  for var in cfg.variations:
    # each variation gets plot
    cluster = []
    for prob in cfg.problems:
      # aggregate by problems
      for lang in cfg.languages:
        # each problem displays a list of language times for that problem
        cluster.append (values[prob][var][lang][0])

    counts = r.matrix (cluster, len (cfg.languages), len (cfg.problems))
    r.pdf ('bargraph-time-' + var + '.pdf')
    r.barplot2 (counts, beside=True, names=cfg.problems, xpd=False, 
        legend=cfg.languages, **{"plot.grid" : True})
    r.title (main = 'Time for ' + var + ' in seconds')
    r.dev_off()


# produce a graph for each langauge/variation that shows 
# the speed up for each problem

# produce a graph for each problem/variation that shows all languages
# speedup


# produce a graph for each language/variation that shows all problems
# speedups
def speedup_lang_var (cfg, values):
  for var in cfg.variations:
    if var.find ('par') >= 0:
      for lang in cfg.languages:
        r.pdf ('speedup-' + var + '-' + lang + '.pdf')
        r.plot (cfg.threads, cfg.threads)
        for prob in cfg.problems:
          cluster = []
          thread1 = values[1][prob][var][lang][0]
          for thread in cfg.threads:
            cluster.append (thread1 / values[thread][prob][var][lang][0])
          r.lines (cfg.threads, cluster, type='o', pch=cfg.problems.index (prob) + 1)
        r.dev_off()

def create_graph(cfg, graph_name, values, pretty_name, is_relative=False):
  variation_names = {"seq" : "Sequential"
                     , "par" : "Parallel"
                     , "expertseq": "Expert sequential"
                     , "expertpar": "Expert parallel"
                     }
  for i in range(len(cfg.inputs)):
    nmax = 0
    for (language, problem, variation) in get_all(cfg):
      cur = values[problem][variation][language][i]
      if cur == INVALID: continue
      if cur > nmax: nmax = cur

    if is_relative:
      nmax = 16

    for variation in cfg.variations:
      out = []
      out.append("=cluster")
      for language in sorted(cfg.languages):
        out.append(";" + language)
      out.append((
          "\n"
          "colors=light_green,yellow,red,med_blue,cyan,dark_green\n"
          "=table\n"
          "yformat=%g\n"
          "=norotate\n"
          "xscale=1\n"))
      variation_name = variation_names[variation]
      if nmax == 0: nmax = 1
      out.append("max=%f\n" % (nmax * 1.1))
      if is_relative:
        out.append(
            "ylabel=%s %sexecution time (in seconds) relative to smallest\n" % (
                variation_name, pretty_name))
      else:
        out.append(
            "ylabel=%s %sexecution time (in seconds)\n" % (
                variation_name, pretty_name))
      for problem in sorted(cfg.problems):
        out.append(problem)
        nmin = 1
        if is_relative:
          nmin = min(float(values[problem][variation][l][i]) for l in cfg.languages)
        for language in sorted(cfg.languages):
          out.append(" %.10f" % (
            float(values[problem][variation][language][i]) / nmin))
        out.append("\n")

      if not is_relative:
        out.append("\n=yerrorbars\n")
        for problem in sorted(cfg.problems):
          out.append(problem)
          for language in sorted(cfg.languages):
            out.append(" %.10f" % (get_tdelta(
                cfg.all_values[cfg.threads[-1]][problem][variation][language][i],
                                cfg.alpha)))
          out.append("\n")

      output_file_name = "graph-%s-%s-%d" % (graph_name, variation, i)
      if is_relative:
        output_file_name = "graph-rel-%s-%s-%d" % (
            graph_name, variation, i)
      output_file = "%s/images/%s" % (
              cfg.output_dir, output_file_name)
      write_to_file("%s.perf" % (output_file), ''.join(out))

      cmd = (os.path.join (bargraph_dir, "bargraph.pl") + " -fig %s.perf | fig2dev -L ppm -m 4 > %s.ppm" % (output_file, output_file))
      print cmd
      system(cmd)
      cmd = ("mogrify -reverse -flatten %s.ppm" % output_file)
      system(cmd)
      cmd = ("mogrify -resize %dx%d -format png %s.ppm" % (
          cfg.graph_size, cfg.graph_size, output_file))
      system(cmd)

def create_speedup_graph(cfg, graph_name, values, use_subfigure=True):
  for problem in sorted(cfg.problems):
    for language in sorted(cfg.languages):
      for i in range(len(cfg.inputs)):
        for variation in ["par","expertpar"]:
          out = []
          for nthreads in cfg.threads:
            tseq = values[1][problem][variation][language][i]
            cur = values[nthreads][problem][variation][language][i]

            print ("base: ", tseq, " current:", cur)

            # if cur == INVALID or cur == 0: continue
            out.append("%d\t%.10f\t%.10f\t%d\t%.10f\t1\n" % (
                nthreads, cur, tseq / cur, nthreads,
                tseq / (nthreads * cur)))

          output_file_name = "graph-%s-%s-%s-%s-%d" % (
              graph_name, language, problem, variation, i)
          output_file = "%s/images/%s" % (cfg.output_dir, output_file_name)
          write_to_file("%s.dat" % output_file, ''.join(out))
          cmd = "cp %s.dat plot.dat" % (output_file)
          system(cmd)
          print cmd
          cmd = "gnuplot %s/plot.script" % (cfg.output_dir)
          system(cmd)
          cmd = "mv plot.png %s/images/%s.png" % (cfg.output_dir, output_file_name)
          system(cmd)
          cmd = "rm plot.dat"
          system(cmd)


def create_problem_speedup_graph(cfg, graph_name, speedup_graph_name, use_subfigure=True):
 for problem in sorted(cfg.problems):
    for i in range(len(cfg.inputs)):
      out = []
      out.append('''
set xrange [0:8]
set xtics 1
set yrange [0:8]
set ytics 1
set xlabel "cfg.threads"
set ylabel "speedup"
set terminal png
set output "plot.png"
set key left
''')
      out.append("plot ")
      first = True
      for language in sorted(cfg.languages):
        output_file_name = "graph-%s-%s-%s-%d" % (
            speedup_graph_name, language, problem, i)
        output_file = "%s/images/%s" % (cfg.output_dir, output_file_name)
        if first:
          first = False
          out.append("'%s.dat' using 1:4 title 'ideal speedup' w lp" % (
              output_file))
        out.append(", ")
        out.append("'%s.dat' using 1:3 title '%s speedup' w lp" % (
            output_file, language))
      output_file_name = "graph-%s-%s-%d" % (graph_name, problem, i)
      script_name = 'other.script'
      write_to_file(script_name, ''.join(out))
      system('gnuplot %s' % script_name)
      cmd = "mv plot.png %s/images/%s.png" % (cfg.output_dir, output_file_name)
      system(cmd)
      cmd = 'rm %s' % script_name
      system(cmd)

def create_language_speedup_graph(cfg, graph_name, speedup_graph_name, use_subfigure=True):
  for language in sorted(cfg.languages):
    for i in range(len(cfg.inputs)):
      out = []
      out.append('''
set xrange [0:8]
set xtics 1
set yrange [0:8]
set ytics 1
set xlabel "cfg.threads"
set ylabel "speedup"
set terminal png
set output "plot.png"
set key left
''')
      out.append("plot ")
      first = True
      for problem in sorted(cfg.problems):
        output_file_name = "graph-%s-%s-%s-%d" % (
            speedup_graph_name, language, problem, i)
        output_file = "%s/images/%s" % (cfg.output_dir, output_file_name)
        if first:
          first = False
          out.append("'%s.dat' using 1:4 title 'ideal speedup' w lp" % (
              output_file))
        out.append(", ")
        out.append("'%s.dat' using 1:3 title '%s speedup' w lp" % (
            output_file, problem))
      
      output_file_name = "graph-%s-%s-%d" % (graph_name, language, i)
      script_name = 'other.script'
      write_to_file(script_name, ''.join(out))
      system('gnuplot %s' % script_name)
      cmd = "mv plot.png %s/images/%s.png" % (cfg.output_dir, output_file_name)
      system(cmd)
      cmd = 'rm %s' % script_name
      system(cmd)


