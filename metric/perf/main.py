import os
import math

def mean(x):
  n = len(x)
  total = 0
  for xi in x:
    total += xi
  return total / n

def stddev(x):
  n = len(x)
  total = 0
  meansq = mean([xi * xi for xi in x])
  meanx = mean(x)
  sqmean = meanx * meanx
  return math.sqrt((n * meansq - n * sqmean) / (n - 1))

yindex = []
xindex = []
table = []

def get_t(alpha, df):
  alpha *= 100
  y = -1
  for i in range(len(yindex)):
    if yindex[i] >= alpha:
      y = i
      break
  x = -1
  for i in range(len(xindex)):
    if xindex[i] > df:
      x = i
      break
  return table[x][y]

def ttest(xa, xb, alpha):
  '''
  meana = mean(xa)
  meanb = mean(xb)
  sa = stddev(xa)
  sb = stddev(xb)
  meandif = meana - meanb
  na = len(xa)
  nb = len(xb)
  sa2na = sa * sa / na
  sb2nb = sb * sb / nb
  s = math.sqrt(sa2na + sb2nb)
  v = s * s * s * s / (
      (sa2na * sa2na / (na - 1)) + (sb2nb * sb2nb / (nb - 1))) - 2
  #print 'meana: %f\nsa2: %f\nna: %d\n' % (meana, sa * sa, na)
  #print 'meanb: %f\nsb2: %f\nnb: %d\n' % (meanb, sb * sb, nb)
  #print 'meandif: %f\ns: %f\nv: %f\n' % (meandif, s, v)
  #print 't %f %f' % (1 - alpha / 2, v)
  #t = float(raw_input())
  t = get_t(1 - alpha / 2, v)
  #print 't: %f\n' % t
  left = meandif - t * s
  right = meandif + t * s
  #print '%f: [%f, %f]' % (alpha, left, right)
  return left > 0
'''
  conf = 1 - alpha
  f = open('input.data', 'w')
  f.write('x y\n')
  for i in range(min(len(xa), len(xb))):
    f.write('%.10f\t%.10f\n' % (xa[i], xb[i]))
  f.close()
  cmd = '../r/test.r %f > out.data' % conf
  # cmd = '../r/test.r %f' % conf
  system(cmd)
  pvalue = read_file_values('out.data')[0]
  # print '%f ' % pvalue,
  # return pvalue <= alpha
  return pvalue
  #os.exit(0)

def get_tdelta(xa, alpha):
  meana = mean(xa)
  sa = stddev(xa)
  na = len(xa)
  t = get_t(1 - alpha / 2, na - 2)
  #print 'meana: %f\nsa2: %f\nna: %d\nt: %f' % (meana, sa * sa, na, t)
  return t * sa / math.sqrt(na)

def read_table():
  f = open('tdist.txt', 'r')
  linenum = 0
  for line in f:
    linenum += 1
    words = line.split()
    if linenum == 1:
      for i in range(len(words)):
        if i == 0:
          continue
        words[i] = words[i][0:-1]
        f = float(words[i])
        yindex.append(f)
    elif linenum == 2:
      continue;
    else:
      line = []
      for i in range(len(words)):
        if i == 0:
          xindex.append(float(words[i]))
        else:
          line.append(float(words[i]))
      table.append(line)
    #print words
  #f.close()
  #print yindex
  #print table


#languages = set(["chapel", "cilk", "erlang", "go", "scoop", "tbb"])
#languages = ["chapel", "cilk", "go", "tbb"]
#languages = ["chapel", 'cilk']
languages = ["erlang"]
#languages = ["chapel", "cilk", "go", "tbb", 'erlang']
#problems = set(["chain", "outer", "product", "randmat", "thresh", "winnow"])
#problems = ["randmat", "thresh"]
problems = ["randmat"]
variations = ["seq", "par"]
#variations = ["seq"]

def system(cmd, timeout=False):
  ret = os.system(cmd)
  if ret != 0 and not timeout:
    print cmd
    assert(False)

def write_to_file(output, content):
  f = open(output, 'w')
  f.write(content)
  f.close()

def read_from_file(file_name):
  f = open(file_name, 'r')
  content = f.read()
  f.close()
  return content

def read_file_values(file_name):
  result = []
  f = open(file_name, 'r')
  for line in f:
    try:
      value = float(line)
      result.append(value)
    except ValueError:
      f.close()
      return []
  f.close()
  return result

def get_directory(language, problem, variation=""):
  directory = "../../%s/%s" % (language, problem);
  if language != "cpp":
    directory += "/%s" % variation;
  return directory

def get_problems_with_variations():
  for problem in sorted(problems):
    for variation in sorted(variations):
      yield (problem, variation)

ERLANG_MAIN = ("#!/bin/sh\n"
               "cd ~/lucia/metric/perf/%s\n"
               "erl -noshell +S $1 -s main main is_bench -s init stop\n")

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

class Problem(object):
  def __init__(self):
    self.name = "problem"

  def input_file_name(self, data):
    return self.file_name(data) + ".in"

  def output_file_name(self, data):
    return self.file_name(data) + ".out"

  def file_name(self, data):
    return "%s_%d_%d_%d_%d_%d" % (self.name, data.nrows, data.ncols,
        data.seed, data.percent, data.nelts)

class RandmatProblem(Problem):
  def __init__(self):
    self.name = "randmat"

  def file_name(self, data):
    return "%s_%d_%d_%d" % (self.name, data.nrows, data.ncols, data.seed)

  def get_input(self, data):
    return "%d %d %d\n" % (data.nrows, data.ncols, data.seed)

class ThreshProblem(Problem):
  def __init__(self):
      self.name = "thresh"

  def file_name(self, data):
    return "%s_%d_%d_%d" % (self.name, data.nrows, data.ncols, data.percent)

  def get_input(self, data):
    return "%d %d %d\n" % (data.nrows, data.ncols, data.percent)

class WinnowProblem(Problem):
  def __init__(self):
      self.name = "winnow"

  def file_name(self, data):
    return "%s_%d_%d_%d_%d" % (
        self.name, data.nrows, data.ncols, data.percent, data.nelts)

  def get_input(self, data):
    return "%d %d %d\n" % (data.nrows, data.ncols, data.nelts)

class OuterProblem(Problem):
  def __init__(self):
      self.name = "outer"

  def file_name(self, data):
    return "%s_%d" % (self.name, data.nelts)

  def get_input(self, data):
    return "%d\n" % (data.nelts)

class ProductProblem(Problem):
  def __init__(self):
      self.name = "product"

  def file_name(self, data):
    return "%s_%d" % (self.name, data.nelts)

  def get_input(self, data):
    return "%d\n" % (data.nelts)

class ChainProblem(Problem):
  def __init__(self):
      self.name = "chain"

  def file_name(self, data):
    return "%s_%d_%d_%d_%d" % (self.name, data.nrows, data.seed,
        data.percent, data.nelts)

  def get_input(self, data):
    return "%d\n%d\n%d\n%d\n" % (data.nrows, data.seed, data.percent,
        data.nelts)

problem_classes = [RandmatProblem(), ThreshProblem(), WinnowProblem(),
    OuterProblem(), ProductProblem(), ChainProblem()]

problem_map = { Problem().name: Problem() }
for p in problem_classes:
  problem_map[p.name] = p

class ProblemInput(object):
  def __init__(self, nrows, ncols, seed, percent, nelts):
    self.nrows = nrows
    self.ncols = ncols
    self.seed = seed
    self.percent = percent
    self.nelts = nelts

# histogram size is 256, so other input values should probably be bigger
inputs = [
    #ProblemInput(100, 100, 666, 50, 100),
    #ProblemInput(250, 250, 666, 50, 125),
    #ProblemInput(250, 250, 666, 1, 25 * (2500 / 100.)),
# chapel-all
    #ProblemInput(250, 250, 666, 10, 25 * (2500 / 100.)),
    #ProblemInput(500, 500, 666, 50, 250),
    #ProblemInput(1000, 1000, 666, 50, 1000),
# chapel-all
    #ProblemInput(500, 500, 666, 10, 500 * (500 / 100.)),
# chapel-outer, chapel-product, chapel-chain
    #ProblemInput(500, 500, 666, 1, 500 * (500 / 100.)),
# chapel-outer, chapel-product, chapel-chain
    #ProblemInput(500, 500, 666, 2, 2 * 500 * (500 / 100.)),
    #ProblemInput(1000, 1000, 666, 1, 500 * (1000 / 100.)),
# chapel-winnow
    #ProblemInput(1000, 1000, 666, 1, 1000 * (1000 / 100.)),
    #ProblemInput(1000, 1000, 666, 2, 1500 * (1000 / 100.)),
# chapel-winnow
    #ProblemInput(2000, 2000, 666, 1, 2000 * (2000 / 100.)),
# chapel-randmat, chapel-thresh
    #ProblemInput(2000, 2000, 666, 50, 2000),
# chapel-randmat, chapel-thresh
    #ProblemInput(3000, 3000, 666, 50, 3000),
    #ProblemInput(10000, 10000, 666, 50, 10000),
# cilk-thresh
    #ProblemInput(20000, 20000, 666, 1, 1),
# cilk-winnow, cilk-outer, cilk-product, cilk-randmat?, cilk-chain
# cilk-all, tbb-all, all-all
    ProblemInput(20000, 20000, 666, 1, 10000),
# erlang
    #ProblemInput(2000, 20000, 666, 1, 10000),
# cilk-randmat
    #ProblemInput(30000, 30000, 666, 1, 1),
  ]

threads = [1, 2, 3, 4, 5, 6, 7, 8]
#threads = [1, 2, 3, 4]
#threads = [2, 4]
#threads = [1, 2, 8]
#threads = [1, 4]
#threads = [4]
#threads = [1, 2]

##

# ===== general =====
#inputs = ["10 10 55", "100 100 666", "100 250 777"] #, "100 1000 888"] #, "100 1000 888"]
#input_thresh = ["55", "66", "77", "55"]
#input_winnow = ["10", "100", "125", "250"]

# ===== chapel =====
#inputs = ["10000 10000 888"]
#input_thresh = ["55"]
#input_winnow = ["250"]

# ===== cilk =====
#inputs = ["10000 100000 888"]
#input_thresh = ["55"]
#input_winnow = ["250"]

# ===== tbb =====
#inputs = ["10000 100000 888"]
#input_thresh = ["55"]
#input_winnow = ["250"]

# ===== go =====
#inputs = ["10000 10000 888"]
#input_thresh = ["55"]
#input_winnow = ["250"]

# ===== erlang =====
#inputs = ["10000 1000 888"]
#input_thresh = ["55"]
#input_winnow = ["250"]

# ===== scoop =====
#inputs = ["4 25000 888"]
#input_thresh = ["55"]
#input_winnow = ["250"]

def create_inputs():
  for i in range(len(problem_classes)):
    problem = problem_classes[i]
    for j in range(len(inputs)):
      cur = inputs[j]
      file_name = problem.input_file_name(cur)
      write_to_file(file_name, problem.get_input(cur))

TIMEOUT = 3

def get_time_output(language, problem, variation, i, nthreads):
  return "time-%s-%s-%s-%d-%d.out" % (
      language, problem, variation, i, nthreads)

def run_all(redirect_output=True):
  # TODO: check processor usage
  for nthreads in threads:
    for (language, problem, variation) in get_all():
      if variation == 'seq' and nthreads != threads[-1]: continue
      for i in range(len(inputs)):
        #TODO: get time output file name
        time_output = get_time_output(
            language, problem, variation, i, nthreads)
        # TODO: refactor variations
        #print time_output
        cmd = ""
        if language == "go" and variation == 'par':
          cmd += "GOMAXPROCS=%d " % nthreads

        #cmd += "timeout %d " % (TIMEOUT)

        directory = get_directory(language, problem, variation)
        cmd += "/usr/bin/time -a -f %%e -o %s %s/" % (time_output,
            directory)

        if language == "erlang":
          cmd += "main.sh"
        elif language == "scoop":
          cmd += "main -i"
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
        system(cmd, timeout=True)
        value = read_from_file(time_output)
        print value

results = {}
all_values = {}
INVALID = 999

def get_results():
  for nthreads in threads:
    if nthreads not in results:
      results[nthreads] = {}
      all_values[nthreads] = {}
    for (language, problem, variation) in get_all():
      if variation == 'seq' and nthreads != threads[-1]: continue
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

ALPHA = 0.001

ttest_res = {}

def test_significance_speedup():
  for nthreads in threads:
    if nthreads == 1:
      continue
    for (language, problem, variation) in get_all():
      if variation == 'seq': continue

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
  for language in sorted(languages):
    for t in ['spseq', 'spprev']:
      # print '%s:%s' % (language, t)
      out = []
      out.append(
'''
\\begin{table}[htbp]
  %\\centering
  \\begin{tabular}{c|cccccc}
nthreads''')
      for problem in sorted(problems):
        out.append(' & %s' % problem)
      out.append('\\\\\n\\hline\n')
      for nthreads in threads:
        if nthreads == 1:
          continue
        out.append('%d' % nthreads)
        for problem in sorted(problems):
          out.append(' & %.3e' %  (
              ttest_res[t][nthreads][problem]['par'][language][0]))
        out.append('\\\\\n')
      out.append('''
  \end{tabular}
  \caption{p-values for speedup of language %s vs %s time}
  \label{tab:spd-pv-%s-%s}
\end{table}''' % (language, pretty[t], language, t))
      outstr = ''.join(out)
      output_file_name = "table-pvalue-%s-%s.tex" % (language, t)
      output_file = "%s/chapters/%s" % (
              output_dir, output_file_name)
      write_to_file(output_file, outstr)

def test_significance():
  ttest_types = ['spseq', 'spprev', 'seq', 'par']
  for t in ttest_types:
    ttest_res[t] = {}

  # test_significance_speedup()

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
            if la not in ttest_res['seq']:
              ttest_res['seq'][la] = {}
            if lb not in ttest_res['seq'][la]:
              ttest_res['seq'][la][lb] = {}
            ttest_res['seq'][la][lb][i] = pvalue

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
            if la not in ttest_res['par']:
              ttest_res['par'][la] = {}
            if lb not in ttest_res['par'][la]:
              ttest_res['par'][la][lb] = {}
            ttest_res['par'][la][lb][i] = pvalue

            passed = pvalue <= ALPHA
            if passed:
              print ' %s' % lb, 
        print ''

  pretty = {'seq' : 'sequential', 'par' : 'parallel'}
  for problem in sorted(problems):
    for t in ['seq', 'par']:
      out = []
      out.append(
'''
\\begin{table}[htbp]
  \\centering
  \\begin{tabular}{c|cccccc}
language''')
      for language in sorted(languages):
        out.append(' & %s' % language)
      out.append('\\\\\n\\hline\n')
      for la in sorted(languages):
        out.append('%s' % la)
        for lb in sorted(languages):
          if la == lb:
            out.append(' & --')
          else:
            out.append(' & %.3e' %  (ttest_res[t][la][lb][0]))
        out.append('\\\\\n')
      out.append('''
  \end{tabular}
  \caption{p-values for %s execution time in problem %s}
  \label{tab:exec-pv-%s-%s}
\end{table}''' % (pretty[t], problem, problem, t))
      outstr = ''.join(out)
      output_file_name = "table-pvalue-%s-%s.tex" % (problem, t)
      output_file = "%s/chapters/%s" % (
              output_dir, output_file_name)
      write_to_file(output_file, outstr)


GRAPH_SIZE = 700

output_dir = "../../../ufrgs/tc"

def create_graph(graph_name, values, pretty_name, use_subfigure=True):
  variation_names = {"seq" : "Sequential", "par" : "Parallel"}
  for i in range(len(inputs)):
    nmax = 0
    for (language, problem, variation) in get_all():
      cur = values[problem][variation][language][i]
      if cur == INVALID: continue
      if cur > nmax: nmax = cur

    latex_out = []
    if use_subfigure:
      latex_out.append('\\begin{figure}[ht]\n'
                       '\\centering\n')
    for variation in variations:
      out = []
      out.append("=cluster")
      for language in sorted(languages):
        out.append(";" + language)
      out.append((
          "\n"
          "colors=light_green,yellow,red,med_blue,cyan\n"
          "=table\n"
          "yformat=%g\n"
          "=norotate\n"
          "xscale=1\n"))
      variation_name = variation_names[variation]
      if nmax == 0: nmax = 1
      out.append("max=%f\n" % (nmax * 1.1))
      out.append(
          "ylabel=%s %sexecution time in seconds for input %d\n" % (
              variation_name, pretty_name, i))
      for problem in sorted(problems):
        out.append(problem)
        for language in sorted(languages):
          out.append(" %.10f" % (
            float(values[problem][variation][language][i])))
        out.append("\n")

      out.append("\n=yerrorbars\n")
      for problem in sorted(problems):
        out.append(problem)
        for language in sorted(languages):
          out.append(" %.10f" % (get_tdelta(
              all_values[threads[-1]][problem][variation][language][i],
              ALPHA)))
        out.append("\n")

      output_file_name = "graph-%s-%s-%d" % (graph_name, variation, i)
      output_file = "%s/images/%s" % (
              output_dir, output_file_name)
      write_to_file("%s.perf" % (output_file), ''.join(out))

      cmd = (
          "%s/bargraph.pl -fig %s.perf | fig2dev -L ppm -m 4 > %s.ppm" % (
              output_dir, output_file, output_file))
      print cmd
      system(cmd)
      cmd = ("mogrify -reverse -flatten %s.ppm" % output_file)
      system(cmd)
      cmd = ("mogrify -resize %dx%d -format png %s.ppm" % (
          GRAPH_SIZE, GRAPH_SIZE, output_file))
      system(cmd)

      caption = "%s" % (variation_name)
      if variation == "par":
        caption += " (using %d threads)" % (threads[-1])
      label = "fig:exec:time:%s:%d" % (variation, i)
      if use_subfigure:
        latex_out.append((
            "\\subfigure[%s]{\n"
            "  \\includegraphics[width=65mm]{images/%s.png}\n"
            "  \\label{%s}\n"
            "}\n") % (caption, output_file_name, label))
      else:
        latex_out.append((
            "\\begin{figure}[htbp]\n"
            "  %%\\centering\n"
            "  \\includegraphics[width=125mm]{images/%s.png}\n"
            "  \\caption{%s}\n"
            "  \\label{%s}\n"
            "\\end{figure}\n") % (output_file_name, caption, label))

      
    if use_subfigure:
      latex_out.append('\\caption{Execution Time}\n')
      latex_out.append('\\label{fig:exec:time}\n')
      latex_out.append('\\end{figure}\n')
    latex_file_name = "%s/chapters/graph-%s-%d.tex" % (
        output_dir, graph_name, i)
    write_to_file(latex_file_name, ''.join(latex_out))

def create_speedup_graph(graph_name, values, use_subfigure=True):
  """
plot 'plot.dat' using 1:4 title "ideal speedup" w lp, 'plot.dat' using 1:3 title 'actual speedup' w lp, 'plot.dat' using 1:6 title "ideal efficiency" w lp, 'plot.dat' using 1:5 title "actual efficiency" w lp
  """
  latex_all = []
  if use_subfigure:
    latex_all.append('\\begin{figure}[ht]\n'
                     '\\centering\n')
  for problem in sorted(problems):
    for language in sorted(languages):
      for i in range(len(inputs)):
        out = []
        for nthreads in threads:
            variation = "par"
            if "seq" not in values[threads[-1]][problem]: continue
            tseq = values[threads[-1]][problem]["seq"][language][i]
            cur = values[nthreads][problem][variation][language][i]
            if cur == INVALID or cur == 0: continue
            out.append("%d\t%.10f\t%.10f\t%d\t%.10f\t1\n" % (
                nthreads, cur, tseq / cur, nthreads,
                tseq / (nthreads * cur)))

        output_file_name = "graph-%s-%s-%s-%d" % (
            graph_name, language, problem, i)
        output_file = "%s/images/%s" % (output_dir, output_file_name)
        write_to_file("%s.dat" % output_file, ''.join(out))
        cmd = "cp %s.dat plot.dat" % (output_file)
        system(cmd)
        print cmd
        cmd = "gnuplot %s/plot.script" % (output_dir)
        system(cmd)
        cmd = "mv plot.png %s/images/%s.png" % (output_dir, output_file_name)
        system(cmd)
        cmd = "rm plot.dat"
        system(cmd)

        latex_out = []
        caption = (
            "Speedup and Efficiency for language %s in problem %s" % (
            language, problem))
        label = "fig:exec:spd:%s:%s:%d" % (language, problem, i)
        if use_subfigure:
          latex_out.append((
              "\\subfigure[%s]{\n"
              "  \\includegraphics[width=65mm]{images/%s.png}\n"
              "  \\label{%s}\n"
              "}\n") % (caption, output_file_name, label))
        else:
          latex_out.append((
              "\\begin{figure}[htbp]\n"
              "  %%\\centering\n"
              "  \\includegraphics[width=100mm]{images/%s.png}\n"
              "  \\caption{%s}\n"
              "  \\label{%s}\n"
              "\\end{figure}\n") % (output_file_name, caption, label))

        latex_file_name = "%s/chapters/%s.tex" % (
            output_dir, output_file_name)
        write_to_file(latex_file_name, ''.join(latex_out))

        latex_all.append("\input{chapters/%s.tex}\n" % output_file_name)
        if len(latex_all) % 6 == 0:
          latex_all.append("\clearpage\n")
  if use_subfigure:
    latex_all.append('\\end{figure}\n')
  latex_all_file_name = "%s/chapters/graph-%s.tex" % (
      output_dir, graph_name)
  write_to_file(latex_all_file_name, ''.join(latex_all))

def create_problem_speedup_graph(graph_name, speedup_graph_name, use_subfigure=True):
  """
plot 'plot.dat' using 1:4 title "ideal speedup" w lp, 'plot.dat' using 1:3 title 'actual speedup' w lp, 'plot.dat' using 1:6 title "ideal efficiency" w lp, 'plot.dat' using 1:5 title "actual efficiency" w lp
  """
  latex_all = []
  if use_subfigure:
    latex_all.append('\\begin{figure}[ht]\n'
                     '\\centering\n')
  for problem in sorted(problems):
    for i in range(len(inputs)):
      out = []
      out.append('''
set xrange [0:8]
set xtics 1
set yrange [0:8]
set ytics 1
set xlabel "threads"
set ylabel "speedup"
set terminal png
set output "plot.png"
set key left
''')
      out.append("plot ")
      first = True
      for language in sorted(languages):
        output_file_name = "graph-%s-%s-%s-%d" % (
            speedup_graph_name, language, problem, i)
        output_file = "%s/images/%s" % (output_dir, output_file_name)
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
      cmd = "mv plot.png %s/images/%s.png" % (output_dir, output_file_name)
      system(cmd)
      cmd = 'rm %s' % script_name
      system(cmd)

      latex_out = []
      if use_subfigure:
        caption = "%s" % (problem)
      else:
        caption = ("Speedup for problem %s in all languages" % (problem))
      label = "fig:exec:spd:%s:%d" % (problem, i)
      if use_subfigure:
        latex_out.append((
            "\\subfigure[%s]{\n"
            "  \\includegraphics[width=65mm]{images/%s.png}\n"
            "  \\label{%s}\n"
            "}\n") % (caption, output_file_name, label))
      else:
        latex_out.append((
            "\\begin{figure}[htbp]\n"
            "  %%\\centering\n"
            "  \\includegraphics[width=100mm]{images/%s.png}\n"
            "  \\caption{%s}\n"
            "  \\label{%s}\n"
            "\\end{figure}\n") % (output_file_name, caption, label))

      latex_file_name = "%s/chapters/%s.tex" % (
          output_dir, output_file_name)
      write_to_file(latex_file_name, ''.join(latex_out))

      latex_all.append("\input{chapters/%s.tex}\n" % output_file_name)
      if len(latex_all) % 6 == 0:
        latex_all.append("\clearpage\n")

  if use_subfigure:
    latex_all.append('\\caption{Speedup per problem, in all languages}\n'
                     '\\label{fig:speedup:problem}\n'
                     '\\end{figure}\n')
  latex_all_file_name = "%s/chapters/graph-%s.tex" % (
    output_dir, graph_name)
  write_to_file(latex_all_file_name, ''.join(latex_all))

def create_language_speedup_graph(graph_name, speedup_graph_name, use_subfigure=True):
  """
plot 'plot.dat' using 1:4 title "ideal speedup" w lp, 'plot.dat' using 1:3 title 'actual speedup' w lp, 'plot.dat' using 1:6 title "ideal efficiency" w lp, 'plot.dat' using 1:5 title "actual efficiency" w lp
  """
  latex_all = []
  if use_subfigure:
    latex_all.append('\\begin{figure}[ht]\n'
                     '\\centering\n')
  for language in sorted(languages):
    for i in range(len(inputs)):
      out = []
      out.append('''
set xrange [0:8]
set xtics 1
set yrange [0:8]
set ytics 1
set xlabel "threads"
set ylabel "speedup"
set terminal png
set output "plot.png"
set key left
''')
      out.append("plot ")
      first = True
      for problem in sorted(problems):
        output_file_name = "graph-%s-%s-%s-%d" % (
            speedup_graph_name, language, problem, i)
        output_file = "%s/images/%s" % (output_dir, output_file_name)
        if first:
          first = False
          out.append("'%s.dat' using 1:4 title 'ideal speedup' w lp" % (
              output_file))
        out.append(", ")
        out.append("'%s.dat' using 1:3 title '%s speedup' w lp" % (
            output_file, problem))
        #out.append(", ")
        #out.append("'%s.dat' using 1:3:($3*0.9):($3*1.1) title '%s error' w errorbars" % (
            #output_file, problem))
      output_file_name = "graph-%s-%s-%d" % (graph_name, language, i)
      script_name = 'other.script'
      write_to_file(script_name, ''.join(out))
      system('gnuplot %s' % script_name)
      cmd = "mv plot.png %s/images/%s.png" % (output_dir, output_file_name)
      system(cmd)
      cmd = 'rm %s' % script_name
      system(cmd)

      latex_out = []
      if use_subfigure:
        caption = "%s" % (language)
      else:
        caption = ("Speedup for language %s in all problems" % (language))
      label = "fig:exec:spd:%s:%d" % (language, i)
      if use_subfigure:
        latex_out.append((
            "\\subfigure[%s]{\n"
            "  \\includegraphics[width=65mm]{images/%s.png}\n"
            "  \\label{%s}\n"
            "}\n") % (caption, output_file_name, label))
      else:
        latex_out.append((
            "\\begin{figure}[htbp]\n"
            "  %%\\centering\n"
            "  \\includegraphics[width=100mm]{images/%s.png}\n"
            "  \\caption{%s}\n"
            "  \\label{%s}\n"
            "\\end{figure}\n") % (output_file_name, caption, label))

      latex_file_name = "%s/chapters/%s.tex" % (
          output_dir, output_file_name)
      write_to_file(latex_file_name, ''.join(latex_out))

      latex_all.append("\input{chapters/%s.tex}\n" % output_file_name)
      if len(latex_all) % 6 == 0:
        latex_all.append("\clearpage\n")

  if use_subfigure:
    latex_all.append('\\caption{Speedup per language, in all problems}\n'
                     '\\label{fig:speedup:language}\n'
                     '\\end{figure}\n')
  latex_all_file_name = "%s/chapters/graph-%s.tex" % (
    output_dir, graph_name)
  write_to_file(latex_all_file_name, ''.join(latex_all))

def output_graphs():
  read_table()
  create_graph("exec-time", results[threads[-1]], "")
  speedup_graph_name = 'speedup'
  create_speedup_graph(speedup_graph_name, results)
  create_problem_speedup_graph("problem-speedup", speedup_graph_name)
  create_language_speedup_graph("language-speedup", speedup_graph_name)

TOTAL_EXECUTIONS = 30

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
  #total_time = (
      #len(languages) * len(problems) * TOTAL_EXECUTIONS * len(threads) *
      #TIMEOUT * len(inputs))
  #print "%fs or %fm or %fh or %fd" % (
      #total_time, total_time / 60., total_time / (
          #60. * 60), total_time / (60. * 60 * 24))
  #raw_input('press enter to start...')
  generate_erlang_main()
  make_all()
  create_inputs()
  for _ in range(TOTAL_EXECUTIONS):
    run_all(redirect_output=False)  # TODO: remove outputs
  #get_results()
  #calculate()
  #test_significance()
  #output_graphs()
  #system('xmessage " ALL DONE " -nearmouse -timeout 1')
  #raw_input("done! press enter to continue...")
  #system('cd %s && make' % output_dir)

if __name__ == '__main__':
  main()
