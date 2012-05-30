import os

languages = set(["chapel", "cilk", "erlang", "go", "scoop", "tbb"])
#languages = set(["chapel", "cilk", "erlang", "go", "tbb"])
#languages = ["cilk"]
#problems = set(["chain", "outer", "product", "randmat", "thresh", "winnow"])
problems = ["randmat"]
variations = ["seq", "par"]

def system(cmd, timeout=False):
  ret = os.system(cmd)
  if ret != 0 and not timeout:
    print cmd
    assert(False)

def write_to_file(output, content):
  with open(output, 'w') as f:
    f.write(content)

def append_to_file(output, content):
  with open(output, 'a') as f:
    f.write(content)

def read_from_file(file_name):
  with open(file_name, 'r') as f:
    content = f.read()
  return content

def read_from_file_skipping_first_line(file_name):
  with open(file_name, "r") as f:
    f.readline()
    content = f.read()
  return content

def read_file_values(file_name):
  result = []
  with open(file_name, 'r') as f:
    for line in f:
      try:
        value = float(line)
        result.append(value)
      except ValueError:
        return []
  return result

def file_exists(file_name):
  return os.path.isfile(file_name)

def get_directory(language, problem, variation=""):
  directory = "../../%s/%s" % (language, problem);
  if problem != "chain" and language != "cpp":
    directory += "/%s" % variation;
  return directory

def get_problems_with_variations():
  for problem in sorted(problems):
    for variation in sorted(variations):
      if problem == "chain" and variation == "seq":
        continue
      yield (problem, variation)

ERLANG_MAIN = ("#!/bin/sh\n"
               "cd ~/tudo/tcc/apmc/lucia/metric/perf/%s\n"
               "erl -noshell -s main main -s init stop\n")

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
    return "%s_%d_%d_%d" % (self.name, data.nrows, data.ncols, data.nelts)

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

class DumbProblem(Problem):
  def __init__(self):
    self.name = "dumb"

  def file_name(self, data):
    return "dumb_file_name"

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

inputs = [
    #ProblemInput(100, 100, 666, 50, 100),
    ProblemInput(1000, 1000, 666, 50, 1000),
    ProblemInput(10000, 10000, 666, 50, 10000)]

#threads = [1, 2, 3, 4, 5, 6, 7, 8]
threads = [1, 2, 3, 4]

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
  randmat_problem = RandmatProblem()
  chain_problem = ChainProblem()
  for i in range(len(inputs)):
    cur = inputs[i]
    file_name = randmat_problem.input_file_name(cur)
    write_to_file(file_name, randmat_problem.get_input(cur))

  for i in range(len(inputs)):
    cur = inputs[i]
    file_name = chain_problem.input_file_name(cur)
    write_to_file(file_name, chain_problem.get_input(cur))

  for i in range(len(problem_classes) - 1):
    problem = problem_classes[i]
    next_problem = problem_classes[i + 1]
    print problem.name
    for j in range(len(inputs)):
      cur = inputs[j]
      input_file = problem.input_file_name(cur)
      output_file = problem.output_file_name(cur)
      directory = get_directory("cpp", problem.name)
      if not file_exists(output_file):
        cmd = "%s/main < %s > %s" % (
            directory, input_file, output_file);
        #print cmd
        system(cmd)
      if problem.name != "product":
        next_input_file = next_problem.input_file_name(cur)
        if not file_exists(next_input_file):
          cmd = "cp %s %s" % (output_file, next_input_file)
          #print cmd
          system(cmd)
          if next_problem.name == "thresh":
            append_to_file(next_input_file, "%d\n" % cur.percent)
          if next_problem.name == "winnow":
            randmat_input = randmat_problem.output_file_name(cur)
            cmd = "cp %s %s" % (randmat_input, next_input_file)
            #print cmd
            system(cmd)
            content = read_from_file_skipping_first_line(output_file)
            append_to_file(next_input_file, "\n%s\n%s\n" % (
                content, cur.nelts))

TIMEOUT = 1

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
        if language == "go":
          cmd += "GOMAXPROCS=%d " % nthreads

        cmd += "timeout %d " % (TIMEOUT)

        directory = get_directory(language, problem, variation)
        cmd += "time -a -f %%e -o %s %s/" % (time_output, directory)

        if language == "erlang":
          cmd += "main.sh"
        elif language == "scoop":
          cmd += "main -i"
        else:
          cmd += "main"

        if language == "chapel":
          cmd += " --numLocales=1 --numThreadsPerLocale=%d " % nthreads
        elif language == "cilk":
          if variation == 'par':
            cmd += " --nproc %d " % nthreads
          else:
            pass
            #cmd += " --nproc 1 "

        if language != "scoop":
          cmd += " <";

        if redirect_output:
          cmd += " %s > /dev/null 1>&0 2>&0" % (
              problem_map[problem].input_file_name(inputs[i]));
        else:
          cmd += " %s" % (
              problem_map[problem].input_file_name(inputs[i]));

        print cmd
        system(cmd, timeout=True)
        value = read_from_file(time_output)
        print value

results = {}
INVALID = 999

def get_results():
  for nthreads in threads:
    if nthreads not in results:
      results[nthreads] = {}
    for (language, problem, variation) in get_all():
      if variation == 'seq' and nthreads != threads[-1]: continue
      if problem not in results[nthreads]:
        results[nthreads][problem] = {}
      if variation not in results[nthreads][problem]:
        results[nthreads][problem][variation] = {}
      if language not in results[nthreads][problem][variation]:
        results[nthreads][problem][variation][language] = {}

      for i in range(len(inputs)):
        results[nthreads][problem][variation][language][i] = INVALID
        time_output = get_time_output(
            language, problem, variation, i, nthreads)
        #print time_output
        cur = read_file_values(time_output)

        if (len(cur) == 0):
          continue

        total = 0.0
        for j in range(len(cur)):
          total += cur[j]
        avg = total / len(cur)
        print nthreads, problem, variation, language, i, avg
        if language == "erlang": # IMPORTANT !!!
          assert(avg > 1)
          avg -= 1
        results[nthreads][problem][variation][language][i] = avg
  #print results

GRAPH_SIZE = 700

output_dir = "../../../ufrgs/meu"

def create_graph(graph_name, values, pretty_name):
  variation_names = {"seq" : "Sequential", "par" : "Parallel"}
  for i in range(len(inputs)):
    nmax = 0
    for (language, problem, variation) in get_all():
      cur = values[problem][variation][language][i]
      if cur == INVALID: continue
      if cur > nmax: nmax = cur

    latex_out = []
    for variation in variations:
      out = []
      out.append("=cluster")
      for language in sorted(languages):
        out.append(";" + language)
      out.append((
          "\n"
          "colors=black,yellow,red,med_blue,light_green,cyan\n"
          "=table\n"
          "yformat=%g\n"
          "=norotate\n"
          "xscale=1\n"))
      variation_name = variation_names[variation]
      if nmax == 0: nmax = 1
      out.append("max=%f\n" % (nmax * 1.5))
      out.append(
          "ylabel=%s %sexecution time in seconds for input %d\n" % (
              variation_name, pretty_name, i))
      for problem in sorted(problems):
        if problem == "chain" and variation == "seq":
          continue
        out.append(problem)
        for language in sorted(languages):
          out.append(" %.10f" % (
            float(values[problem][variation][language][i])))
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

      caption = "%s Execution Time for Input %d" % (variation_name, i)
      label = "fig:exec:time:%s:%d" % (variation, i)
      latex_out.append((
          "\\begin{figure}[htbp]\n"
          "  %%\\centering\n"
          "  \\includegraphics[width=125mm]{images/%s.png}\n"
          "  \\caption{%s}\n"
          "  \\label{%s}\n"
          "\\end{figure}\n") % (output_file_name, caption, label))

      latex_file_name = "%s/chapters/graph-%s-%d.tex" % (
          output_dir, graph_name, i)
      write_to_file(latex_file_name, ''.join(latex_out))

def create_speedup_graph(graph_name, values):
  """
plot 'plot.dat' using 1:4 title "ideal speedup" w lp, 'plot.dat' using 1:3 title 'actual speedup' w lp, 'plot.dat' using 1:6 title "ideal efficiency" w lp, 'plot.dat' using 1:5 title "actual efficiency" w lp
  """
  for language in sorted(languages):
    for i in range(len(inputs)):
      out = []
      for nthreads in threads:
        for (a_language, problem, variation) in get_all():
          if variation == "seq" or language != a_language: continue
          tseq = values[threads[-1]][problem]["seq"][language][i]
          cur = values[nthreads][problem][variation][language][i]
          if cur == INVALID: continue
          out.append("%d\t%.10f\t%.10f\t%d\t%.10f\t1\n" % (
              nthreads, cur, tseq / cur, nthreads, tseq / (nthreads * cur)))

      output_file_name = "graph-%s-%s-%d" % (graph_name, language, i)
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
      caption = "Speedup and Efficiency for Language %s Input %d" % (
          language, i)
      label = "fig:exec:spd:%s:%d" % (language, i)
      latex_out.append((
          "\\begin{figure}[htbp]\n"
          "  %%\\centering\n"
          "  \\includegraphics[width=125mm]{images/%s.png}\n"
          "  \\caption{%s}\n"
          "  \\label{%s}\n"
          "\\end{figure}\n") % (output_file_name, caption, label))

      latex_file_name = "%s/chapters/%s.tex" % (
          output_dir, output_file_name)
      write_to_file(latex_file_name, ''.join(latex_out))

def output_graphs():
  create_graph("exec-time", results[threads[-1]], "")
  create_speedup_graph("speedup", results)

"""
SIZE=700
images/%.png: images/%.ppm
	mogrify -reverse -flatten $<
	mogrify -resize ${SIZE}x${SIZE} -format png $<
images/%.ppm: images/%.perf
	./bargraph.pl -fig $< | fig2dev -L ppm -m 4 > $@"""

TOTAL_EXECUTIONS = 3

def main():
  #generate_erlang_main()
  #make_all()
  #create_inputs()
  for _ in range(TOTAL_EXECUTIONS):
    run_all(redirect_output=False)
  get_results()
  output_graphs()

if __name__ == '__main__':
  main()
