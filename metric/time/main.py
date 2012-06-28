#!/usr/bin/python

from datetime import datetime
import sys
import subprocess
import os
import math

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

def write_to_file(output, content):
  f = open(output, 'w')
  f.write(content)
  f.close()

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
  '''meana = mean(xa)
  sa = stddev(xa)
  na = len(xa)
  delta = get_tdelta(xa, alpha)
  left = meana - delta
  right = meana + delta
  return left > 0'''
  conf = 1 - alpha
  with open('input.data', 'w') as f:
    f.write('x y\n')
    for i in range(len(xa)):
      f.write('%.10f\t%.10f\n' % (xa[i], xb[i]))
  cmd = '../r/test_paired.r %f > out.data' % conf
  # cmd = '../r/test_paired.r %f' % conf
  system(cmd)
  pvalue = read_file_values('out.data')[0]
  # print '%f ' % pvalue,
  # return pvalue <= alpha
  return pvalue

def get_tdelta(xa, alpha):
  meana = mean(xa)
  sa = stddev(xa)
  na = len(xa)
  t = get_t(1 - alpha / 2, na - 2)
  #print 'meana: %f\nsa2: %f\nna: %d\nt: %f' % (meana, sa * sa, na, t)
  return t * sa / math.sqrt(na)

def read_table():
  with open('tdist.txt', 'r') as f:
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
  #print yindex
  #print table



start_actions = set(["start", "started", "restart", "resume"])
other_actions = set(["commit", ".fuse_hidden*", "branch", "working", ".*.swp", "restart-done", "reduce2d", "fill_histogram", "prefixsum", "binary_search", "fill_mask", "ReduceState", "read_matrix", "max_matrix", "get_threshold", "reduce2d_with_filter", "split", "read_integer", "testing", "still_synchronous", "asynchrony", "matrix", "parallel_scan", "using", "with_filter", "refac", "sequential_sort", "fill_values", "parallel_sort", "parfor", "parsort", "tuple_sorter", "almost", "quit", "merged", "compiles", "files", "tests"])
end_actions = set(["done", "pause"])

start_times = {}
total_times = {}

problems = set()
languages = set()
variations = ["seq", "par"]
result = {}
wc_result = {}
table_types = {"loc" : "-l", "now" : "-w"}

total_lines = 0

def system(cmd, timeout=False):
  ret = os.system(cmd)
  if ret != 0 and not timeout:
    print cmd
    assert(False)

def load_data():
  f = open("log_reverse.txt", "r")
  for line in f:
    bad_string = " -0300 "
    bad_string_index = line.find(bad_string)
    time_string = line[:bad_string_index]
    fmt = "%a %b %d %H:%M:%S %Y"
    parsed_date = datetime.strptime(time_string, fmt)
    commit = line[bad_string_index + len(bad_string):]
    words = commit.split()

    if len(words) > 1:
      index = words[0]
      action = words[1]
      if action in start_actions:
        assert(index not in start_times);
        start_times[index] = parsed_date
      elif action in end_actions:
        assert (index in start_times)
        end_time = parsed_date
        diff = end_time - start_times[index]
        del start_times[index]
        assert(diff.days == 0)
        diff = diff.seconds / 60.0
        if index in total_times:
          total_times[index] += diff
        else:
          total_times[index] = diff
      elif action in other_actions:
        pass
      else:
        print action
        print line
        assert(False)

  assert(len(start_times) == 0)

  for key, value in total_times.iteritems():
    words = key.split("-")
    assert (len(words) == 2 or len(words) == 3)
    language = words[0]
    problem = words[1]
    if language == "cpp":
      assert (len(words) == 2)
      words.append("seq")
    if problem == "refac":
      assert(False)
      continue

    if len(words) != 3:
      print words
      assert(False)

    variation = words[2]

    problems.add(problem)
    if language != "cpp":
      languages.add(language)

    if language not in result:
      result[language] = {}

    if problem not in result[language]:
      result[language][problem] = {}

    assert (variation not in result[language][problem])

    result[language][problem][variation] = value

  for problem in problems:
    if problem != "chain":
      result["tbb"][problem]["seq"] += result["cpp"][problem]["seq"]

  for language in languages:
    for problem in problems:
      if problem != "chain":
        result[language][problem]["par"] += result[language][problem]["seq"]

  result["erlang"]["chain"]["seq"] = result["erlang"]["chain"]["par"]
  result["scoop"]["chain"]["seq"] = result["scoop"]["chain"]["par"]

def output_tables():
  def create_table(table_name, output_value, extra):
    old_stdout = sys.stdout

    for variation in variations:
      sys.stdout = open("../../../ufrgs/tc/chapters/table-%s-%s.tex" % (
        table_name, variation), "w")

      first = True
      print " & ",
      for problem in sorted(problems):
        if True:
          if not first:
            print " & ", 
          print problem,
          first = False;
      print " \\\\ \\hline"

      for language in sorted(languages):
        print language,
        for problem in sorted(problems):
          output_value(language, problem, variation, extra)
        print " \\\\"

    sys.stdout = old_stdout

  ########## time tables ###############

  def time_table_output(language, problem, variation, extra):
    assert(variation in result[language][problem])
    print " & ",
    print("%.2f" % result[language][problem][variation])

  create_table("time", time_table_output, None)

  ########## LoC-NoW-NoC tables ###############

  extensions = { "chapel" : "chpl", "cilk" : "cilk", "erlang" : "erl",
      "go" : "go", "scoop" : "e", "tbb" : "cc" }

  def wc_table_output(language, problem, variation, extra):
    extension = extensions[language]
    table_flag = extra["table_flag"]
    table_type = extra["table_type"]
    cmd = "find ../../%s/%s/%s/ | grep \"\\.%s$\" | xargs cat | grep . | wc %s > wc.out" % (
        language, problem, variation, extension, table_flag)
    if problem == "chain" and language in [
        "erlang", "scoop"]:
      cmd = "find ../../%s/%s/ | grep \"\\.%s$\" | xargs cat | grep . | wc %s > wc.out" % (
          language, problem, extension, table_flag)
    os.system(cmd)

    if table_type not in wc_result:
      wc_result[table_type] = {}
    if language not in wc_result[table_type]:
      wc_result[table_type][language] = {}
    if problem not in wc_result[table_type][language]:
      wc_result[table_type][language][problem] = {}
    assert(variation not in wc_result[table_type][language][problem])
    value = open("wc.out", "r").read()

    wc_result[table_type][language][problem][variation] = value
    if table_type == "loc":
      global total_lines
      total_lines += int(value)
    print " & ", value,

  for table_name, table_flag in table_types.iteritems():
    create_table(table_name,  wc_table_output, {"table_flag": table_flag,
      "table_type" : table_name})

GRAPH_SIZE = 700
output_dir = "../../../ufrgs/tc"

def output_graphs():
  def create_graph(graph_name, values, max_value, pretty_name, is_relative=True):
    old_stdout = sys.stdout

    variation_names = {"seq" : "Sequential", "par" : "Parallel"}
    for variation in variations:
      if is_relative:
        output_file = "../../../ufrgs/tc/images/graph-%s-%s" % (
          graph_name, variation)
      else:
        output_file = "../../../ufrgs/tc/images/other-graph-%s-%s" % (
          graph_name, variation)
      sys.stdout = open('%s.perf' % output_file, "w")

      sys.stdout.write("=cluster")
      for language in sorted(languages):
        sys.stdout.write(";" + language)
      print '''
colors=black,yellow,red,med_blue,light_green,cyan
=table
yformat=%g
=norotate
xscale=1
'''
      variation_name = variation_names[variation]
      print "max=%d" % max_value
      if is_relative:
        print "ylabel=%s %s versus smallest" % (variation_name, pretty_name)
      else:
        print "ylabel=%s %s" % (variation_name, pretty_name)
      for problem in sorted(problems):
        print problem,
        nmin = min(
            float(
              values[language][problem][variation]) for language in sorted(
                languages))
        if not is_relative:
          nmin = 1
        for language in sorted(languages):
            print("%.2f" % (
              float(values[language][problem][variation]) / nmin)),
        print ""

      sys.stdout = old_stdout
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


    sys.stdout = old_stdout

  pretty_names = {"time" : "time to code", "loc" : "LoC", "noc" : "NoC",
      "now" : "NoW"}
  create_graph("time", result, 10, pretty_names["time"])
  for table_name in table_types:
    pretty_name = pretty_names[table_name]
    create_graph(table_name, wc_result[table_name], 8, pretty_name)

  create_graph("time", result, 800, pretty_names["time"], is_relative=False)
  max_sizes = {"loc" : 1200, "now" : 3000}
  for table_name in table_types:
    pretty_name = pretty_names[table_name]
    max_size = max_sizes[table_name]
    create_graph(table_name, wc_result[table_name], max_size, pretty_name,
        is_relative=False)


ALPHA = 0.1

ttest_res = {}

def output_pvalues(table, pretty, code):
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
        out.append(' & %.3e' %  (table[la][lb]))
    out.append('\\\\\n')
  out.append('''
  \end{tabular}
  \caption{p-values for %s}
  \label{tab:pv-%s}
\end{table}''' % (pretty, code))
  outstr = ''.join(out)
  output_file_name = "table-pvalue-%s.tex" % (code)
  output_file = "%s/chapters/%s" % (
          output_dir, output_file_name)
  write_to_file(output_file, outstr)

def test_significance():
  types = ['tsing', 'tboth', 'size']
  for t in types:
    ttest_res[t] = {}

  for variation in variations:
    ttest_res['tsing'][variation] = {}
    for la in languages:
      ttest_res['tsing'][variation][la] = {}
      for lb in languages:
        if la == lb:
          continue
        left = []
        right = []
        for problem in problems:
          left.append(result[la][problem][variation])
          right.append(result[lb][problem][variation])
        pvalue = ttest(left, right, ALPHA)
        ttest_res['tsing'][variation][la][lb] = pvalue
        passed = pvalue <= ALPHA
        if passed:
          print '%s:%s:%s passed SINGLE' % (la, lb, variation)

  for la in languages:
    ttest_res['tboth'][la] = {}
    for lb in languages:
      if la == lb:
        continue
      left = []
      right = []
      for problem in problems:
        for variation in variations:
          left.append(result[la][problem][variation])
          right.append(result[lb][problem][variation])
      pvalue = ttest(left, right, ALPHA)
      ttest_res['tboth'][la][lb] = pvalue
      passed = pvalue <= ALPHA
      if passed:
        print '%s:%s passed BOTH' % (la, lb)

  for la in languages:
    ttest_res['size'][la] = {}
    for lb in languages:
      if la == lb:
        continue
      left = []
      right = []
      for problem in problems:
        for variation in variations:
          for table_type in table_types:
            left.append(int(wc_result[table_type][la][problem][variation]))
            right.append(
                int(wc_result[table_type][lb][problem][variation]))
      pvalue = ttest(left, right, ALPHA)
      ttest_res['size'][la][lb] = pvalue
      passed = pvalue <= ALPHA
      if passed:
        print '%s:%s passed SIZE' % (la, lb)

  output_pvalues(ttest_res['tsing']['seq'],
      'time to code sequential version', 'seq-ttc')
  output_pvalues(ttest_res['tsing']['par'],
      'time to code parallel version', 'par-ttc')
  output_pvalues(ttest_res['tboth'], 'time to code both versions',
      'both-ttc')
  output_pvalues(ttest_res['size'], 'size of source code', 'size')

def calculate():
  # left.append(result[la][problem][variation])
  variations = ['par']
  nmax = {}
  nmin = {}
  for problem in problems:
    nmax[problem] = {}
    nmin[problem] = {}
    for variation in variations:
      nmax[problem][variation] = 0
      nmin[problem][variation] = 999999999999
      for language in languages:
        nmax[problem][variation] = max(nmax[problem][variation],
            result[language][problem][variation])
        nmin[problem][variation] = min(nmin[problem][variation],
            result[language][problem][variation])
  calc = {}
  nsum = 0.0
  for language in languages:
    num = 0.0
    den = 0.0
    for problem in problems:
      for variation in variations:
        #num += result[language][problem][variation]
        #num -= nmin[problem][variation]
        #den += nmax[problem][variation]
        #den -= nmin[problem][variation]
        num += (result[language][problem][variation] / (
            float(nmin[problem][variation])))
        den += 1
    calc[language] = num / float(den)
    nsum += calc[language]

  vec = []
  for language in languages:
    #vec.append((calc[language] / nsum, language))
    vec.append((calc[language], language))

  print '\n\ntime to code (par)\n'

  vec.sort()
  for x in vec:
    (val, lang) = x
    print '%s : %f' % (lang, val)
  # int(wc_result[table_type][lb][problem][variation]))
  nmax = {}
  nmin = {}
  table_types = ['now']
  for t in table_types:
    nmax[t] = {}
    nmin[t] = {}
    for problem in problems:
      nmax[t][problem] = {}
      nmin[t][problem] = {}
      for variation in variations:
        nmax[t][problem][variation] = 0
        nmin[t][problem][variation] = 999999999999
        for language in languages:
          nmax[t][problem][variation] = max(nmax[t][problem][variation],
              int(wc_result[t][language][problem][variation]))
          nmin[t][problem][variation] = min(nmin[t][problem][variation],
              int(wc_result[t][language][problem][variation]))
  calc = {}
  nsum = 0.0
  for language in languages:
    num = 0.0
    den = 0.0
    for t in table_types:
      for problem in problems:
        for variation in variations:
          num += (int(wc_result[t][language][problem][variation]) / (
              float(nmin[t][problem][variation])))
          den += 1
    calc[language] = num / float(den)
    nsum += calc[language]

  vec = []
  for language in languages:
    #vec.append((calc[language] / nsum, language))
    vec.append((calc[language], language))

  print '\n\nsource code size (NoW - par)\n'

  vec.sort()
  for x in vec:
    (val, lang) = x
    print '%s : %f' % (lang, val)

def main():
  read_table()
  load_data()
  output_tables()
  #calculate()
  #test_significance()
  output_graphs()
  global total_lines
  print "total lines: %d" % total_lines
  print "done"

if __name__ == "__main__":
  main()
