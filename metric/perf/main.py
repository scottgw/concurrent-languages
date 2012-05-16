import os
import sys

languages = set(["chapel", "cilk", "erlang", "go", "scoop", "tbb"])
problems = set(["chain", "outer", "product", "randmat", "thresh", "winnow"])
variations = ["seq", "par"]

def generate_erlang_main():
  language = "erlang"
  for problem in sorted(problems):
    for variation in sorted(variations):
      if problem == "chain" and variation == "seq":
        continue
      directory = "../../%s/%s" % (language, problem);
      if problem != "chain":
        directory += "/%s" % variation;
      output = directory + "/main.sh"
      print output
      f = open(output, "w")
      f.write("""
#!/bin/sh
cd ~/tudo/tcc/apmc/lucia/metric/perf/%s
erl -noshell -s main main -s init stop
""" % directory)
      f.close()

def make_all():
  # TODO: make go chain
  # TODO: make scoop
  for problem in sorted(problems):
    for variation in sorted(variations):
      if problem == "chain" and variation == "seq":
        continue
      for language in sorted(languages):
        cmd = "cd ../../%s/%s" % (language, problem);
        if problem != "chain":
          cmd += "/%s" % variation;
        cmd += " && make -B"
        print cmd
        os.system(cmd)
  cmd = "cp ../../go/chain/src/main/main ../../go/chain/main"
  print cmd
  os.system(cmd)

inputs = ["10 10 55", "100 100 666", "100 250 777"] #, "10000 10000 888"]
input_thresh = ["55", "66", "77", "88"]
input_winnow = ["10", "100", "250", "10000"]

def create_inputs():
  problems = ["randmat", "thresh", "winnow", "outer", "product", "final"]
  for i in range(len(inputs)):
    cur = inputs[i]
    file_name = "%s%d.in" % (problems[0], i)
    f = open(file_name, "w")
    f.write(cur + "\n")
    f.close()

  for i in range(len(inputs)):
    cur = inputs[i]
    file_name = "chain%d.in" % i
    f = open(file_name, "w")
    data = inputs[i].split()
    f.write("%s\n%s\n%s\n%s\n" % (
      data[1], data[2], input_thresh[i], input_winnow[i]))
    f.close()

  for i in range(len(problems) - 1):
    problem = problems[i]
    next_problem = problems[i + 1]
    print problem
    for j in range(len(inputs)):
      input_file = "%s%d.in" % (problem, j)
      output_file = "%s%d.out" % (problem, j)
      next_input_file = "%s%d.in" % (next_problem, j)
      cmd = "../../%s/%s/main < %s > %s" % (
          "cpp", problem, input_file, output_file);
      print cmd
      os.system(cmd)
      cmd = "cp %s %s" % (output_file, next_input_file)
      print cmd
      os.system(cmd)
      if next_problem == "thresh":
        f = open(next_input_file, "a")
        f.write(input_thresh[j] + "\n")
        f.close()
      if next_problem == "winnow":
        cmd = "cp randmat%d.out %s" % (j, next_input_file)
        print cmd
        os.system(cmd)
        output = open(next_input_file, "a")
        f = open(output_file, "r")
        f.readline()
        output.write("\n")
        output.write(f.read() + "\n")
        output.write(input_winnow[j] + "\n")
        f.close()
        output.close()

def run_all():
  # chapel: works!
  # cilk: --nproc 4
  # erlang: main.sh
  # go: GOMAXPROCS=4
  # scoop: exception
  # tbb: 
  for problem in sorted(problems):
    for variation in sorted(variations):
      if problem == "chain" and variation == "seq":
        continue
      for language in sorted(languages):
        if language == "scoop": # TODO
          continue
        for i in range(len(inputs)):
          time_output = "time-%s-%s-%s-%d.out" % (
              language, problem, variation, i)
          print time_output
          cmd = ""
          #cmd += "GOMAXPROCS=4 "
          cmd += "time -a -f %%e -o %s ../../%s/%s/" % (
              time_output, language, problem)
          if problem != "chain":
            cmd += "%s/" % variation
          if language == "erlang":
            cmd += "main.sh"
          elif language == "scoop":
            cmd += "main -i"
          else:
            cmd += "main"
          cmd += " < %s%d.in > %s-%s-%s-%d.out" % (
              problem, i, language, problem, variation, i)
          #cmd += "./main.sh < %s.in > /dev/null 1>&0 2>&0" % (
          #cmd += "main < %s.in > /dev/null 1>&0 2>&0" % (
          #cmd += "main --nproc 2 < %s.in > /dev/null 1>&0 2>&0" % (
          #cmd += "main < %s.in" % (
              #problem);
          #print cmd
          os.system(cmd)
          f = open(time_output, "r")
          value = f.read()
          print value

results = {}

def get_results():
  for problem in sorted(problems):
    results[problem] = {}
    for variation in sorted(variations):
      if problem == "chain" and variation == "seq":
        continue
      results[problem][variation] = {}
      for language in sorted(languages):
        results[problem][variation][language] = {}
        for i in range(len(inputs)):
          if language == "scoop": # TODO
            results[problem][variation][language][i] = 999
            continue
          results[problem][variation][language][i] = 999
          cur = []
          time_output = "time-%s-%s-%s-%d.out" % (
              language, problem, variation, i)
          print time_output
          f = open(time_output, "r")
          for line in f:
            value = float(line)
            cur.append(value)
          f.close()

          total = 0.0
          for j in range(len(cur)):
            total += cur[j]
          avg = total / len(cur)
          print problem, variation, language, i, avg
          results[problem][variation][language][i] = avg
  print results

def output_graphs():
  def create_graph(graph_name, values, max_value, pretty_name):
    old_stdout = sys.stdout

    variation_names = {"seq" : "Sequential", "par" : "Parallel"}
    for variation in variations:
      sys.stdout = open("../../../ufrgs/meu/images/graph-%s-%s.perf" % (
        graph_name, variation), "w")

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
      print "ylabel=%s %s execution time" % (variation_name, pretty_name)
      for problem in sorted(problems):
        if problem == "chain" and variation == "seq":
          continue
        print problem,
        #nmin = min(
            #float(
              #values[problem][language][variation]) for language in sorted(
                #languages))
        for language in sorted(languages):
          #print("%.2f" % (
            #float(values[problem][language][variation]) / nmin)),
          print("%.2f" % (
            float(values[problem][variation][language][len(inputs) - 1]))),
        print ""

    sys.stdout = old_stdout

  create_graph("exec-time", results, 60, "pretty_name_time")

#generate_erlang_main()
#make_all()
create_inputs()
#run_all()
#get_results()
#output_graphs()
