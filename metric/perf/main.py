import os

languages = set(["chapel", "cilk", "erlang", "go", "scoop", "tbb"])
problems = set(["chain", "outer", "product", "randmat", "thresh", "winnow"])
variants = set(["seq", "par"])

def make_all():
  for problem in sorted(problems):
    for variant in sorted(variants):
      if problem == "chain" and variant == "seq":
        continue
      for language in sorted(languages):
        cmd = "cd ../../%s/%s/%s && make -B" % (language, problem, variant);
        if problem == "chain":
          cmd = "cd ../../%s/%s && make -B" % (language, problem);
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
    if problem == "chain": # TODO
      continue
    for variant in sorted(variants):
      if problem == "chain" and variant == "seq":
        continue
      for language in sorted(languages):
        if language == "erlang" or language == "scoop": # TODO
          continue
        for i in range(len(inputs)):
          time_output = "time-%s-%s-%s-%d.out" % (
              language, problem, variant, i)
          print time_output
          cmd = ""
          #cmd += "GOMAXPROCS=4 "
          cmd += "time -a -f %%e -o %s ../../%s/%s/" % (
              time_output, language, problem)
          if problem != "chain":
            cmd += "%s/" % variant
          if language == "erlang":
            cmd += "main.sh"
          elif language == "scoop":
            cmd += "main -i"
          else:
            cmd += "main"
          cmd += " < %s%d.in > %s-%s-%s-%d.out" % (
              problem, i, language, problem, variant, i)
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
    if problem == "chain": # TODO
      continue
    results[problem] = {}
    for variant in sorted(variants):
      if problem == "chain" and variant == "seq":
        continue
      results[problem][variant] = {}
      for language in sorted(languages):
        if language == "erlang" or language == "scoop": # TODO
          continue
        results[problem][variant][language] = {}
        for i in range(len(inputs)):
          results[problem][variant][language][i] = 0
          cur = []
          time_output = "time-%s-%s-%s-%d.out" % (
              language, problem, variant, i)
          print time_output
          f = open(time_output, "r")
          for line in f:
            value = float(line)
            cur.append(value)
          f.close()

          total = 0.0
          for i in range(len(cur)):
            total += cur[i]
          avg = total / len(cur)
          print avg
          results[problem][variant][language][i] = avg
  print results

#make_all()
create_inputs()
run_all()
get_results()
