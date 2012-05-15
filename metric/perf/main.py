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

def create_inputs():
  problems = ["randmat", "thresh", "winnow", "outer", "product", "final"]
  inputs = ["100 100 666", "1000 1000 777"] #, "10000 10000 888"]
  input_thresh = ["66", "77"] #, "88"]
  input_winnow = ["100", "1000"] #, "10000"]

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
      cmd = "cd ../../%s/%s && ./main < ../../metric/perf/%s > ../../metric/perf/%s" % (
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
    problem = "thresh"
    for variant in sorted(variants):
      if problem == "chain" and variant == "seq":
        continue
      for language in sorted(languages):
        language = "erlang"
        time_output = "time-%s-%s-%s.out" % (language, problem, variant)
        cmd = ""
        cmd += "GOMAXPROCS=4 "
        cmd += "time -a -f %%e -o %s ../../%s/%s/" % (
            time_output, language, problem)
        if problem != "chain":
          cmd += "%s/" % variant
        cmd += "main.sh"
        #cmd += "main"
        cmd += " < %s.in" % problem
        #cmd += "./main.sh < %s.in > /dev/null 1>&0 2>&0" % (
        #cmd += "main < %s.in > /dev/null 1>&0 2>&0" % (
        #cmd += "main --nproc 2 < %s.in > /dev/null 1>&0 2>&0" % (
        #cmd += "main < %s.in" % (
            #problem);
        print cmd
        os.system(cmd)
        f = open(time_output, "r")
        value = f.read()
        print value
        break
    break

#make_all()
create_inputs()
#run_all()
