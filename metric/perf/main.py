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
        cmd = "cd ../../%s/%s/%s && make" % (language, problem, variant);
        if problem == "chain":
          cmd = "cd ../../%s/%s && make" % (language, problem);
        print cmd
        os.system(cmd)

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
run_all()
