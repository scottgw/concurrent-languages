import math

def mean(x):
  n = len(x)
  total = 0
  for xi in x:
    total += xi
  if n == 0:
    print all_values
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
  conf = 1 - alpha
  f = open('input.data', 'w')
  f.write('x y\n')
  for i in range(min(len(xa), len(xb))):
    f.write('%.10f\t%.10f\n' % (xa[i], xb[i]))
  f.close()
  cmd = '../r/test.r %f > out.data' % conf

  system(cmd)

  pvalue = read_file_values('out.data')[0]
  return pvalue

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


