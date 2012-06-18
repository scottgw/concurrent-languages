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
  y = 0
  for i in range(len(yindex)):
    if yindex[i] >= alpha:
      y = i
      break
  x = 0
  for i in range(len(xindex)):
    if xindex[i] > df:
      x = i
      break
  return table[x][y]

def ttest(xa, xb, alpha):
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
  print '%f: [%f, %f]' % (alpha, left, right)
  return left > 0

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

def main():
  read_table()
  ttest([5.36, 16.57, 0.62, 1.41, 0.64, 7.26],
      [19.12, 3.52, 3.38, 2.50, 3.60, 1.74],
      0.1)

if __name__ == '__main__':
  main()
