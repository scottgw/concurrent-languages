import os

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


