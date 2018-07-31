import sys

try:
 f=open(sys.argv[1])
except:
  print("unable to open Q file")
  exit(0)

for line in f:
  if len(line) > 5 or line[0] is not '#' :
    print("print(\"" + line.rstrip('\n') + "\")")
