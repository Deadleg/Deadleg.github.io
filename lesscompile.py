import os

path = 'F:\Code\github\gitweb\Deadleg.github.io'
print(path)
os.chdir(path)

print('current dir:' + os.getcwd())

execString = 'lessc css/styles.less > css/styles.css'
print(execString);
os.system(execString)