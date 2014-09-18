import os

print("Changing to F:")
os.system('F:')

path = 'F:\Google Drive\Code\github\gitweb\Deadleg.github.io'
print(path)
os.chdir(path)

print(os.getcwd())

execString = 'bundle exec jekyll serve --watch --drafts'
print(execString);
os.system(execString)
input('Press ENTER to exit')