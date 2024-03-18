# Haskell 2 - A Scripting Language

# Setup #
This project uses cabal as its build system.
Before trying to run, make sure to have cabal installed.
Make sure to run all scripts / cabal commands in the Code directory
```console
cd Code/
cabal update
```

# Running #
To run the project, use the script in the Code directory:
```console
./run.sh
```
or run with cabal:
```console
cabal run
```

# Usage #
Printing values:
```console
print 1
```

Assigning variables:
```console
a = 100
```

Calculations can be done almost anywhere
```console
a = 10*50
print a
```
OR
```console
print 10*50
```

Defining and calling functions:
```console
def square(a) { pow(a,2) }
print square(4)
```

# Tests #
To run the tests, use the script in the Code directory:
```console
./test.sh
```
or run with cabal:
```console
cabal test
```
