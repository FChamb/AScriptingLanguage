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

# Tests #
To run the tests, use the script in the Code directory:
```console
./test.sh
```
or run with cabal:
```console
cabal test
```
To run manual tests via file input, use the script in the Code directory:
```console
./runTests.sh
```

# Usage #
## Basics
Printing values:
```console
print 1
print "Hello World!"
print "Hello World! " ++ toString(1)
```

Assigning variables:
```console
a = 100
b = 10.75
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
## Operations

Addition and Subtraction
```console
a = 1 + 3
b = 3 - 1
```
Multiplication, Division and Modulo
```console
c = 4 * 5
d = 10 / 5
e = 8 % 3
```
Absolute Value
```console
f = abs(-5 * 7)
```
Power and Square Root
```console
g = pow(2,4)
h = sqrt(4)
```
## If/Else Construct
```console
x = 0
if (x == 0) then {print "Yay"; x = x+1} else {print "Nah"}
```
## Repetition & Looping
Repeat command
```console
repeat 10 {print "Hello"; print "World"}
```
For loops
```console
x = 0
for (x < 10; x = x+2) {print x}
```
While loops
```console
x = 5
while (x > 0) {print x*5; x=x-1}
```
## Defining new functions
Define a function with the `def` command.
A function is pure the final statement is the returned expression.
```console
def square(a) { pow(a,2) }
print square(4)
```

You can also assign variables before the final expression.
```console
def pythagorus(a,b) { c2 = pow(a,2) + pow(b,2); sqrt(c2) }
print pythagorus(3, 4)
```
## Loading a file
```console
:load <filename>
```
## Quit and Help
```console
:help -- show program commands 
:quit  -- exit program
```

