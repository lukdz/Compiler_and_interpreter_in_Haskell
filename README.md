# Compiler and interpreter in Haskell

The project was implemented as part of the Programming Methodology (in the 2016/2017 summer semester) at the Faculty of Computer Science of the University of Wrocław.

As part of the task I wrote a compiler and interpreter working with the HDML programming language acc. specification provided. The executable code is for the Motorola 6809 processor, the emulator of which was created by the leading laboratory ([poles-p](#poles-p)) on the basis of technical documentation.

## Versions

The repository contains 3 versions being successive iterations of the task, each of which supports the expanded set of simple HDML language instructions created for the needs of the studio.

| Compiler | Interpreter | Supported language elements |
| --- | --- | --- |
|  | `inter4` | Arithmetic and logical expressions |
| `comp5` | `inter5` | + recursive functions (first order), pair and list |
| `comp6` | `inter6` | + higher-order functions and anonymous functions |


## Source code

| Task | Location of the file with the source code |
| --- | --- |
| `inter4` | `interpreters/inter4/LukaszDzwoniarek.hs` |
| `inter6` | `interpreters/inter6/LukaszDzwoniarek.hs` |
| `inter5` | `interpreters/inter5/LukaszDzwoniarek.hs` |
| `comp5` | `compilers/comp5/LukaszDzwoniarekCompiler.hs` |
| `comp6` | `compilers/comp6/LukaszDzwoniarekCompiler.hs` |


### Requirements

In order to run the software you need:

```
Glasgow Haskell Compiler = GHC
C Compiler
```


## Compiler


### Compiling

Compilation compilation (version 6):

```
# cd compilers/comp6/
ghc Comp6
# cd ../..
```

<a name="emu6809">
Compilation of Motorola 6809 processor emulator:

```
# cd compilers/emu6809
make
cp emu6809 ../comp5/
cp emu6809 ../comp6/
# cd ../..
```
</a>


### Testing individual programs

File `./JST/fib.pp5` contains an example program calculating the nth number of the Fibonacci sequence.

Compilation of the source code to the executable file (on the Motorola 6809 processor or emulator [emu6809](#emu6809)):

```
# cd compilers/comp6
./Comp6 ./JST/fib.pp5
```

Compilation on the "screen" (displaying the result code in the form of instructions in human readable form):

```
# cd compilers/comp6
./Comp6 -m ./JST/fib.pp5
```

Compilation of the compiled program using the emulator:

```
# cd compilers/comp6
./emu6809 ./JST/fib.b09
```


### Testing a set of programs

The script that compares the results, the actions of the sample programs obtained on the interpreter, with the help of the compiler and the emulator:

1. Open the comp6 directory:

   ```
   cd compilers/comp6
   ```

2. Give executive privileges to the script that performs the tests:

   ```
   chmod +x test.sh
   ```

3. Place executable files in the comp6 directory:

   - [emu6809](#emu6809)
   ```
   cp ../emu6809/emu6809 ./
   ```

   - [Prac6](#Prac6)
   ```
   cp ../../interpreters/inter6/Prac6 ./
   ```

4. Run tests

   ```
   ./test.sh
   ```

Performing all tests should take about 5 seconds.

The results of my compiler can be found in the testReslut file.


## Interpreter Testing

<a name="Prac6">
Compiling interpreter and checker

```
# cd interpreters/inter6
ghc Prac6
```
</a>

Running all tests

```
# cd interpreters/inter6
./Prac6 -t
```


## Built With

* [GHC](https://www.haskell.org/ghc/) - The Glasgow Haskell Team


## Acknowledgments

* <a name="poles-p">mgr Piotr Polesiuk (https://github.com/poles-p) - author of task from the Programming Methods (checker, emulator)</a>
* Adam Sawicki (https://github.com/Asdamos) - shared test sets (6AST)
* Jan Sierpina - shared test sets (JST)
* Paweł Smolak (https://github.com/psmolak) - collections and test sets of students of the subject
* other participants of the studio with Programming Methods in 2016/2017, who have released their test sets
