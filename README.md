
This repository contains an implementation of a branch-and-bound algorithm solving the integer *K_m,n* quadratic
optimization problem.

# Introduction

## Integer *K_m,n* quadratic programming problem

Let *A* is an *m*×*n* matrix of integer values.  
The goal is to compute the maximal value of *XAY*, where *X* and *Y* are vectors over {-1,1}.


## Branch-and-bound solution

The branch-and-bound solution of the problem is given in the II. section and the appendix of
[P. Diviánszky, E. Bene and T. Vértesi, "Qutrit witness from the Grothendieck constant of order four" In Physical Review A, 96(1):012113 (2017)](https://arxiv.org/abs/1707.04719)

The paper contains an application of the problem too.


## Implementation

The branch-and-bound solution of the problem is implemented in Haskell.

The program does the following steps:

1.  reads in the matrix *A* from a text file
2.  preprocessing: transform *A* to speed up the calculation without affecting the result
3.  generates an x86-64 assembly program which efficiently computes the maximum of *XAY*
4.  jumps to the generated assembly code
5.  the assembly code computes and prints the final result (and also partial results if requested)

    The full state of the assemby program is stored in registers,
    which drastically reduces the number of RAM accesses during the computation.
    This, and the use of SSE instructions speeds up the computation considerably.


## Limitations

-   The minimum of the matrix width and height should be less than 129.
-   The sum of the absolute values of the input matrix elements should be less than 2^31.
    The upper limit can be relaxed to 2^63 in some cases.

# Installation

Installation with [Cabal](https://www.haskell.org/cabal/):

    $ cabal install --installdir=$HOME

# Usage

## Basic usage

The most basic usage is to print the minimal value of *XAY*, where *A* is given in a text file:

    $ kmn-programming Wmat44.txt --silent
    1206540

## `--help`

The `--help` option prints a summary of the available options and commands:

    kmn-programming - specialized quadratic binary optimization

    Usage: kmn-programming (COMMAND | [-d|--delete] [-t|--transpose]
                           [-s|--sort SORTMETHOD] [-m|--multiply] [-p|--print]
                           [-s|--silent] [--levelin FILE] [--levelout FILE]
                           [-l|--level NAT] [--trace NAT] [-u|--unroll NAT]
                           [-a|--align NAT] [-o|--output FILE] FILE [--timeout ARG]
                           [--partial ARG])
      Maximalize sum of the input matrix multiplied by tensor products of two
      vectors of +-1 elements

    Available options:
      -h,--help                Show this help text
      -d,--delete              delete 0 rows and columns
      -t,--transpose           transpose matrix if it has more rows than columns
      -s,--sort SORTMETHOD     sort method - default is nosort
      -m,--multiply            multiply rows by +-1 to improve the first guess
      -p,--print               print matrix before optimization
      -s,--silent              print just the result
      --levelin FILE           precomputed levels input file
      --levelout FILE          levels output file
      -l,--level NAT           level - default is number of rows / 4
      --trace NAT              trace level - default is 0
      -u,--unroll NAT          unroll cycles - default is 4
      -a,--align NAT           code alignment - default is 8
      -o,--output FILE         output file
      --timeout ARG            timeout in seconds
      --partial ARG            do partial computation

    Available commands:
      sample                   print sample matrix
      test                     basic self-test
      timerandom               measure optimization time on random matrices

Note that for each command, there is a specific `--help` option too.

## Commands

There are 4 commands (modes of operation):

- maximize (this is the default, should not be written on the command line)
- `test`
- `sample`
- `timerandom`

The `test`, `sample` and `timerandom` modes are implemented for self-testing and benchmarks.

## Self-testing and benchmarking commands

The `test` command performs a basic self-test. The result should be `OK`:

    $ kmn-programming test
    OK

The `sample` command prints an n*(n-1) sized square matrix for each input n:

    $ kmn-programming sample 3
     2  3  3  0  3  3
     3  2  3  3  0 -3
     3  3  2 -3 -3  0
     0  3 -3  2  3 -3
     3  0 -3  3  2  3
     3 -3  0 -3  3  2

These matrices are described in Fishburn, P. C.; Reeds, J. A. (1994), Bell Inequalities, Grothendieck’s Constant, and Root Two, 7, SIAM Journal on Discrete Mathematics, pp. 48–56.
The maximal value of these matrices are known, so they are good for testing and benchmarking.

The produced matrix can be used in a separate step for benchmarking:

    $ kmn-programming sample 7 -o sample_7.txt
    $ time kmn-programming sample_7.txt 
    42 x 42
    levels: 10
    0000000000000000000001ffffffffff    924
    924

    real	0m42.987s
    user	0m42.980s
    sys	0m0.006s

The `timerandom` command can be used for benchmarking with random matrices.  
Type `--help` after `timerandom` to get some hints how to use this command:

    $ kmn-programming timerandom --help
    Usage: kmn-programming timerandom NAT NAT NAT NAT NAT NAT NAT NAT NAT NAT
      measure optimization time on random matrices

    Available options:
      NAT                      biggest integer in random matrices
      NAT                      smallest width
      NAT                      biggest width
      NAT                      width step
      NAT                      smallest height
      NAT                      biggest height
      NAT                      height step
      NAT                      smallest level
      NAT                      biggest level
      NAT                      repeat computation
      -h,--help                Show this help text

Example usage:

    $ kmn-programming timerandom 100 40 40 0 10 24 1 1 1 5
    (10,40,1)  2.302585092994046    -7.076399754920059
    (11,40,1)  2.3978952727983707    -7.10426931422738
    (12,40,1)  2.4849066497880004    -6.771162178173219
    (13,40,1)  2.5649493574615367    -6.889843064842471
    (14,40,1)  2.6390573296152584    -6.437137838157279
    (15,40,1)  2.70805020110221    -6.345387851709545
    (16,40,1)  2.772588722239781    -5.964019978283643
    (17,40,1)  2.833213344056216    -5.831543826032325
    (18,40,1)  2.8903717578961645    -5.635646650329268
    (19,40,1)  2.9444389791664403    -5.199033999407417
    (20,40,1)  2.995732273553991    -4.460960878254131
    (21,40,1)  3.044522437723423    -4.3522254540413625
    (22,40,1)  3.091042453358316    -3.5638282821343283
    (23,40,1)  3.1354942159291497    -3.0882569225539966
    (24,40,1)  3.1780538303479458    -2.822858809755754

Explanation of arguments:

100: the random matrix elements are chosen from {-100,-99,...,99,100} uniformly  
40: width of matrices in the first round  
40: width of matrices in the last round  
0: matrix with increment (no increment)  
10: height of matrices in the first round  
24: height of matrices in the last round  
1: matrix height increment between rounds  
1: level value in the first round (see later)  
1: level value in the last round  
5: make 5 samplings in each round

Explanation of the output:

There is one line of output for each round.

1st column: (matrix with, matrix height, level value)  
2nd column: logarithm of the matrix height  
3rd column: logarithm of the average runtime


## Maximization

Maximization is the default command. 

### Input file format

The input file should contain the elements of *A*, separated by whitespaces.  
Rows should be separated by one newline character.  
Elements of a row should be separated by spaces.  
Elements should be integers in decimal notation.

### Command line output formatting

Default output formatting (exmaple):

    $ kmn-programming Wmat44.txt 
    44 x 44
    levels: 11
    0000000000000000000007ffffffffff    1204990
    0000000000000000000007fffeffffff    1205496
    0000000000000000000007fffcfff7fc    1205584
    0000000000000000000007fffcdffffc    1205720
    0000000000000000000007fffaeffffb    1206022
    0000000000000000000006effcdffefc    1206372
    0000000000000000000006eff6fffffc    1206540
    1206540

44 x 44 is the matrix width and height.

`levels` is explained in the code generation section.

Then a line is printed each time when a better maximum is found.  
The second column is the new maximum.  
The first column is a hexadecimal number. The last 43 digit in the binary form of this number encodes the
last 43 elements of the *X* vector corresponding the maximum (1 --> +1, 0 --> -1).
The first element of *X* is always chosen to be 1.

At the end, the global maximum 1206540 is printed.

With the `--trace` option, one extra line is printed which part of the search space is evaluated.  
The search space is divided into 2^(*n*-1) equal parts where *n* is given by the user.  
The parts are numbered downwards, like 7. 6. 5. 4. 3. 2. 1. 0.:

    $ kmn-programming Wmat44.txt --trace 4
    44 x 44
    levels: 11
    7.
    0000000000000000000007ffffffffff    1204990
    0000000000000000000007fffeffffff    1205496
    0000000000000000000007fffcfff7fc    1205584
    0000000000000000000007fffcdffffc    1205720
    0000000000000000000007fffaeffffb    1206022
    6.
    0000000000000000000006effcdffefc    1206372
    0000000000000000000006eff6fffffc    1206540
    5.
    4.
    3.
    2.
    1.
    0.
    1206540

The `--silent` option suppresses all output but the found maximum:

    $ kmn-programming Wmat44.txt --silent
    1206540

There is another option affecting the output, `--print` which prints the preprocessed input matrix (see next section).


### Preprocessing

In the preprocessing phase the input matrix is transformed such that
the following steps run faster without affecting the found maximum.

The preprocessing phase has the following steps:

1.  Zero rows and columns are deleted if the `--delete` option is present.
2.  The matrix is transposed if the `--transpose` option is present and if
    the matrix has more rows than columns.
3.  The matrix rows are sorted if the `--sort` option is present.
    The following sorting methods are implemented:

    `asum`: sort the rows by decreasing absolute sum  
    `random`: the rows are premuted randomly, each time with a different seed  
    `random`NUM: the rows are premuted randomly with seed NUM  
    `b`: *not documented*
4.  Negate the rows of the matrix if the `--multiply` option is present.
    Not all rows are negated.
    The idea behind negation is that the first local maximum depends on the negation of the rows,
    but the global maximum does not.
    More subtrees can be pruned if we know that global maximum is at least *N*, if *N* is higher.
    `--multiply` tries to negate rows to increase the first local maximum.


### Code generation

The branch-and-bound algorithm skips subtrees in the searching phase if it cannot give a maximum
higher than the already found highest value.

Skipping subtrees has a cost, because the criteria for skipping should be evaluated.
We would like to reduce this cost, so on the first n level of the searching tree we do not
evaluate the skipping criteria.
The value of n can be given at the command line with the `--level` option.
If `--level` is missing, then n is one fourth of the number of rows.

The `--unroll` unroll option performs loop unrolling on the inner loop of generated assembly code.
Details:  
Each row is padded with 0 values such they will contain 2*ni* elements.
The inner row will be executed *i* times, evaluating 2*n* elements each time.
The number *n* can be given after `--unroll`.

The `--aling` option places certain labels in the assembly code on memory addresses
dividable by *n*. The number *n* can be given after `--align`.


### Distributed computation

The `--levelin`, `--levelout` and `--partial` options are implemented to support distributed computation
of the maximal value.

The `--timeout` option is also parsed but it is not implemented.

Here is an example, how to compute the maximum for a 90x90 sized matrix.

Assume that the matrix is given in the `mat90.txt` text file.

1.  Run

        $ kmn-programming mat90.txt --levelout levels.txt --level 0 --silent --partial 1/2^15

    2^15 is the number of subtasks. Any other power of 2 is OK.

2.  Run

        $ runhaskell SplitProblem.hs 15 mat90.txt --silent --levelin levels.txt >commands.sh

    After this, `commands.sh` will contain the subtasks.

4.  For each row in `commands.sh`, evaluate the row in a shell and collect the results.

5.  Compute the maximum of the results.


