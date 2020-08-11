## G2 Haskell Symbolic Execution Engine
---
#### About
G2 performs lazy symbolic execution of Haskell programs to detect state reachability.
It is capable of generating assertion failure counterexamples and solving for higher-order functions.

For instance, suppose we have a function `add` that sums two `Peano` numbers:
```
data Peano = Succ Peano | Zero deriving (Show, Eq)

add :: Peano -> Peano -> Peano
add Zero p = p
add (Succ p) p2 = add p (Succ p2)
```
along with the assertion:
```
equalsFour :: Peano -> Peano -> Peano -> Bool
equalsFour _ _ p = toInt p == 4
```
When G2 is run as follows:
```
cabal run G2 ./tests/Samples/Peano.hs add -- --merge-states --assert equalsFour
```
It yields several counterexamples, some of which are illustrated here:
```
add (Succ (Succ (Succ Zero))) (Succ (Succ Zero)) = Succ (Succ (Succ (Succ (Succ Zero))))
add (Succ (Succ (Succ Zero))) Zero = Succ (Succ (Succ Zero))
add Zero (Succ (Succ (Succ Zero))) = Succ (Succ (Succ Zero))
add Zero (Succ (Succ Zero)) = Succ (Succ Zero)
```
The repository also contains G2Q, which allows haskell constraint programming with Haskell predicates,
as well as G2QMerge, which is an extension of G2Q that integrates State Merging.

---

#### Dependencies
* GHC 8.2.2: https://www.haskell.org/ghc/
* Custom Haskell Standard Library: https://github.com/AntonXue/base-4.9.1.0
* Z3 SMT Solver: https://github.com/Z3Prover/z3

---
#### Setup:
1) Install GHC 8.2.2 (other GHC 8 versions might also work)
2) Install Z3
3) Either:

  a) Pull the Custom Haskell Standard Library into ~/.g2 by running `bash base_setup.sh` 

  b) Manually pull the base library.  Add a file to the G2 folder, called g2.cfg that contains:
		`base = /path/to/custom/library`
	
---
#### Command line:

###### Reachability:

`cabal run G2 ./tests/Samples/Peano.hs add`

###### LiquidHaskell:

`cabal run G2 -- --liquid ./tests/Liquid/Peano.hs --liquid-func add`

###### G2QMerge:

`cabal run G2M` (Assumes that functions to be run are contained in `g2m/Main.hs`)

###### Arguments:

* `--n` number of reduction steps to run
* `--max-outputs` number of inputs/results to display
* `--smt` Pass "z3" or "cvc4" to select a solver [Default: Z3]
* `--time` Set a timeout in seconds
* `--merge-states` Enable state merging. (Not compatible with LiquidHaskell)

---

#### Authors
* Bill Hallahan (Yale)
* Anton Xue (Yale)
* Rushyendra Maganty (Yale)
* Maxwell Troy Bland (UCSD)
* Ranjit Jhala (UCSD)
* Ruzica Piskac (Yale)
