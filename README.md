## Project - Crossy Road
- Own version of the iconic game Crossy Road coded in pure Haskell, this project was developed within the scope of the subject "Computer Laboratory I" in the first semester of my Degree in Software and Information Technology at UMinho.

## MENU
![MENU](src/images/MENU.png)
#
![SKINS](src/images/SKINS.png)
#
![FIM](src/images/FIM.png)
#
## Repositório

If you already have an SSH key configured, simply clone the repository:

```bash
$ git clone git@gitlab.com:uminho-di/li1/2223/2022li1g035.git
$ cd 2022li1g035
```

If you don't, use the the link:

```bash
$ git clone https://gitlab.com/uminho-di/li1/2223/projetos/2022li1g035.git
$ cd 2022li1g035
```

## Interpreter

Open the Interpreter of Haskell (GHCi) using cabal ou directly.

1. Using cabal

```bash
$ cabal repl
```

2. Using GHCi

```bash
$ ghci -i="src" -i="tests" src/Main.hs
```

## Tests

The project uses the [HUnit](https://hackage.haskell.org/package/HUnit) library to perform unit tests.

You can run the tests using one of the following alternatives:

1. Using `cabal`

```bash
$ cabal test
```

2. Using GHCi

```bash
$ ghci -i="src" -i="tests" tests/Spec.hs
>>> runTestsT1 -- Run task 1 tests
>>> runTestsT2 -- Run task 2 tests
>>> runTestsT3 -- Run task 3 tests
>>> runTestsT4 -- Run task 4 tests
>>> main -- Run all tests
```

3. Using the `runhaskell` wrapper

```bash
$ runhaskell -i="src" -i="tests" tests/Spec.hs
```

## Documentation

You can generate documentation with [Haddock](https://haskell-haddock.readthedocs.io/).

1. Using `cabal`

```bash
$ cabal haddock --haddock-all
```

2. Using `haddock` directly

```bash
$ haddock -h -o doc/html src/*.hs
```
## Grupo 35

- **A103995** José António Costa Soares;
- **A104090** Nuno Miguel Parente Morais;
