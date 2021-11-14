[![Build Status](https://travis-ci.org/pindaroso/free-monads.svg?branch=master)](https://travis-ci.org/pindaroso/free-monads)

# free-monads

*Code inspired by Gabriel Gonzalez's fantastic <a href="http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html" target="_blank">article</a> on <a href="http://hackage.haskell.org/package/free-2.0.3" target="_blank">free monads</a>*

## Setup

**Requirements**

* Docker or Stack

## Building

**Stack**

`make`

**Docker**

`make docker`

## Examples

Run `stack ghci` to play with the example programs and interpreter:

```haskell
interpreter0 = putStrLn (showProgram program6)
  where
    subroutine2 :: Subroutine
    subroutine2 = output 'A'

    program6 :: Program
    program6 = do
        subroutine2
        bell
        done
```

```changelog
λ > interpreter0
output 'A'
bell
done
```

```haskell
interpreter1 = putStrLn (showProgram program7)
  where
    program7 :: Program
    program7 = do
      output 'A'
      output 'B'
      output 'C'
      bell
```

```changelog
λ > interpreter1
output 'A'
output 'B'
output 'C'
bell
done
```

