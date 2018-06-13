# commuter

# What
This is an EDSL for Martin Erwig's CS 585 DSL course. It describes commuting
diagrams as equations of arrows/morph isms and compiles that to the commuting
diagram via the diagram library.

# Useful Modules
The first module you'll want to look at is `app/Main`, this is where you'll want
to call the main function to render the commuting diagram via `mainWith $ sem
<your program>`. The `sem` function is re-exported from the `Sem` module via the
`Api` module. The `Api` module should provide most things that you need to build
commuting diagrams, but if it does not be sure to check if your needs are
satisfied in the `Internal.Position` and `Internal.Core` modules. Lastly, you'll
want to read, in detail, the `Examples` module, which will provide you with
ready made example that you can tweak to your hearts content.

# How to build the library
This library is built using `stack`. If you do not have `stack` on your pc then
I am not sure how you will be able to build it. You should be able to simply
unzip the zip file, then run `stack build` as seen in the next section. Stack
was chosen purposefully because it provides reproducible builds for Haskell,
which is a marked improvement over cabal. The libraries that this dsl rely on
are also quite heave (`diagrams` and `lens` especially), so if you do
successfully build be prepared to get a cup of coffee because chances are that
you'll be building dependencies for awhile. Lastly, feel free to contact me if
something goes wrong and you can find more resources on stack
[here](http://groups.engr.oregonstate.edu/fpc/tutorials/stackOverview.html).
Also, due to the use of stack this ghc **will not** interfere with your global
ghc or global stack setup. It will be sandboxed only to the directory that you
unzip it in. You will also notice that there is no cabal file that I'm exporting
with this, the cabal file should be automatically generated with `stack build`
as specified in the `stack.yaml` and the `package.yaml` files.

# How to Execute a program
Execution is done in the `app/Main.hs` file. Simply place your program in `Examples.hs` or import your file and then call `sem` and `mainWith` like so:

```
main :: IO ()
main = mainWith $ sem five_lemma # pad 3
```

You'll notice that I've used the `pad` function from the diagrams library to
give the image a little more space on the edges. Then you can compile the image
from the command line like so:

```
DSLs/commuter $ stack build
DSLs/commuter $ stack exec -- commuter -h 1000 -w 1000 -o test.pdf
DSLs/commuter $ firefox test.pdf
```
You'll want to pay attention to the `-h` `-w` and `-o` flags.
These stand for `height`, `width`, and `output` respectively.
The height and width in particular can lead to strange interactions between the
commuter DSL and the diagrams library. So if you find yourself calling
transition functions to move a diagram like `transX` or `transC` and the diagram
is not responding in the way you think it should then you should experiment with
these values. I have found that 1000 for each seems to work most of the time.
The reason this happens is because the diagrams library will try to place things
for it's user and this sometimes clashes with the internal coordinate system of
the commuter DSL.

Here is a full example of the compilation of the five lemma example:
```
DSLs/commuter $ stack build; stack exec -- commuter -h 1000 -w 1000 -o five_lemma.pdf; firefox five_lemma.pdf
WARNING: Ignoring out of range dependency (allow-newer enabled): lens-4.16.1. diagrams-contrib requires: >=4.0 && <4.16
WARNING: Ignoring out of range dependency (allow-newer enabled): lens-4.16.1. diagrams-svg requires: >=4.0 && <4.16
WARNING: Ignoring out of range dependency (allow-newer enabled): base-4.10.1.0. diagrams-pgf requires: >=4.4 && <4.10
WARNING: Ignoring out of range dependency (allow-newer enabled): optparse-applicative-0.14.2.0. diagrams-pgf requires: >=0.13 && <0.14
WARNING: Ignoring out of range dependency (allow-newer enabled): process-1.6.1.0. diagrams-pgf requires: >=1.0 && <1.5
WARNING: Ignoring out of range dependency (allow-newer enabled): base-4.10.1.0. texrunner requires: >=4.6 && <4.10
WARNING: Ignoring out of range dependency (allow-newer enabled): time-1.8.0.2. diagrams-pgf requires: >=1.2 && <1.7
```

# Problems you may encounter
## A bug with the LaTeX engine
There are some major problems that can come up and will be confusing for you if
you do not read this section. The first is a major bug that is in the `product2`
examples. Essentially, this DSL allows the user to specify LaTeX in the object
labels via the `mathify` function. This LaTeX, is then passed to the `diagrams`
library and rendered using the LaTeX engine that library provides. You'll notice
that in `product2L` one label is `mathify "A \\times B"`. That double backslash
is extremely important because it escapes the actual backslash that LaTeX will
use to call the `times` verb. For some reason, that I have no figured out yet,
the first backslash is mutated or lost during the construction of this commuting
diagram, even though nothing should be operating on it. So this bug is known if
you come across it. The error state that is generated by this bug occurs at
compile time where `Data.Map` will throw an error that a key is not in state map
during a lookup (`(!)`). This essentially means that a pointer in a morphism did
not match the keys in the state map.

## Errors you may receive from the Diagrams library
Because this DSL compiles to the `diagrams` library, it uses the `mainWith`
function from that library to generate the actual image. If an object is placed
at the same location as another object you'll receive an error such as this:
```
-- | Will throw an uncaught error at compile time because A and B are in the
-- same location
iThrowAnUnCaughtError :: Sem Equ
iThrowAnUnCaughtError = do a <- arrowAt "A" "f" "B" (0,0) (0,0)
                           return . pure . pure $ a

-- | An example that will definitely throw an error
main :: IO ()
main = mainWith $ sem iThrowAnUnCaughtError # pad 3

-- Now the execution
commuter: scale by zero!  Halp!
CallStack (from HasCallStack):
  error, called at src/Diagrams/Core/Transform.hs:426:11 in diagrams-core-1.4.1-J3nYDNEadOMDx30T1FSlf7:Diagrams.Core.Transform
```
