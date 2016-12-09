# Notes

## SBT
All operators except for +=, ++= and := are deprecated.
If you are using sbt 13.x or greater you should not have to use those operators in favours of macros.

http://www.scala-sbt.org/0.13/docs/Migrating-from-sbt-012x.html

## Haskell
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#redirecting-output
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using.html#getting-started-compiling-programs
http://blog.tweag.io/posts/2016-10-17-inline-java.html
http://www.scala-sbt.org/0.13/docs/Howto-Triggered.html
https://wiki.haskell.org/Mac_OS_X_Common_Installation_Paths

## Library paths
~Library/Haskell/ghc-<version>/lib

## Haskell User Guide
https://downloads.haskell.org/~ghc/latest/docs/users_guide.pdf

## .hi files
When asked to compile a source file, GHC normally generates two files: an object file, and an
interface file.

GHC's *.hi files are "interface files", a product of GHC's separate
compilation. `foo.hi` contains the information needed to compile a
different module which depends on foo.

So a binary library package needs to contain both the object code, and
the matching interface description, which is the *.hi file.  By the way,
both are very sensitive to the version of GHC that compiled them.
There's no attempt or claim at binary compatibility between GHC versions.