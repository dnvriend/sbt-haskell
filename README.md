# sbt-haskell
A very simple sbt plugin for compiling and running haskell programs using sbt.

## Introduction
I want to learn Haskell the language and am not (yet) interested in the Haskell build infrastructure. As I know
Sbt and Scala (a little) I have created this very simple plugin that watches Haskell source files put in
`src/main/haskell`, compiles them using ghc that you must install on your system and puts the object files
in `target/haskell/output` and puts the executable in `target/haskell`. It will also run the Haskell executable.

The plugin is great for studying Haskell using the workflow you already know so using SBT with the `~haskellTest` task
and using IntelliJ IDEA with the [official Haskell Language support plugin from Jetbrains v0.5.2](https://plugins.jetbrains.com/plugin/7453)
which gives your syntax highlighting and more really feels familiar.

To recap, this plugin is for studying purposes only and to have a familiar workflow.

## Install
You should use sbt v0.13.13 or higher and put the following in your `plugins.sbt` file:

```scala
resolvers += Resolver.url(
  "bintray-dnvriend-ivy-sbt-plugins",
  url("http://dl.bintray.com/dnvriend/sbt-plugins"))(
  Resolver.ivyStylePatterns)

addSbtPlugin("com.github.dnvriend" % "sbt-haskell" % "0.0.1")
```

__Note:__ Don't forget to add the resolver!

## Usage
- Install the [IntelliJ plugin for Haskell](https://plugins.jetbrains.com/plugin/7453)
- (optionally): create a new project using: `sbt new dnvriend/haskell-seed.g8`
- Put your files in `src/main/haskell`
- Be sure to start out with a `Main.hs` file
- use sbt so launch the sbt console
- type: `~haskellTest` so it watches source files for triggered execution
- see the Haskell output on the console
- Have fun learning Haskell and FP using the workflow/tools you already know; IntelliJ and SBT!

## Available settings
The available settings are:

```scala
val haskellExecutableName: SettingKey[String] = settingKey[String]("set the executable haskell file name; defaults to the project name and will be saved in 'target/haskell'")
val haskellVerbosityLevel: SettingKey[Int] = settingKey[Int]("set verbosity level [1..3]; defaults to 1, can be changed by typing 'set haskellVerbosityLevel := 3'")
val haskellCompilerCommand: SettingKey[String] = settingKey[String]("ghc")
val haskellSource: SettingKey[File] = settingKey[File]("The haskell source dir")
val haskellTargetDir: SettingKey[File] = settingKey[File]("The haskell target dir; defaults to 'target/haskell'")
val haskellOutputDir: SettingKey[File] = settingKey[File]("'-outputdir ⟨dir⟩': set output directory; defaults to 'target/haskell/output'")
```

## Available tasks
The available tasks are

```scala
val haskellCompile: TaskKey[Try[Unit]] = taskKey[Try[Unit]]("compile haskell files")
val haskellRun: TaskKey[Try[Unit]] = taskKey[Try[Unit]]("Run haskell program")
val haskellTest: TaskKey[Try[Unit]] = taskKey[Try[Unit]]("alias for ';clean;haskellCompile;haskellRun'")
```

## Installing Haskell
I successfully installed Haskell using [brew](http://brew.sh/), so if you don't have brew installed yet, click on the
link and follow the instructions:

```
brew install ghc cabal-install
brew link ghc
brew link --overwrite ghc
cabal update
cabal install ghc-mod
# to plot graphics
brew cask install aquaterm
brew install gnuplot
cabal install easyplot
```

## Example
Create two files `Main.hs` and `PatternMatching.hs` in `src/main/haskell` and put the following code in it:

__Main.hs__:

```haskell
module Main where
import Data.Monoid
import PatternMatching

doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100 then x else doubleMe x

concat' :: (Enum a, Num a) => [a] -> [a] -> [a]
concat' xs ys = xs `Data.Monoid.mappend` ys

length' :: (Enum a, Num a) => [a] -> a
length' xs = sum [1 | _ <- xs]

doubled :: (Enum a, Num a) => a -> [a]
doubled n = [x*2 | x <- [1..n]]

-- output
echo :: String -> String -> IO ()
echo msg_one msg_two = putStrLn $ msg_one `Data.Monoid.mappend` msg_two

triangles :: (Eq a, Enum a, Num a) => a -> [(a, a, a)]
triangles n = [(a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10], a^2 + b^2 == c^2, a + b + c == n]

main :: IO ()
main = do
       echo "doubleMeeeee 5: " $ show $ doubleMe 5
       echo "doubleSmallNumber 100: " $ show $ doubleSmallNumber 100
       echo "doubleSmallNumber 101: " $ show $ doubleSmallNumber 101
       echo "concat' [1,2] [3,4]: " $ show $ concat' [1,2] [3,4]
       echo "length' [1,2,3]: " $ show $ length' [1,2,3]
       echo "doubled 20: " $ show $ doubled 20
       echo "fst (8, 11): " $ show $ fst (8,11)
       echo "snd (8, 11): " $ show $ snd (8,11)
       echo "zip [1..3] [\"One\",\"Two\",\"Three\"]: " $ show $ zip [1..] ["One", "Two", "Three"]
       echo "unzip [(1,\"One\"), (2, \"Two\"), (3, \"Three\")]: " $ show $ unzip [(1,"One"), (2, "Two"), (3, "Three")]
       echo "triangles: " $ show $ triangles 24
       echo "lucky 7: " $ lucky 7
       echo "lucky 5: " $ lucky 5
       echo "factorial 5: " $ show $ factorial 5
       echo "charName 'a'  " $ charName 'a'
       echo "charName 'b'  " $ charName 'b'
       echo "charName 'c'  " $ charName 'c'
       echo "charName 'h'  " $ charName 'h'
       echo "addTuples [(1,2), (3,4)]: " $ show $ addTuples [(1,2), (3,4)]
       echo "head' [1,2,3]: " $ show $ head' [1,2,3]
       echo "head' \"hello\": " $ show $ head' "hello"
```

__PatternMatching.hs__:

```haskell
-- see: http://learnyouahaskell.com/syntax-in-functions#pattern-matching

module PatternMatching where
import Data.Monoid

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number seven!"
lucky x = "Sorry, you're out of luck pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
charName char = "Unexpected input " `Data.Monoid.mappend` show char

addTuples :: (Enum a, Num a) => [(a, a)] -> [a]
addTuples xs = [a + b | (a, b) <- xs]

-- returns the head of a list
head' :: (Enum a) => [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x : _) = x
```

## Releases
- v0.0.4 (2016-12-06)
  - Initial release
