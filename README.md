# sbt-haskell
A very simple sbt plugin for compiling and running haskell programs using sbt.

[ ![Download](https://api.bintray.com/packages/dnvriend/sbt-plugins/sbt-haskell/images/download.svg) ](https://bintray.com/dnvriend/sbt-plugins/sbt-haskell/_latestVersion)
[![License](http://img.shields.io/:license-Apache%202-red.svg)](http://www.apache.org/licenses/LICENSE-2.0.txt)

## Introduction
I want to learn Haskell the language and am not (yet) interested in the Haskell build infrastructure. As I know
Sbt and Scala (a little) I have created this very simple plugin that watches Haskell source files put in
`src/main/haskell` and Haskell test files in `src/test/haskell`, compiles them using ghc that you must
installed on your system (read [Installing Haskell] further down) using the `compile` task and
test your project using the `test` task. The plugin will put object files in `target/haskell/output` and
the executable in `target/haskell`. It will also run the Haskell executable using the `run` task.

The plugin is great for studying Haskell using the workflow you already know so using SBT with the `~test` task
and using IntelliJ IDEA with the [official Haskell Language support plugin from Jetbrains v0.5.2](https://plugins.jetbrains.com/plugin/7453)
which gives your syntax highlighting and more really feels familiar.

To recap, this plugin is for studying purposes only and to have a familiar workflow.

## Haskell for Mac
An even more friendly IDE is [Haskell for Mac](http://haskellformac.com/). It is not free ($25) and only available on the
apple store but well worth it! I have a copy. It also contains a very good [tutorial](http://learn.hfm.io/).

## Install
You should use sbt v0.13.13 or higher and put the following in your `plugins.sbt` file:

```scala
resolvers += Resolver.url(
  "bintray-dnvriend-ivy-sbt-plugins",
  url("http://dl.bintray.com/dnvriend/sbt-plugins"))(
  Resolver.ivyStylePatterns)

addSbtPlugin("com.github.dnvriend" % "sbt-haskell" % "0.0.8")
```

__Note:__ Don't forget to add the resolver!

## Usage
- Install the [IntelliJ plugin for Haskell](https://plugins.jetbrains.com/plugin/7453)
- (optionally): create a new project using: `sbt new dnvriend/haskell-seed.g8`
- Put your files in `src/main/haskell`
- Be sure to start out with a `Main.hs` file
- use sbt so launch the sbt console

You now have a cloned project, please enter the new directory and you can type:

```bash
# to test (both Haskell and Scala)
sbt test

# to compile (both Haskell and Scala)
sbt compile

# to run (both Haskell and Scala)
sbt run
```

Please put Haskell test files in `src/test/haskell` and Haskell source files in `src/main/haskell`.

Have fun learning Haskell and FP using the workflow/tools you already know; IntelliJ and SBT!

## Available settings
The available settings are:

```scala
val haskellExecutableName: SettingKey[String] = settingKey[String]("set the executable haskell file name; defaults to the project name and will be saved in 'target/haskell'")
val haskellVerbosityLevel: SettingKey[Int] = settingKey[Int]("set verbosity level [1..3]; defaults to 1, can be changed by typing 'set haskellVerbosityLevel := 3'")
val haskellCompilerCommand: SettingKey[String] = settingKey[String]("ghc")
val haskellSource: SettingKey[File] = settingKey[File]("The haskell source dir")
val haskellTargetDir: SettingKey[File] = settingKey[File]("The haskell target dir; defaults to 'target/haskell'")
val haskellOutputDir: SettingKey[File] = settingKey[File]("'-outputdir ⟨dir⟩': set output directory; defaults to 'target/haskell/output'")
val cabalPackages: SettingKey[Seq[String]] = settingKey[Seq[String]]("""list of Haskell packages to put on the library path, the format should be Seq("name-version") so for example Seq("adjunctions-4.3"); defaults to Seq.empty[String]""")
```

## Available tasks
The available tasks are

- __haskellCompile__: compile haskell files
- __haskellRun__: run the `Main.hs` haskell program
- __haskellTest__: alias for ';clean;haskellCompile;haskellRun'
- __run__: alias for haskellRun
- __test__: alias for haskellTest which is an alias for ';clean;haskellCompile;haskellRun'
- __cabalUpdate__: download the latest package list from 'hackage.haskell.org'
- __cabalInstall__: download and install packages from Hackage defined in the settingKey 'cabalPackages'

## Installing Haskell
I successfully installed Haskell using [brew](http://brew.sh/), so if you don't have brew installed yet, click on the
link and follow the instructions:

```
brew install ghc cabal-install
brew link ghc
brew link --overwrite ghc
cabal update
cabal install ghc-mod
```

## A Haskell Application
Both the Haskell application and the test project are best structured using a single `Main.hs` file that does the
execution of the program using the `main` entry that uses `IO` to do, well, Input and Output :)

The `src/main/haskell/Main.hs` must be structured as follows:

- Put all your functions in modules like for example the module `Chapter2.BabyFirstFunctions`
- Import the module in `src/main/haskell/Main.hs`
- Execute your program in main :: IO ()

## A Haskell Test Project
As stated above, both the Haskell application and the test project are best structured using a single entrypoint, which
contains the `main` definition that does all IO. For testing the same structure applies, but now with the following structure:

- Put all your tests that will test a certain module in a <ModuleName>Test module like eg. `Chapter2.BabyFirstFunctionsTest`,
- The `Chapter2.BabyFirstFunctionsTest` will test the module `Chapter2.BabyFirstFunctions` so it will contain a lot of tests,
- All these tests should be named (Labeled) and grouped together, by convention you can use the name `tests` to group all these small tests
  as can be seen in `Chapter2.BabyFirstFunctionsTest`  for example:

```haskell
tests = TestList [TestLabel "test1" test1]
```

- Import the test module in `src/test/haskell/Main.hs`
- The module `src/test/haskell/Main.hs` is actually your test suite
- The module `src/test/haskell/Main.hs` will call all the grouped `tests` from all the test modules like `Chapter2.BabyFirstFunctionsTest`
- The module `src/test/haskell/Main.hs` will execute all the grouped tests and will output the results on the console
- The module `src/test/haskell/Main.hs` will be executed when you call `sbt test`

## Releases
- v0.0.8 (2016-12-13)
  - Separating test sources and the main application by defining an `application` build-run cycle and a `test` build-run cycle.
  - Put your application code in `src/main/haskell`
  - Put your test code in `src/test/haskell`
  - Structure your Haskell application using `/src/main/haskell/Main.hs` that defines main :: IO and put your functions and definitions in modules
  - Put your tests in modules, group them in that `<ModuleName>Test.hs` __test-module__ and import this test-module in `src/test/haskell/Main.hs`
  - Compile your application by calling `sbt compile`
  - Run your application by calling `sbt run`
  - Test your application by calling `sbt test`
  - The Scala workflow also works

- v0.0.7 (2016-12-11)
  - Cabal integration

- v0.0.6 (2016-12-09)
  - run and test tasks

- v0.0.4 (2016-12-06)
  - Initial release
