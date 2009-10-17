KnockOff - A Parser + Object Model (in Scala)
=============================================

Knockoff is Yet Another Markdown processor, this time, with an object model, aimed
for ease of use in a [Scala](http://scala-lang.org)-based environment.

See the [project home](http://tristanhunt.com/projects/knockoff) for more
information.

## Filesystem Notes ##

* I've added the source Markdown perl script and test suite to the main directory.

* The `test` script is used to run the Markdown Test suite. (Note: I've made some
  subtle adjustments.)

* The `discounter` script is used to run the default knockoff Discounter object that
  has been compiled.

* `plugins` are special [sbt](http://code.google.com/p/simple-build-tool/) plugins
  in use by the project. (Just build the plugins in here before running the
  project.)
