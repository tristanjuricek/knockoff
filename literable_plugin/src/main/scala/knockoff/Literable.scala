package knockoff

import com.tristanhunt.site_step.files.ScriptingImplicits._
import com.tristanhunt.literable._
import sbt._
import scala.xml._

trait Literable extends ScalaProject {
    
    def literableCodePrefix : List[ String ] = Nil
    
    /**
     * The current markdown generator. The generator will caches everything each run, 
     * so that the plugin methods are not constantly re-evaluating what it needs to do.
     */
    var generator : Generator = _
    
    /**
     * When a file change is detected in the markdown source, it would be good to reset
     * the generator.
     */
    def resetGenerator {
         generator = new Generator {
             override def headerContent( doc : WebDocument ) : Node = Group(
                 <style> {"""
                 .header ul { list-style-type: none; margin: 0; padding: 0 }
                 .header ul li { display: inline; }
                 """} </style>
                 <ul style="list-style-type: none">
                     <li><a href={ doc.relativePathTo( "../.." ) }>tristanhunt.com</a></li>
                     <li><a href="http://github.com/tristanjuricek/knockoff">GitHub</a></li>
                     <li><a href={ doc.relativePathTo("main/api/index.html") }>API</a></li>
                 </ul>
             )
         }
     }

    resetGenerator
    
    /** 
     * The list of markdown source files we will use to generate the code and 
     * documentation trees.
     */
    def markdownSources : Iterable[ Path ] =
        generator.markdownThings.map( m => Path.fromFile( m.inputFile ) )
    
    /**
     * The list of all source control products that we will change.
     */
    def codeProducts : Iterable[ Path ] = {
        generator.codes.map( code => Path.fromFile( generator.resolve( code ) ) )
    }
    
    /** Generates code, and then resets */
    // lazy val literate = fileTask( codeProducts from markdownSources ) {
    lazy val literate = task {
        generator.storeEverything
        generator.storeDefaultResources
        resetGenerator
        None
    }
}