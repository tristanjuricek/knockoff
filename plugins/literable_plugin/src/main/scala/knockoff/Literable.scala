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
        override def renderHead( webDocument : WebDocument ) : Node = {
          <head>
            <title>{ webDocument.title }</title>
            { renderHeadBlueprintCSS( webDocument ) }
            { renderHeadPrettify( webDocument ) }
            <style> {"""
            /* Overriding a few defaults from blueprint. */
            body {font-size:100%;color:#222;background:#f2e5ca;font-family: Georgia, Times, serif;}
            h1, h2, h3, h4, h5, h6 {font-weight:normal;color:#111;font-family: Georgia, Times, serif;text-transform:uppercase;}
            a:focus, a:hover {color: #222;}
            a {color:#7f2f18;text-decoration:none;}
            code { text-transform: none; }
            
            /* Style the code blocks */
            pre {
              background: #333;
              color: #f2e5ca;
              border: solid #444 1px;
              border-left-width: 1em;
            }
            .str { color:rgb( 237, 239, 125 ); font-style:italic }
            .kwd { color:rgb( 152, 129, 85 ); }
            .com { color:#999 }
            .typ { color:rgb( 255, 101, 33 ); }
            .lit { color:rgb( 112, 83, 147 ); }
            .pun { color:#aaa; font-weight:bold  }
            .pln { color:#f2e5ca }
            .tag { color:rgb( 137, 150, 168 ); font-weight:bold  }
            .atn { color:#939; font-weight:bold  }
            .atv { color:#181 }
            .dec { color:#606 }
            
            /* Custom styling */
            .header { background: #7f2f18; color: #f2e5ca; text-transform:uppercase; margin-bottom: 2em; vertical-align: center; }
            .header a { color: #f2e5ca }
            .header a:focus, a:hover {color: #222; background: #7f2f18 }
            .header ul { list-style-type: none; margin: 0; padding: 0; }
            .header ul li { display: inline; padding-right: 2em; }
            """} </style>
          </head>
        }
         
        override def headerContent( doc : WebDocument ) : Node = Group(
          <ul style="list-style-type: none">
              <li><a href={ doc.relativePathTo( "../.." ) }>tristanhunt.com</a></li>
              <li><a href="http://github.com/tristanjuricek/knockoff">GitHub</a></li>
              <li><a href={ doc.relativePathTo( "../literable" ) }>Literable</a></li>
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