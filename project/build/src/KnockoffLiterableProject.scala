import com.tristanhunt.literable.{ Code, Generator, WebDocument }
import com.tristanhunt.literable.plugin.LiterableProject
import sbt._
import scala.xml._

trait KnockoffLiterableProject extends LiterableProject {
  
  override def resetGenerator {
    generator = new Generator {
      
      override def rootPath = path(".").asFile
      
      override def renderHeadElements( webDocument : WebDocument ) : Node = {
        new Group( super.renderHeadElements( webDocument ) ++
                   localStyle )
      }

      override def renderHeadElements( code : Code ) : Node = {
        new Group( super.renderHeadElements( code ) ++
                   localStyle )
      }
      
      private def localStyle = {
        <style> {"""
        /* Overriding a few defaults from blueprint. */
        body {
          background:#43180D;
          font-size:100%; color: #f2e5ca;
          font-family: Times, serif;
        }
        h1, h2, h3, h4, h5, h6 {
          font-weight:normal;font-family: Georgia, Times, serif;
          color: #f2e5ca;
        }
        /* 3, 2, 1.5, 1.2 */
        h1 {font-size:3em;line-height:1;margin-bottom:0.5em;}
        h2 {font-size:2em;margin-bottom:0.75em;}
        h3 {font-size:1.5em;line-height:1;margin-bottom:1em;}
        h4 {font-size:1em;line-height:1.25;margin-bottom:1.25em;}
        h5 {font-size:1em;font-weight:bold;margin-bottom:1.5em;}
        h6 {font-size:1em;font-weight:bold;}
        
        a:focus, a:hover {color: #CEA162; text-decoration: underline; }
        a {color:#CEA162;text-decoration:none;}

        code { text-transform: none; }

        /* Style the code blocks */
        pre {
          color: #f2e5ca;
          border: solid #180A07 1px;
          border-right: 0;
          border-left-width: 1em;
          padding-left: 1em;
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
        .container { float: left; margin-top: 0em; } /* 7f2f18 */
        .header { color: #f2e5ca; 
          text-transform:uppercase; margin-bottom: 2em; vertical-align: center;
        }
        a:focus, a:hover {  background: #43180D; }
        .header ul { list-style-type: none; margin: 0; padding: 0; }
        .header ul li { display: inline; padding-right: 2em; }

        .nav .current { background: #43180D; border-left: .3em solid #000; }
    		.nav li { padding-left: .3em; padding-right: 3px; }
    		.nav hr { background: #43180D; }
    		
    		hr { background: #000; }
        """} </style>
      }

      override def headerContent( doc : WebDocument ) : Node = Group(
        <ul style="list-style-type: none">
            <li><a href={ doc.relativePathTo( "../.." ) }>tristan hunt.com</a></li>
            <li>{ projectName.get.get }</li>
        </ul>
      )

      override def headerContent( code : Code ) : Node = Group(
        <ul style="list-style-type: none">
            <li><a href={ code.relativePathTo( "../.." ) }>tristan hunt.com</a></li>
            <li>{ projectName.get.get }</li>
        </ul>
      )
      
      override def renderArticle( doc : WebDocument ) : Node =
        <div class="article span-13 prepend-1 last">{ articleContent( doc ) }</div>
      
      override def renderArticle( code : Code ) : Node =
        <div class="article span-13 prepend-1 last">{ articleContent( code ) }</div>
    }
  }
}