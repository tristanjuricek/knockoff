import com.tristanhunt.literable.{ Code, Generator, WebDocument }
import com.tristanhunt.literable.plugin.LiterableProject
import sbt._
import scala.xml._

trait KnockoffLiterableProject extends LiterableProject {
  
  override def resetGenerator {
    generator = new Generator {
      
      override def rootPath = path(".").asFile

      override def renderHeader( doc : WebDocument ) : Node = Group(
        <div class="heading span-24 small">
    	    <ul class="inline">
            <li> <a href={ doc.relativePathTo( "../.." ) }> Home </a> </li>
            <li> <a href="http://github.com/tristanjuricek/knockoff"> Github </a> </li>
            <li> <a href="http://www.linkedin.com/in/tristanjuricek">Linked In</a> </li>
            <li> <a href={ doc.relativePathTo( "../../resume.html" ) }>Resume</a> </li>
            <li> <a href="http://twitter.com/mr_tristan">Twitter</a> </li>
            <li> <a href="mailto:mr.tristan@gmail.com">Email</a> </li>
    	    </ul>
      		<hr />
      		<h3> Knockoff </h3>
        </div>
      )

      override def renderHeader( code : Code ) : Node = Group(
        <div class="heading span-24 small">
    	    <ul class="inline">
            <li> <a href={ code.relativePathTo( "../.." ) }> Home </a> </li>
            <li> <a href="http://github.com/tristanjuricek/knockoff"> Github </a> </li>
            <li> <a href="http://www.linkedin.com/in/tristanjuricek">Linked In</a> </li>
            <li> <a href={ code.relativePathTo( "../../resume.html" ) }>Resume</a> </li>
            <li> <a href="http://twitter.com/mr_tristan">Twitter</a> </li>
            <li> <a href="mailto:mr.tristan@gmail.com">Email</a> </li>
    	    </ul>
      		<hr />
      		<h3> Knockoff </h3>
        </div>
      )    
      
      override def navContent( doc : WebDocument ) : Node = Group(
        baseNav( doc ) ++ overview( doc )
      )
      
      def baseNav( doc : WebDocument ) : Node = Group(
        <ul id="tabs" class="inline">
          <li class="current"> Documentation </li>
          <li>
            <a href={
              codes.firstOption match {
                case None => null
                case Some( code ) => doc.relativePathTo( code.name + ".html" )
              }
            }>Sources</a></li>
        </ul>
        <hr/>
      )
      
      override def navContent( code : Code ) : Node = new Group(
        baseNav( code ) ++
        codeOverview( code )
      )
    
      def baseNav( code : Code ) : Node = Group(
        <ul id="tabs" class="inline">
          <li>
            <a href={
              webDocuments.firstOption match {
                case None => null
                case Some( page ) => code.relativePathTo( page.relativePathToPage )
              }
            } >Documentation</a>
          </li>
          <li class="current">Sources</li>
        </ul>
        <hr/>
      )
      
      override def renderArticle( doc : WebDocument ) : Node =
        <div class="article span-13 prepend-1 last">{ articleContent( doc ) }</div>
      
      override def renderArticle( code : Code ) : Node =
        <div class="article span-13 prepend-1 last">{ articleContent( code ) }</div>
    }
  }
}