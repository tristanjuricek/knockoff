package com.tristanhunt.knockoff.latex

import com.tristanhunt.knockoff.{ Span, Block }
import scala.util.parsing.input.{ Position }

case class LatexSpan( latex : String ) extends Span

case class LatexBlock( latex : String, position : Position ) extends Block

