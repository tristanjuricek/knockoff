package com.tristanhunt.knockoff2

import scala.util.parsing.input.{ Position }

case class LinkTitle( val original : String, val pos : Position,
                      val text : String )
