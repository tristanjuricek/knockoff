package com.tristanhunt.knockoff.extra

import com.tristanhunt.knockoff.{ Block }
import scala.util.parsing.input.Position

case class MetaData( data : Map[String, String], position : Position )
extends Block

