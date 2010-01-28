package com.tristanhunt.knockoff.extra

import com.tristanhunt.knockoff.{ BlockType }

case object MetaDatas
extends BlockType[ MetaData ] { def wrappedClass = classOf[ MetaData ] }
