package com.tristanhunt.knockoff.extra

import com.tristanhunt.knockoff.{ Block, Paragraph, Discounter }
import com.tristanhunt.knockoff.latex.{ LatexDiscounter, LatexWriter }

trait Wholesaler extends Discounter with MetaDataConverter
  with MetaDataXHTMLWriter with LatexDiscounter with LatexWriter
  with SCAMLDiscounter with SCAMLLatexWriter with SCAMLMetaDataWriter
