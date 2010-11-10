# Knocked Up #

Knockoff: The next iteration.

Major goals:

- All elements, like spans, need to be able to have their positions referenced
  exactly.
- Be capable of reverse construction - any Knockoff AST should be completely
  reversable

Implementation switches:

- Create my own token definition, which is then used to construct trees, which
  eventually become the Knockoff AST


## Lexical Tokens

    // In com/tristanhunt/knockoff/KToken.scala
    package com.tristanhunt.knockoff
    
    /**
     * Provides all the lexical token classes recognized by KLexical.
     * 
     * @author Tristan Juricek
     */
    trait KTokens {
        
      /** Each token in the system should remember it's position. */
      trait KToken {
        def position : Position
      }
      
      /**
       * The most generic token in the system. If it can't be recognized as anything
       * else, it's recognized as this.
       */
      case class KText ( val position : Position ) extends KToken

      /**
       * Maps a newline (in whatever wonderful platform-specific way it comes in.
       */
      case class KNewline( val position : Position ) extends KToken
      
      /**
       * A space chunk is 1-4 spaces, or a tab.
       */
      case class KSpaceChunk( val position : Position ) extends KToken
      
      /**
       * Hashes are a group of ## characters that either follow a KNewline or the
       * start of a document.
       */
      case class KHashes( val position : Position ) extends KToken
      
      
    }