---
layout: post
title: The making of a LaTeX pre-processor with Haskell - Part I
---

## Introduction

I've written a decent amount LaTeX from relatively simple documents such as math assignments and essays, and longer more involved documents such as thesis's and CV's. In writting these simple documents I often find the LaTeX syntax a bit too much, such as having to emphasis text as `\emph{italic text}` and the syntax for lists being somewhat verbose. So I plan on writting a pre-processor using the Parsec library to acheive something similar do Jekyll's system of using markdown for content, and yaml headers for additional information.

<!--end excerpt-->

Why Haskell? Haskell is my first foray into functional programming, with my experience only consisting of doing a few problems on Project Euler. And even with these simple programs, I enjoy programming in Haskell *a lot*. I think its mostly due to my math background and the similarity in thinking styles. As of writing this post, my knowledge extends to almost to the end of the chapters on Monads in Learn You a Haskell For Great Good.

The plan for the preprocessor is take something such as this:

    This is a sample latex document. *I am emphasized*, **but I am in bold**. And here is a list

    - Item 1
    - Item 2
    - Item 3

And produce a .tex file reading for processing:

    {% highlight latex %}

    \documentclass{article}

    \begin{document}
        This is a sample latex document. \emph{I am emphasized}, \textbf{but I am in bold}. And here is a list 

        \begin{itemize}
            \item Item 1
            \item Item 2
            \item Item 3
        \end{itemize}
    \end{document}

    {% endhighlight %}

## Part I - First steps

The first to sort out is how to parse a file in the first place. After a quick google, I was a led to [a wikibooks book](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing) [Real World Haskell](http://book.realworldhaskell.org/read/using-parsec.html).

A simple first goal is to parse text containing italics and/or bold characters. For italics, text will be wrapped with `*`, bold with `**`, and bold italics with `*_text_*`. Note that this does not include mixed emphasis like `*italics **bold italic** italics*`. 

Here is my first attempt at the parser:

    {% highlight haskell linenos %}

    import System.IO
    import Control.Monad
    import Text.ParserCombinators.Parsec
    import Data.List

    type Latex = String

    emphasisSymbol :: Parser Char
    emphasisSymbol = char '*'

    boldSymbol :: Parser String
    boldSymbol = string "**"

    beginBoldEmphasisSymbol :: Parser String
    beginBoldEmphasisSymbol = string "*_"

    endBoldEmphasisSymbol :: Parser String
    endBoldEmphasisSymbol = string "_*"

    emphacizedChar :: Parser Char
    emphacizedChar = noneOf "*"

    boldEmphasizedChar :: Parser Char
    boldEmphasizedChar = try (do char '_'
                                 noneOf "*")
                     <|> noneOf "_"
                     <?> "Didn't find bold emph."

    boldChar :: Parser Char
    boldChar = try (do char '*'
                       noneOf "*")
           <|> noneOf "*"
           <?> "Didn't find bold." 

    boldEmphasis = do beginBoldEmphasisSymbol
                      content <- many1 boldEmphasizedChar
                      endBoldEmphasisSymbol
                      return content

    emphasis = do emphasisSymbol
                  content <- many1 emphacizedChar
                  emphasisSymbol
                  return content

    bold = do boldSymbol
              content <- many1 boldChar
              boldSymbol
              return content

    bodyText = try (boldEmphasis) <|> try (bold) <|> try (emphasis) <|> many1 (noneOf "*")

    htexFile = many bodyText

    readInput :: String -> [Latex]
    readInput input = case parse htexFile "" input of
        Left err  -> ["No match " ++ show err]
        Right val -> val

    main = do 
        contents <- readFile "input.htex"
        putStrLn $ intercalate "\n" (readInput contents)

    {% endhighlight %}

The function `bodyText` is where all of the parsers are put together. The order in important: `boldEmphasis` and `bold` should come before `emphasis`, otherwise emphasis will consume the first `*`, then consume the second `*` recognising it as the end of an emphasis, or consume `_` and just recoginise it as a character. I can forsee combining these three functions, and choosing which one to do after the first `*` has been chosen.

Each of the smaller parsers are straight forward: they parse the first character as starting the special block, then process the stuff inside until it hits the end block.

Each of these parsers are then fairly straight forward: they parse the first charactising symbol, then read text until they hit the closing characteristic symbol. Incorporating text such as `*italics **bold italic** italics*` could involve modifying `emphasis` to something like

        emphasis = do emphasisSymbol
                  content <- many1 emphacizedChar <|> boldEmphasis
                  emphasisSymbol
                  return content

This would then involve having two parsers for bold italics, which is not not ideal. For this reason, and that bold italics should not be used all that often (if at all), plus the way LaTeX renderes bold italics ([stack exchange discussion](http://tex.stackexchange.com/questions/46690/standard-order-for-bolditalic), and [another on LaTeX emphasis commands](http://tex.stackexchange.com/questions/41681/correct-way-to-bold-italicize-text)), I will exclude bold italics until its *really* needed.

### Output

Compiling and running with `input.htex` containing `This is not in italics. *But this is.* **This is bold.** This is not bold. *_This is in bold italics._* This is not in bold italics.` will print the following:

    This is not in italics.
    But hits is.

    This is bold.
     This is not bold.
    This is in bold italics
     Thisi is not in bold italics.

Promising!

Next up, I will introduce links using the same format as markdown (\[link\](url)), and also attempt at actually producing LaTeX output.
