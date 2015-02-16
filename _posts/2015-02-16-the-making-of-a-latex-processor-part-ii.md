---
layout: post
title: The making of a LaTeX pre-processor with Haskell - Part II
---

## Part II - A working program (kinda)
In the previous part, the progam could parse text for emphasized and bold characters. So there's some functionality, except that it doesn't *do* anything useful like produce output. In this part, I'll implement parsing links Markdown stlye (this is really more of an implementation of Markdown than anything else at the moment), and then turn the parsed text into something useful!

<!--end excerpt-->

To start off with, I removed the ability for bold emphasized text. This is largely because there's no plan on how to deal with embedded styles, so I'd rather not start now. Implementation of link parsing exists in the `link` function, which is very similiar to the other parsers, except it produces two strings. Suprisingly easy to put together in this case, and illustrates the parsing process a bit more clearly. Here is the resulting code:

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

    emphacizedChar :: Parser Char
    emphacizedChar = noneOf "*"

    boldChar :: Parser Char
    boldChar = try (do char '*'
                       noneOf "*")
           <|> noneOf "*"
           <?> "Didn't find bold." 

    textChar :: Parser Char
    textChar = noneOf "*("

    linkChar :: Parser Char
    linkChar = noneOf "]"

    linkDescriptionChar :: Parser Char
    linkDescriptionChar = noneOf ")"

    emphasis = do emphasisSymbol
                  content <- many1 emphacizedChar
                  emphasisSymbol
                  return (content

    bold = do boldSymbol
              content <- many1 boldChar
              boldSymbol
              return content

    link = do char '('
              description <- many1 linkDescriptionChar
              string ")["
              link <- many1 linkChar
              char ']'
              return (link ++ " " ++ description)

    bodyText = try (link) <|> try (bold) <|> try (emphasis) <|> many1 (textChar)

    htexFile = many bodyText

    readInput :: String -> [Latex]
    readInput input = case parse htexFile "" input of
        Left err  -> ["No match " ++ show err]
        Right val -> val

    main = do 
        contents <- readFile "input.htex"
        putStrLn $ foldl (\acc x -> acc ++ x) "" (readInput contents)

    {% endhighlight %}

So not a whole lot changed. without too much effort its resonably easy follow the parser from `bodyText`.

The next thing to do is make the parser return LaTeXified text. This turned out to be more simple than I thought, though it helps that all I am doing it returning strings. All that was required is to wrap the content in the return statements with LaTeX commands, and altering the `main` function simply concatanate the resulting parsed text:

    {% highlight haskell linenos %}

    emphasis = do emphasisSymbol
              content <- many1 emphacizedChar
              emphasisSymbol
              return ("\\emph{" ++ content ++ "}")

    bold = do boldSymbol
          content <- many1 boldChar
          boldSymbol
          return ("\\textbf{" ++ content ++ "}")

    link = do char '('
          description <- many1 linkDescriptionChar
          string ")["
          link <- many1 linkChar
          char ']'
          return ("\\href{" ++ link ++ "}{" ++ description ++ "}")

    main = do 
        contents <- readFile "E:\\Google Drive\\Code\\latexpreprocessor\\input.htex"
        putStrLn $ foldl (\acc x -> acc ++ x) "" (readInput contents)

    {% endhighlight %}

 Running it with `input.htex` as `This is not in italics. *But this is.* **This is bold.** This is not bold. (Description for a link)[link]. This is not in bold italics.` results in

    This is not in italics. \emph{But this is.} \textbf{This is bold.} This is not bold. \href{link}{Description for a link}. This is not in bold italics.

Easy! It's not quite right, since you can't just run LaTeX on it on the output, but that requires a bit more thinking to manage package handling and other preamble bits.

### Where to?

At this point, I could continue and create a fully fledged pre-preprocessor, but would this satisfy a true need for it. As I said in the previous part, LaTeX syntax is somewhat verbose for a lot of uses. What I can see my pre-processor doing is allowing users to only need content to create pdfs, with some customization via something like a YAML header (like Jekyll), allowing for a bit of a "hands off" experience. Pandoc already allows for the creation of LaTeX files from Markdown, so what further use would a slightly different Markdown derivative be?

Templates are another possible extension, but this functionality is already available in Pandoc, and can be implemented fairly easily with Python using Jinja. One problem with templates is that with the seperation of content and design, the content must be written without regard for where it is in the page. This reduces to having to edit a `.tex` file anyway.

This all applies to normal text, not math notation since there's no real alternative markup. Which is real shame since latex is over a bit *too* verbose what you need it for (especially if you're writing any calculus). To suit the needs of ordinary text, in my experience with helping create my partners masters thesis in LyX, all that's needed it a good framework to handle bibliographies and references.

So with all of the above, I'm going leave this project as is. On the plus side, what if you could write math in Haskell? Say, if you wanted to typeset

    {% highlight haskell %}

    naturalNumbers = [1..]
    [x | x <- naturalNumbers, x < 20]

    {% endhighlight %}

as LaTeX? This doesn't look too diffucult, and could produce the output:

    {% highlight latex %}

    \{x | x \in \mathbb{N}, x < 20 \}

    {% endhighlight %}

Looks fairly trivial to do. How about some calculus? How would you represent

    {% highlight latex %}

    \frac{d \theta}{d \gamma} = \lambda^{x + \epsilon} \frac{d^2 \theta}{d \gamma^2}

    {% endhighlight %}

in Haskell? This example is a bit complicated complicated math wise, since we don't know what `\theta` is as a function, and `\lambda` may not be invertable, so rearraging is not so easy. In addition, both side of the equations have functions applied to `\theta`. But luckely, we don't care about the math, but the typesettings instead! Inside of a LaTeX (or even html file), one could have:

    {% highlight haskell linenos %}

    gamma = Var
    lambda = Var
    epsilon = Var
    x = Var
    theta = Func gamma

    leftEquation = Derivative theta gamma
    rightEquation = (lambda `pow` (x + epsilon)) `multiply` (Derivative (Derivative theta gamma) gamma)

    equationOne :: (Expression, Expression)
    equationOne = (leftEquation, rightEquation)

    {% endhighlight %}

Where the type definitions are:

    {% highlight haskell linenos %}

    class ExpressionOps a where
        multiply :: a -> a -> b
        pow :: a -> a -> b
        frac :: a -> a -> b

    data Expression = Var
                    | Func Expression
                    | Func2 Expression Expression
                    | Derivative Expression Expression

    instance ExpressionOps Expression where
        multiply exp1 exp2 = Func2 exp1 exp2
        pow exp1 exp2 = Func2 exp1 exp2
        frac exp1 exp2 = Func2 exp1 exp2
    
    {% endhighlight %}

Then a parser could use type introspection (for variable names/operations) alongside evaluating `equationOne`, which maps names to LaTeX commands contained within a text file.

In this case, the Haskell version is a bit longer, and it's readability debatable compared to the LaTeX version. But one thing it does allow you to do is reuse functions functions quickly (without those bloody backslashes everywhere!). For instance, if say you wanted to rewrite the above to a system of differential equations, all you would have to do is write

    {% highlight haskell %}

        diff1 = (y1, leftExpression)
        diff2 = (y2, rightExpression)

    {% endhighlight %}

And your set!

One other way that may work is to take working mathematical functions and rely heavily on type introspection to get the meta-data. But in the case of the above differential equation, `theta` is unknown and may not have a closed form. So you would need some method of saying *`theta` is a function that explicity depends on `x`, `y`, and `z`*. This method opens up support for incorperating other languages, but I don't there'd be a reliable way of implementing it considering scientific conputing uses languages from C to Matlab.
