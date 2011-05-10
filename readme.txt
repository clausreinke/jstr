jsparse is a simple library of parser combinators for Javascript based
on Packrat parsers [1] and Parsing expression grammars [2]. 

[1] http://pdos.csail.mit.edu/~baford/packrat/
[2] http://en.wikipedia.org/wiki/Parsing_expression_grammar

The only documentation currently available in these blog entries:

http://www.bluishcoder.co.nz/2007/10/javascript-packrat-parser.html
http://www.bluishcoder.co.nz/2007/10/javascript-parser-combinators.html

Originally written by Chris Double. In the process of being revived,
rewritten, and extended, by Claus Reinke. Since my main interest is in
parsing ECMAScript 5 and beyond, I've been trying to keep the parser
combinator API, just extending the grammar and implementation.

jsparse.js
  the combinator library

es5.js 
  incomplete/work-in-progress ECMAScript 5 parser

test.js
  sample setup, for testing

test.html
  use test.js in browser

test.wsf
  use test.js with WScript

