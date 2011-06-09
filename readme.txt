== What:

jstr (the 'e's are silent) stands for Javascript Transformations.

https://github.com/clausreinke/jstr

Right now, it only consists of an updated version of jsparse, with
a grammar edging ever closer to ES5:

  jsparse is a simple library of parser combinators for Javascript based
  on Packrat parsers [1] and Parsing expression grammars [2].

  [1] http://pdos.csail.mit.edu/~baford/packrat/
  [2] http://en.wikipedia.org/wiki/Parsing_expression_grammar

  The only documentation currently available in these blog entries:

  http://www.bluishcoder.co.nz/2007/10/javascript-packrat-parser.html
  http://www.bluishcoder.co.nz/2007/10/javascript-parser-combinators.html

  Originally written by Chris Double: http://github.com/doublec/jsparse

  In the process of being revived, rewritten, and extended, by Claus Reinke.

Since my main interest is in parsing, analysing and transforming ECMAScript 5
and beyond, I initially tried to keep the parser combinator API, just extending
the grammar and implementation. That has begun to change now, so don't expect
stability. Consider this a semi-public preview, many of the comments are still
notes to myself, rather than sufficient user documentation.

Chris' blog posts are still a good starting point for the combinators, and the
near-ES5 grammar is an extended usage example, but I should add a blog post
documenting the API/grammar/code differences. Brief summaries of changes and
usage can be found below.

== Files:

  jsparse.js
    the parser combinator library

  es5.js
    incomplete/work-in-progress ECMAScript 5 grammar

  test.js
    sample setup, for testing

  test.html
    use test.js in browser

  test.wsf
    use test.js with WSH/CScript

== Usage

There is no proper tool interface yet. You can either use the test interfaces
or use them as guidelines for writing your own wrappers. As you can see from
test.html and test.wsf, we really only need to be able to load jstr's modules,
log to a console, and load source files to be parsed (log/load should really
be passed as parameters instead of placed in the global environment - TODO).

These requirements shouldn't pose a problem for most JS environments.  A
cross-platform test wrapper for the commandline would be nice to have (there
is one for Windows Scripting Host, but that runs an old sluggish pre-IE9 JS
engine) - it is on the TODO list, and suggestions for suitable *cross-platform*
commandline JS engines are welcome.

In-browser test use (works with IE9, FF4, Safari, ..; just needs a JS console):

- open test.html in browser

  Nothing to see here.

- open the browser's JS console

  You should see parse errors from a JS file that uses non-ES5 extensions.

  (if you are curious about the extensions, you could change LANGUAGE.tailnests
   near the top of es5.js to 'true' and run 'test(file,"pu",pc,grammar)', but
   for normal use, you will want to keep the default setting of 'false').

- in JS console, evaluate

    test("es5.js","p",pc,grammar)

  This should successfully parse one of jstr's own JS files.

- the command format is

    test(<file>, <options>, <parser combinators>, <grammar>)

  where <file> is fetched via XHR, <parser combinators> and <grammar> are
  loaded from jstr by test.html, and <options> currently include:

  p  parse file
  m  show matched/processed source
  r  show unmatched/remaining source
  a  show AST
  u  unparse AST back to source
  l  show grammar
  t- show grammar-rule-level trace information during parse
  t  show parser-combinator-level trace information during parse
  s  add rule stack information

  Both trace and stack information can be filtered by regular expressions
  (see test.js for usage examples), but tracing really needs to be wrapped
  in a good pager with search (something like less on the commandline; note
  that you can search for rule entries '>rule' and rule exits '<rule'). Even
  then: the smaller the input file, the more helpful the trace output.

  Rule stack info is used in two contexts: parse error messages and debugger
  sessions (for the latter, have the debugger monitor the variable 'rule_stack').

== Summary of changes (jstr vs jsparse-as-of-2007):

- code wrapped in functions for modularity control
- test wrappers added for in-browser and WSH commandline use, with options

- grammar
  - add MultiLineComment, RegularExpressionLiteral, LineContinuation, ..
  - replace generic whitespace trim with grammar-specified whitespace,
    move comment handling into whitespace
  - wrap rules in rule-combinator, to facilitate coding with grammar
    objects, to support tracing and self-describing grammar
  - add ASI handling
  - wrap rules for AST creation (close to SpiderMonkey AST)
  - fix a few bugs (ordering of choice alternatives, missing forward
    declarations,.. )

- combinators
  - add parser.toString(), to output parsers as combinators instead of
    implementation code (with max_depth limit against unguarded recursion)
  - add rule combinator to mark grammar rules (for tracing, debugging, and
    self-describing grammars)
  - add debug and trace output (rule tracing and rule stack info, with filters)
  - add line count
  - add whitespace.trim hook, to replace default trim with
    grammar-specified whitespace
  - trace partially successful parses, to make grammar/parse errors
    easier to spot (needs more work: NLTH messes up parse length, hiding
    some parse error info; also need to reinstate preference for abstract,
    non-terminal, over terminal error contexts)
  - experiment with alternate caching scheme
  - more whitespace handling control (w* parsers only wrap literal token
    arguments in whitespace(.), not arbitrary arguments)
  - removed a couple of combinators
  - flatten ast by default, structure can be added based on rules
    (parse tree) or AST nodes (abstract syntax tree), whitespace is
    stored outside AST, indexed by start location of next token
  - support faithful unparsing (comments and whitespace preserved)
  - added combinators for binary operators and left recursive rules

