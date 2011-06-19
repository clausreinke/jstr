
// left-associative function application,
f(a)(b)(c);       // standard: with parens
f a b c;          // new: no parens for PrimaryExpression arguments

// right-associative function application,
f(g(h(x)));       // standard: tail nest, with parens
f @< g @< h @< x; // new: right-associative infix application, paren-free

// new: this now parses as a function application;
//      should trigger ASI warnings, as ASI no longer applies
f
1
this
{}
function(){}
;

// standard callback chain: syntactic tail nests, parens, braces, returns
       operation1().then(function(a) {
return operation2().then(function(b) {
return operation3(a,b);
       });
       });

// new callback chain: brace/return-free expression-returning function definitions,
// with paren-free FunctionExpression arguments
( operation1().then function(a) =>
  operation2().then function(b) =>
  operation3(a,b)
);

// with paren-free right-associative infix application
( operation1().then @< function(a) =>
  operation2().then @< function(b) =>
  operation3(a,b)
);

// curried expression-returning function definition
function Add(x) { return function (y) { return x+y } };
function Add(x) => function (y) => x+y;
// (could be shortened further, by shorter prefix, or by
// using the same syntax for curried definitions and calls)

