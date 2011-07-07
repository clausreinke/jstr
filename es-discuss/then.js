
// examples of "then-able" computations
// (if_,while_,mapM, yield_)
//
// if you know continuation-passing: instead of every operation
//   taking a continuation, every operation returns a then-able,
//   which can pass an intermediate result to a continuation; a
//   slightly more modular variant of continuation-passing style;
//
// (if you know monads: if we could guarantee types and laws,
//  .then would be bind/>>=, and value would be return)
//
// otherwise, just think of
//
//    stmt .then @< function(result) => stmts
//
// as a slightly clumsy way of writing
//
//    let result <- stmt; stmts
//
// which is almost a standard let-binding, but without hoisting,
// and more flexible (read as "result from stmt, in stmts")

// ------------------------------------------------

// TODO: once we're sure this pattern is sufficiently general,
//       introduce syntactic sugar for such let-then
//
//  let pattern <- expression1 in expression2
//
// should become
//
//  expression1.then @< function(pattern) => expression2
//

// TODO: in combination with object literal sugar, simplify
//
// { then: function(cont) => result }
//
// to
//
// { then(cont) => result }
//

// TODO: currently, effects have to be side-effects - we are working in
// JS's side-effecting expression world to emulate basic statement blocks;
// for more complex effects, we'll want to store effect-specific data in the
// then-able; but then we'll need effect-preserving 'value' and 'then', in
// effect- specific implementations of the then-able interface.

// ------------------------------------------------

// value doesn't do anything itself, just passes a value when asked
function value(val) => { then: function(cont) => cont(val) };

// run thenable and return its result
function run(thenable) => thenable.then(function(x) => x);

// conditional then-able statement
function if_(cond) => function(fthen) => function(felse) =>
  cond.then @< function(c) => c ? fthen() : felse();

// while-loop with then-able condition and body:
function while_(cond) => function(body) =>
  if_(cond)
    (function() => body().then @< function() => while_(cond)(body) )
    (function() => value( null ) );

// we can pretend that JS statements follow the then pattern
function stmt_(stmt) => value( null );

// or, for better control, we can wrap JS statements in then-ables, eg
function log_(msg) => { then: function(cont) => cont( log(msg) ) };

// TODO: JS's handling of References does not allow for 
//        abstractions over assignment, ie, none of this would work:
//
// function assign(lhs,rhs) => { then: function(cont) => (lhs = rhs) };
// function inc(x) => { then: function(cont) => (x++) };

function test_if() => 
  run( if_(value(true))
        (function()=> log_("it is true!"))
        (function()=> log_("no")) );

// proper let expressions would come out of let-then sugar (see top TODO);
// for now, we'll fake them
function let_(val) => function(body) => body(val);

// JS References make assignment abstraction unnecessarily tricky
// (can't pass reference to i, so have to unfold value(i) in condition)
function test_while(arr) =>
  run( let_ (arr.length) @< function(i) =>
       while_({then:function(cont)=>cont(i)})
          (function() => log_("while: "+arr[--i]) ) );

// wrap an Array to provide then-ables
// mapM(body): apply body to every element, chain the resulting
//             computations in sequence, collect their intermediate
//             results in an Array
function $(arr) =>
  { mapM: function (body) =>
            arr.length>0
              ? body(arr[0])               .then @< function(el)  =>
                $(arr.slice(1)).mapM(body) .then @< function(els) =>
                value( [el].concat(els) )
              : value( [] )
  };

// compose continuations
// c1, when applied to val, returns a then
// c2 gets passed to that then
// the result of comp_then(c2,c1) can be used as the next c1
function comp_then(c2,c1) => function(val) => c1(val).then(c2);

// auxiliary function for yield_: unwind and track stack
// .then(cont)  : continuation is added to stack
// .value(cont) : continuation receives yielded value
// .next()      : stacked continuations are applied to yielded value
function stack(val) => function(c1) =>
  { then:  function(c2)   => stack(val)(comp_then(c2,c1))
  , value: function(cont) => cont(val)
  , next:  function()     => c1(val) }; // alternatively, next could
                                        // provide a modified val

// yield a value out of a then-chain
// ignores but stores all .then continuations
// use .value(cont) to receive yielded value
// use .next() to restart stored continuations from yield-point
function yield_(val) => { then: stack(val) };

// mapM square over an Array, yielding each element in sequence,
// then returning the resulting Array of squared elements
function generator_mapM(arr) =>
  $(arr).mapM @< function(el) =>
  yield_(el).then @< function(el) => // yield_ could change el
  value(el*el);

// while loop over an Array, yielding each element in sequence
// before logging it
function generator_while(arr) =>
  let_ (arr.length) @< function(i) =>
  while_({then:function(cont)=>cont(i)})
     (function() => yield_(arr[--i]).then @< function(ai) =>
                    log_("while: "+ai) );


// iterate over a generator, log yielded values and end-result
function iterate(generator) =>
  generator.next ? generator                   .value @< function(val) =>
                   log_("iterate:"+val)        .then  @< function() =>
                   iterate( generator.next() )
                 : generator;

// iterate over while_ generator
// should log elements, via log_
function test1() => iterate(generator_while([1,2,3,4,5]));

// iterate over mapM generator
// should log elements, via log_, and array, via log_obj
function test2() => iterate(generator_mapM([1,2,3,4,5])).then @< log_obj;

log("---- test1()"); test1();
log("---- test2()"); test2();
