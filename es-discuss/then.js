
// examples of "then-able" computations
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

// TODO: introduce syntactic sugar for such let-then
//
//  let pattern <- expression1 in expression2
//
// should become
//
//  expression1.then @< function(pattern) => expression2
//

// ------------------------------------------------

// value doesn't do anything itself, just passes a value when asked
function value(val) => { then: function(cont) => cont(val) };

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
  , next:  function()     => c1(val) };

// yield a value out of a then-chain
// ignores but stores all .then continuations
// use .value(cont) to receive yielded value
// use .next() to restart stored continuations from yield-point
function yield_(val) => { then: stack(val) };

// pretend that JS statements follow the then pattern
function stmt_(stmt) => value( null );

// loop over an Array, yielding each element in sequence
function generator(arr) =>
  $(arr).mapM @< yield_;

// iterate over a generator, log yielded values and end-result
function iterate(generator) =>
  generator.next ? generator                   .value @< function(val) =>
                   stmt_( log_obj(val) )       .then  @< function() =>
                   iterate( generator.next() )
                 : generator                   .then @< log_obj;

// should log elements and array, via log_obj
function test() => iterate(generator([1,2,3,4,5]));

