// a parser combinator library (packrat parsing of parsing expression grammars)
//
// Modifications: Copyright Claus Reinke.
// Original code: Copyright (C) 2007 Chris Double.
//                http://github.com/doublec/jsparse
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice,
//    this list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
// INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
// FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
// DEVELOPERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
// OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
// ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

var pc = (function(){

function foldl(f, initial, seq) {
    for(var i=0; i< seq.length; ++i)
        initial = f(initial, seq[i]);
    return initial;
}

// log ast in nested tree form
function log_tree(pre,ast) {
  if (ast instanceof Array)
    if (ast.length>0)
      for (var i=0; i<ast.length; i++)
        log_tree(pre+(i%10).toString(),ast[i]);
        // log_tree(pre+'-',ast[i]); // less interesting/confusing output variation
    else
        log(pre+'|[]|');
  else if (ast instanceof Rule)
    log_tree(pre/* +"{"+ast.name+"}" */,ast.ast); // names too long, obscure tree
  else if (ast instanceof Named)
    log_tree(pre/* +"."+ast.name */,ast.ast); // names too long, obscure tree
  else if (ast instanceof Node) {
    for (var n in ast)
      if (ast.hasOwnProperty(n) && n!=='ast')
        log_tree(pre+"."+n+":",ast[n]); // names too long..
  } else
    log(pre+'|'+ast+'|');
}

// log ast as nested array (to help cscript default logging)
function log_array(ast) {
  function aux(ast) {
    var result = [];
    if (ast instanceof Array) {
      for (var i=0; i<ast.length; i++)
        result.push(aux(ast[i]));
      return "["+result.join(",")+"]";
    } else
      return ast;
  }
  log(aux(ast));
}

// log ast by unparsing
function log_ast_as_string(whitespace,ast) {
  var result = [], done = [];
  function aux(ast) {
    if (ast) {
      if (ast.index && !done[ast.index] && whitespace[ast.index]) {
        done[ast.index] = true; // don't output whitespace twice
        result.push(whitespace[ast.index]);
      }
      if (ast instanceof Array)
        for (var i=0; i<ast.length; i++)
          aux(ast[i]);
      else if (ast instanceof Rule)
        aux(ast.ast);
      else if (ast instanceof Node)
        aux(ast.ast); // bypass AST structure, for simple generic traversal
      else if (ast instanceof Named)
        aux(ast.ast); // bypass AST structure, for simple generic traversal
      else
        result.push(ast);
    }
  }
  aux(ast);
  log(result.join(""));
}

var memoize = true;
var debug   = false;

function set_debug(flag) { debug = flag; }

function ParseState(input, index, line) {
    this.input = input;
    this.index = index || 0;
    this.length = input.length - this.index;
    this.cache = { };
    this.line  = line || 1; // line number
    this.NL = false;  // linebreak in previous token?
    this.whitespace = []; // whitespace, indexed by index of next token
    this.partials = []; // partial parses, by index
    return this;
}

ParseState.prototype.incLine = function(s) {
  var nls = s.match(/(\r\n|\r|\n)/g);
  if (nls) {
    this.line += nls.length;
    this.NL = nls.length > 0;
  } else
    this.NL = false;
};

// TODO: column and indent tracking
//        (would like to rule out tabs, but can't..;
//         instead, use a run-length encoding of indent? :-()

ParseState.prototype.from = function(index,s) {
    var r = new ParseState(this.input, this.index + index, this.line);
    r.cache      = this.cache;
    r.whitespace = this.whitespace;
    r.partials   = this.partials;
    r.length     = this.length - index;
    if (s)
      r.incLine(s);
    else
      r.NL = this.NL;
    return r;
};

ParseState.prototype.substring = function(start, end) {
    return this.input.substring(start + this.index, (end==null ? this.length : end) + this.index);
};

ParseState.prototype.trimLeft = function() {
    var s = this.substring(0);
    var m = s.match(/^\s+/);
    return m ? this.from(m[0].length,m) : this;
};

ParseState.prototype.at = function(index) {
    return this.input.charAt(this.index + index);
};

ParseState.prototype.toString = function() {
    return 'PS"' + this.substring(0) + '"';
};

ParseState.prototype.getCached = function(pid) {
    if(!memoize)
        return false;

    var p = this.cache[pid];
    if(p)
        var cached = p[this.index];
    else
        var cached = false;
    if (debug&&cached) log("getCached ("+pid+"/"+this.index+") "+cached.matched+"\n:"+cached.ast);
    return cached;
};

ParseState.prototype.putCached = function(pid, cached) {
    if(!memoize)
        return false;

    var p = this.cache[pid];
    if(!p)
      p = this.cache[pid] = { };
    p[this.index] = cached;
    if (debug) log("putCached ("+pid+"/"+this.index+") "+(cached?cached.matched+"\n:"+cached.ast:"false"));
};

function ps(str) {
    return new ParseState(str);
}

// 'r' is the remaining string to be parsed.
// 'matched' is the portion of the string that
// was successfully matched by the parser.
// 'ast' is the AST returned by the successfull parse.
function make_result(r, matched, ast) {
    return { remaining: r, matched: matched, ast: ast };
}

// 'r' is the remaining string to be parsed.
// 'matched' is the portion of the string that
// was successfully matched by the parser.
// 'ast' is the AST returned by the partially successfull parse.
// 'msg' is the error message terminating the partial parse
function make_partial_result(r, matched, ast, msg) {
    return { remaining: r, matched: matched, ast: ast,
             msg: msg, rule_stack: rule_stack };
}

var parser_id = 0;

var cache = { token: {}, wtoken: {}, ch: {}, range: {}, regex: {} };

function clear_cache() { cache = { token: {}, wtoken: {}, ch: {}, range: {}, regex: {} }; }

// 'token' is a parser combinator that given a string, returns a parser
// that parses that string value. The AST contains the string that was parsed.
function token(s) {
    var pid = parser_id++;
    if (!cache.token[s]) cache.token[s] = [];
    var my_cache = cache.token[s];
    var tokenparser = rule("pc.token("+s+")",function(state) {
        var savedState = state;
        // var cached = savedState.getCached(pid);
        var cached = my_cache[savedState.index];
        if(cached)
            return cached;

        var r = state.length >= s.length && state.substring(0,s.length) == s;
        if(r) {
            cached = { remaining: state.from(s.length,s), matched: s, ast: s };
        } else
            cached = false;
        // savedState.putCached(pid, cached);
        my_cache[savedState.index] = cached;
        return cached;
    });
    tokenparser.toString = function() { return "token(\""+s+"\")"; };
    return tokenparser;
}

// 'wtoken' is like 'token', but consumes whitespace
function wtoken(s) {
  var wtokenparser = whitespace(token(s));
  wtokenparser.toString = function() { return "wtoken("+s+")"; };
  return wtokenparser;
}

// Like 'token' but for a single character. Returns a parser that given a string
// containing a single character, parses that character value.
function ch(c) {
    var pid = parser_id++;
    if (!cache.ch[c]) cache.ch[c] = [];
    var my_cache = cache.ch[c];
    var chparser = rule("pc.ch("+c+")",function(state) {
        var savedState = state;
        // var cached = savedState.getCached(pid);
        var cached = my_cache[savedState.index];
        if(cached)
            return cached;
        var r = state.length >= 1 && state.at(0) == c;
        if(r) {
            cached = { remaining: state.from(1,c), matched: c, ast: c };
        } else
            cached = false;
        // savedState.putCached(pid, cached);
        my_cache[savedState.index] = cached;
        return cached;
    });
    chparser.toString = function() { return "ch(\""+c+"\")"; };
    return chparser;
}

// 'wch' is like 'ch', but consumes whitespace
function wch(c) {
  var wchparser = whitespace(ch(c));
  wchparser.toString = function() { return "wch("+c+")"; };
  return wchparser;
}

// 'range' is a parser combinator that returns a single character parser
// (similar to 'ch'). It parses single characters that are in the inclusive
// range of the 'lower' and 'upper' bounds ("a" to "z" for example).
function range(lower, upper) {
    var pid = parser_id++;
    if (!cache.range[lower+upper]) cache.range[lower+upper] = [];
    var my_cache = cache.range[lower+upper];
    var rangeparser = rule("pc.range("+lower+","+upper+")",function(state) {
        var savedState = state;
        // var cached = savedState.getCached(pid);
        var cached = my_cache[savedState.index];
        if(cached)
            return cached;

        if(state.length < 1)
            cached = false;
        else {
            var ch = state.at(0);
            if(ch >= lower && ch <= upper)
                cached = { remaining: state.from(1,ch), matched: ch, ast: ch };
            else
                cached = false;
        }
        // savedState.putCached(pid, cached);
        my_cache[savedState.index] = cached;
        return cached;
    });
    rangeparser.toString = function() { return "range("+lower+","+upper+")"; };
    return rangeparser;
}

// 'wrange' is like 'range', but consumes whitespace
function wrange(l,u) {
  var wrangeparser = whitespace(range(l,u));
  wrangeparser.toString = function() { return "wrange("+l+","+u+")"; };
  return wrangeparser;
}

function regex(r) {
    var pid = parser_id++;
    if (!cache.regex[r]) cache.regex[r] = [];
    var my_cache = cache.regex[r];
    var regexparser = rule("pc.regex("+r+")",function(state) {
        var savedState = state;
        // var cached = savedState.getCached(pid);
        var cached = my_cache[savedState.index];
        if(cached)
            return cached;

        var res = state.substring(0).match(r);
        if(res) {
            cached = { remaining: state.from(res[0].length,res[0]), matched: res[0], ast: res[0] };
        } else
            cached = false;
        // savedState.putCached(pid, cached);
        my_cache[savedState.index] = cached;
        return cached;
    });
    regexparser.toString = function() { return "regex(\""+r+"\")"; };
    return regexparser;
}

// Helper function to convert string literals to token parsers
// and perform other implicit parser conversions.
function toParser(p,white) {
    return (typeof(p) == "string") ? (white ? wtoken(p) : token(p)) : p;
}

// grammar rule trace, controlled by regex 
// (depth limit for toString cycles not protected by named rules)
var depth = 0, max_depth = 1000000;
var trace = false, no_trace = false;
function set_trace(include_regex,exclude_regex) {
  trace    = include_regex;
  no_trace = exclude_regex;
}

// Parser combinator that returns a parser that
// skips whitespace before applying parser.
// TODO: - should this be sequence(whitespace.trim,p)?
//         (would need to inline those extraneous sequences, to avoid
//          extra complexity and limited error messages from too short partial parses)
//       - if p fails, leading whitespace does not count for partial parse
//         (and registering a partial parse here would give error msg without context)
function whitespace(p) {
    var p = toParser(p);
    var pid = parser_id++;
    var whiteparser = rule("pc.whitespace("+p+")",function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        // TODO: we assume that trim always succeeds..
        var trimmed = whitespace.trim(state);

        // allow trace control in comments
        if (trimmed.matched.match(/trace[:\+-]/)) {
          if (trimmed.matched.match(/trace:on/)) trace = new RegExp('.*');
          if (trimmed.matched.match(/trace:off/)) trace = false;
          var match = trimmed.matched.match(/trace\+([^+]*)\+/);
          if (match) {
            log('tracing '+match[1]);
            trace = new RegExp(match[1]);
          }
          var match = trimmed.matched.match(/trace-([^+]*)-/);
          if (match) {
            log('not tracing '+match[1]);
            no_trace = new RegExp(match[1]);
          }
        }

        // look at trim parser result as a whole
        // and don't lose adjacent whitespace NL info
        if (trimmed.remaining.line>savedState.line)
          trimmed.remaining.NL = true;
        else
          trimmed.remaining.NL = trimmed.remaining.NL || state.NL;

        var result = p(trimmed.remaining);
        if (result) {
          if (trimmed.ast && trimmed.ast!="") {
            // store whitespace outside main ast, indexed by index of next token
            savedState.whitespace[trimmed.remaining.index] = trimmed.ast;
          }
          if (result.ast) {
            // can't attach info to plain string, so wrap
            if (typeof result.ast==="string")
              result.ast = new String(result.ast);

            // TODO: since the SpiderMonkey AST is evaluation-biased, 
            //       we'll have to deal with other non-objects here..

            // attach source location, for access to whitespace
            result.ast.index = trimmed.remaining.index;
          }
          cached = make_result(result.remaining,
                               trimmed.matched + (result.matched||""),
                               result.ast
                              );
        } else
          cached = result;
        savedState.putCached(pid, cached);
        return cached;
    });
    whiteparser.toString = function() { return "whitespace("+(depth++>max_depth?"...":p)+")"; };
    return whiteparser;
}
// hook for grammar-specified whitespace, with default trim
whitespace.trim = function(input) { return input.trimLeft(); };

// Parser combinator that passes the AST generated from the parser 'p'
// to the function 'f'. The result of 'f' is used as the AST in the result.
function action(p, f) {
    var p = toParser(p);
    var pid = parser_id++;
    var actionparser = function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        var x = p(state);
        if(x) {
            cached = make_result(x.remaining,x.matched,f(x.ast));
        }
        else {
            cached = x;
        }
        savedState.putCached(pid, cached);
        return cached;
    };
    actionparser.toString = function() { return "action("+(depth++>max_depth?"...":p)+","+f+")"; };
    return actionparser;
}

// Given a parser that produces an array as an ast, returns a
// parser that produces an ast with the array joined by a separator.
function join_action(p, sep) {
    var join_actionparser = action(p, function(ast) { return ast.join(sep); });
    join_actionparser.toString = function() {
      return "join_action("+(depth++>max_depth?"...":p)+","+sep+")";
    };
    return join_actionparser;
}

// Given an ast of the form [ Expression, [ a, b, ...] ], convert to
// [ [ [ Expression [ a ] ] b ] ... ]
// This is used for handling left recursive entries in the grammar. e.g.
// MemberExpression:
//   PrimaryExpression
//   FunctionExpression
//   MemberExpression [ Expression ]
//   MemberExpression . Identifier
//   new MemberExpression Arguments
function left_factor(ast) { return ast; // ONGOING: ast flattening
    return foldl(function(v, action) {
                     return [ v, action ];
                 },
                 ast[0],
                 ast[1]);
}

// Return a parser that left factors the ast result of the original
// parser.
function left_factor_action(p) {
    var left_factorparser = action(p, left_factor);
    left_factorparser.toString = function() {
      return "left_factor_action("+(depth++>max_depth?"...":p)+")";
    };
    return left_factorparser;
}

// 'negate' will negate a single character parser. So given 'ch("a")' it will successfully
// parse any character except for 'a'. Or 'negate(range("a", "z"))' will successfully parse
// anything except the lowercase characters a-z.
function negate(p) {
    var p = toParser(p);
    var pid = parser_id++;
    var negateparser = rule("pc.negate("+p+")",function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        if(state.length >= 1) {
            var r = p(state);
            if(!r) {
                cached =  make_result(state.from(1,state.at(0)), state.at(0), state.at(0));
            } else
                cached = false; // TODO: any use of partial result here?
        }
        else {
            cached = false;
        }
        savedState.putCached(pid, cached);
        return cached;
    });
    negateparser.toString = function () { return "negate("+(depth++>max_depth?"...":p)+")"; };
    return negateparser;
}

// 'end_p' is a parser that is successful if the input string is empty (ie. end of parse).
function end_p(state) {
    if(state.length == 0)
        return make_result(state, undefined, undefined);
    else
        return false;
}
end_p.toString = function() { return "end_p"; };

// 'nothing_p' is a parser that always fails.
function nothing_p(state) {
    return false;
}
nothing_p.toString = function() { return "nothing_p"; };

// add sub ast to ast Array; produce flat array by default
// (Rule can be used to preserve parse tree structure;
//  Named can be used to name ast substructures)
function ast_add(ast,sub) {
  if (sub instanceof Array)
    return ast.concat(sub);   // flatten ast by default
  else
    return ast.concat([sub]);
}

// ast node;
// TODO: - add source location
//       - what about whitespace/comments?
//       - add original ast stream, for unparsing?
//       - watch out: keep ES and general PC aspects separate
function Node(init) {
  if (typeof init==="string")
    this.type = init;
  else {
    for(var i in init)
      if (init.hasOwnProperty(i)) this[i]=init[i];
  }
}
Node.prototype.toString = function() {
  var props = [];
  for (var p in this)
    if (this.hasOwnProperty(p))
      props.push(p+":"+this[p]);
  return "Node{"+props.join(",")+"}";
  // return "Node{"+this.type+"}";
}

// showing wrap/as in toString is good for reproducing rules,
// but confusing for error messages (where users don't care
// about AST construction)
var toString_AST = false;
function set_toString_AST(flag) { toString_AST = flag; }

// AST building, make a Node of 'type' from parser 'p's Array ast result;
// named ast elements become properties of the resulting Node
function wrap(type,p) {
  var wrapparser = action(p, function(ast) {
       var node = new Node(type);
       if (ast instanceof Array) {
         for (var i=0; i<ast.length; i++)
           if (ast[i] instanceof Named)
             node[ast[i].name] = ast[i].ast;
           else if (ast[i] instanceof Rule)
             throw('Sorry, mixing of parse tree and asts currently not recommended.');
           else
             ; // dropping/losing info here ??
       } else if (ast instanceof Named)
         node[ast.name] = ast.ast;
       else if (ast instanceof Rule)
         throw('Sorry, mixing of parse tree and asts currently not recommended.');
       else
         ; // dropping/losing info here ??
       node.ast = ast; // preserve full parse tree info, for faithful unparsing
                       // TODO: investigate storage efficiency
       return node;
     } );
  wrapparser.toString = function() { return (toString_AST
                                            ? 'wrap("'+type+'",'+p+')'
                                            : p.toString()); }
  return wrapparser;
}

// named ast element
function Named(name,ast) { this.name = name; this.ast = ast; }
Named.prototype.toString = function(){ return "Named("+this.name+","+this.ast+")"; };

// 'as(name,p)' parses as 'p', but produces a named ast result
// 'as(name)' but produces a named null ast result
// (named ast elements end up stored in Node properties)
function as(name,p) {
  var p = typeof p==="undefined" ? epsilon_p : toParser(p);
  var asparser = action(p, function(ast){ return new Named(name,ast||null); } );
  asparser.toString = function() { return (toString_AST
                                          ? 'as("'+name+'",'+p+')'
                                          : p.toString()); };
  return asparser;
}

// 'sequence' is a parser combinator that processes a number of parsers in sequence.
// It can take any number of arguments, each one being a parser. The parser that 'sequence'
// returns succeeds if all the parsers in the sequence succeeds. It fails if any of them fail.
function sequence() {
    var parsers = [];
    for(var i = 0; i < arguments.length; ++i)
        parsers.push(toParser(arguments[i]));
    var pid = parser_id++;
    var sequenceparser = rule("pc.sequence("+parsers+")",function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached) {
            return cached;
        }

        var ast = [];
        var matched = "";
        var i;
        for(i=0; i< parsers.length; ++i) {
            var parser = parsers[i];
            var result = parser(state);
            // log('sequence.'+i+': '+parser+' = '+result);
            if(result) {
              state = result.remaining;
              if(result.ast != undefined) {
                  ast = ast_add(ast,result.ast);
                  matched = matched + (result.matched||"");
              }
            } else {
                break;
            }
        }
        if(i == parsers.length) {
            cached = make_result(state, matched, ast);
        }
        else {
            if (i>0) { // TODO: colours instead of caps?
              var msg = ['partial parse (line '+state.line
                                      +'/ index '+state.index
                                      +'/ depth '+rule_stack.length+')'
                        ,'STACK '+(stack_pattern ? rule_stack.join('\n  ') : '-')
                        ,'MATCHED '+matched
                        ,'CONTEXT '+parsers
                        ,'PARSING '+parser
                        ,'REMAINING '+state.substring(0,30)].join("\n");
              if (trace) {
                log(msg);
              } 
              if (state.index>=state.partials.length-1) {
                var partial = state.partials[state.index];
                if (partial)
                  state.partials[state.index] = 
                    partial.concat([make_partial_result(state, matched, ast, msg)]);
                else {
                  delete state.partials[state.partials.length-1];
                  state.partials[state.index] = 
                    [make_partial_result(state, matched, ast, msg)];
                }
              }
            }
            cached = false;
        }
        savedState.putCached(pid, cached);
        return cached;
    });
    sequenceparser.toString = function () { return "sequence("+(depth++>max_depth?"...":parsers)+")"; };
    return sequenceparser;
}

// Like sequence, but tokens consume whitespace by default
function wsequence() {
    var parsers = [],args = [];
    for(var i=0; i < arguments.length; ++i) {
        args.push(toParser(arguments[i],true));
        parsers.push(toParser(arguments[i],true)); // TODO: whitespace
    }
    var wsequenceparser = sequence.apply(null, parsers);
    wsequenceparser.toString = function () {
      return "wsequence("+(depth++>max_depth?"...":args)+")";
    };
    return wsequenceparser;
}

// 'then(p,f)' is a monadic variant of a two-parser sequence;
// it parses with 'p', followed by 'f' applied to 'p's ast
// NOTE: does not currently store 'p's ast (in general, it should? but
//        that gets in the way for its current main use case leftrec)
function then(p,f) {
  // TODO: caching
  var thenparser = function(input) {
       var cached  = false;
       var ast     = [];
       var matched = "";

       var pr = p(input);

       if (pr) {
         if (pr.ast != undefined) {
           // ast = ast_add(ast,pr.ast);
           matched = matched + (pr.matched||"");
         }

         var fr = f(pr.ast)(pr.remaining);

         if (fr) {
           if (fr.ast != undefined) {
             ast = ast_add(ast,fr.ast);
             matched = matched + (fr.matched||"");
           }
           cached = make_result(fr.remaining, matched, ast);
         }
       }
       return cached;
     };
  thenparser.toString = function(){ return "then("+p+","+f+")"; };
  return thenparser;
}

// 'leftrec' captures a typical left-recursive grammar pattern:
//    rule : base | rule rest1 | rule rest2
// becomes
//    leftrec(base,function(left){ return choice(sequence(left,rest1),
//                                               sequence(left,rest2)); })
// (we're unfolding the grammar rule, dynamically, to the extent it parses,
//  making the result of the previous iteration available via a 'const_p')
function leftrec(base,rec) {
  function leftrec_aux(i,base,rec) {
    return rule("leftrec("+i+")",then(base,function(ast){
                                  // log_array("leftrec: "+ast);
                                  // NOTE: avoid const_p's toString here (it would convert
                                  //       increasingly complex asts to strings, at parse time)
                                  var base = const_p(ast,"leftrec("+i+")");
                                  return choice(leftrec_aux(i+1,rec(base),rec),base);
                                 }));
  }
  return leftrec_aux(0,base,rec);
}

// Named version of 'leftrec'.
// This is usually the one you want, and gives more useful trace and parse error messages.
// NOTE: parse error messages will not include matched input for the recursive part, as
//       const_p doesn't consume input - anything we can do about that?
// TODO: sort out toString representation
function rule_leftrec(name,base,rec) {
  function leftrec_aux(i,base,rec) {
    return rule(name+"("+i+")",then(base,function(ast){
                                  // log_array(name+"("+i+"): "+ast);
                                  // NOTE: avoid const_p's toString here (it would convert
                                  //       increasingly complex asts to strings, at parse time)
                                  var base = const_p(ast,name+"("+i+")");
                                  return choice(leftrec_aux(i+1,rec(base),rec),base);
                                 }));
  }
  return leftrec_aux(0,base,rec);
}

// 'choice' is a parser combinator that provides a choice between other parsers.
// It takes any number of parsers as arguments and returns a parser that will try
// each of the given parsers in order. The first one that succeeds results in a
// successfull parse. It fails if all parsers fail.
// 'choice' ignores null arguments, which is useful for conditional grammar rules.
function choice() {
    var parsers = [];
    for(var i = 0; i < arguments.length; ++i) {
        if (arguments[i]) parsers.push(toParser(arguments[i]));
    }
    var pid = parser_id++;
    var choiceparser = rule("pc.choice("+parsers+")",function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached) {
            return cached;
        }
        var i;
        for(i=0; i< parsers.length; ++i) {
            var parser=parsers[i];
            var result = parser(state.from(0)); // run on copy
            if(result) {
              result = make_result(result.remaining,result.matched,result.ast);
              break;
            }
        }
        if(i == parsers.length)
            cached = false;
        else
            cached = result;
        savedState.putCached(pid, cached);
        return cached;
    });
    choiceparser.toString = function () { return "choice("+(depth++>max_depth?"...":parsers)+")"; };
    return choiceparser;
}

// 'wchoice' is like 'choice', but tokens consume whitespace by default
function wchoice() {
    var parsers = [];
    for(var i = 0; i < arguments.length; ++i)
        parsers.push(toParser(arguments[i],true));
    var wchoiceparser = choice.apply(null,parsers);
    return wchoiceparser;
}

// 'butnot' is a parser combinator that takes two parsers, 'p1' and 'p2'.
// It returns a parser that succeeds if 'p1' matches and 'p2' does not, or
// 'p1' matches and the matched text is longer that p2's.
// Useful for things like: butnot(IdentifierName, ReservedWord)
function butnot(p1,p2) {
    var p1 = toParser(p1);
    var p2 = toParser(p2);
    var pid = parser_id++;

    // match a but not b. if both match and b's matched text is shorter
    // than a's, a failed match is made
    var butnotparser = rule("butnot("+p1+","+p2+")",function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        // TODO: no need to run p2 if p1 fails, right?
        var br = p2(state);
        if(!br) {
            cached = p1(state);
        } else {
            var ar = p1(state);
            if(ar && (ar.matched.length > br.matched.length))
                cached = ar;
            else
                cached = false; // TODO: use of partial result?
        }
        savedState.putCached(pid, cached);
        return cached;
    });
    butnotparser.toString = function () { var p1s="...",p2s="...";
                                    if (depth++<=max_depth) {
                                      p1s = p1.toString();
                                      p2s = p2.toString();
                                    }
                                    return "butnot("+p1s+","+p2s+")"; };
    return butnotparser;
}

// A parser combinator that takes one parser. It returns a parser that
// looks for zero or more matches of the original parser.
function repeat0(p) {
    var p = toParser(p);
    var pid = parser_id++;

    var repeat0parser = rule("pc.repeat0("+p+")",function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached) {
            return cached;
        }

        var ast = [];
        var matched = "";
        var result;
        while(result = p(state.from(0))) {
            ast = ast_add(ast,result.ast);
            matched = matched + result.matched;
            if(result.remaining.index == state.index)
                break;
            state = result.remaining; // TODO: copy or not?
        }
        cached = make_result(state, matched, ast);
        savedState.putCached(pid, cached);
        return cached;
    });
    repeat0parser.toString = function () { return "repeat0("+(depth++>max_depth?"...":p)+")"; };
    return repeat0parser;
}

// A parser combinator that takes one parser. It returns a parser that
// looks for one or more matches of the original parser.
function repeat1(p) {
    var p = toParser(p);
    var pid = parser_id++;

    var repeat1parser = rule("pc.repeat1("+p+")",function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        var ast = [];
        var matched = "";
        var result= p(state);
        if(!result)
            cached = result;
        else {
            while(result) {
                ast = ast_add(ast,result.ast);
                matched = matched + result.matched;
                if(result.remaining.index == state.index)
                    break;
                state = result.remaining;
                result = p(state.from(0));
            }
            cached = make_result(state, matched, ast);
        }
        savedState.putCached(pid, cached);
        return cached;
    });
    repeat1parser.toString = function () { return "repeat1("+(depth++>max_depth?"...":p)+")"; };
    return repeat1parser;
}

// A parser combinator that takes one parser. It returns a parser that
// matches zero or one matches of the original parser.
function optional(p) {
    var p = toParser(p);
    var pid = parser_id++;
    var optionalparser = rule("pc.optional("+p+")",function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;
        var r = p(state.from(0));
        cached = r ? r : make_result(state, "", undefined);
        savedState.putCached(pid, cached);
        return cached;
    });
    optionalparser.toString = function () { return "optional("+(depth++>max_depth?"...":p)+")"; };
    return optionalparser;
}

// A parser combinator that ensures that the given parser succeeds but
// ignores its result. This can be useful for parsing literals that you
// don't want to appear in the ast. eg:
// sequence(expect("("), Number, expect(")")) => ast: Number
function expect(p) {
    return action(p, function(ast) { return undefined; });
}

function chain(p, s, f) {
    var p = toParser(p);

    var chainparser = action(sequence(p, repeat0(action(sequence(s, p), f))),
                  function(ast) { return ast; // ONGOING: ast flattening
                                  return [ast[0]].concat(ast[1]); });
    chainparser.toString = function() {
      var ps="...",ss="...";
      if (depth++<=max_depth) {
        ps = p.toString();
        ss = s.toString();
      }
      return "chain("+ps+","+ss+","+f+")";
    };
    return chainparser;
}

// A parser combinator to do left chaining and evaluation. Like 'chain', it expects a parser
// for an item and for a seperator. The seperator parser's AST result should be a function
// of the form: function(lhs,rhs) { return x; }
// Where 'x' is the result of applying some operation to the lhs and rhs AST's from the item
// parser.
function chainl(p, s) {
    var p = toParser(p);
    var chainlparser = action(sequence(p, repeat0(sequence(s, p))),
                  function(ast) {
                      return foldl(function(v, action) { return action[0](v, action[1]); }, ast[0], ast[1]);
                  });
    chainlparser.toString = function() {
      var ps="...",ss="...";
      if (depth++<=max_depth) {
        ps = p.toString();
        ss = s.toString();
      }
      return "chainl("+ps+","+ss+")";
    };
    return chainlparser;
}

// A parser combinator that returns a parser that matches lists of things. The parser to
// match the list item and the parser to match the seperator need to
// be provided. The AST is the array of matched items. For the moment, separators are
// included in the result.
function list(p, s) {
    var listparser = chain(p, s, function(ast) { return ast /* [1] */; });
    listparser.toString = function () { var ps="...",ss="...";
                                    if (depth++<=max_depth) {
                                      ps = p.toString();
                                      ss = s.toString();
                                    }
                                    return "list("+ps+","+ss+")"; };
    return listparser;
}

// Like list, but tokens consume whitespace by default
function wlist(p, s) { return list(toParser(p,true),toParser(s,true)); }

// A parser that always returns a zero length match, no ast
function epsilon_p(state) {
    return make_result(state, "", undefined);
}
epsilon_p.toString = function() { return "epsilon_p"; }

// A parser that always returns a zero length match, constant ast
function const_p(c,label) {
  var constparser = function(state) {
                  return make_result(state, "", c);
                };
  constparser.toString = function() { return "const_p("+(label ? label : c)+")"; }
  return constparser;
}

// Allows attaching of a function anywhere in the grammar. If the function returns
// true then parse succeeds otherwise it fails. Can be used for testing if a symbol
// is in the symbol table, etc.
function semantic(f) {
    var pid = parser_id++;
    var semanticparser = function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;
        cached = f() ? make_result(state, "", undefined) : false;
        savedState.putCached(pid, cached);
        return cached;
    };
    semanticparser.toString = function () { return "semantic("+f+")"; };
    return semanticparser;
}

// The and predicate asserts that a certain conditional
// syntax is satisfied before evaluating another production. Eg:
// sequence(and("0"), oct_p)
// (if a leading zero, then parse octal)
// It succeeds if 'p' succeeds and fails if 'p' fails. It never
// consume any input however, and doesn't put anything in the resulting
// AST.
function and(p) {
    var p = toParser(p);
    var pid = parser_id++;
    var andparser = rule("pc.and("+p+")",function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;
        var r = p(state);
        cached = r ? make_result(state, "", undefined) : r;
        savedState.putCached(pid, cached);
        return cached;
    });
    andparser.toString = function () { return "and("+(depth++>max_depth?"...":p)+")"; };
    return andparser;
}

// The opposite of 'and'. It fails if 'p' succeeds and succeeds if
// 'p' fails. It never consumes any input. This combined with 'and' can
// be used for 'lookahead' and disambiguation of cases.
//
// Compare:
// sequence("a",choice("+","++"),"b")
//   parses a+b
//   but not a++b because the + matches the first part and peg's don't
//   backtrack to other choice options if they succeed but later things fail.
//
// sequence("a",choice(sequence("+", not("+")),"++"),"b")
//    parses a+b
//    parses a++b
//
function not(p) {
    var p = toParser(p);
    var pid = parser_id++;
    var notparser = rule("pc.not("+p+")",function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;
        var r = p(state);
        cached = r ? false : make_result(state, "", undefined);
          // TODO: use of partial result?
        savedState.putCached(pid, cached);
        return cached;
    });
    notparser.toString = function () { return "not("+(depth++>max_depth?"...":p)+")"; };
    return notparser;
}

// binary operators '[op,..]' with arguments 'e': e op e op e ..
function binops(ops,e) {
  var opsParser = whitespace( ops.length===1 ? token(ops[0]) : choice.apply(null,ops) );
  return sequence(whitespace(e), repeat0(sequence(opsParser, whitespace(e))));
}

// binary operators '[op,..]' with arguments 'e', associating to the left:
// ((e op e) op e) ..
// TODO: bring the nested binops into a single precedence-driven loop
function binops_left_assoc(ops,e,f) {
  var opsParser = whitespace( ops.length===1 ? token(ops[0]) : choice.apply(null,ops) );
  return action(sequence(whitespace(e), repeat0(sequence(opsParser, whitespace(e)))),
                function(ast){
                  var white = ast.whitespace;
                  while (ast.length>=3) {
                    ast = [f(ast[0],ast[1],ast[2])].concat(ast.slice(3));
                  }
                  if (white) log("lost whitespace in binops_left_assoc("+ops+",..)");
                  return ast[0];
                });
}

var rules = {},       // map rule name -> rule representation
    listrules = [],   // list of rule names
    rule_stack = [],  // grammar path to currently active rule
    nesting = '';     // trace prefix for nested grammar rule invocations

// filter pattern for grammar rule stack
var stack_pattern = null; // /^(?!pc)/;
function set_stack_pattern(pat) { pat && (stack_pattern = pat); }

// simple parse tree grammar-rule-based node representation
function Rule(name,ast) {
  this.name = name;
  this.ast  = ast;
}
Rule.prototype.toString = function () { return "{"+this.name+":"+this.ast+"}"; }

// rule attaches a name to a parser combinator, representing a rule 'lhs : rhs'
// used for tracing (which rule is active, and what does it return?),
// debugging (what is the grammar path to the currently active parser?),
// self-representation (avoiding recursion in parser.toString, listing finite 
//                      set of rules instead of an infinite grammar expansion)
// parse trees (avoid default "ast" flattening by prefixing rules with '#')
function rule(name,p) {
  var ruleparser = function(state) {
    rule_stack.push(name.match(stack_pattern) ? name+':'+state.index : '-');
    if (trace && name.match(trace) && (!no_trace || !name.match(no_trace))) {

      var input = state.substring(0,30); // TODO: use a couple of lines instead of 30 chars

      log(nesting+'>'+name+"("+state.line+"/"+state.index+")["
          +input.split(/\r\n|\r|\n/)[0]+"]");

      nesting+='|'; 
        var r = p(state); 
      nesting=nesting.substring(1);

      log(nesting+'<'+name+"("+state.line+"/"+state.index+")"
          +(r ? " = "+r.matched+"\n"+nesting
                +":"+r.ast
              : "[failed]"));

      if (r && r.matched && r.matched.substring(0,30)!==input.substring(0,r.matched.length))
        log('WARNING: possibly bogus parser return '+r.matched.length);

    } else
        var r = p(state); 

    // optionally reflect grammar rule structure in ast (parse tree)
    if (r && name.charAt(0)==='#')
      r = make_result(r.remaining,r.matched,new Rule(name,r.ast));

    rule_stack.pop();

    return r;
  };
  ruleparser.toString = function () {
    if (!rules[name]) {
      rules[name] = true;
      listrules.push(name);
      var ps = (depth++>max_depth?"...":p.toString());
      rules[name] = name+" = "+ps;
    }
    return name;
  };
  return ruleparser;
}

function log_rules(log,rule) {
  rule.toString(); // TODO: don't (ab)use toString?
  for (var r=listrules.length-1; r>=0; r--) log("\n"+rules[listrules[r]]);
}

return {
  ps : ps,
  log_tree : log_tree,
  log_array : log_array,
  log_ast_as_string : log_ast_as_string,
  toParser : toParser,
  token : token,
  ch : ch,
  range : range,
  wtoken : wtoken,
  wch : wch,
  wrange : wrange,
  regex : regex,
  whitespace : whitespace,
  action : action,
  join_action : join_action,
  left_factor_action : left_factor_action,
  negate : negate,
  end_p : end_p,
  nothing_p : nothing_p,
  sequence : sequence,
  wsequence : wsequence,
  then : then,
  choice : choice,
  wchoice : wchoice,
  rule_leftrec : rule_leftrec,
  leftrec : leftrec,
  butnot : butnot,
  repeat0 : repeat0,
  repeat1 : repeat1,
  optional : optional,
  expect : expect,
  chain : chain,
  chainl : chainl,
  list : list,
  wlist : wlist,
  epsilon_p : epsilon_p,
  const_p : const_p,
  semantic : semantic,
  and : and,
  not : not,
  binops : binops,
  binops_left_assoc : binops_left_assoc,
  Named : Named,
  Node : Node,
  wrap : wrap,
  as : as,
  rule : rule,
  log_rules : log_rules,
  set_toString_AST : set_toString_AST,
  set_trace : set_trace,
  set_stack_pattern : set_stack_pattern,
  set_debug : set_debug,
  clear_cache : clear_cache
  };

}());
