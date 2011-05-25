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

ParseState.prototype.from = function(index,s) {
    var r = new ParseState(this.input, this.index + index, this.line);
    r.cache = this.cache;
    r.partials = this.partials;
    r.length = this.length - index;
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
    if (debug&&cached) log("getCached ("+pid+"/"+this.index+") "+cached.matched);
    return cached;
};

ParseState.prototype.putCached = function(pid, cached) {
    if(!memoize)
        return false;

    var p = this.cache[pid];
    if(!p)
      p = this.cache[pid] = { };
    p[this.index] = cached;
    if (debug) log("putCached ("+pid+"/"+this.index+") "+(cached?cached.matched:"false"));
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
                 msg: msg, depth: depth };
}

var parser_id = 0;

// 'token' is a parser combinator that given a string, returns a parser
// that parses that string value. The AST contains the string that was parsed.
function token(s) {
    var pid = parser_id++;
    var parser = rule("pc.token("+s+")",function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        var r = state.length >= s.length && state.substring(0,s.length) == s;
        if(r) {
            cached = { remaining: state.from(s.length,s), matched: s, ast: s };
        } else
            cached = false;
        savedState.putCached(pid, cached);
        return cached;
    });
    parser.toString = function() { return "token(\""+s+"\")"; };
    return parser;
}

// Like 'token' but for a single character. Returns a parser that given a string
// containing a single character, parses that character value.
function ch(c) {
    var pid = parser_id++;
    var parser = function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;
        var r = state.length >= 1 && state.at(0) == c;
        if(r) {
            cached = { remaining: state.from(1,c), matched: c, ast: c };
        } else
            cached = false;
        savedState.putCached(pid, cached);
        return cached;
    };
    parser.toString = function() { return "ch(\""+c+"\")"; };
    return parser;
}

// 'range' is a parser combinator that returns a single character parser
// (similar to 'ch'). It parses single characters that are in the inclusive
// range of the 'lower' and 'upper' bounds ("a" to "z" for example).
function range(lower, upper) {
    var pid = parser_id++;
    var parser = function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
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
        savedState.putCached(pid, cached);
        return cached;
    };
    parser.toString = function() { return "range("+lower+","+upper+")"; };
    return parser;
}

// Helper function to convert string literals to token parsers
// and perform other implicit parser conversions.
function toParser(p) {
    return (typeof(p) == "string") ? token(p) : p;
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
// TODO: should this be sequence(whitespace.trim,p)?
//       if p fails, leading whitespace does not count for partial parse
//        (and registering a partial parse here would give error msg without context)
function whitespace(p) {
    var p = toParser(p);
    var pid = parser_id++;
    var parser = rule("pc.whitespace("+p+")",function(state) {
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
        if (result)
          cached = make_result(result.remaining,
                               trimmed.matched + (result.matched||""),
                               [trimmed.ast, result.ast]
                              );
        else
          cached = result;
        savedState.putCached(pid, cached);
        return cached;
    });
    parser.toString = function() { return "whitespace("+(depth++>max_depth?"...":p)+")"; };
    return parser;
}
// hook for grammar-specified whitespace, with default trim
whitespace.trim = function(input) { return input.trimLeft(); };

// Parser combinator that passes the AST generated from the parser 'p'
// to the function 'f'. The result of 'f' is used as the AST in the result.
function action(p, f) {
    var p = toParser(p);
    var pid = parser_id++;
    var parser = function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        var x = p(state);
        if(x) {
            x.ast = f(x.ast);
            cached = x;
        }
        else {
            cached = x;
        }
        savedState.putCached(pid, cached);
        return cached;
    };
    parser.toString = function() { return "action("+(depth++>max_depth?"...":p)+","+f+")"; };
    return parser;
}

// Given a parser that produces an array as an ast, returns a
// parser that produces an ast with the array joined by a separator.
function join_action(p, sep) {
    var parser = action(p, function(ast) { return ast.join(sep); });
    parser.toString = function() {
      return "join_action("+(depth++>max_depth?"...":p)+","+sep+")";
    };
    return parser;
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
function left_factor(ast) {
    return foldl(function(v, action) {
                     return [ v, action ];
                 },
                 ast[0],
                 ast[1]);
}

// Return a parser that left factors the ast result of the original
// parser.
function left_factor_action(p) {
    var parser = action(p, left_factor);
    parser.toString = function() {
      return "left_factor_action("+(depth++>max_depth?"...":p)+")";
    };
    return parser;
}

// 'negate' will negate a single character parser. So given 'ch("a")' it will successfully
// parse any character except for 'a'. Or 'negate(range("a", "z"))' will successfully parse
// anything except the lowercase characters a-z.
function negate(p) {
    var p = toParser(p);
    var pid = parser_id++;
    var parser = function(state) {
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
    };
    parser.toString = function () { return "negate("+(depth++>max_depth?"...":p)+")"; };
    return parser;
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

// 'sequence' is a parser combinator that processes a number of parsers in sequence.
// It can take any number of arguments, each one being a parser. The parser that 'sequence'
// returns succeeds if all the parsers in the sequence succeeds. It fails if any of them fail.
function sequence() {
    var parsers = [];
    for(var i = 0; i < arguments.length; ++i)
        parsers.push(toParser(arguments[i]));
    var pid = parser_id++;
    var parser = rule("pc.sequence("+parsers+")",function(state) {
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
                  ast.push(result.ast);
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
            if (i>0) {
              var msg = ['partial parse '+depth+'('+state.line+"/"+state.index+')'
                        ,'matched '+matched
                        ,'context '+parsers
                        ,'parsing '+parser
                        ,'remaining '+state.substring(0,30)].join("\n");
              if (trace) {
                log(msg);
              } 
//              if (state.index>=state.partials.length-1) {
                var partial = state.partials[state.index];
                if (partial)
                  state.partials[state.index] = 
                    partial.concat([make_partial_result(state, matched, ast, msg)]);
                else {
//                  delete state.partials[state.partials.length-1];
                  state.partials[state.index] = 
                    [make_partial_result(state, matched, ast, msg)];
                }
//              }
            }
            cached = false;
        }
        savedState.putCached(pid, cached);
        return cached;
    });
    parser.toString = function () { return "sequence("+(depth++>max_depth?"...":parsers)+")"; };
    return parser;
}

// Like sequence, but ignores whitespace between individual parsers.
function wsequence() {
    var parsers = [],args = [];
    for(var i=0; i < arguments.length; ++i) {
        args.push(toParser(arguments[i]));
        parsers.push(whitespace(toParser(arguments[i])));
    }
    var parser = sequence.apply(null, parsers);
    parser.toString = function () {
      return "wsequence("+(depth++>max_depth?"...":args)+")";
    };
    return parser;
}

// 'choice' is a parser combinator that provides a choice between other parsers.
// It takes any number of parsers as arguments and returns a parser that will try
// each of the given parsers in order. The first one that succeeds results in a
// successfull parse. It fails if all parsers fail.
function choice() {
    var parsers = [];
    for(var i = 0; i < arguments.length; ++i)
        parsers.push(toParser(arguments[i]));
    var pid = parser_id++;
    var parser = rule("pc.choice("+parsers+")",function(state) {
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
    parser.toString = function () { return "choice("+(depth++>max_depth?"...":parsers)+")"; };
    return parser;
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
    var parser = function(state) {
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
    };
    parser.toString = function () { var p1s="...",p2s="...";
                                    if (depth++<=max_depth) {
                                      p1s = p1.toString();
                                      p2s = p2.toString();
                                    }
                                    return "butnot("+p1s+","+p2s+")"; };
    return parser;
}

// 'difference' is a parser combinator that takes two parsers, 'p1' and 'p2'.
// It returns a parser that succeeds if 'p1' matches and 'p2' does not. If
// both match then if p2's matched text is shorter than p1's it is successfull.
// TODO: what does this do?
// function difference(p1,p2) {
//     var p1 = toParser(p1);
//     var p2 = toParser(p2);
//     var pid = parser_id++;
// 
//     // match a but not b. if both match and b's matched text is shorter
//     // than a's, a successfull match is made
//     var parser = function(state) {
//         var savedState = state;
//         var cached = savedState.getCached(pid);
//         if(cached)
//             return cached;
// 
//         var br = p2(state);
//         if(!(br && br.success)) {
//             cached = p1(state);
//         } else {
//             var ar = p1(state);
//             if(ar.matched.length >= br.matched.length)
//                 cached = br;
//             else
//                 cached = ar;
//         }
//         savedState.putCached(pid, cached);
//         return cached;
//     };
//     parser.toString = function () { var p1s="...",p2s="...";
//                                     if (depth++<=max_depth) {
//                                       p1s = p1.toString();
//                                       p2s = p2.toString();
//                                     }
//                                     return "difference("+p1s+","+p2s+")"; };
//     return parser;
// }


// 'xor' is a parser combinator that takes two parsers, 'p1' and 'p2'.
// It returns a parser that succeeds if 'p1' or 'p2' match but fails if
// they both match.
function xor(p1, p2) {
    var p1 = toParser(p1);
    var p2 = toParser(p2);
    var pid = parser_id++;

    // match a or b but not both
    var parser = function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        var ar = p1(state);
        var br = p2(state);
        if(ar && br)
            cached = false; // TODO: use of partial result?
        else
            cached = ar ? ar : br;
        savedState.putCached(pid, cached);
        return cached;
    };
    parser.toString = function () { var p1s="...",p2s="...";
                                    if (depth++<=max_depth) {
                                      p1s = p1.toString();
                                      p2s = p2.toString();
                                    }
                                    return "xor("+p1s+","+p2s+")"; };
    return parser;
}

// A parser combinator that takes one parser. It returns a parser that
// looks for zero or more matches of the original parser.
function repeat0(p) {
    var p = toParser(p);
    var pid = parser_id++;

    var parser = rule("pc.repeat0("+p+")",function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached) {
            return cached;
        }

        var ast = [];
        var matched = "";
        var result;
        while(result = p(state)) {
            ast.push(result.ast);
            matched = matched + result.matched;
            if(result.remaining.index == state.index)
                break;
            state = result.remaining; // TODO: copy or not?
        }
        cached = make_result(state, matched, ast);
        savedState.putCached(pid, cached);
        return cached;
    });
    parser.toString = function () { return "repeat0("+(depth++>max_depth?"...":p)+")"; };
    return parser;
}

// A parser combinator that takes one parser. It returns a parser that
// looks for one or more matches of the original parser.
function repeat1(p) {
    var p = toParser(p);
    var pid = parser_id++;

    var parser = function(state) {
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
                ast.push(result.ast);
                matched = matched + result.matched;
                if(result.remaining.index == state.index)
                    break;
                state = result.remaining;
                result = p(state);
            }
            cached = make_result(state, matched, ast);
        }
        savedState.putCached(pid, cached);
        return cached;
    };
    parser.toString = function () { return "repeat1("+(depth++>max_depth?"...":p)+")"; };
    return parser;
}

// A parser combinator that takes one parser. It returns a parser that
// matches zero or one matches of the original parser.
function optional(p) {
    var p = toParser(p);
    var pid = parser_id++;
    var parser = rule("pc.optional("+p+")",function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;
        var r = p(state);
        cached = r ? r : make_result(state, "", false);
        savedState.putCached(pid, cached);
        return cached;
    });
    parser.toString = function () { return "optional("+(depth++>max_depth?"...":p)+")"; };
    return parser;
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

    var parser = action(sequence(p, repeat0(action(sequence(s, p), f))),
                  function(ast) { return [ast[0]].concat(ast[1]); });
    parser.toString = function() {
      var ps="...",ss="...";
      if (depth++<=max_depth) {
        ps = p.toString();
        ss = s.toString();
      }
      return "chain("+ps+","+ss+","+f+")";
    };
    return parser;
}

// A parser combinator to do left chaining and evaluation. Like 'chain', it expects a parser
// for an item and for a seperator. The seperator parser's AST result should be a function
// of the form: function(lhs,rhs) { return x; }
// Where 'x' is the result of applying some operation to the lhs and rhs AST's from the item
// parser.
function chainl(p, s) {
    var p = toParser(p);
    var parser = action(sequence(p, repeat0(sequence(s, p))),
                  function(ast) {
                      return foldl(function(v, action) { return action[0](v, action[1]); }, ast[0], ast[1]);
                  });
    parser.toString = function() {
      var ps="...",ss="...";
      if (depth++<=max_depth) {
        ps = p.toString();
        ss = s.toString();
      }
      return "chainl("+ps+","+ss+")";
    };
    return parser;
}

// A parser combinator that returns a parser that matches lists of things. The parser to
// match the list item and the parser to match the seperator need to
// be provided. The AST is the array of matched items.
function list(p, s) {
    var parser = chain(p, s, function(ast) { return ast[1]; });
    parser.toString = function () { var ps="...",ss="...";
                                    if (depth++<=max_depth) {
                                      ps = p.toString();
                                      ss = s.toString();
                                    }
                                    return "list("+ps+","+ss+")"; };
    return parser;
}

// Like list, but ignores whitespace between individual parsers.
function wlist() {
    var parsers = [];
    for(var i=0; i < arguments.length; ++i) {
        parsers.push(whitespace(arguments[i]));
    }
    return list.apply(null, parsers);
}

// A parser that always returns a zero length match
function epsilon_p(state) {
    return make_result(state, "", undefined);
}

// Allows attaching of a function anywhere in the grammar. If the function returns
// true then parse succeeds otherwise it fails. Can be used for testing if a symbol
// is in the symbol table, etc.
function semantic(f) {
    var pid = parser_id++;
    var parser = function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;
        cached = f() ? make_result(state, "", undefined) : false;
        savedState.putCached(pid, cached);
        return cached;
    };
    parser.toString = function () { return "semantic("+f+")"; };
    return parser;
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
    var parser = rule("pc.and("+p+")",function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;
        var r = p(state);
        cached = r ? make_result(state, "", undefined) : r;
        savedState.putCached(pid, cached);
        return cached;
    });
    parser.toString = function () { return "and("+(depth++>max_depth?"...":p)+")"; };
    return parser;
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
    var parser = rule("pc.not("+p+")",function(state) {
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
    parser.toString = function () { return "not("+(depth++>max_depth?"...":p)+")"; };
    return parser;
}

var rules = {}, listrules = [], depth = 0, nesting = '';

function rule(name,p) {
  var parser = function(state) {
    depth++;
    if (trace && name.match(trace) && (!no_trace || !name.match(no_trace))) {
      var input = state.substring(0,30);
      log(nesting+'>'+name+"("+state.line+"/"+state.index+")["
          +input.split(/\r\n|\r|\n/)[0]+"]");
      nesting+='|'; 
        var r = p(state); 
      nesting=nesting.substring(1);
      log(nesting+'<'+name+"("+state.line+"/"+state.index+")"
          +(r ? " = "+r.matched
              : "[failed]"));
      if (r && r.matched && r.matched.substring(0,30)!==input.substring(0,r.matched.length))
        log('WARNING: possibly bogus parser return '+r.matched.length);
    } else
        var r = p(state); 
    depth--;
    return r;
  };
  parser.toString = function () {
    if (!rules[name]) {
      rules[name] = true;
      listrules.push(name);
      var ps = (depth++>max_depth?"...":p.toString());
      rules[name] = name+" = "+ps;
    }
    return name;
  };
  return parser;
}

function log_rules(log,rule) {
  rule.toString(); // TODO: don't (ab)use toString?
  for (var r=listrules.length-1; r>=0; r--) log("\n"+rules[listrules[r]]);
}

return {
  ps : ps,
  token : token,
  ch : ch,
  range : range,
  whitespace : whitespace,
  action : action,
  join_action : join_action,
  left_factor_action : left_factor_action,
  negate : negate,
  end_p : end_p,
  nothing_p : nothing_p,
  sequence : sequence,
  wsequence : wsequence,
  choice : choice,
  butnot : butnot,
//  difference : difference,
  xor : xor,
  repeat0 : repeat0,
  repeat1 : repeat1,
  optional : optional,
  expect : expect,
  chain : chain,
  chainl : chainl,
  list : list,
  wlist : wlist,
  epsilon_p : epsilon_p,
  semantic : semantic,
  and : and,
  not : not,
  rule : rule,
  log_rules : log_rules,
  set_trace : set_trace,
  set_debug : set_debug
  };

}());
