// most of an ECMAScript 5 grammar, using jsparse combinators
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

// TODO: - check for missing details
//       - ASI (in progress)
//       - AST (in progress)
//         we still have some non-AST parse tree elements left over in
//         the AST (such as list separators) and some AST-requirements
//         missing (such as null elements for array elisions)
//       - NoIn variants
//       - get/set
//       - strict mode
//       - do we need to refine the regexp grammar, for early errors?

/*
 suggestion: change syntax highlighting for Vim to distinguish AST/Rule strings

  syn match AST /wrap("[^"]*"/hs=s+5
  syn match AST /as("[^"]*"/hs=s+3
  hi link AST Special
  syn match RULE /rule("[^"]*"/hs=s+5
  syn match RULE /rule_leftrec("[^"]*"/hs=s+13
  hi link RULE Type

 */

exports.grammar = function(LANGUAGE,pc){

// import parser combinators
var toParser = pc.toParser;
var token = pc.token;
var ch = pc.ch;
var range = pc.range;
var wtoken = pc.wtoken;
var wch = pc.wch;
var wrange = pc.wrange;
var regex = pc.regex;
var whitespace = pc.whitespace;
var guard = pc.guard;
var action = pc.action;
var join_action = pc.join_action;
var left_factor_action = pc.left_factor_action;
var negate = pc.negate;
var nothing_p = pc.nothing_p;
var sequence = pc.sequence;
var wsequence = pc.wsequence;
var then = pc.then;
var choice = pc.choice;
var wchoice = pc.wchoice;
var rule_leftrec = pc.rule_leftrec;
var butnot = pc.butnot;
var repeat0 = pc.repeat0;
var repeat1 = pc.repeat1;
var optional = pc.optional;
var list = pc.list;
var wlist = pc.wlist;
var not = pc.not;
var and = pc.and;
var epsilon_p = pc.epsilon_p;
var const_p = pc.const_p;
var binops_left_assoc = pc.binops_left_assoc;
var Named = pc.Named;
var Node = pc.Node;
// var wrap = function(_,p) { return toParser(p); };
// var as = function(_,p) { return p ? toParser(p) : epsilon_p; };
var wrap = pc.wrap;
var as = pc.as;
var rule = pc.rule;
var log_tree = pc.log_tree;
var log_array = pc.log_array;

// conditional grammar rule; use as a choice branch, with a LANGUAGE.x flag
function language(flag,p) { return (flag ? p : null); }

// Forward Declarations
var SourceElement = 
    function(input) { return SourceElement(input); };
var AssignmentExpression = 
    function(input) { return AssignmentExpression(input); };
var Expression = 
    function(input) { return Expression(input); };
var Statement = 
    function(input) { return Statement(input); };
var LeftHandSideExpression = 
    function(input) { return LeftHandSideExpression(input); };

// ASI, in progress
// (remembering NLs avoids making whitespace calls explicit,
//  but explicit whitespace/NL handling would be better,
//  to avoid grammar-specific additions to pc lib?)
// TODO: now that we have more whitespace control, can we drop input.NL?
var NLTH = rule("NLTH",and(whitespace(function(input) { // fail if NL
                            if (input.NL)
                              return nothing_p(input);
                            else
                              return const_p("")(input); })));
var SEMI = rule("SEMI",choice(wtoken(";"), // missing eof, anything else?
                              not(NLTH),
                              and(wtoken("}"))));

var Whitespace = 
    choice(ch("\t"),ch(" "));
var LineTerminator = 
    choice("\r\n",ch("\r"),ch("\n"));
var LineTerminatorSequence = 
    LineTerminator; // TODO: separate these two?

var MultiLineCommentChars = function(input) { return MultiLineCommentChars(input); };
var PostAsteriskCommentChars = function(input) { return PostAsteriskCommentChars(input); };

var MultiLineNotAsteriskChar = choice(LineTerminator,negate("*"));
var MultiLineNotForwardSlashOrAsteriskChar = choice(LineTerminator,negate(choice("/","*")));
var PostAsteriskCommentChars =
    join_action(
    choice(sequence(MultiLineNotForwardSlashOrAsteriskChar,optional(MultiLineCommentChars)),
           sequence(not("*/"),"*",optional(PostAsteriskCommentChars))),"");
var MultiLineCommentChars = 
    join_action(
    choice(sequence(join_action(repeat1(MultiLineNotAsteriskChar),""),
                    optional(MultiLineCommentChars)),
           sequence(not("*/"),"*",optional(PostAsteriskCommentChars))),"");
var MultiLineComment = 
    rule("MultiLineComment",join_action(sequence("/*",optional(MultiLineCommentChars),"*/"),""));

var SingleLineCommentChars = join_action(repeat1(negate(LineTerminator)),"");
var SingleLineComment = 
    rule("SingleLineComment",join_action(sequence("//", optional(SingleLineCommentChars)),""));

var Comment = rule("Comment",choice(MultiLineComment, SingleLineComment));
// TODO: regex shows no visible performance advantage?
// var Comment = rule("Comment",regex(/^(\/\*([^*]|(\*)+[^*/])*\*\/|\/\/[^\r\n]*)/));

// register ES idea of whitespace with combinator library
// TODO: nicer way of handling this hook
whitespace.trim =
  rule("whitespace",join_action(repeat0(choice(Whitespace,LineTerminator,Comment)),""));

var NullLiteral = 
    rule("NullLiteral",token("null"));

var BooleanLiteral = 
    rule("BooleanLiteral",choice("true", "false"));

var Zero = 
    rule("Zero",token("0"));
var DecimalDigit = 
    rule("DecimalDigit",range("0", "9"));
var NonZeroDigit = 
    rule("NonZeroDigit",range("1", "9"));
var DecimalDigits = 
    rule("DecimalDigits",repeat1(DecimalDigit)); 
var DecimalIntegerLiteral = 
    rule("DecimalIntegerLiteral",choice(Zero, sequence(NonZeroDigit, optional(DecimalDigits))));
var SignedInteger = 
    rule("SignedInteger",choice(DecimalDigits, sequence("+", DecimalDigits), sequence("-", DecimalDigits)));
var ExponentIndicator = 
    rule("ExponentIndicator",choice("e", "E"));
var ExponentPart = 
    rule("ExponentPart",sequence(ExponentIndicator, SignedInteger));
var DecimalLiteral = 
    rule("DecimalLiteral",
    choice(sequence(DecimalIntegerLiteral, ".", optional(DecimalDigits), optional(ExponentPart)),
           sequence(".", DecimalDigits, optional(ExponentPart)),
           sequence(DecimalIntegerLiteral, optional(ExponentPart))));

var HexDigit = 
    rule("HexDigit",choice(range("0", "9"), range("a", "f"), range("A", "F")));
var HexIntegerLiteral = 
    rule("HexIntegerLiteral",sequence(choice("0x", "0X"), repeat1(HexDigit)));
var NumericLiteral = 
    rule("NumericLiteral",join_action(choice(HexIntegerLiteral, DecimalLiteral),""));

var SingleEscapeCharacter = 
    rule("SingleEscapeCharacter",choice("'", "\"", "\\", "b", "f", "n", "r", "t", "v"));
var NonEscapeCharacter = 
    rule("NonEscapeCharacter",
    negate(choice(SingleEscapeCharacter,LineTerminator))); // TODO: EscapeCharacter

var CharacterEscapeSequence = 
    rule("CharacterEscapeSequence",
    choice(SingleEscapeCharacter, NonEscapeCharacter));
var HexEscapeSequence = 
    rule("HexEscapeSequence",
    join_action(sequence("x", HexDigit, HexDigit),""));
var UnicodeEscapeSequence = 
    rule("UnicodeEscapeSequence",
    join_action(sequence("u", HexDigit, HexDigit, HexDigit, HexDigit),""));
var EscapeSequence = 
    rule("EscapeSequence",
    choice(HexEscapeSequence, UnicodeEscapeSequence, CharacterEscapeSequence));
var LineContinuation = 
    rule("LineContinuation",
    join_action(sequence("\\",LineTerminatorSequence),""));
var SingleStringCharacter = 
    rule("SingleStringCharacter",
    choice(negate(choice("\'", "\\", "\r", "\n")), // TODO: LineTerminator
           join_action(sequence("\\", EscapeSequence),"")));
var DoubleStringCharacter = 
    rule("DoubleStringCharacter",
    choice(negate(choice("\"", "\\", "\r", "\n")), // TODO: LineTerminator
           join_action(sequence("\\", EscapeSequence),""),
           LineContinuation));
var SingleStringCharacters = 
    rule("SingleStringCharacters",join_action(repeat1(SingleStringCharacter),""));
var DoubleStringCharacters = 
    rule("DoubleStringCharacters",join_action(repeat1(DoubleStringCharacter),""));

var StringLiteral = 
    rule("StringLiteral",
    join_action(choice(sequence("\"", optional(DoubleStringCharacters), "\""),
                       sequence("'", optional(SingleStringCharacters), "'")),""));

var IdentifierPart = function(input) { return IdentifierPart(input); };

var RegularExpressionNonTerminator = 
    rule("RegularExpressionNonTerminator",negate(LineTerminator));
var RegularExpressionBackslashSequence = 
    rule("RegularExpressionBackslashSequence",sequence("\\",RegularExpressionNonTerminator));
var RegularExpressionClassChar = 
    rule("RegularExpressionClassChar",
    choice(butnot(RegularExpressionNonTerminator,choice("]","\\")),
           RegularExpressionBackslashSequence));
var RegularExpressionClassChars = 
    rule("RegularExpressionClassChars",repeat0(RegularExpressionClassChar));
var RegularExpressionClass = 
    rule("RegularExpressionClass",sequence("[",RegularExpressionClassChars,"]"));
var RegularExpressionFirstChar = 
    rule("RegularExpressionFirstChar",
    choice(butnot(RegularExpressionNonTerminator,choice("*","\\","/","[")),
           RegularExpressionBackslashSequence,
           RegularExpressionClass));
var RegularExpressionChar = 
    rule("RegularExpressionChar",
    choice(butnot(RegularExpressionNonTerminator,choice("\\","/","[")),
           RegularExpressionBackslashSequence,
           RegularExpressionClass));
var RegularExpressionChars = 
    rule("RegularExpressionChars",repeat0(RegularExpressionChar));
var RegularExpressionBody = 
    rule("RegularExpressionBody",sequence(RegularExpressionFirstChar,RegularExpressionChars));
var RegularExpressionFlags = 
    rule("RegularExpressionFlags",repeat0(IdentifierPart));
var RegularExpressionLiteral = 
    rule("RegularExpressionLiteral",
    sequence("/",join_action(RegularExpressionBody,""),"/",join_action(RegularExpressionFlags,"")));

// premature evaluation here is wrecking havoc with unparsing, among others;
// at least, delay evaluation until we've secured the raw ast
var Literal =
    rule("Literal",
    action(wrap("Literal", as("value", choice(NullLiteral,
                                              BooleanLiteral,
                                              NumericLiteral,
                                              StringLiteral,
                                              RegularExpressionLiteral))),
           function(lit) {
             switch (lit.value) {
               case "null": lit.value = null; break;
               case "true": lit.value = true; break;
               case "false": lit.value = false; break;
               default: if (lit.value instanceof Array)
                          lit.value = RegExp(lit.value[1],lit.value[3]);
                        else
                           switch (lit.value[0]) {
                             case '"':
                             case "'": break; // StringLiteral
                             default: lit.value = Number(lit.value);
                           };
             };
             return lit;
           }));

var Keyword = 
    rule("Keyword",
    choice("break",
      "case",
      "catch",
      "continue",
      "debugger",
      "default",
      "delete",
      "do",
      "else",
      "finally",
      "for",
      "function",
      "if",
      "in",
      "instanceof",
      "new",
      "return",
      "switch",
      "this",
      "throw",
      "try",
      "typeof",
      "var",
      "void",
      "while",
      "with"));

var FutureReservedWord = 
    rule("FutureReservedWord",
    choice("class","const","enum","export","extends","import","super"));
    // strict mode:
    // "implements", "interface", "let", 
    // "package", "private", "protected", "public", "static", 
    // "yield"
var ReservedWord = 
    rule("ReservedWord",
    choice(Keyword, FutureReservedWord, NullLiteral, BooleanLiteral));

var IdentifierLetter = 
    rule("IdentifierLetter",choice(range("a", "z"), range("A", "Z")));
var IdentifierStart = 
    rule("IdentifierStart",choice(IdentifierLetter, "$", "_"));
var IdentifierPart = 
    rule("IdentifierPart",choice(IdentifierStart, range("0","9")));
var IdentifierName = 
    rule("IdentifierName",
    action(sequence(IdentifierStart, join_action(repeat0(IdentifierPart), "")),
     function(ast) { 
         return ast[0].concat(ast[1]); 
     }));
var Identifier = 
    rule("Identifier",
    whitespace(
    wrap("Identifier",as("name",butnot(IdentifierName, ReservedWord)))));

var StatementList = 
    rule("StatementList",repeat1(Statement));
var Block = 
    rule("Block",
    wrap("BlockStatement",wsequence("{", as("body",optional(StatementList)), "}")));
var Initialiser = 
    rule("Initialiser",wsequence("=", AssignmentExpression));
var VariableDeclaration = 
    rule("VariableDeclaration",
    wrap("",wsequence(as("id",Identifier), as("init",optional(Initialiser)))));
var VariableDeclarationList = 
    rule("VariableDeclarationList",wlist(VariableDeclaration, ","));
var VariableStatement = // TODO: kind: "var"
    rule("VariableStatement",
    wrap("VariableDeclaration",
    wsequence("var", as("declarations",VariableDeclarationList),SEMI)));

var EmptyStatement = 
    rule("EmptyStatement",wrap("EmptyStatement",wtoken(";"))); // no ASI

var IfStatement = 
    rule("IfStatement",
    wrap("IfStatement",
    choice(wsequence("if", "(", as("test",Expression), ")",
                     as("consequent",Statement), "else", as("alternate",Statement)),
           wsequence("if", "(", as("test",Expression), ")",
                     as("consequent",Statement), as("alternate")))));

var WhileStatement =
    wrap("WhileStatement",
    wsequence("while", "(", as("test",Expression), ")", as("body",Statement)));

var DoWhileStatement =
    wrap("DoWhileStatement",
    wsequence("do", as("body",Statement), "while", "(", as("test",Expression), ")", SEMI));

var ForStatement = // TODO: use NoIn variants
    choice(
    wrap("ForStatement",
      wsequence("for", "(", as("init",optional(Expression)), ";",
                            as("test",optional(Expression)), ";",
                            as("update",optional(Expression)), ")",
                            as("body",Statement))),
    wrap("ForStatement",
      wsequence("for", "(", "var", as("init",VariableDeclarationList), ";",
                            as("test",optional(Expression)), ";",
                            as("update",optional(Expression)), ")",
                            as("body",Statement))),
    wrap("ForInStatement",
      wsequence("for", "(", as("left",LeftHandSideExpression),
                            "in", as("right",Expression), ")",
                            as("body",Statement))),
    wrap("ForInStatement",
      wsequence("for", "(", "var", as("left",VariableDeclaration),
                            "in", as("right",Expression), ")",
                            as("body",Statement))))

var IterationStatement =
    rule("IterationStatement",
    choice(DoWhileStatement,
           WhileStatement,
           ForStatement));

// TODO: check the optional/ASI interaction here
var ContinueStatement = 
    rule("ContinueStatement",
    wrap("ContinueStatement",
    choice(wsequence("continue", SEMI, as("label")),
           wsequence("continue", NLTH, as("label",optional(Identifier)), SEMI))));
var BreakStatement = 
    rule("BreakStatement",
    wrap("BreakStatement",
    choice(wsequence("break", SEMI, as("label")),
           wsequence("break", NLTH, as("label",optional(Identifier)), SEMI))));
var ReturnStatement = 
    rule("ReturnStatement",
    wrap("ReturnStatement",
    choice(wsequence("return", SEMI, as("argument")),
           wsequence("return", NLTH, as("argument",Expression), SEMI))));
var WithStatement = 
    rule("WithStatement",
    wrap("WithStatement",
    wsequence("with", "(", as("object",Expression), ")", as("body",Statement))));


// NOTE: repeat0(Statement) instead of optional(StatementList), because of AST
var CaseClause =
    rule("CaseClause",
    wrap("SwitchCase",
    wsequence("case", as("test",Expression), ":", as("consequent",repeat0(Statement)))));
var DefaultClause =
    rule("DefaultClause",
    wrap("SwitchCase",
    wsequence("default", ":", as("consequent",repeat0(Statement)),as("test"))));
var CaseBlock =
    rule("CaseBlock",
    choice(wsequence("{", repeat0(CaseClause), "}"),
      wsequence("{", repeat0(CaseClause), DefaultClause, repeat0(CaseClause), "}")));

var SwitchStatement = 
    rule("SwitchStatement",
    wrap("SwitchStatement",
    wsequence("switch", "(", as("test",Expression), ")", as("cases",CaseBlock))));

var LabelledStatement =
    rule("LabelledStatement",
    wrap("LabeledStatement",wsequence(as("label",Identifier), ":", as("body",Statement))));

var ThrowStatement =
    rule("ThrowStatement",
    wrap("ThrowStatement",
    wsequence("throw", NLTH, as("argument",Expression), SEMI)));

var Catch =
    rule("Catch",
    wrap("CatchClause",
    wsequence("catch", "(", as("param",Identifier), ")", as("body",Block))));
var Finally = rule("Finally",wsequence("finally", Block));
var TryStatement = 
    rule("TryStatement",
    wrap("TryStatement",
    choice(wsequence("try", as("block",Block), as("handler",Catch), as("finalizer",Finally)),
           wsequence("try", as("block",Block), as("finalizer",Finally), as("handler")),
           wsequence("try", as("block",Block), as("handler",Catch), as("finalizer")))));

var DebuggerStatement =
    rule("DebuggerStatement",
    wrap("DebuggerStatement",wsequence("debugger",SEMI)));

var ExpressionStatement = 
    rule("ExpressionStatement",
    wrap("ExpressionStatement",
    whitespace(
    sequence(not("{"),
             not(guard(IdentifierName,
                 function(result){return result.matched==="function"
                                       ? result
                                       : false})),
             as("expression",Expression),SEMI))));

var Statement = 
    rule("Statement",
    choice(Block,
      VariableStatement,
      EmptyStatement,
      LabelledStatement,  // before ExpressionStatement // TODO: avoid fall-through
      ExpressionStatement,  // awful lot of testing before failure here, eg for 'return ..'
      IfStatement,
      IterationStatement,
      ContinueStatement,
      BreakStatement,
      ReturnStatement,
      WithStatement,
      SwitchStatement,
      ThrowStatement,
      TryStatement,
      DebuggerStatement));

var FunctionDeclaration = 
    function(input) { return FunctionDeclaration(input); };

// build a function body ast for the paren-free definitions
function wrap_body(ast) {
  return ["{",
          new Named("body",
                    [new Node({"type":"ReturnStatement",
                               "argument":ast.slice(1),
                               "ast":["return","(",ast.slice(1),")",";"]})]),
          "}"];
}

var FunctionBody = 
    rule("FunctionBody",repeat0(SourceElement));
var FormalParameterList = 
    rule("FormalParameterList",wlist(Identifier, ","));
var FunctionExpression = 
    rule("FunctionExpression",
    wrap("FunctionExpression",
    wsequence("function", as("id",optional(Identifier)),
              "(", as("params",optional(FormalParameterList)), ")",
              wsequence("{", as("body",FunctionBody), "}") )));

var FunctionDeclaration = 
    rule("FunctionDeclaration",
    wrap("FunctionDeclaration",
    wsequence("function", as("id",Identifier),
             "(", as("params",optional(FormalParameterList)), ")",
      choice(wsequence("{", as("body",FunctionBody), "}"),
             // towards paren-free definitions
             // focus on main issue: expression-returning functions
             language(LANGUAGE.tailnests,
             action(wsequence("=>", AssignmentExpression),
                    wrap_body)) ))));


var PrimaryExpression = 
    function(input) { return PrimaryExpression(input); };

// TODO: add reason, suggest fixes (add semi or indent), make warning indent-dependent
function WARN_SEMI(parser) {
  return function(input) {
    var result = parser(input);
    // no error => no error correction => no ASI
    if (result && input.NL) log("WARNING: no semicolon inserted before line "+input.line);
    return result;
  }
}

var ArgumentList = rule("ArgumentList",wlist(AssignmentExpression, ","));
var Arguments = 
    rule("Arguments",
    choice(wsequence("(", ")"),
      wsequence("(", ArgumentList, ")")

      /* towards paren-free application */,
      // some params can be paren-free, right now
      // TODO: - (Expression) is taken for old-style ArgumentList
      //          (suggestion: once spreads, rest, and destructuring are available,
      //                       remove ArgumentList in favour of proper Array arguments,
      //                       freeing (e) for parenthesized function arguments)
      //       - [e] ArrayLiteral overlaps property access in MemberExpression/CallExpression
      //          (suggestion: replace obj[x] with obj.[x] for property access, freeing
      //                       obj [x] for function call with ArrayLiteral)
      language(LANGUAGE.tailnests,
      whitespace(WARN_SEMI( // ASI no longer applies here => provide warning
      action(choice(sequence(not(wchoice("(","[")),PrimaryExpression),
                    FunctionExpression),
             function(ast) {return ["(",ast,")"]; })
      )))

      ));

var MemberExpression = function(input) { return MemberExpression(input); };
var MemberExpression =
    rule_leftrec("MemberExpression",
      choice(wrap("NewExpression",
               wsequence("new", as("constructor",MemberExpression), as("arguments",Arguments))),
             PrimaryExpression,
             FunctionExpression),function(left) {
      return choice(wrap("MemberExpression",
                      wsequence(as("object",left),"[", as("property",Expression), "]",
                                as("computed",const_p(true),true))),
                    wrap("MemberExpression",
                      wsequence(as("object",left),".", as("property",Identifier),
                                as("computed",const_p(false),true)))); });

var NewExpression = function(input) { return NewExpression(input); };
var NewExpression = 
    rule("NewExpression",
    choice(MemberExpression,
           wrap("NewExpression",
             wsequence("new", as("constructor",NewExpression),as("arguments")))));

var CallExpression = 
    rule_leftrec("CallExpression",
      wrap("CallExpression", sequence(as("callee",MemberExpression),as("arguments",Arguments))),
      function(left) {
        return choice(wrap("CallExpression",
                        sequence(as("callee",left),as("arguments",Arguments))),
                      wrap("MemberExpression",
                        wsequence(as("object",left),"[",as("property",Expression),"]",
                                  as("computed",const_p(true),true))),
                      wrap("MemberExpression",
                        wsequence(as("object",left),".",as("property",Identifier),
                                  as("computed",const_p(false),true)))); });

var LeftHandSideExpression =
    rule("LeftHandSideExpression",choice(CallExpression, NewExpression));

var AssignmentOperator = 
    rule("AssignmentOperator",
    whitespace(
    choice("=",
          "*=",
          "/=",
          "%=",
          "+=",
          "-=",
          "<<=",
          ">>=",
          ">>>=",
          "&=",
          "^=",
          "|=")));

var LogicalORExpression = 
    function(input) { return LogicalORExpression(input); };
var LogicalANDExpression = 
    function(input) { return LogicalANDExpression(input); };
var BitwiseORExpression = 
    function(input) { return BitwiseORExpression(input); };
var BitwiseXORExpression = 
    function(input) { return BitwiseXORExpression(input); };
var BitwiseANDExpression = 
    function(input) { return BitwiseANDExpression(input); };
var EqualityExpression = 
    function(input) { return EqualityExpression(input); };
var RelationalExpression = 
    function(input) { return RelationalExpression(input); };
var ShiftExpression = 
    function(input) { return ShiftExpression(input); };
var AdditiveExpression = 
    function(input) { return AdditiveExpression(input); };
var MultiplicativeExpression = 
    function(input) { return MultiplicativeExpression(input); };
var UnaryExpression = 
    function(input) { return UnaryExpression(input); };
var PostfixExpression = 
    function(input) { return PostfixExpression(input); };

var UpdateOperator =
    wrap("UpdateOperator",as("token",wchoice("++","--")));

var PostfixExpression =
    rule("PostfixExpression",
    choice(wrap("UpdateExpression",
            wsequence(as("argument",LeftHandSideExpression),
                      NLTH,
                      as("operator",UpdateOperator),
                      as("prefix",const_p(false),true))),
           LeftHandSideExpression));

var UnaryOperator =
    wrap("UnaryOperator",as("token",wchoice("delete","void","typeof","+","-","~","!")));

var UnaryExpression =
    rule("UnaryExpression",
    choice(PostfixExpression,
     wrap("UpdateExpression",wsequence(as("operator", UpdateOperator),
                                       as("argument",UnaryExpression),
                                       as("prefix",const_p(true),true))),
     wrap("UnaryExpression",wsequence(as("operator",UnaryOperator),
                                      as("argument",UnaryExpression)))
     ));

function wrap_binop(left,op,right) {
  var node = new Node("BinaryExpression");
  node.left     = left;
  node.operator = new Node("BinaryOperator")
  node.operator.token = op;
  node.right    = right;
  node.ast      = [left,op,right];
  return node;
}

var MultiplicativeExpression =
    rule("MultiplicativeExpression",
    binops_left_assoc(["*","/","%"], UnaryExpression,wrap_binop));

var AdditiveExpression =
    rule("AdditiveExpression",
    binops_left_assoc(["+","-"], MultiplicativeExpression,wrap_binop));

var ShiftExpression = 
    rule("ShiftExpression",
    binops_left_assoc(["<<",">>>",">>"], AdditiveExpression,wrap_binop));

var RelationalExpression =
    rule("RelationalExpression",
    binops_left_assoc(["<=",">=","<",">","instanceof"], ShiftExpression,wrap_binop));

var EqualityExpression = // TODO: what about the ..NoIn variants?
    rule("EqualityExpression",
    binops_left_assoc(["===","!==","==","!="], RelationalExpression,wrap_binop));

var BitwiseANDExpression = 
    rule("BitwiseANDExpression",
    binops_left_assoc(["&"], EqualityExpression,wrap_binop));
var BitwiseXORExpression = 
    rule("BitwiseXORExpression",
    binops_left_assoc(["^"], BitwiseANDExpression,wrap_binop));
var BitwiseORExpression = 
    rule("BitwiseORExpression",
    binops_left_assoc(["|"], BitwiseXORExpression,wrap_binop));

// TODO: AST spec wants a LogicalExpression and LogicalOperator here - why?
var LogicalANDExpression =
    rule("LogicalANDExpression",
    binops_left_assoc(["&&"], BitwiseORExpression,wrap_binop));
var LogicalORExpression = 
    rule("LogicalORExpression",
    binops_left_assoc(["||"], LogicalANDExpression,wrap_binop));

var ConditionalExpression = 
    rule("ConditionalExpression",
    choice(wrap("ConditionalExpression",
            wsequence(as("test",LogicalORExpression),
                      "?", as("consequent",AssignmentExpression),
                      ":", as("alternate",AssignmentExpression))),
           LogicalORExpression));

var ExpressionFunctionExpression =
    // towards paren-free definitions
    // focus on main issue: expression-returning functions;
    // open to the right, so we view '=>' as a low priority infix op
    language(LANGUAGE.tailnests,
    rule("FunctionExpression",
    wrap("FunctionExpression",
    wsequence("function", as("id",optional(Identifier)),
              "(", as("params",optional(FormalParameterList)), ")",
              action(wsequence("=>", AssignmentExpression), wrap_body)) )));

var RightAssociativeCallExpression =
    // right-associative application operator, for paren-free params
    // (the mnemonic "application-with-greater-right" is merely
    //  a placeholder for proper syntax; "@" is already taken);
    //  @< is low priority infix op
    language(LANGUAGE.tailnests,
    wrap("CallExpression",
    action(wsequence(as("callee",LeftHandSideExpression),
                     "@<",
                     as("arguments",AssignmentExpression)),
           function(ast){return [ast[0],"(",ast[2],")"];}) ));


var AssignmentExpression = 
    rule("AssignmentExpression",
    choice(wrap("AssignmentExpression",
            sequence(whitespace(as("left",LeftHandSideExpression)),
                     as("operator",AssignmentOperator),
                     as("right",AssignmentExpression))),
           ExpressionFunctionExpression,
           RightAssociativeCallExpression,
           ConditionalExpression));

var Expression = // TODO: don't wrap singleton list
    rule("Expression",
    wrap("SequenceExpression",as("expressions",wlist(AssignmentExpression, ","))));

// TODO: AST spec wants nulls for elided elements
var Elision = rule("Elision",repeat1(wtoken(",")));
var ElementList = 
    rule("ElementList",list(sequence(optional(Elision), AssignmentExpression), wtoken(",")));
var ArrayLiteral = 
    rule("ArrayLiteral",
    wrap("ArrayExpression",
    choice(wsequence("[", as("elements",optional(Elision)), "]"),
      wsequence("[", as("elements",ElementList), "]"),
      wsequence("[", as("elements",sequence(ElementList, optional(Elision))), "]"))));

var PropertyName =
    rule("PropertyName",whitespace(choice(Identifier, StringLiteral, NumericLiteral)));
var PropertyNameAndValueList =
    rule("PropertyNameAndValueList", // TODO: get/set; ast "kind"
    wlist(wsequence(as("key",PropertyName), ":", as("value",AssignmentExpression)), ","));
var ObjectLiteral = 
    rule("ObjectLiteral",
    wrap("ObjectExpression",
    choice(wsequence("{", as("properties"), "}"),
           wsequence("{", as("properties",PropertyNameAndValueList), "}"))));

var PrimaryExpression = 
    rule("PrimaryExpression",
    whitespace(
    choice(wrap("ThisExpression",wtoken("this")),
      wsequence("(", Expression, ")"),
      Identifier,
      ArrayLiteral,
      ObjectLiteral,
      Literal)));
var SourceElement = rule("SourceElement",choice(Statement, FunctionDeclaration));
var Program = rule("Program",wrap("Program",as("elements",repeat0(SourceElement))));

return {Program : Program
       ,Expression : Expression
       };
       // TODO: how much else to expose here?
       //        won't we end up wanting everything?

};
