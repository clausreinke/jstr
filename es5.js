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
//       - strict mode
//       - do we need to refine the regexp grammar?

var grammar = function(pc){

// import parser combinators
var token = pc.token;
var ch = pc.ch;
var range = pc.range;
var wtoken = pc.wtoken;
var wch = pc.wch;
var wrange = pc.wrange;
var whitespace = pc.whitespace;
var action = pc.action;
var join_action = pc.join_action;
var left_factor_action = pc.left_factor_action;
var negate = pc.negate;
var nothing_p = pc.nothing_p;
var sequence = pc.sequence;
var wsequence = pc.wsequence;
var choice = pc.choice;
var wchoice = pc.wchoice;
var butnot = pc.butnot;
var repeat0 = pc.repeat0;
var repeat1 = pc.repeat1;
var optional = pc.optional;
var list = pc.list;
var wlist = pc.wlist;
var not = pc.not;
var and = pc.and;
var epsilon_p = pc.epsilon_p;
var binops = pc.binops;
var rule = pc.rule;

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
var NLTH = rule("NLTH",whitespace(function(input) { // fail if NL
                        if (input.NL)
                          return nothing_p(input);
                        else
                          return epsilon_p(input); }));
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
    rule("MultiLineComment",sequence("/*",optional(MultiLineCommentChars),"*/"));
  // TODO: extract line terminators from multiline comment

var SingleLineCommentChars = join_action(repeat1(negate(LineTerminator)),"");
var SingleLineComment = 
    rule("SingleLineComment",sequence("//", optional(SingleLineCommentChars)));

var Comment = rule("Comment",join_action(choice(MultiLineComment, SingleLineComment),""));

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
    rule("NumericLiteral",choice(HexIntegerLiteral, DecimalLiteral));
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
    sequence("/",RegularExpressionBody,"/",RegularExpressionFlags));

var Literal = 
    rule("Literal",
    choice(NullLiteral, BooleanLiteral, NumericLiteral, StringLiteral, RegularExpressionLiteral));

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
    rule("Identifier",whitespace(butnot(IdentifierName, ReservedWord)));

var StatementList = 
    rule("StatementList",repeat1(Statement));
var Block = 
    rule("Block",wsequence("{", optional(StatementList), "}"));
var Initialiser = 
    rule("Initialiser",wsequence("=", AssignmentExpression));
var VariableDeclaration = 
    rule("VariableDeclaration",wsequence(Identifier, optional(Initialiser)));
var VariableDeclarationList = 
    rule("VariableDeclarationList",wlist(VariableDeclaration, ","));
var VariableStatement = 
    rule("VariableStatement",wsequence("var", VariableDeclarationList,SEMI));

var EmptyStatement = 
    rule("EmptyStatement",wtoken(";")); // no ASI

var IfStatement = 
    rule("IfStatement",
    choice(wsequence("if", "(", Expression, ")", Statement, "else", Statement),
      wsequence("if", "(", Expression, ")", Statement)));

var IterationStatement =
    rule("IterationStatement",
    choice(wsequence("do", Statement, "while", "(", Expression, ")", SEMI),
      wsequence("while", "(", Expression, ")", Statement),
      wsequence("for", "(", optional(Expression), ";", optional(Expression), ";", optional(Expression), ")", Statement),
      wsequence("for", "(", "var", VariableDeclarationList, ";", optional(Expression), ";", optional(Expression), ")", Statement),
      wsequence("for", "(", LeftHandSideExpression, "in", Expression, ")", Statement),
      wsequence("for", "(", "var", VariableDeclaration, "in", Expression, ")", Statement)));

var ContinueStatement = 
    rule("ContinueStatement",choice(wsequence("continue", SEMI),
                                    wsequence("continue", NLTH, optional(Identifier), SEMI)));
var BreakStatement = 
    rule("BreakStatement",choice(wsequence("break", SEMI),
                                 wsequence("break", NLTH, optional(Identifier), SEMI)));
var ReturnStatement = 
    rule("ReturnStatement",choice(wsequence("return", SEMI),
                                  wsequence("return", NLTH, Expression, SEMI)));
var WithStatement = 
    rule("WithStatement",wsequence("with", "(", Expression, ")", Statement));


var CaseClause =
    rule("CaseClause",wsequence("case", Expression, ":", optional(StatementList)));
var DefaultClause =
    rule("DefaultClause",wsequence("default", ":", optional(StatementList)));
var CaseBlock =
    rule("CaseBlock",
    choice(wsequence("{", repeat0(CaseClause), "}"),
      wsequence("{", repeat0(CaseClause), DefaultClause, repeat0(CaseClause), "}")));

var SwitchStatement = 
    rule("SwitchStatement",wsequence("switch", "(", Expression, ")", CaseBlock));
var LabelledStatement = 
    rule("LabelledStatement",wsequence(Identifier, ":", Statement));
var ThrowStatement = 
    rule("ThrowStatement", wsequence("throw", NLTH, Expression, SEMI));

var Catch = rule("Catch",wsequence("catch", "(", Identifier, ")", Block));
var Finally = rule("Finally",wsequence("finally", Block));
var TryStatement = 
    rule("TryStatement",
    choice(wsequence("try", Block, Catch),
      wsequence("try", Block, Finally),
      wsequence("try", Block, Catch, Finally)));
var DebuggerStatement = rule("DebuggerStatement",wsequence("debugger",SEMI));

var ExpressionStatement = 
    rule("ExpressionStatement",
    whitespace(
    choice(sequence(choice("{", "function"), nothing_p),
           sequence(Expression,SEMI))));
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

var FunctionBody = 
    rule("FunctionBody",repeat0(SourceElement));
var FormalParameterList = 
    rule("FormalParameterList",wlist(Identifier, ","));
var FunctionExpression = 
    rule("FunctionExpression",
    wsequence("function", optional(Identifier), "(", optional(FormalParameterList), ")", 
      choice(wsequence("{", FunctionBody, "}") /* ,
             wsequence("=>", FunctionBody) */ )));

var FunctionDeclaration = 
    rule("FunctionDeclaration",
    wsequence("function", Identifier, "(", optional(FormalParameterList), ")", 
      choice(wsequence("{", FunctionBody, "}") /*,
             wsequence("=>", FunctionBody) */ )));


var PrimaryExpression = 
    function(input) { return PrimaryExpression(input); };

var ArgumentList = rule("ArgumentList",wlist(AssignmentExpression, ","));
var Arguments = 
    rule("Arguments",
    choice(wsequence("(", ")"),
      wsequence("(", ArgumentList, ")") /*,
      wsequence("@", AssignmentExpression) */));

var MemberExpression = function(input) { return MemberExpression(input); };
var MemberExpression =
    rule("MemberExpression",
    left_factor_action(sequence(choice(wsequence("new", MemberExpression, Arguments),
                                       PrimaryExpression,
                                       FunctionExpression),
                                repeat0(choice(wsequence("[", Expression, "]"),
                                               wsequence(".", Identifier))))));

var NewExpression = function(input) { return NewExpression(input); };
var NewExpression = 
    rule("NewExpression",
    choice(MemberExpression,
      wsequence("new", NewExpression)));
var CallExpression = 
    rule("CallExpression",
    left_factor_action(sequence(sequence(MemberExpression, Arguments),
                                repeat0(choice(Arguments,
                                  wsequence("[", Expression, "]"),
                                  wsequence(".", Identifier))))));
  
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

var PostfixExpression =
    rule("PostfixExpression",
    choice(wsequence(LeftHandSideExpression, NLTH, "++"),
      wsequence(LeftHandSideExpression, NLTH, "--"),
      LeftHandSideExpression));

var UnaryExpression =
    rule("UnaryExpression",
    choice(PostfixExpression,
      wsequence("delete", UnaryExpression),
      wsequence("void", UnaryExpression),
      wsequence("typeof", UnaryExpression),
      wsequence("++", UnaryExpression),
      wsequence("--", UnaryExpression),
      wsequence("+", UnaryExpression),
      wsequence("-", UnaryExpression),
      wsequence("~", UnaryExpression),
      wsequence("!", UnaryExpression)));

var MultiplicativeExpression =
    rule("MultiplicativeExpression", binops(["*","/","%"], UnaryExpression));

var AdditiveExpression =
    rule("AdditiveExpression", binops(["+","-"], MultiplicativeExpression));

var ShiftExpression = 
    rule("ShiftExpression", binops(["<<",">>>",">>"], AdditiveExpression));

var RelationalExpression =
    rule("RelationalExpression", binops(["<=",">=","<",">","instanceof"], ShiftExpression));

var EqualityExpression = // TODO: what about the ..NoIn variants?
    rule("EqualityExpression", binops(["===","!==","==","!="], RelationalExpression));

var BitwiseANDExpression = 
    rule("BitwiseANDExpression", binops(["&"], EqualityExpression));
var BitwiseXORExpression = 
    rule("BitwiseXORExpression", binops(["^"], BitwiseANDExpression));
var BitwiseORExpression = 
    rule("BitwiseORExpression", binops(["|"], BitwiseXORExpression));
var LogicalANDExpression = 
    rule("LogicalANDExpression", binops(["&&"], BitwiseORExpression));

var LogicalORExpression = 
    rule("LogicalORExpression", binops(["||"], LogicalANDExpression));

var ConditionalExpression = 
    rule("ConditionalExpression",
    choice(wsequence(LogicalORExpression, "?", AssignmentExpression, ":", AssignmentExpression),
           LogicalORExpression));

var AssignmentExpression = 
    rule("AssignmentExpression",
    choice(sequence(whitespace(LeftHandSideExpression), AssignmentOperator, AssignmentExpression),
      ConditionalExpression));

var Expression = rule("Expression",wlist(AssignmentExpression, ","));

var Elision = rule("Elision",repeat1(wtoken(",")));
var ElementList = 
    rule("ElementList",list(sequence(optional(Elision), AssignmentExpression), wtoken(",")));
var ArrayLiteral = 
    rule("ArrayLiteral",
    choice(wsequence("[", optional(Elision), "]"),
      wsequence("[", ElementList, "]"),
      wsequence("[", ElementList, optional(Elision), "]")));

var PropertyName =
    rule("PropertyName",whitespace(choice(Identifier, StringLiteral, NumericLiteral)));
var PropertyNameAndValueList =
    rule("PropertyNameAndValueList",
    wlist(wsequence(PropertyName, ":", AssignmentExpression), ","));
var ObjectLiteral = 
    rule("ObjectLiteral",
    choice(wsequence("{", "}"),
      wsequence("{", PropertyNameAndValueList, "}")));

var PrimaryExpression = 
    rule("PrimaryExpression",
    whitespace(
    choice(wtoken("this"),
      wsequence("(", Expression, ")"),
      Identifier,
      ArrayLiteral,
      ObjectLiteral,
      Literal)));
var SourceElement = rule("SourceElement",choice(Statement, FunctionDeclaration));
var Program = rule("Program",repeat0(SourceElement));

return {Program : Program };  // TODO: how much else to expose here?
                              //        won't we end up wanting everything?

};
