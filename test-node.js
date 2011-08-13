
var fs = require("fs");

global.log  = function(msg)  { console.log(msg); };
global.load = function(file) { return fs.readFileSync(file,"utf8"); }

var file = process.argv.length>2
         ? process.argv[2]
         : "test_inputs/test_input.js";
// var file = "test_inputs/mini.js";  // use small source excerpts for tracing
var opts = process.argv.length>3
         ? process.argv[3]
         : "p";

var pc      = require("./jsparse.js").pc;
var grammar = require("./es5.js").grammar;
var test    = require("./test.js").test;
var tests   = require("./tests.js").tests;

if (opts.match(/#/))
  tests(test,opts,pc,grammar);
else
  test(file,opts,pc,grammar);

