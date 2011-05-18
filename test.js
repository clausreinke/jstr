var test = function(file,opts,pc,grammar,testcase){

    function log_lines(msg) {
      var lines = msg ? msg.split(/\r\n\|\r\|\n/) : [];
      for (var l=0; l<lines.length; l++) log(lines[l]);
    }

    log('\nprocessing '+file);
    var src = testcase ? testcase.src : load(file);
    var rule = testcase ? testcase.rule : grammar.Program;

    var startTime = (new Date()).getTime();
    if (opts.match(/l/)) pc.log_rules(log,rule);
    if (opts.match(/t/)) pc.set_trace(/.*/); //,/^pc\.|white/);
    if (opts.match(/d/)) pc.set_debug(true);

    if (opts.match(/p/)) {
      var parser = pc.sequence(rule,pc.whitespace(pc.end_p));
      var parsed = parser(pc.ps(src)); 
        // jsparse.js: 867l/13k
        // jsparse.js(cscript): 937l/26k (+)
        // jsparse.js(ff4): 937l/2852 (+)
        // jsparse.js(ie9): 937l/2112 (+)
        //
        // es5.js: 563l/7k,
        // es5.js(cscript): 586l/13k, (+)
        // es5.js(ff4): 586l/1630, (+)
        // es5.js(ie9): 586l/1147, (+)
        //
        // fulljslint: 6856l/114k
        // fulljslint(cscript): --/-- OOM! (+)
        // fulljslint(ff4): 6857l/30k (+)
        // fulljslint(ie9): 6857l/27k (+)
        //
        // read-json.js(cscript): 398l/13k (+) [needs ASI]
        // read-json.js(ff4): 398l/3356 (+) [needs ASI]
        // read-json.js(ie9): 398l/1101 (+) [needs ASI]
        //
        // TODO: lines still off (often just by one,
        //         but by 300 for fulljslint.js)
        //       way too slow (more so with ASI
        //         and error messages (+))
        //       uses too much memory now, at least via cscript?
        //        (how to use ie9 engine for cscript?)

      if (parsed) {
        if (opts.match(/m/)) {
          log('------------------------');
          log_lines(parsed.matched);
        }
        if (opts.match(/r/)) {
          log('------------------------');
          log_lines(parsed.remaining.toString());
        }
        if (opts.match(/a/)) {
          log('------------------------');
          log_lines(parsed.ast);
        }
        log('------------------------');
        parsed.remaining && log('pos.line: '+parsed.remaining.line);
        if (!(parsed && parsed.success)) {
          log('msg: '+parsed.msg);
          parsed.longest && log('longest.msg: '+parsed.longest.msg);
        }
      }

      log((parsed && parsed.success ? 'success' : 'fail')
          +' in '+((new Date()).getTime()-startTime));
    }

  };
