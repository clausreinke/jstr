var test = function(file,opts,pc,grammar,testcase){

    function log_lines(msg) {
      var lines = msg ? msg.split(/\r\n\|\r\|\n/) : [];
      for (var l=0; l<lines.length; l++) log(lines[l]);
    }

    function eta(msg) {
      return msg.replace(/function.input. { return ([^(]*)(\(input\))?; }/g,"$1");
    }

    log('\nprocessing '+file);
    var src = testcase ? testcase.src : load(file);
    var rule = testcase ? testcase.rule : grammar.Program;

    var startTime = (new Date()).getTime();
    if (opts.match(/l/)) pc.log_rules(log,rule);
    if (opts.match(/t/)) pc.set_trace(/.*/); //,/^pc\.|white/);
    if (opts.match(/d/)) pc.set_debug(true);
    if (opts.match(/s/)) pc.set_stack_pattern(/^(?!pc)/);

    if (opts.match(/p/)) {
      var parser = pc.sequence(rule,pc.whitespace(pc.end_p));
      var input  = pc.ps(src);
      var parsed = parser(input); 
        // jsparse.js(cscript): 925l/26k
        // jsparse.js(ff4): 925l/6k
        // jsparse.js(ie9): 925l/2.2k
        //
        // es5.js(cscript): 586l/14.5k
        // es5.js(ff4): 586l/3.4k
        // es5.js(ie9): 586l/1.3k
        //
        // fulljslint(cscript): 6857l/223.5k
        // fulljslint(ff4): 6857l/50k
        // fulljslint(ie9): 6857l/31.5k
        //
        // read-json.js(cscript): 398l/15k [needs ASI]
        // read-json.js(ff4): 398l/3.5k [needs ASI]
        // read-json.js(ie9): 398l/1.3k [needs ASI]
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
      }
      if (!parsed) {
        var pi = input.partials.length-1;
        var partials = input.partials[pi];
        for (var partial_i=0; partial_i<partials.length; partial_i++)
          log('\n'+eta(partials[partial_i].msg));
      }

      log((parsed ? 'success' : 'fail')
          +' in '+((new Date()).getTime()-startTime));
    }

  };
