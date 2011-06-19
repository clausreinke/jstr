var test = function(file,opts,pc,grammar,testcase){ // TODO: restructure args

    function log_lines(msg) {
      var lines = msg ? msg.split(/\r\n\|\r\|\n/) : [];
      for (var l=0; l<lines.length; l++) log(lines[l]);
    }

    function eta(msg) {
      return msg.replace(/function.input. { return ([^(]*)(\(input\))?; }/g,"$1");
    }

    // default language settings
    var language = testcase && testcase.language
                   ? testcase.language
                   : {tailnests:false // reduce parens/braces for syntactic tail nests
                                      // (function applications and definitions, especially
                                      //  in callback chains)
                     };

    log('\nprocessing '+(testcase && testcase.src ? 'source' : file));
    pc.clear_cache();
    var src = testcase && testcase.src ? testcase.src : load(file);
    var rule = testcase && testcase.rule ? testcase.rule : grammar(language,pc).Program;

    var startTime = (new Date()).getTime();
    if (opts.match(/l/)) pc.log_rules(log,rule);
    if (opts.match(/t/)) pc.set_trace(/.*/);
    if (opts.match(/t-/)) pc.set_trace(/.*/,/^pc\.|white/);
    if (opts.match(/d/)) pc.set_debug(true);
    if (opts.match(/s/)) pc.set_stack_pattern(/^(?!pc)/);

    if (opts.match(/p/)) {
      var parser = pc.sequence(rule,pc.whitespace(pc.end_p));
      var input  = pc.ps(src);
      try {
        var parsed = parser(input);
      } catch(e) {
        log("EXCEPTION "+e);
        var parsed = null;
      }
        // jsparse.js(ff4): 1214l/9.1k
        // jsparse.js(ie9): 1214l/7.7k
        //
        // es5.js(ff4): 793l/5.1k
        // es5.js(ie9): 793l/4.1k
        //
        // fulljslint(ff4): 6558l/53.9k
        // fulljslint(ie9): 6558l/41.6k
        //
        // read-json.js(ff4): 398l/3.6k [needs ASI]
        // read-json.js(ie9): 398l/2.5k [needs ASI]
        //
        // peg-0.6.1.js(ff4): 4794l/29.7k
        // peg-0.6.1.js(ie9): 4794l/27.5k
        //
        // TODO: lines off by one, because we start at 1 and increase for every lineend
        //       way too slow (more so since ASI and error messages); timing unreliable
        //       uses too much memory now (a heap profiler would be nice; about:memory
        //        is a start); can we free some cache early? other memory hogs?
        //       how to use ie9 engine for cscript?
        //        (cscript/ie8 is unusably slow here, is it worth figuring out why?)

      if (parsed) {
        if (opts.match(/m/)) {
          log('------------------------ matched source');
          log_lines(parsed.matched);
        }
        if (opts.match(/r/)) {
          log('------------------------ remaining source');
          log_lines(parsed.remaining.toString());
        }
        if (opts.match(/u/)) {
          log('------------------------ unparse from ast');
          try {
            pc.log_ast_as_string(input.whitespace,parsed.ast);
          } catch(e) { log('EXCEPTION '+e); }
        }
        if (opts.match(/a/)) {
          log('------------------------ ast');
          pc.log_tree('',parsed.ast);
        }
        log('------------------------');
        parsed.remaining && log('pos.line: '+parsed.remaining.line);
      }
      if (!parsed && input.partials && input.partials.length>0) {
        var pi = input.partials.length-1;
        var partials = input.partials[pi];
        for (var partial_i=0; partial_i<partials.length; partial_i++)
          log('\n'+eta(partials[partial_i].msg));
      }

      log((parsed ? 'success' : 'fail')
          +' in '+((new Date()).getTime()-startTime));
    }

  };
