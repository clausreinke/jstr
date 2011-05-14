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
      var parsed = parser(pc.ps(src)); // input.js: 366l/5k,
                                       // jsparse.js: 867l/13k
                                       // es5.js: 563l/7k,
                                       // fulljslint: 6856l/114k
                                       // TODO: too many lines, way too slow

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
        log('msg: '+parsed.msg);
        parsed.longest && log('longest.msg: '+parsed.longest.msg);
      }

      log((parsed && parsed.success ? 'success' : 'fail')
          +' in '+((new Date()).getTime()-startTime));
    }

  };
