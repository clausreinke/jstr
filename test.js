var test = (function(file,opts,pc,grammar){

    function log_lines(msg) {
      var lines = msg ? msg.split(/\r\n\|\r\|\n/) : [];
      for (var l=0; l<lines.length; l++) log(lines[l]);
    }

    log('processing '+file);
    var src = load(file);
    // log(src);

    var startTime = (new Date()).getTime();
    if (opts.match(/l/)) pc.log_rules(log,grammar.Program);
    if (opts.match(/t/)) pc.set_trace(/.*/,/^pc\.|white/); // ,/white/);
    if (opts.match(/d/)) pc.set_debug(true);

    if (opts.match(/p/)) {
      var parsed = grammar.Program(pc.ps(src)); // input.js: 366l/5k,
                                                // jsparse.js: 867l/13k
                                                // es5.js: 563l/7k,
                                                // fulljslint: 6856l/114k
                                                // TODO: too many lines, way too slow

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
      log('pos.line: '+parsed.remaining.line);

      var trailing = pc.whitespace.trim(parsed.remaining);

      log((trailing.remaining.substring(0)==='' ? 'success' : 'fail')
          +' in '+((new Date()).getTime()-startTime));
    }

  });
