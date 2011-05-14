var tests = function (test,opts,pc,grammar) {

  var tests = {

    test_token: function () {
      var parser = pc.token("hi");
      var src    = "hihih";
      test("test_token",opts,pc,grammar,{rule:parser,src:src});
    },

    test_sequence: function () {
      var parser = pc.sequence("hi","ho");
      var src    = "hihih";
      test("test_sequence",opts,pc,grammar,{rule:parser,src:src});
    },

    test_repeat0: function () {
      var parser = pc.repeat0("hi");
      var src    = "hihih";
      test("test_repeat0",opts,pc,grammar,{rule:parser,src:src});
    },
    
    test_choice: function () {
      var parser = pc.choice("hi","ho");
      var src    = "hihih";
      test("test_choice",opts,pc,grammar,{rule:parser,src:src});
    },

    test_complex: function () {
      var parser = pc.repeat0(pc.sequence(pc.choice(pc.sequence("hi","ho")
                                                   ,pc.sequence("hi","x")
                                                   ,pc.optional("hu")
                                                   ,""
                                                   )
                                         ,"hi"
                                         ));
      var src    = "hihih";
      test("test_complex",opts,pc,grammar,{rule:parser,src:src});
    }

  };
  tests.test_complex();
  /*
  for (var t in tests)
    if (tests.hasOwnProperty(t))
      tests[t]();
  */

};

