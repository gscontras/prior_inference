



function make_slides(f) {
  var   slides = {};

  slides.i0 = slide({
     name : "i0",
     start: function() {
      exp.startT = Date.now();
     }
  });

  slides.instructions1 = slide({
    name : "instructions1",
    start: function() {
      // $(".instruction_condition").html("Between subject intruction manipulation: "+ exp.instruction);
    }, 
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.training1 = slide({
    name : "training1",
    start: function() {
    }, 
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.training2 = slide({
    name : "training2",
    start: function() {
    }, 
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.training3 = slide({
    name : "training3",
    start: function() {
    }, 
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.multi_slider = slide({
    name : "multi_slider",
    present : _.shuffle(stimuli_listener),
    present_handle : function(stim) {
      $(".err").hide();
      this.stim = stim; 
      this.init_sliders();      
      exp.sliderPost = null;

      var names_list = _.shuffle(names);

      var person1 = names_list[0] ;
      var person2 = names_list[1] ;
      $(".person1").html(person1);
      $(".person2").html(person2);

      // var utteranceType = _.sample(["targetShape","targetTexture","targetColor"])
      // exp.utterance = stim[utteranceType]

      var utteranceType = stim["utteredtype"]
      exp.utterance = stim["utterance"]
      $(".utterance").html(exp.utterance);

      this.order = _.shuffle(["target","obj2","obj3"])

      this.speakerTarget = _.shuffle(["speakerTarget","normal","normal"])
      this.listenerTarget = _.shuffle(["listenerTarget","normal","normal"])

      var object1 = "<div class=" + this.listenerTarget[0] + "><img src='images/" + stim[this.order[0]] + ".png' width='120' class= " + this.speakerTarget[0] + "></img></div>"
      var object2 = "<div class=" + this.listenerTarget[1] + "><img src='images/" + stim[this.order[1]] + ".png' width='120' class= " + this.speakerTarget[1] + "></img></div>"
      var object3 = "<div class=" + this.listenerTarget[2] + "><img src='images/" + stim[this.order[2]] + ".png' width='120' class= " + this.speakerTarget[2] + "></img></div>"

      $("#object1").html(object1)
      $("#object2").html(object2)
      $("#object3").html(object3)

      // var property1 = stim.property1
      // var prop1Labels = []
      // if (property1=="color") {
      //   var prop1Labels = ["blue things", "red things", "green things"]
      // } else if (property1=="texture") {
      //   var prop1Labels = ['solid things','striped things','polka-dotted things']
      // } else {
      //   var prop1Labels = ['clouds','circles','squares']
      // }
      // var property2 = stim.property2
      // var prop2Labels = []
      // if (property2=="color") {
      //   var prop2Labels = ["blue things", "red things", "green things"]
      // } else if (property2=="texture") {
      //   var prop2Labels = ['solid things','striped things','polka-dotted things']
      // } else {
      //   var prop2Labels = ['clouds','circles','squares']
      // }

      // if (utteranceType == "shape") {
      //   var property1 = "texture"
      //   var prop1Labels = ['solid things','striped things','polka-dotted things']
      //   var property2 = "color"
      //   var prop2Labels = ["blue things", "red things", "green things"]
      // } else if (utteranceType == "texture") {
      //   var property1 = "shape"
      //   var prop1Labels = ['clouds','circles','squares']
      //   var property2 = "color"
      //   var prop2Labels = ["blue things", "red things", "green things"]
      // } else if (utteranceType == "color") {
      //   var property1 = "shape"
      //   var prop1Labels = ['clouds','circles','squares']
      //   var property2 = "texture"
      //   var prop2Labels = ['solid things','striped things','polka-dotted things']
      // }

      // this.labels = _.shuffle([prop1Labels,prop2Labels])
      // this.preferences1 = this.labels[0]
      // this.preferences2 = this.labels[1]
      // this.preferences = this.preferences1.concat(this.preferences2)


      // this.n_sliders_1 = this.preferences1.length;
      // $(".slider_row1").remove();
      // for (var i=0; i<this.n_sliders_1; i++) {
      //   $("#multi_slider_table_1").append('<tr class="slider_row1"><td class="slider_target" id="object1' + i + '">' + this.preferences1[i] +  '</td><td colspan="3" bgcolor="#eee" ><div id="slider1' + i + '" class="slider">-------[ ]--------</div></td></tr>');
      //   utils.match_row_height("#multi_slider_table_1", ".slider_target");
      // }

      // this.n_sliders_2 = this.preferences2.length;
      // $(".slider_row2").remove();
      // for (var i=0; i<this.n_sliders_2; i++) {
      //   $("#multi_slider_table_2").append('<tr class="slider_row2"><td class="slider_target" id="object2' + i + '">' + this.preferences2[i] +  '</td><td colspan="3" bgcolor="#eee" ><div id="slider2' + i + '" class="slider">-------[ ]--------</div></td></tr>');
      //   utils.match_row_height("#multi_slider_table_2", ".slider_target");
      // }

      // // this.init_sliders(this.preferences);
      // this.init_sliders_1(this.preferences1);
      // this.init_sliders_2(this.preferences2);
      // exp.sliderPost1 = [];
      // exp.sliderPost2 = [];

      this.n_sliders = 1;

    },

    
    button : function() {
      console.log(exp.sliderPost);
      if (exp.sliderPost != null) {
        this.log_responses();
        _stream.apply(this); //use exp.go() if and only if there is no "present" data.
      } else {
        $(".err").show();
      }
    },

    init_sliders : function() {
      utils.make_slider("#slider0", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },

    // init_sliders_1 : function() {
    //   for (var i=0; i<this.preferences1.length; i++) {
    //      utils.make_slider("#slider1" + i, this.make_slider_callback_1(i));
    //   }
    // },
    // init_sliders_2 : function() {
    //   for (var i=0; i<this.preferences1.length; i++) {
    //      utils.make_slider("#slider2" + i, this.make_slider_callback_2(i));
    //   }
    // },
    // make_slider_callback_1 : function(i) {
    //   return function(event, ui) {
    //     exp.sliderPost1[i] = ui.value;
    //   };
    // },
    // make_slider_callback_2 : function(i) {
    //   return function(event, ui) {
    //     exp.sliderPost2[i] = ui.value;
    //   };
    // },

    log_responses : function() {
      exp.data_trials.push({
        "trial_type" : "prior_inference",
        "utterance" : exp.utterance,
        // "property1" : this.stim.property1,
        // "property2" : this.stim.property2,
        "response": exp.sliderPost,
        "slide_number" : exp.phase,
        "item" : this.stim.ID,
        "itemCode" : this.stim.itemCode,
        "target" : this.stim.target,
        "obj2" : this.stim.obj2,
        "obj3" : this.stim.obj3,
        "ambiguous" : this.stim.listenerAmbiguous
      });
    },
  });

  // slides.multi_slider = slide({
  //   name : "multi_slider",
  //   present : _.sample(stimuli,10),
  //   present_handle : function(stim) {
  //     $(".err").hide();
  //     this.stim = stim; 

  //     var names_list = _.shuffle(names);

  //     var person1 = names_list[0] ;
  //     var person2 = names_list[1] ;

  //     $(".person1").html(person1.name);

  //     $(".person2").html(person2.name);

  //     $(".noun").html(stim.Noun);

  //     this.order = _.shuffle(["target","obj2","obj3"])

  //     var object1 = "<img src='images/" + stim[this.order[0]] + ".png' width='120' ></img>"
  //     var object2 = "<img src='images/" + stim[this.order[1]] + ".png' width='120' ></img>"
  //     var object3 = "<img src='images/" + stim[this.order[2]] + ".png' width='120' ></img>"

  //     this.objects = [object1, object2, object3]

  //     $("#object1").html(object1)
  //     $("#object2").html(object2)
  //     $("#object3").html(object3)

  //     this.n_sliders = this.objects.length;
  //     $(".slider_row").remove();
  //     for (var i=0; i<this.n_sliders; i++) {
  //       $("#multi_slider_table").append('<tr class="slider_row"><td class="slider_target" id="object' + i + '">' + this.objects[i] +  '</td><td colspan="2"><div id="slider' + i + '" class="slider">-------[ ]--------</div></td></tr>');
  //       utils.match_row_height("#multi_slider_table", ".slider_target");
  //     }

  //     this.init_sliders(this.objects);
  //     exp.sliderPost = [];

  //   },

  //   button : function() {
  //     if (exp.sliderPost.length < this.n_sliders) {
  //       $(".err").show();
  //     } else {
  //       this.log_responses();
  //       _stream.apply(this); //use _stream.apply(this); if and only if there is "present" data.
  //     }
  //   },

  //   init_sliders : function() {
  //     for (var i=0; i<this.objects.length; i++) {
  //        utils.make_slider("#slider" + i, this.make_slider_callback(i));
  //     }
  //   },
  //   make_slider_callback : function(i) {
  //     return function(event, ui) {
  //       exp.sliderPost[i] = ui.value;
  //     };
  //   },

  //   log_responses : function() {
  //     // for (var i=0; i<3; i++) {
  //     //   var object = this.order[i];
  //     //   exp.data_trials.push({
  //     //     "trial_type" : "multi_slider",
  //     //     "object" : object,
  //     //     "response" : exp.sliderPost[i],
  //     //     "slide_number" : exp.phase,
  //     //     "item" : this.stim.ID,
  //     //     "condition" : this.stim.condition
  //     //   });
  //     // }
  //     exp.data_trials.push({
  //       "trial_type" : "multi_slider",
  //       "object1" : this.order[0],
  //       "response1" : exp.sliderPost[0],
  //       "object2" : this.order[1],
  //       "response2" : exp.sliderPost[1],
  //       "object3" : this.order[2],
  //       "response3" : exp.sliderPost[2],
  //       "slide_number" : exp.phase,
  //       "item" : this.stim.ID,
  //       "condition" : this.stim.condition
  //     });
  //   },
  // });

  slides.instructions_utterance = slide({
    name : "instructions_utterance",
    start: function() {
      $(".instruction_condition").html("Between subject intruction manipulation: "+ exp.instruction);
    }, 
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.utterance_choice = slide({
    name : "utterance_choice",
    present : _.shuffle(stimuli_speaker),
    present_handle : function(stim) {
      $(".err").hide();
      this.stim = stim; 

      var names_list = _.shuffle(names);

      var person1 = names_list[0] ;
      var person2 = names_list[1] ;
      $(".person1").html(person1);
      $(".person2").html(person2);

      this.order = _.shuffle(["target","obj2","obj3"])

      var object1 = "<img src='images/" + stim[this.order[0]] + ".png' width='120'></img>"
      var object2 = "<img src='images/" + stim[this.order[1]] + ".png' width='120'></img>"
      var object3 = "<img src='images/" + stim[this.order[2]] + ".png' width='120'></img>"

      $("#object1_utt").html(object1)
      $("#object2_utt").html(object2)
      $("#object3_utt").html(object3)

      var numUtterances = stim["numFeatures"]

      this.preferences = _.shuffle(stim["featuresPresent"])


      this.n_sliders = this.preferences.length;
      $(".slider_row").remove();
      for (var i=0; i<this.n_sliders; i++) {
        $("#multi_slider_table").append('<tr class="slider_row"><td class="slider_target" id="object' + i + '">' + "\"" + this.preferences[i] + "\"" + '</td><td colspan="2"><div id="slider' + i + '" class="slider">-------[ ]--------</div></td></tr>');
        utils.match_row_height("#multi_slider_table", ".slider_target");
      }

      this.init_sliders(this.preferences);
      exp.sliderPost = [];

    },

    
    button : function() {
      var ok_to_go_on = true
      for (var i=0; i<this.n_sliders; i++) {
        if (exp.sliderPost[i]==undefined){
          ok_to_go_on = false
        }
      }
      if (ok_to_go_on) {            
           this.log_responses();
           _stream.apply(this);
          } else {
           $(".err").show();
          }
    },

    init_sliders : function() {
      for (var i=0; i<this.preferences.length; i++) {
         utils.make_slider("#slider" + i, this.make_slider_callback(i));
      }
    },
    make_slider_callback : function(i) {
      return function(event, ui) {
        exp.sliderPost[i] = ui.value;
      };
    },

    log_responses : function() {
      exp.data_trials.push({
        "trial_type" : "utterance_choice",
        "numFeatures" : this.stim["numFeatures"],
        "pref1" : this.preferences[0],
        "response1" : exp.sliderPost[0],
        "pref2" : this.preferences[1],
        "response2" : exp.sliderPost[1],
        "pref3" : this.preferences[2],
        "response3" : exp.sliderPost[2],
        "pref4" : this.preferences[3],
        "response4" : exp.sliderPost[3],
        "pref5" : this.preferences[4],
        "response5" : exp.sliderPost[4],
        "pref6" : this.preferences[5],
        "response6" : exp.sliderPost[5],
        "pref7" : this.preferences[6],
        "response7" : exp.sliderPost[6],
        "pref8" : this.preferences[7],
        "response8" : exp.sliderPost[7],
        "pref9" : this.preferences[8],
        "response9" : exp.sliderPost[8],
        "slide_number" : exp.phase,
        "item" : this.stim.ID,
        "condition" : this.stim.itemCode,
        "obj1" : this.stim.target,
        "obj2" : this.stim.obj2,
        "obj3" : this.stim.obj3,
        "ambiguous" : this.stim.speakerAmbiguous
      });
    },
  });

  slides.subj_info =  slide({
    name : "subj_info",
    submit : function(e){
      //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = {
        language : $("#language").val(),
        enjoyment : $("#enjoyment").val(),
        assess : $('input[name="assess"]:checked').val(),
        age : $("#age").val(),
        gender : $("#gender").val(),
        education : $("#education").val(),
        comments : $("#comments").val(),
      };
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.thanks = slide({
    name : "thanks",
    start : function() {
      exp.data= {
          "trials" : exp.data_trials,
          "catch_trials" : exp.catch_trials,
          "system" : exp.system,
          //"condition" : exp.condition,
          "subject_information" : exp.subj_data,
          "time_in_minutes" : (Date.now() - exp.startT)/60000
      };
      setTimeout(function() {turk.submit(exp.data);}, 1000);
    }
  });

  return slides;
}

/// init ///
function init() {
  repeatWorker = false;
  (function(){
    var ut_id = "gscontras-prior-inference-20190115";
    if (UTWorkerLimitReached(ut_id)) {
      $('.slide').empty();
      repeatWorker = true;
      alert("You have already completed the maximum number of HITs allowed by this requester. Please click 'Return HIT' to avoid any impact on your approval rating.");
    }
  })();

  // (function(){
  //   var ut_id = "630dd99cbca25c2b1097304059419a50";
  //   if (UTWorkerLimitReached(ut_id)) {
  //       document.getElementById('mturk_form').style.display = 'none';
  //       document.getElementsByTagName('body')[0].innerHTML = "You have already completed the maximum number of HITs allowed by this requester. Please click 'Return HIT' to avoid any impact on your approval rating.";
  //   }
  // })(); 

  exp.trials = [];
  exp.catch_trials = [];
  // exp.instruction = _.sample(["instruction1","instruction2"]);
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  // exp.structure=["i0", "instructions1","training1","training2","training3",'multi_slider', 'instructions_utterance','utterance_choice','subj_info', 'thanks'];
  exp.structure=["i0", "instructions1","training1","training2","training3",'multi_slider','subj_info', 'thanks'];
  
  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function() {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function() {$("#mustaccept").show();});
      exp.go();
    }
  });

  exp.go(); //show first slide
}