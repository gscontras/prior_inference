



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
    present : _.shuffle(MUstims),
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

      if (stim.targetind==0) {
        this.speakerTarget = ["speakerTarget","normal","normal"]
      } else if (stim.targetind==1) {
        this.speakerTarget = ["normal","speakerTarget","normal"]
      } else if (stim.targetind==2) {
        this.speakerTarget = ["normal","normal","speakerTarget"]
      }

      if (stim.listenerpick==0) {
        this.listenerTarget = ["listenerTarget","normal","normal"]
      } else if (stim.listenerpick==1) {
        this.listenerTarget = ["normal","listenerTarget","normal"]
      } else if (stim.listenerpick==2) {
        this.listenerTarget = ["normal","normal","listenerTarget"]
      }
      
      var object1 = "<div class=" + this.listenerTarget[0] + "><img src='images/" + stim.item[0] + ".png' width='120' class= " + this.speakerTarget[0] + "></img></div>"
      var object2 = "<div class=" + this.listenerTarget[1] + "><img src='images/" + stim.item[1] + ".png' width='120' class= " + this.speakerTarget[1] + "></img></div>"
      var object3 = "<div class=" + this.listenerTarget[2] + "><img src='images/" + stim.item[2] + ".png' width='120' class= " + this.speakerTarget[2] + "></img></div>"

      $("#object1").html(object1)
      $("#object2").html(object2)
      $("#object3").html(object3)

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