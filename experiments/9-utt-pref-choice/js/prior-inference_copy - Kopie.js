// function onlyTarFeaTrials() {
//   var present = _.shuffle(this.stim);
//   var tarFeaTrials = [];
//   var k = 0;
//   for (var i = 0; i < present.length; i++) {
//     console.error("present stimuli"+present+this.targetFeatureBlock);
//     if (present.targetfeature == this.targetFeatureBlock) {
//       tarFeaTrials[k] == present[i];
//       k++;
//     } else {console.error("no stimuli present"+present+this.targetFeatureBlock);}
//   }
//   return tarFeaTrials;
// };

// function onlyTarFeaTrials() {
//   console.error("onlyTarFeaTrials accessed 1");
//   var present = _.shuffle(stimuli);
//   console.error("onlyTarFeaTrials accessed 2" + present);
//   for (var i = 0; i < present.length; i++) {
//     if (present.targetfeature !== this.targetFeatureBlock) {
//       present.splice(i, 1);
//     }
//   }
//   console.error("onlyTarFeaTrials accessed 5 "+present);
//   return present;
// };

function make_slides(f) {
  var slides = {};

  slides.i0 = slide({
    name: "i0",
    start: function () {
      exp.startT = Date.now();
    }
  });

  slides.instructions1 = slide({
    name: "instructions1",
    start: function () {
      $(".instruction_condition").html("Between subject instruction manipulation: " + exp.instruction);
    },
    button: function () {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  // function onlyTarFeaTrials() {
  //   return _.shuffle(stimuli);
  // };

  


  slides.task = slide({//multi_radio = slide({
    // presentStimuli: [],
    

    name: "task",//"multi_radio",
    
    present: _.shuffle(stimuli),
    // present: this.onlyTarFeaTrials(),
    actualSliderPosition: [0.5, 0.5, 0.5],
    //order: [1, 2, 3],
    generateOrder: function(){
      var order = _.shuffle(this.stim['item']);
      return order;
    },

    present_handle: function (stim, targetFeatureBlock) {
      $('#responseText').hide();
      $(".err").hide();
      this.stim = stim;

      var names_list = names;//var names_list = _.shuffle(names);

      var person1 = names_list[0];
      var person2 = names_list[1];
      $(".person1").html(person1);
      $(".person2").html(person2);

      $(".targetFeature").html(targetFeatureBlock);

      // this.order = _.shuffle(stim['item']);
      var order = this.generateOrder();

      var object1 = "<img src='images/" + order[0] + ".png' width='120'></img>";
      var object2 = "<img src='images/" + order[1] + ".png' width='120'></img>";
      var object3 = "<img src='images/" + order[2] + ".png' width='120'></img>";

      $("#object1").html(object1);
      $("#object2").html(object2);
      $("#object3").html(object3);

      this.possibleUtterances = _.shuffle(stim["featuresPresent"]);

      this.n_radios = this.possibleUtterances.length;
      $(".radio_row").remove();
      for (var i = 0; i < this.n_radios; i++) {
        $("#multi_radio_table").append('<tr class="radio_row"><td class="radio_target" id="object' + i + '">' + "\"" + this.possibleUtterances[i] + "\"" + '</td><td colspan="2"><div id="radio' + i + '" class="slider"><input type="radio" name="utterances"  value=' + i + ' unchecked></div></td></tr>');
        utils.match_row_height("#multi_radio_table", ".radio_target");
      }

      this.init_radios(this.possibleUtterances);

      if (this.stim.targetfeature == "texture"){
        this.preferences = ["solid","striped","polka-dotted"]
      } else if (this.stim.targetfeature == "color"){
        this.preferences = ["red", "blue", "green"]
      } else if (this.stim.targetfeature == "shape"){
        this.preferences = ["cloud","circle","square"]
      } else {
        this.preferences = ["ERROR", "ERROR", "ERROR"]
      }

      this.n_sliders = this.preferences.length;

      $(".slider_row").remove();
      for (var i = 0; i < this.n_sliders; i++) {
        $("#multi_slider_table").append('<tr class="slider_row"><td class="slider_target" id="object' + i + '">' + "\"" + this.preferences[i] + "\"" + '</td><td colspan="2"><div id="slider' + i + '" class="slider">--[ ]-------------</div></td></tr>');
        utils.match_row_height("#multi_slider_table", ".slider_target");
      }

      this.init_sliders(this.preferences);
      exp.sliderPost = [];
    },

    ok_to_response: false,
    ok_to_next_slide: false,

    responseButton: function () {
      if ($('input[name="utterances"]:checked').val()) {
        this.ok_to_response = true
      }
      if (this.ok_to_response) {
        document.getElementById('responseButton').style.visibility = 'hidden';
        // $('#object1').toggleClass('target');
        this.simulateSim();
        $(this.simAnswer).toggleClass('target');
        //console.error("well, that didn't work with the simAnswer..."+this.simAnswer);
        $('#responseText').show();
        $(".err").hide();
      } else {
        $(".err").show();
        document.getElementById('responseButton').style.visibility = 'visible';
      }
    },

    simPreferences: [0.7, 0.3, 0.5],

    findUttCategory: function(){
      // return 1;
      if (["cloud","circle","square"].includes(this.find_chosen_utterance())){
        return 1;
      } else if (["solid","striped","polka-dotted"].includes(this.find_chosen_utterance())){
        return 2;
      } else if (["red", "blue", "green"].includes(this.find_chosen_utterance())){
        return 3;
      } else {
        print("well, that didn't work with the findUttCategory...");
        return 1;
      }
    },

    findUttNum: function(){
      var chosenUtterance = this.find_chosen_utterance();
      if (chosenUtterance == "cloud"||chosenUtterance == "solid"||chosenUtterance == "blue"){
        return 1;
      } else if (chosenUtterance == "circle"||chosenUtterance == "striped"||chosenUtterance == "red"){
        return 2;
      } else if (chosenUtterance == "square"||chosenUtterance == "polka-dotted"||chosenUtterance == "green"){
        return 3;
      } else if (chosenUtterance == "none"){
        console.error("find_chosen_utterance() returns 'none' ");// print("find_chosen_utterance() returns 'none' ");
      } else {
        console.error("well, that didn't work with the findUttNum...");// print("well, that didn't work with the findUttNum...");
      }
    },

    findPossibleChoices: function(uttCategory, uttNum){
      var order = this.generateOrder();
      possibleChoices = [];
      for (var i = 0; i < 3; i++) {
        if (order[i].slice(uttCategory-1, uttCategory) == uttNum){
          possibleChoices.push(order[i]);
        }
      }
      return possibleChoices;
    },

    simulateSimCode: function(){
      return this.findPossibleChoices(this.findUttCategory(), this.findUttNum())[0];
      // for (var i = 0; i < this.findPossibleChoices().length; i++) {
      //   this.findPossibleChoices(this.findUttCategory(), this.findUttNum())[i];
      // }
    },
    simAnswer: ' ',
    
    simulateSim: function(){
      var order = this.generateOrder();
      var simSimCode = this.simulateSimCode();
      if (simSimCode == order[0]){
        this.simAnswer = '#object1';
        //return '#object1';
      } else if (simSimCode == order[1]){
        this.simAnswer = '#object2';
        // return '#object2';
      } else if (simSimCode == order[2]){
        this.simAnswer = '#object3';
        //return '#object3';
      } else {
        console.error("well, that didn't work with the simulateSim..."+simSimCode+order);// print("well, that didn't work with the simulateSim...");
      }
    },

    
    
    continueButton: function () {
      for (var i = 0; i < this.n_sliders; i++) {
        if (exp.sliderPost[i] !== undefined) {
          this.ok_to_next_slide = true;
          this.actualSliderPosition[i] = exp.sliderPost[i];
        }
      }
      if (this.ok_to_response && this.ok_to_next_slide) {
        // $('#object1').toggleClass('target');
        $(this.simAnswer).toggleClass('target');
        document.getElementById('responseButton').style.visibility = 'visible';
        $(".err").hide();
        this.ok_to_next_slide = false;
        this.ok_to_response = false;
        this.log_responses();
        _stream.apply(this);
      } else {
        $(".err").show();
      }
    },

    // toggleTarget: function() {
    //   $('#object1').toggleClass('target');
    // },

    init_radios: function () {
      for (var i = 0; i < this.possibleUtterances.length; i++) {
        utils.make_slider("#slider" + i, this.make_radio_callback(i));
      }
    },

    make_radio_callback: function (i) {
      return function (event, ui) {
        exp.$('input[value= ' + i + ']').is(':checked');
      };
    },

    find_chosen_utterance: function () {
      var chosenUtterance = "none";
      for (var i = 0; i < this.n_radios; i++) {
        if ($('input[name="utterances"]:checked')) {
          chosenUtterance = this.possibleUtterances[$("input:radio[name='utterances']:checked").val()];
          return chosenUtterance;
        } else { return chosenUtterance };
      }
    },

    init_sliders: function () {
      for (var i = 0; i < this.preferences.length; i++) {
        utils.make_slider("#slider" + i, this.make_slider_callback(i), this.actualSliderPosition[i]);
      }
    },

    make_slider_callback: function (i) {
      return function (event, ui) {
        exp.sliderPost[i] = ui.value;
      };
    },

    log_responses: function () {
      exp.data_trials.push({
        "trial_type": "task",//"multi_radio",
        "numFeatures": this.stim["numFeatures"],
        "utterance": this.find_chosen_utterance(),
        "pref1": this.preferences[0],
        "response0": exp.sliderPost[0],
        "pref2": this.preferences[1],
        "response1": exp.sliderPost[1],
        "pref3": this.preferences[2],
        "response2": exp.sliderPost[2],
        "slide_number": exp.phase,
        "item": this.stim.ID,
        "uttCode": this.stim.utterancecode,
        "targetUttCode": this.stim.targeteduttcode,
        "obj1": this.stim.item[0],
        "obj2": this.stim.item[1],
        "obj3": this.stim.item[2],
        "targetFeature": this.stim.targetfeature,
        "targetFeatureNum": this.stim.targetfeaturenum,
        "numFeatures": this.stim.numFeatures,
      });
    },

  });

  slides.subj_info = slide({
    name: "subj_info",
    submit: function (e) {
      //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = {
        language: $("#language").val(),
        enjoyment: $("#enjoyment").val(),
        assess: $('input[name="assess"]:checked').val(),
        age: $("#age").val(),
        gender: $("#gender").val(),
        education: $("#education").val(),
        comments: $("#comments").val(),
      };
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.thanks = slide({
    name: "thanks",
    start: function () {
      exp.data = {
        "trials": exp.data_trials,
        "catch_trials": exp.catch_trials,
        "system": exp.system,
        //"condition" : exp.condition,
        "subject_information": exp.subj_data,
        "time_in_minutes": (Date.now() - exp.startT) / 60000
      };
      setTimeout(function () { turk.submit(exp.data); }, 1000);
    }
  });

  return slides;
}

/// init ///
function init() {
  repeatWorker = false;
  (function () {
    var ut_id = "prior_inference";
    if (UTWorkerLimitReached(ut_id)) {
      $('.slide').empty();
      repeatWorker = true;
      alert("You have already completed the maximum number of HITs allowed by this requester. Please click 'Return HIT' to avoid any impact on your approval rating.");
    }
  })();

  exp.trials = [];
  exp.catch_trials = [];
  exp.instruction = _.sample(["instruction1", "instruction2"]);
  exp.system = {
    Browser: BrowserDetect.browser,
    OS: BrowserDetect.OS,
    screenH: screen.height,
    screenUH: exp.height,
    screenW: screen.width,
    screenUW: exp.width
  };
  //blocks of the experiment:
  exp.structure = ["i0", "instructions1", 'task', 'subj_info', 'thanks'];

  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
  //relies on structure and slides being defined

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function () {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function () { $("#mustaccept").show(); });
      exp.go();
    }
  });

  exp.go(); //show first slide
}