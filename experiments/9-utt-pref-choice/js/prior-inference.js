var totalBlock = 0;
var expBlock = totalBlock;



function make_slides(f) {
  var slides = {};

  slides.i0 = slide({
    name: "i0",
    start: function () {
      exp.startT = Date.now();
      $(".progress").hide();
    }
  });

  slides.instructions1 = slide({
    name: "instructions1",
    expBlock: expBlock,
    start: function () {
      $(".progress").hide();
      $(".instruction_condition").html("Between subject instruction manipulation: " + exp.instruction);
      $(".expBlock").html(expBlock);
    },
    button: function () {
      refreshValues();
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  var simPreferences = [0, 0.5, 1];
  var actualSliderPosition = [0.5, 0.5, 0.5];
  var fixedTargetFeature = shuffle(['color', 'shape', 'texture'])[0];
  var sex = shuffle(["female", "male"])[0];

  var names_list = [];
  var personPic = undefined;
  var pronoun = undefined;
  var dirPronoun = undefined;
  if (sex == "female") {
    personPic = "<img class = 'face' src='faces/f" + Math.round(Math.random() * (18 - 1) + 1) + ".jpg' width='160' height = auto></img>";
    names_list = _.shuffle(femaleNames);
    pronoun = "she";
    persPronoun = "her";
    dirPronoun = "her";
  } else if (sex == "male") {
    personPic = "<img class = 'face' src='faces/m" + Math.round(Math.random() * (18 - 1) + 1) + ".jpg' width='160' height = auto></img>";
    names_list = _.shuffle(maleNames);
    pronoun = "he";
    persPronoun = "his";
    dirPronoun = "him";
  };

  if (fixedTargetFeature == "texture") {
    var preferences = ["solid", "striped", "polka-dotted"]
  } else if (fixedTargetFeature == "color") {
    var preferences = ["blue", "red", "green"]
  } else if (fixedTargetFeature == "shape") {
    var preferences = ["cloud", "circle", "square"]
  } else {
    var preferences = ["ERROR", "ERROR", "ERROR"]
  };

  var maxTrialNr = 0;
  var presentColor = _.shuffle(stimuli_color).splice(0, maxTrialNr);
  var presentShape = _.shuffle(stimuli_shape).splice(0, maxTrialNr);
  var presentTexture = _.shuffle(stimuli_texture).splice(0, maxTrialNr);

  // var fixedTargetFeature = shuffle(['color', 'shape', 'texture'])[0];
  // if (this.fixedTargetFeature == "texture") {
  //   this.preferences = ["solid", "striped", "polka-dotted"]
  // } else if (this.fixedTargetFeature == "color") {
  //   this.preferences = ["blue", "red", "green"]
  // } else if (this.fixedTargetFeature == "shape") {
  //   this.preferences = ["cloud", "circle", "square"]
  // } else {
  //   this.preferences = ["ERROR", "ERROR", "ERROR"]
  // };
  // this.simPreferences = shuffle([0, 0.33, 0.66]);
  // this.actualSliderPosition = [0.5, 0.5, 0.5];
  // var preferences = undefined;
  // var simPreferences = undefined;
  // var actualSliderPosition = undefined;
  console.error("fixedTargetFeature, preferences, simPreferences, actualSliderPosition" + fixedTargetFeature, preferences, simPreferences, actualSliderPosition);

  function refreshValues() {
    slides.task.fixedTargetFeature = slides.comparison.fixedTargetFeature = shuffle(['color', 'shape', 'texture'])[0];
    slides.task.preferences = slides.comparison.preferences = [];
    if (slides.task.fixedTargetFeature == "texture") {
      slides.task.preferences = slides.comparison.preferences = ["solid", "striped", "polka-dotted"]
    } else if (slides.task.fixedTargetFeature == "color") {
      slides.task.preferences = slides.comparison.preferences = ["blue", "red", "green"]
    } else if (slides.task.fixedTargetFeature == "shape") {
      slides.task.preferences = slides.comparison.preferences = ["cloud", "circle", "square"]
    } else {
      slides.task.preferences = slides.comparison.preferences = ["ERROR", "ERROR", "ERROR"]
    };
    slides.task.simPreferences = slides.comparison.simPreferences = shuffle([0, 0.5, 1]);
    slides.task.actualSliderPosition = slides.comparison.actualSliderPosition = [0.5, 0.5, 0.5];
    slides.task.presentColor = _.shuffle(stimuli_color).splice(0, maxTrialNr);
    slides.task.presentShape = _.shuffle(stimuli_shape).splice(0, maxTrialNr);
    slides.task.presentTexture = _.shuffle(stimuli_texture).splice(0, maxTrialNr);
    var sex = shuffle(["female", "male"])[0];
    if (sex == "female") {
      slides.task.personPic = slides.comparison.personPic = "<img src='faces/f" + Math.round(Math.random() * (18 - 1) + 1) + ".jpg' width='160'></img>";
      slides.task.names_list = slides.comparison.names_list = _.shuffle(femaleNames);
      slides.task.pronoun = slides.certainty.pronoun = slides.comparison.pronoun = slides.blockPause.pronoun = "she";
      slides.task.persPronoun = slides.certainty.persPronoun = slides.comparison.persPronoun = slides.blockPause.persPronoun = "her";
      slides.task.dirPronoun = slides.certainty.dirPronoun = slides.comparison.dirPronoun = slides.blockPause.dirPronoun = "her";
    } else if (sex == "male") {
      slides.task.personPic = slides.comparison.personPic = "<img src='faces/m" + Math.round(Math.random() * (18 - 1) + 1) + ".jpg' width='160'></img>";
      slides.task.names_list = slides.comparison.names_list = _.shuffle(maleNames);
      slides.task.pronoun = slides.certainty.pronoun = slides.comparison.pronoun = slides.blockPause.pronoun = "he";
      slides.task.persPronoun = slides.certainty.persPronoun = slides.comparison.persPronoun = slides.blockPause.persPronoun = "his";
      slides.task.dirPronoun = slides.certainty.dirPronoun = slides.comparison.dirPronoun = slides.blockPause.dirPronoun = "him";
    };
    slides.certainty.sliderValue = undefined;

    // $(".personPic").html(personPic);

    // var person2 = names_list[0];
    // $(".person2").html(person2);
  };


  // function refreshValues() {
  //   this.fixedTargetFeature = shuffle(['color', 'shape', 'texture'])[0];
  //   this.preferences = [];
  //   if (this.fixedTargetFeature == "texture") {
  //     this.preferences = ["solid", "striped", "polka-dotted"]
  //   } else if (this.fixedTargetFeature == "color") {
  //     this.preferences = ["blue", "red", "green"]
  //   } else if (this.fixedTargetFeature == "shape") {
  //     this.preferences = ["cloud", "circle", "square"]
  //   } else {
  //     this.preferences = ["ERROR", "ERROR", "ERROR"]
  //   };
  //   this.simPreferences = shuffle([0, 0.33, 0.66]);
  //   this.actualSliderPosition = [0.5, 0.5, 0.5];
  //   console.error("in refreshValues: fixedTargetFeature, preferences, simPreferences, actualSliderPosition" + this.fixedTargetFeature, this.preferences, this.simPreferences, this.actualSliderPosition);
  //   return [fixedTargetFeature, preferences, simPreferences, actualSliderPosition];
  // };

  blockNr = 0;
  trialNum = -1;
  // totalBlock = 5; //take the amout of blocks (expBlock) you want + one test block
  // expBlock = this.totalBlock - 1;
  $(".expBlock").html(this.expBlock+1);

  slides.task = slide({//multi_radio = slide({
    // presentStimuli: [],
    blockNr: blockNr,
    name: "task",
    presentColor: presentColor,
    presentShape: presentShape,
    presentTexture: presentTexture,
    actualSliderPosition: actualSliderPosition,
    fixedTargetFeature: fixedTargetFeature,
    preferences: preferences,
    order: [],
    personPic: personPic,
    names_list: names_list,
    pronoun: pronoun,
    persPronoun: persPronoun,
    dirPronoun: dirPronoun,
    sex: sex,
    trialNum: trialNum,

    present_handle: function (stim, fixedTargetFeature, order) {
      $(".progress").show();
      $('.pronoun').html(this.pronoun);
      $('.persPronoun').html(this.persPronoun);
      $('.dirPronoun').html(this.dirPronoun);
      $('#responseText').hide();
      $(".firstErr").hide();
      $(".secondErr").hide();
      this.stim = stim;
      this.order = order;
      console.log(this.trialNum + " trialNum")
      // var names_list = _.shuffle(names);

      // var person1 = names_list[0];
      // var person2 = names_list[1];
      // $(".person1").html(person1);
      // $(".person2").html(person2);
      $(".personPic").html(this.personPic);

      var person2 = this.names_list[0];
      $(".person2").html(person2);

      $(".targetFeature").html(fixedTargetFeature);

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
        // $("#multi_radio_table").append('<p class="radio_target" id = "object' + i + '"> <input type = "radio"  class= "radio_target"  id="radio" name="utterances" value=' + i + ' unchecked > <label for="object' + i + '">' + "\""  + this.possibleUtterances[i] + "\""  + '</label> </p > ');
        // $("#multi_radio_table").append('<tr class="radio_row"><td class="radio_target" id="object' + i + '">' + "\"" + this.possibleUtterances[i] + "\"" + '</td><td colspan="2"><div id="radio' + i + '" class="slider"><input type="radio" name="utterances" id="radio" value=' + i + ' unchecked> <label for="object' + i + '">' + "\""  + this.possibleUtterances[i] + "\""  + '</label> </div></td></tr>');
        $("#multi_radio_table").append('<tr class="radio_row"><td class="radio_target" id="object' + i + '"></td><td colspan="2"><div id="radio' + i + '" class="slider"><input type="radio" name="utterances" id="radio" value=' + i + ' unchecked> <label for="object' + i + '">' + this.possibleUtterances[i] + '</label> </div></td></tr>');
        utils.match_row_height("#multi_radio_table", ".radio_target");
      }

      this.init_radios(this.possibleUtterances);

      this.n_sliders = this.preferences.length;

      $(".slider_row").remove();
      for (var i = 0; i < this.n_sliders; i++) {
        $("#multi_slider_table").append('<tr class="slider_row"><td class="slider_target" id="object' + i + '">' + this.preferences[i] + '</td><td colspan="2"><div id="slider' + i + '" class="slider">--[ ]-------------</div></td></tr>');
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
        // document.getElementById('responseButton').style.visibility = 'hidden';
        document.getElementById('responseButton').style.backgroundColor = 'lightgray';
        document.getElementById('continueButton').style.backgroundColor = '#ffdd42';
        // document.getElementsByClassName('prefButton').style.visibility = '#ffdd42';
        document.getElementById('multi_slider_table').style.pointerEvents = 'visiblePainted';
        document.getElementById('multi_radio_table').style.pointerEvents = 'none';
        this.simulateSim();
        $(this.simAnswer).toggleClass('target');
        $('#responseText').show();
        $(".firstErr").hide();
      } else {
        $(".firstErr").show();
        document.getElementById('responseButton').style.visibility = 'visible';
      }
    },

    simPreferences: simPreferences,

    findUttCategory: function () {
      if (["cloud", "circle", "square"].includes(this.find_chosen_utterance())) {
        return 1;
      } else if (["solid", "striped", "polka-dotted"].includes(this.find_chosen_utterance())) {
        return 2;
      } else if (["red", "blue", "green"].includes(this.find_chosen_utterance())) {
        return 3;
      } else {
        print("well, that didn't work with the findUttCategory...");
        return 1;
      }
    },

    findUttNum: function () {
      var chosenUtterance = this.find_chosen_utterance();
      if (chosenUtterance == "cloud" || chosenUtterance == "solid" || chosenUtterance == "blue") {
        return 1;
      } else if (chosenUtterance == "circle" || chosenUtterance == "striped" || chosenUtterance == "red") {
        return 2;
      } else if (chosenUtterance == "square" || chosenUtterance == "polka-dotted" || chosenUtterance == "green") {
        return 3;
      } else if (chosenUtterance == "none") {
        console.error("find_chosen_utterance() returns 'none' ");// print("find_chosen_utterance() returns 'none' ");
      } else {
        console.error("well, that didn't work with the findUttNum...");// print("well, that didn't work with the findUttNum...");
      }
    },

    findPossibleChoices: function (uttCategory, uttNum) {
      var order = this.order;
      possibleChoices = [];
      for (var i = 0; i < 3; i++) {
        if (order[i].slice(uttCategory - 1, uttCategory) == uttNum) {
          possibleChoices.push(order[i]);
        }
      }
      return possibleChoices;
    },

    simulateSimCode: function () {
      var possibleChoices = this.findPossibleChoices(this.findUttCategory(), this.findUttNum());
      var notPossibleChoices = this.order.filter(x => !possibleChoices.includes(x));
      var toGetTransparent = undefined;
      for (var i = 0; i < notPossibleChoices.length; i++) {
        if (notPossibleChoices[i] == this.order[0]) {
          toGetTransparent = 'object1';
        } else if (notPossibleChoices[i] == this.order[1]) {
          toGetTransparent = 'object2';
        } else if (notPossibleChoices[i] == this.order[2]) {
          toGetTransparent = 'object3';
        };
        document.getElementById(toGetTransparent).style.opacity = 0.5;
        // $(toGetTransparent).toggleClass('notPossibleChoices');
      };
      var targetFeatureNum = this.stim.targetfeaturenum;
      var simPreferences = this.simPreferences;
      var possibleChoicesNum = [];
      var individTarFeaNum = 0; // ranges from 1-3 like all numbers whih refer to featurevalues
      for (var i = 0; i < possibleChoices.length; i++) {
        individTarFeaNum = possibleChoices[i].slice(targetFeatureNum - 1, targetFeatureNum);
        if (!possibleChoicesNum.includes(individTarFeaNum)) {
          possibleChoicesNum.push(individTarFeaNum);
        };
      };

      var randNumMax = 0;
      for (var i = 0; i < possibleChoicesNum.length; i++) {
        randNumMax += Number(simPreferences[possibleChoicesNum[i] - 1]);
      };

      // var randNum = Math.random() * randNumMax;
      var answerInd = -1; // ranges from 1-3 like all numbers whih refer to featurevalues
      var incrementPreferences = 0;
      var answerPossInd = -1;
      console.log(simPreferences + " simPreferences")
      var sortedSimPreferences = simPreferences.slice();
      sortedSimPreferences.sort(function(a, b){return b-a});
      console.log(sortedSimPreferences + " sortedSimPreferences" + simPreferences + " simPreferences");
      possSimPreferences = [];
      for (var i = 0; i < possibleChoicesNum.length; i++) {
        possSimPreferences[i] = simPreferences[possibleChoicesNum[i]-1]
      };
      console.log(possSimPreferences);
      possSimPreferences.sort(function(a, b){return b-a});
      // for (var i = 0; i < simPreferences.length; i++) {
        console.log(possibleChoicesNum + " possibleChoicesNum");
        // if (possibleChoicesNum.includes(String(simPreferences.indexOf(sortedSimPreferences[i])+1))){
          if (possSimPreferences.indexOf(1) != -1){
            answerPossInd = possSimPreferences.indexOf(1);
          } else if (possSimPreferences.indexOf(0.5) != -1){
            answerPossInd = possSimPreferences.indexOf(0.5);
          } else if (possSimPreferences.indexOf(0) != -1){
            answerPossInd = possSimPreferences.indexOf(0);
          } 
          // if (sortedSimPreferences[i] == 1){
          //   answerSortedInd = i;
          //   console.log(incrementPreferences + " incrementPreferences " + answerSortedInd + " answerSortedInd")
          //   break;
          // } else if (sortedSimPreferences[i] == 0.5){
          //   answerSortedInd = i;
          //   console.log(incrementPreferences + " incrementPreferences " + answerSortedInd + " answerSortedInd")
          // } else if (sortedSimPreferences[i] == 0){
          //   answerSortedInd = i;
          //   console.log(incrementPreferences + " incrementPreferences " + answerSortedInd + " answerSortedInd")
          // }
          // incrementPreferences += sortedSimPreferences[i];
          // if (randNum <= incrementPreferences) {
          //   answerSortedInd = i;
          //   console.log(randNum + " randNum " + incrementPreferences + " incrementPreferences " + answerSortedInd + " answerSortedInd")
          //   break;
          // };
        // }
      // }
      console.log(answerPossInd + " answerPossInd")
      answerInd = simPreferences.indexOf(possSimPreferences[answerPossInd])+1
      console.log(answerInd + " answerInd");
      // for (var i = 0; i < simPreferences.length; i++) {
      //   indexNumberFeature = i + 1;
      //   if (possibleChoicesNum.includes(String(indexNumberFeature))) {
      //     incrementPreferences += simPreferences[i];
      //     if (randNum <= incrementPreferences) {
      //       answerInd = indexNumberFeature;
      //       break;
      //     };
      //   }
      // }
      if (answerInd == -1) console.error('AnswerInd not found (-1)');

      for (var i = 0; i < possibleChoices.length; i++) {
        individTarFeaNum = possibleChoices[i].slice(targetFeatureNum - 1, targetFeatureNum);
        if (answerInd == individTarFeaNum) {
          return possibleChoices[i];
        } else console.error('possibleChoice not found' + possibleChoices);
      };
    },

    simAnswer: ' ',

    simSimCode: ' ',
    simulateSim: function () {
      var order = this.order;
      this.simSimCode = this.simulateSimCode();
      // for (var i = 0; i < order.length; i++) {
      //   console.error("order[" + i + "]" + order[i]);
      // };
      if (this.simSimCode == order[0]) {
        this.simAnswer = '#object1';
      } else if (this.simSimCode == order[1]) {
        this.simAnswer = '#object2';
      } else if (this.simSimCode == order[2]) {
        this.simAnswer = '#object3';
      } else {
        console.error("well, that didn't work with the simulateSim..." + this.simSimCode + order);// print("well, that didn't work with the simulateSim...");
      }
    },

    allObjVisible: function () {
      var allObjects = ["object1", "object2", "object3"];
      for (var i = 0; i < allObjects.length; i++) {
        // $(allObjects[i]).toggleClass('allObjects');
        document.getElementById(allObjects[i]).style.opacity = 1;
      }
    },

    continueButton: function () {
      this.ok_to_next_slide = true;
      for (var i = 0; i < this.n_sliders; i++) {
        if (exp.sliderPost[i] == undefined) {
          this.ok_to_next_slide = false;
          this.actualSliderPosition[i] = exp.sliderPost[i];
        } else {
          this.actualSliderPosition[i] = exp.sliderPost[i];
        }
      }
      if (this.ok_to_response && this.ok_to_next_slide) {
        this.trialNum++;
        slides.blockPause.trialNum = this.trialNum;
        console.log(this.trialNum + " trialNum");
        this.allObjVisible();
        document.getElementById('multi_slider_table').style.pointerEvents = 'none';
        document.getElementById('multi_radio_table').style.pointerEvents = 'auto';
        $(this.simAnswer).toggleClass('target');
        // document.getElementById('responseButton').style.visibility = 'visible';
        document.getElementById('continueButton').style.backgroundColor = 'lightgray';
        document.getElementById('responseButton').style.backgroundColor = '#ffdd42';
        $(".secondErr").hide();
        this.ok_to_next_slide = false;
        this.ok_to_response = false;
        this.norm();
        this.log_responses();
        slides.comparison.fixedTargetFeature = this.fixedTargetFeature;
        slides.comparison.actualSliderPosition = this.actualSliderPosition;
        slides.comparison.preferences = this.preferences;
        slides.comparison.simPreferences = this.simPreferences;
        _stream.apply(this);
      } else {
        $(".secondErr").show();
      }
    },

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
        // console.error(ui.value + "ui.value");
      };
    },

    normResponse: [],
    norm: function () {
      sumResponse = 0;
      for (var i = 0; i < this.actualSliderPosition.length; i++) {
        sumResponse += this.actualSliderPosition[i];
      };
      for (var i = 0; i < this.actualSliderPosition.length; i++) {
        this.normResponse[i] = this.actualSliderPosition[i] / sumResponse;
      };
      console.log("this.normResponse " + this.normResponse)
    },

    log_responses: function () {
      exp.data_trials.push({
        "blockNr": this.blockNr,
        "trialNum": this.trialNum,
        "trial_type": "task",//"multi_radio",
        "numFeatures": this.stim["numFeatures"],
        "featuresPresent": this.stim["featuresPresent"],
        "utterance": this.find_chosen_utterance(),
        "pref0": this.preferences[0],
        // "response0": exp.sliderPost[0],
        "response0": this.actualSliderPosition[0],
        "normResponse0": this.normResponse[0],
        "pref1": this.preferences[1],
        // "response1": exp.sliderPost[1],
        "response1": this.actualSliderPosition[1],
        "normResponse1": this.normResponse[1],
        "pref2": this.preferences[2],
        // "response2": exp.sliderPost[2],
        "response2": this.actualSliderPosition[2],
        "normResponse2": this.normResponse[2],
        "slide_number": exp.phase,
        "item": this.stim.ID,
        "uttCode": this.stim.utterancecode,
        "targetUttCode": this.stim.targeteduttcode,
        "obj1": this.stim.item[0],
        "obj2": this.stim.item[1],
        "obj3": this.stim.item[2],
        "order0": this.order[0],
        "order1": this.order[1],
        "order2": this.order[2],
        "simPreference0": this.simPreferences[0],
        "simPreference1": this.simPreferences[1],
        "simPreference2": this.simPreferences[2],
        "simulatedAnswer": this.simSimCode,
        "targetFeature": this.stim.targetfeature,
        "targetFeatureNum": this.stim.targetfeaturenum,
        "numFeatures": this.stim.numFeatures,
        "sex": this.sex,
        "name": this.name,
      });
      console.error("targetUttCode " + this.stim.targeteduttcode);
      console.error("uttCode" + this.stim.utterancecode);
    },

  });


  slides.certainty = slide({
    name: "certainty",
    sliderValue: undefined,
    pronoun: pronoun,
    persPronoun: persPronoun,
    dirPronoun: dirPronoun,

    make_slider_callback: function (i) {
      return function (event, ui) {
        slides.certainty.sliderValue = ui.value;
      };
    },
    log_certainty: function () {
      exp.data_trials.push({
        "certainty": slides.certainty.sliderValue,
      });
    },
    start: function () {
      $('.pronoun').html(this.pronoun);
      $('.persPronoun').html(this.persPronoun);
      $('.dirPronoun').html(this.dirPronoun);
      utils.make_slider("#certaintySlider", this.make_slider_callback(0), 0.5);
      _stream.apply(this);
      this.sliderValue = undefined;
      $(".err").hide();
    },
    button: function () {
      if (this.sliderValue == undefined) {
        $(".err").show();
      } else {
        this.log_certainty();
        exp.go();
        $(".err").hide();
        this.sliderValue = undefined;
        okToGo = undefined;
      }
    }
  });

  slides.comparison = slide({
    name: "comparison",
    blockNr: blockNr,
    actualSliderPosition: undefined,
    fixedTargetFeature: fixedTargetFeature,
    preferences: undefined,
    simPreferences: undefined,
    personPic: personPic,
    names_list: names_list,
    pronoun: pronoun,
    persPronoun: persPronoun,
    dirPronoun: dirPronoun,

    start: function () {
      $('.pronoun').html(this.pronoun);
      $('.persPronoun').html(this.persPronoun);
      $('.dirPronoun').html(this.dirPronoun);
      $('.fixedTargetFeature').html(this.fixedTargetFeature);
      var preferences = this.preferences;
      var actualSliderPosition = this.actualSliderPosition;
      var simPreferences = this.simPreferences;
      $(".blockNr").html(this.blockNr + 2);
      console.error(simPreferences + " simPreferences")

      var maxGuessIndex = actualSliderPosition.indexOf(Math.max.apply(Math, actualSliderPosition))
      var favGuess = preferences[maxGuessIndex];
      console.log(favGuess + " favGuess " + maxGuessIndex + " maxGuessIndex")
      $(".favGuess").html(favGuess);
      var minGuessIndex = actualSliderPosition.indexOf(Math.min.apply(Math, actualSliderPosition))
      var disGuess = preferences[minGuessIndex];
      console.log(disGuess + " disGuess " + minGuessIndex + " minGuessIndex")
      $(".disGuess").html(disGuess);
      firstNLastGuess = [disGuess, favGuess]
      var midGuess = preferences.filter(e => !firstNLastGuess.includes(e))
      console.log(midGuess + " midGuess ")
      $(".midGuess").html(midGuess);

      var maxSimIndex = simPreferences.indexOf(Math.max.apply(Math, simPreferences))
      var simFav = preferences[maxSimIndex];
      console.log(simFav + " simFav " + maxSimIndex + " maxSimIndex " + preferences + " preferences")
      $(".simFav").html(simFav);
      var minSimIndex = simPreferences.indexOf(Math.min.apply(Math, simPreferences))
      var simDis = preferences[minSimIndex];
      console.log(simDis + " simDis " + minSimIndex + " minSimIndex " + preferences + " preferences")
      $(".simDis").html(simDis);
      firstNLastSim = [simDis, simFav]
      var simMid = preferences.filter(e => !firstNLastSim.includes(e))
      console.log(simMid + " simMid " + preferences + " preferences")
      $(".simMid").html(simMid);

      simRank = [String(simFav), String(simMid), String(simDis)];
      guessRank = [String(favGuess), String(midGuess), String(disGuess)];
      evalNum = 0;

      console.log(guessRank + "guessRank");
      console.log(simRank + "simRank");

      
      
      if (JSON.stringify(guessRank)==JSON.stringify(simRank)){
        evalNum = 3;
      } else if(JSON.stringify(guessRank)==JSON.stringify([simRank[0], simRank[2], simRank[1]]) || JSON.stringify(guessRank)==JSON.stringify([simRank[1], simRank[0], simRank[2]])
        ){
        evalNum = 2;
      } else if(JSON.stringify(guessRank)==JSON.stringify([simRank[1], simRank[2], simRank[0]]) || JSON.stringify(guessRank)==JSON.stringify([simRank[2], simRank[0], simRank[1]])
        ){
        evalNum = 1;
      } else if(JSON.stringify(guessRank)==JSON.stringify([simRank[2], simRank[1], simRank[0]])
        ){
        evalNum = 0;
      } else {
        console.log( "no appropriate ranking possible");
      }

      var emotion = "Great";
      var feedback = "Work hard, play hard!"
      if (evalNum == 3) {
        emotion = "very happy!" //"Wow, great!";
        feedback = "Awesome! Great present!"
      } else if (evalNum == 2) {
        emotion = "happy." // "Okay, not bad.";
        feedback = "That was a good present."
      } else if (evalNum == 1) {
        emotion = "okay."
        feedback = "Maybe you need to listen better."
      } else {
        emotion = "very sad." // "Well... at least you tried :)"; 
        feedback = "Next time you should really try to find a better present."
      };
      $(".emotion").html(emotion);
      $(".feedback").html(feedback);
      $(".personPic").html(this.personPic);

      var person2 = this.names_list[0];
      $(".person2").html(person2);

      exp.data_trials.push({
        "evalNum": evalNum,
        "simRank0": simRank[0],
        "simRank1": simRank[1],
        "simRank2": simRank[2],
        "guessRank0": guessRank[0],
        "guessRank1": guessRank[1],
        "guessRank2": guessRank[2],
      });
    },
    // updateValue: function () {
    //   var fixedTargetFeature = shuffle(['color', 'shape', 'texture'])[0];
    //   var preferences = [];
    //   if (fixedTargetFeature == "texture") {
    //     preferences = ["solid", "striped", "polka-dotted"]
    //   } else if (fixedTargetFeature == "color") {
    //     preferences = ["blue", "red", "green"]
    //   } else if (fixedTargetFeature == "shape") {
    //     preferences = ["cloud", "circle", "square"]
    //   } else {
    //     preferences = ["ERROR", "ERROR", "ERROR"]
    //   };
    //   var simPreferences = shuffle([0, 0.33, 0.66]);
    //   var actualSliderPosition = [0.5, 0.5, 0.5];
    //   console.error("fixedTargetFeature, preferences, simPreferences, actualSliderPosition" + fixedTargetFeature, preferences, simPreferences, actualSliderPosition);
    //   return [fixedTargetFeature, preferences, simPreferences, actualSliderPosition];
    // },
    button: function () {
      exp.go();
      this.blockNr++;
      slides.blockPause.blockNr = this.blockNr;
      refreshValues();
      // this.setValue();
    }
  });

  slides.blockPause = slide({
    name: "blockPause",
    blockNr: blockNr,
    expBlock: expBlock,
    pronoun: pronoun,
    persPronoun: persPronoun,
    dirPronoun: dirPronoun,
    trialNum: trialNum,
    start: function () {
      $('.pronoun').html(this.pronoun);
      $('.persPronoun').html(this.persPronoun);
      $('.dirPronoun').html(this.dirPronoun);
      var blockText = "";
      if (this.blockNr == -1) {
        blockText = "After this first test block, you can take a deep breath and then start with the first real block. ";
      } else if (this.blockNr == this.expBlock) {
        blockText = "The end is near! Take a deep breath and get ready to start with the last block. ";
      } else {
        blockText = "Take a deep breath and get ready to start with the next block. ";
      };
      $(".blockText").html(blockText);
      $(".instruction_condition").html("Between subject instruction manipulation: " + exp.instruction);
      console.log(this.trialNum + " trialNum")
      this.trialNum = -1;
      slides.task.trialNum = -1;
      console.log(this.trialNum + " trialNum")
    },
    button: function () {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
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
  function createStructure() {
    var structure = ["i0", "instructions1",
      'task', 'certainty',
      'comparison'];
    for (var i = 0; i < this.expBlock-1; i++) {
      structure.push("blockPause", 'task', 'certainty',
        'comparison');
    }
    structure.push('subj_info', 'thanks');
    return structure;
  }

  // exp.structure = ["i0", "instructions1", 'task', 'certainty', 'comparison', "blockPause", 'task', 'certainty', 'comparison', 'subj_info', 'thanks'];
  exp.structure = createStructure();


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