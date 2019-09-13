/// Stream ///

var slide = function (_slide, _private) {
  var s = _slide || {}, p = _private || {};
  s.phaseid = 0;
  s.init = function () {
    utils.showSlide(this.name);
    this.phaseid++;
    if (this.start) { this.start(); };
    if (this.handle) this.handle();
    _stream.apply(this);
  };

  //what to do when done presenting all the slides
  s.callback = s.callback !== undefined ? s.callback : function () { 
    this.blockNr++;
    exp.go();  };
  return (s);
};

function shuffle(a) {
  var j, x, i;
  for (i = a.length - 1; i > 0; i--) {
    j = Math.floor(Math.random() * (i + 1));
    x = a[i];
    a[i] = a[j];
    a[j] = x;
  }
  return a;
}

// var targetFeaturesShuffled = shuffle(['color', 'shape', 'texture']);
// var targetFeatureBlock = targetFeaturesShuffled[0];

var _stream = function () {
  if (exp.nQs) {
    //if number of total questions is defined, then show progress bar
    $('.bar').css('width', ((exp.phase /(exp.nQs+10)) * 100 + "%"));
  } else {
    $(".progress").hide();
  }
  var present = undefined;
  if (this.fixedTargetFeature == 'color') {
    present = this.presentColor;
  } else if (this.fixedTargetFeature == 'shape') {
    present = this.presentShape;
  } else if (this.fixedTargetFeature == 'texture') {
    present = this.presentTexture;
  } else {
    // present = undefined;
  }
  if (present == undefined) {
    exp.phase++;
    //not a presented slide (i.e. there are not multiple trials using the same slide)
  } else {
    var presented_stims = present || [];

    if (presented_stims.length === 0) {
      //done with slide
      if (this.end) { this.end(); };
      // this.compairHandle(this.actualSliderPosition, this.simPreferences, this.preferences, )
      this.callback();
    } else if (this.present_handle) {
      exp.phase++;
      // var stim = [];
      // do {
      //   presented_stims.shift();
      // } while (presented_stims[1].targetfeature !== this.fixedTargetFeature);
      // var stim = presented_stims[1];
      var stim = presented_stims.shift();
      var order = _.shuffle(stim['item']);
      if (this.catch_trial_handle && stim.catchT) {
        //Catch Trial
        this.catch_trial_handle(stim);
      } else {
        //Normal Trial
        this.present_handle(stim, this.fixedTargetFeature, order);
      }
    }
  }
};