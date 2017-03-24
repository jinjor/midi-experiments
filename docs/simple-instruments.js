var SimpleInstruments = (function() {
  window.AudioContext = window.AudioContext || window.webkitAudioContext;
  let audioContext = new AudioContext();
  let destination = audioContext.destination;

  let indices = [];
  for(let i = 0; i < 16; i++) {
    indices.push(i);
  }
  var ports = indices.map(i => {

    let globalGain = audioContext.createGain();
    globalGain.gain.value = 0.03;
    globalGain.connect(destination);

    let noteState = [];

    function noteOn(noteNum, velocity, at) {
      velocity = 127;
      if(!noteState[noteNum]) {
        let frequency = 440 * Math.pow(Math.pow(2, 1 / 12), noteNum - 69);

        let gain = audioContext.createGain();
        gain.connect(globalGain);

        let oscillator = audioContext.createOscillator();
        oscillator.type = 'square';
        oscillator.start = oscillator.start || oscillator.noteOn;
        oscillator.frequency.setValueAtTime(frequency, at);
        oscillator.connect(gain);

        oscillator.start(at);

        noteState[noteNum] = {
          gain: gain,
          oscillator: oscillator
        };
      }
      noteState[noteNum].gain.gain.setValueAtTime(1.0 * (velocity / 127), at);
    }

    function noteOff(noteNum, at) {
      if(noteState[noteNum]) {
        noteState[noteNum].gain.gain.setValueAtTime(0, at);
      }
    }

    function allSoundOff(at) {
      for(var i = noteState.length - 1; i >= 0; i--) {
        var n = noteState[i];
        if(n) {
          n.oscillator.frequency.cancelScheduledValues(at);
          n.gain.gain.cancelScheduledValues(at);
          n.gain.gain.setValueAtTime(0, at + 0.001);
          n.oscillator.stop(at);
          delete noteState[i];
        }
      }
    }

    let id = i + '';
    return {
      id: id,
      name: 'Simple ' + id,
      send: (message, at) => {
        at = audioContext.currentTime + (at ? Math.max(at - performance.now(), 0) / 1000 : 0);
        if(message[0] === 0x80) {
          noteOff(message[1], at);
        } else if(message[0] === 0x90) {
          noteOn(message[1], message[2], at);
        } else if(message[0] === 0xb0) {
          // all notes/sound off
          if(message[1] === 123 || message[1] === 120) {
            allSoundOff(at);
          }
        }
      }
    };
  });
  return {
    ports: ports
  };
})();
