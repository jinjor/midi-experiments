var SimpleInstruments = (function() {
  window.AudioContext = window.AudioContext || window.webkitAudioContext;
  let audioContext = new AudioContext();
  audioContext.createGain = audioContext.createGain || audioContext.createGainNode;
  audioContext.createOscillator = audioContext.createOscillator || audioContext.createOscillatorNode;
  let destination = audioContext.destination;

  let indices = [];
  for(let i = 1; i <= 16; i++) {
    indices.push(i);
  }
  var ports = indices.map(i => {

    let globalGain = audioContext.createGain();
    globalGain.gain.value = 0.03;
    globalGain.connect(destination);

    let noteState = [];

    function ensureOscillator(noteNum) {
      if(!noteState[noteNum]) {
        let frequency = 440 * Math.pow(Math.pow(2, 1 / 12), noteNum - 69);

        let gain = audioContext.createGain();
        gain.gain.setValueAtTime(0, audioContext.currentTime);
        gain.connect(globalGain);

        let oscillator = audioContext.createOscillator();
        oscillator.start = oscillator.start || oscillator.noteOn;
        oscillator.type = 'square';
        oscillator.frequency.setValueAtTime(frequency, audioContext.currentTime);
        oscillator.connect(gain);
        oscillator.start();

        noteState[noteNum] = {
          gain: gain,
          oscillator: oscillator
        };
      }
    }

    function noteOn(noteNum, velocity, at) {
      ensureOscillator(noteNum);
      velocity = 127;
      noteState[noteNum].gain.gain.setValueAtTime(1.0 * (velocity / 128), at);
    }

    function noteOff(noteNum, at) {
      ensureOscillator(noteNum);
      noteState[noteNum].gain.gain.setValueAtTime(0, at);
    }

    function allSoundOff(at) {
      for(var i = noteState.length - 1; i >= 0; i--) {
        var n = noteState[i];
        if(n) {
          n.oscillator.frequency.cancelScheduledValues(at);
          n.gain.gain.cancelScheduledValues(at);
          n.gain.gain.setValueAtTime(0, at);
          n.oscillator.stop(at);
          delete noteState[i];
        }
      }
    }

    return {
      id: '#' + ("0" + i).slice(-2),
      name: 'Simple ' + ("0" + i).slice(-2),
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
