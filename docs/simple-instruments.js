var SimpleInstruments = (function() {
  window.AudioContext = window.AudioContext || window.webkitAudioContext;
  let audioContext = new AudioContext();
  let destination = audioContext.destination;

  let oscillators = [];
  let gains = [];


  for(let i = 0; i < 15; i++) {
    let oscillator = audioContext.createOscillator();
    oscillator.type = 'square';
    oscillator.start = oscillator.start || oscillator.noteOn;
    oscillators[i] = oscillator;
  }
  for(let i = 0; i < 15; i++) {
    let gain = audioContext.createGain();
    gain.gain.value = 0;
    gains[i] = gain;
  }
  for(let i = 0; i < 15; i++) {
    let oscillator = oscillators[i];
    let gain = gains[i];
    oscillator.connect(gain);
    gain.connect(destination);
  }
  var ports = oscillators.map((oscillator, i) => {
    let gain = gains[i];
    let id = i + '';
    return {
      id: id,
      name: 'Simple ' + id,
      send: message => {
        if(message[0] === 0x80) {
          gain.gain.value = 0;
        } else if(message[0] === 0x90) {
          gain.gain.value = 0.03;
          let frequency = 440 * Math.pow(Math.pow(2, 1 / 12), message[1] - 69);
          oscillator.frequency.setValueAtTime(frequency, 0);
        } else if(message[0] === 0xb0) {
          // all notes/sound off
          if(message[1] === 123 || message[1] === 120) {
            gains.forEach(gain => {
              gain.gain.value = 0;
            });
          }
        }
      }
    };
  });
  oscillators.forEach(o => o.start());
  return {
    ports: ports
  };
})();
