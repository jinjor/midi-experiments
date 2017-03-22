var SimpleInstruments = (function() {
  window.AudioContext = window.AudioContext || window.webkitAudioContext;
  let audioContext = new AudioContext();
  let destination = audioContext.destination;

  let indices = [];
  for(let i = 0; i < 16; i++) {
    indices.push(i);
  }
  var ports = indices.map(i => {
    // Oscillator
    let oscillator = audioContext.createOscillator();
    oscillator.type = 'square';
    oscillator.start = oscillator.start || oscillator.noteOn;

    // Gain
    let gain = audioContext.createGain();
    gain.gain.value = 0;

    // Connect
    oscillator.connect(gain);
    gain.connect(destination);

    // Start
    oscillator.start();

    let id = i + '';
    return {
      id: id,
      name: 'Simple ' + id,
      send: (message, at) => {
        at = at || 0;
        if(message[0] === 0x80) {
          gain.gain.setValueAtTime(0, at);
        } else if(message[0] === 0x90) {
          let frequency = 440 * Math.pow(Math.pow(2, 1 / 12), message[1] - 69);
          gain.gain.setValueAtTime(0.03, at);
          oscillator.frequency.setValueAtTime(frequency, at);
        } else if(message[0] === 0xb0) {
          // all notes/sound off
          if(message[1] === 123 || message[1] === 120) {
            gain.gain.setValueAtTime(0, at);
          }
        }
      }
    };
  });
  return {
    ports: ports
  };
})();
