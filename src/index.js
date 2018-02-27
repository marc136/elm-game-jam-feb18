import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import { Howl, Howler } from 'howler';

// Change global volume.
Howler.volume(0.8);

const game = Main.embed(document.getElementById('root'));

game.ports.sounds.subscribe(playSound);

function playSound(event) {
  try {
    if (event.play) {
      sounds[event.play].play();

    } else if (event.loop) {
      sounds[event.loop].loop(true);
      sounds[event.loop].play();

    } else if (event.stop) {
      sounds[event.stop].stop();
    }
    console.log('Sound:', event);
  } catch (ex) {
    console.log('Sound error:', event);
  }
}


const paths = {
  'buy': require('../sounds/cash-register-06.wav'),
  'drop-hat2': require('../sounds/hit-01.wav'),
  'drop-hat': require('../sounds/groan-01.wav'),
  'factory': require('../sounds/factory2.wav'),
  'get-hat': require('../sounds/ohhh.wav'),
  'sleep': require('../sounds/yawn-01.wav'),
  'whistle': require('../sounds/factorywhistle-01.mp3'),
};

const sounds = {};
for (let key in paths) {
  sounds[key] = new Howl({ src: [paths[key]] });
}
sounds.factory.volume(0.3); // reduce volume of background loop
sounds.factory.loop(true);

registerServiceWorker();

window.debug = { game, paths, sounds, Howler, Howl }
