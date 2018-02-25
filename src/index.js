import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const game = Main.embed(document.getElementById('root'));

game.ports.sound.subscribe(playSound)

function playSound(name) {
  console.log(`play "${name}"`)
}

registerServiceWorker();
