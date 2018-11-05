import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const node = document.getElementById('root');

fetch('env.json')
  .then(response => response.json())
  .then(flags => Elm.Main.init({ flags, node }))
  .catch(() => node.innerText = 'Oops! Unreachable or invalid configuration.');

registerServiceWorker();
