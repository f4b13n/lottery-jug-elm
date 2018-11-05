import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    lotteryUrl: 'http://localhost:8088'
  }
});

registerServiceWorker();
