import {Maybe} from './lib';

const elm: Maybe<HTMLElement> = Maybe.of(document.getElementById('popup'));

elm.map(el => el.innerHTML = "boo");



