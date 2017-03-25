import {Maybe, fmap} from './lib';

const elm: Maybe<HTMLElement> = document.getElementById('popup');

const setInnerHtml: (x: string) => (y: HTMLElement) => void =
    x => y => y.innerHTML = x;

const setBoo = setInnerHtml("boo");

fmap(setBoo)(elm);

