import {Maybe, fmap} from './lib';

interface State {
    readonly urls: string[];
}

let state: State = {
    urls: []
}

const elm: Maybe<HTMLElement> = document.getElementById('popup');

const setInnerHtml: (x: string) => (y: HTMLElement) => void =
    x => y => y.innerHTML = x;

const updateState = (newState: State) => {
    state = newState;

    const setUrls = fmap(setInnerHtml(state.urls.join(', ')));

    setUrls(elm);
};

(<any>window).updateState = updateState;
