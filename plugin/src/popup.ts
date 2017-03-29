import {Maybe, fmap} from './libs/maybe';

interface State {
    readonly urls: string[];
}

let state: State = {
    urls: []
}

const background = chrome.extension.getBackgroundPage();
const getState = (x: any): State => x.getState();
const maybeState: Maybe<State> = getState(background);

state = maybeState ? maybeState : state;

const elm: Maybe<HTMLElement> = document.getElementById('popup');

const setInnerHtml: (x: string) => (y: HTMLElement) => void =
    x => y => y.innerHTML = x;

const updateState = (newState: State) => {
    state = newState;

    const setUrls = fmap(setInnerHtml(state.urls.join(', ')));

    setUrls(elm);
};

updateState(state);

(<any>window).updateState = updateState;
