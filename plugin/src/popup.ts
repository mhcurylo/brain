import { fmap, Maybe } from './libs/maybe';
import { emptyEvent, pageShownEvent } from './reducers/pages/actions/action.creators';
import { Page, PageEvent, Place, State } from './state/state.interface';
import { redrawPopup } from './views/popup.view';

let redraw: boolean = false;
let viewState: Maybe<Page> = null;

const elm: Maybe<HTMLElement> = document.getElementById('popup');

const updateState = (newState: State): void => {
    const setState = fmap((url: string) => viewState = newState.pages[url]);

    chrome.tabs.query({active: true, lastFocusedWindow: true}, (tabs) => {
        const url: Maybe<string> = tabs[0].url;

        setState(url);
        redraw = true;
    });
};

const drawLoop = (): number => requestAnimationFrame(() => {
    if (redraw && viewState && elm) {
        redrawPopup(elm, viewState);
        redraw = false;
    }
    drawLoop();
});

drawLoop();

(<any>window).updateState = updateState;

const background = chrome.extension.getBackgroundPage();

const lastShown = (): void => {
    if (viewState && background) {
        const place: Place = viewState.at;
        const shown: number = new Date().getTime();

        (<any>background).next(pageShownEvent(place, shown));
    }
};

if (background) {
    (<any>background).next(emptyEvent());
    window.addEventListener('unload', lastShown);
};
