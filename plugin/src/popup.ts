import { fmap, Maybe } from './libs/maybe';
import { emptyEvent, pageShownEvent } from './reducers/pages/actions/action.creators';
import { Page, PageEvent, Place, State } from './state/state.interface';
import { redrawPopup } from './views/popup.view';

let shouldRedraw: boolean = false;
let viewState: Maybe<Page> = null;

const elm: Maybe<HTMLElement> = document.getElementById('popup');
const background: Maybe<Window> = chrome.extension.getBackgroundPage();

const updateState = (newState: State): void => {
    const setState = fmap((url: string) => viewState = newState.pages[url]);

    chrome.tabs.query({ active: true, lastFocusedWindow: true }, (tabs) => {
        const url: Maybe<string> = tabs[0].url;
        setState(url);
        shouldRedraw = true;
    });
};

(window as any).updateState = updateState;

const drawLoop = (): number => requestAnimationFrame(() => {
    if (shouldRedraw && viewState && elm) {
        redrawPopup(elm, viewState);
        shouldRedraw = false;
    }
    drawLoop();
});

drawLoop();

const onUnload = (): void => {
    if (viewState && background) {
        const place: Place = viewState.at;
        const shown: number = new Date().getTime();

        (background as any).next(pageShownEvent(place, shown));
    }
};

if (background) {
    (background as any).next(emptyEvent());
    window.addEventListener('unload', onUnload);
}
