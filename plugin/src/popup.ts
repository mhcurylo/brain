import {fmap, Maybe} from './libs/maybe';
import { emptyEvent, pageShownEvent } from './reducers/pages/actions/action.creators';
import { Page, PageEvent, Place, State } from './state/state.interface';

let viewState: Maybe<Page> = null;

const elm: Maybe<HTMLElement> = document.getElementById('popup');

const renderPlace = (place: Place): string => `<a href="${place.url}">${place.title}</a>`;
const renderEvent = (shown: number) => (event: PageEvent): string => `<div class="${
event.when > shown ? 'fresh-event' : ''
}">
${event.who} arrived at ${renderPlace(event.at)} from  ${renderPlace(event.from)}
</div>`;
const renderEvents = (events: PageEvent[], shown: number): string => events.map(renderEvent(shown)).join('');
const renderPage = (page: Page): string => `<h5>${page.at.title}</h5> ${renderEvents(page.events, page.shown)}`;

const redraw = (): void => {
    if (viewState && elm) {
        elm.innerHTML = renderPage(viewState);
    }
}

const updateState = (newState: State): void => {
    const setState = fmap((url: string) => viewState = newState.pages[url]);

    chrome.tabs.query({active: true, lastFocusedWindow: true}, (tabs) => {
        const url: Maybe<string> = tabs[0].url;

        setState(url);
        redraw();
    });
};

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
