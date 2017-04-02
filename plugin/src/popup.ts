import {fmap, Maybe} from './libs/maybe';
import { Page, PageEvent, Place, State } from './state/state.interface';

let viewState: Maybe<Page> = null;

const elm: Maybe<HTMLElement> = document.getElementById('popup');

const renderPlace = (place: Place) => `<a href="${place.url}">${place.title}</a>`;
const renderEvent = (shown: number) => (event: PageEvent) => `<div class="${event.when > shown ? 'fresh-event' : ''}">
${event.who} arrived at ${renderPlace(event.at)} from  ${renderPlace(event.from)}
</div>`;
const renderEvents = (events: PageEvent[], shown: number) => events.map(renderEvent(shown)).join('');
const renderPage = (page: Page) => `<h5>${page.at.title}</h5> ${renderEvents(page.events, page.shown)}`;

const redraw = () => {
    if (viewState && elm) {
        elm.innerHTML = renderPage(viewState);
    }
}

const updateState = (newState: State) => {
    const setState = fmap((url: string) => viewState = newState.pages[url]);

    chrome.tabs.query({active: true, lastFocusedWindow: true}, (tabs) => {
        const url: Maybe<string> = tabs[0].url;

        setState(url);
        redraw();
    });
};

(<any>window).updateState = updateState;

const background = chrome.extension.getBackgroundPage();

if (background) {
    (<any>background).forceUpdate();
};
