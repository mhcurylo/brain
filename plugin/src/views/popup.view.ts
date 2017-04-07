import { Page, PageEvent, Place, State } from '../state/state.interface';

const renderPlace = (place: Place): string => `<a href="${place.url}">${place.title}</a>`;

const renderEvent = (shown: number) => (event: PageEvent): string => `<div class="${
event.when > shown ? 'fresh-event' : ''
}">${event.who} arrived at ${renderPlace(event.at)} from ${renderPlace(event.from)}</div>`;

const renderEvents = (events: PageEvent[], shown: number): string => events.map(renderEvent(shown)).join('');

export const renderPopup = (page: Page): string => `<h5>${page.at.title}</h5> ${renderEvents(page.events, page.shown)}`;

export const redrawPopup = (elm: HTMLElement, pageState: Page): void => {
    elm.innerHTML = renderPopup(pageState);
}
