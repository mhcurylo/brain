import { Page, PageEvent, Place, State } from '../state/state.interface';

const escapeHtml = (str: string): string => {
    if (typeof document !== 'undefined') {
        const div = document.createElement('div');
        div.appendChild(document.createTextNode(str));

        return div.innerHTML;
    } else {
        return str;
    }
}

const renderPlace = (place: Place): string => `<a target="_blank" href="${place.url}">${escapeHtml(place.title)}</a>`;

const renderEventText = (url: string, { who, at, req, from }: PageEvent) => {
    if (url === at.url) {
        return `${who} arrived from ${renderPlace(from)}`;
    } else if (url === from.url) {
        return `${who} departed for ${renderPlace(at)}`;
    } else {
        return `${who} travelled from ${renderPlace(from)} to ${renderPlace(at)}`;
    }
};

const renderEvent = (shown: number, url: string) => (event: PageEvent): string => `<div class="${
    event.when > shown ? 'fresh-event' : ''}">${renderEventText(url, event)}</div>`;

const renderEvents = ({ events, shown, at }: Page): string => events.map(renderEvent(shown, at.url)).join('');

export const renderPopup = (page: Page): string => `<h5>${page.at.title}</h5> ${renderEvents(page)}`;

export const redrawPopup = (elm: HTMLElement, pageState: Page): void => {
    elm.innerHTML = renderPopup(pageState);
}
