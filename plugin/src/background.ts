import { next } from './app/app';
import { fmap, Maybe } from './libs/maybe';
import { addPageEvent } from './reducers/pages/actions/action.creators';
import { Page, PageEvent, Place } from './state/state.interface';

(window as any).next = next;

// MOCK FOR NOW

const arriveAtNewPlace = fmap((url: string, title: string): void => {
    const place: Place = {
        url,
        title,
    }

    const pageEvent: PageEvent = {
        at: place,
        from: place,
        req: place,
        when: (new Date()).getTime(),
        who: 'Usr',
    }

    next((addPageEvent(pageEvent)));
});

// SERVER COMMUNICATION

const ws = new WebSocket('ws://localhost:3000/');

ws.onmessage = console.log.bind(console);

ws.onopen = () => ws.send('Hello Brain');

// ASSIGNING LISTENERS

const tabActivated = (activeInfo: chrome.tabs.TabActiveInfo): void => {
    chrome.tabs.get(activeInfo.tabId, ({ url, title }: chrome.tabs.Tab) => {
        arriveAtNewPlace(url, title);
        if (ws.OPEN) {
            //ws.send(JSON.stringify({ url, title }))
        }
    });
};

chrome.tabs.onActivated.addListener(tabActivated);
