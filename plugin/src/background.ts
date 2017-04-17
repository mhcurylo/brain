import { next } from './app/app';
import { fmap, Maybe } from './libs/maybe';
import { initSockets } from './libs/websockets';
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

// ASSIGNING LISTENERS

const send = initSockets('ws://localhost:3000', (msg: MessageEvent) => {console.log('MSG', msg.data);});

const tabActivated = (activeInfo: chrome.tabs.TabActiveInfo): void => {
    chrome.tabs.get(activeInfo.tabId, ({ url, title }: chrome.tabs.Tab) => {
        arriveAtNewPlace(url, title);
        send(JSON.stringify({url, title}));
    });
};

chrome.tabs.onActivated.addListener(tabActivated);
