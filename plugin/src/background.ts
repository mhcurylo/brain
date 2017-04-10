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

let ws: WebSocket = serverConnect('ws://localhost:3000');
let waitingForOpen: string[] = [];

function serverConnect(url: string): WebSocket {
    const nws = new WebSocket(url);

    nws.onerror = () => ws = serverConnect(url);
 //   nws.onclose = () => ws = serverConnect(url);
    waitingForOpen = ['Hello Brain'];

    nws.onopen = () => waitingForOpen.forEach((msg) => {
        ws.send(msg);
        console.log('m', msg);
    });

    return nws;
}

// ASSIGNING LISTENERS

const tabActivated = (activeInfo: chrome.tabs.TabActiveInfo): void => {
    chrome.tabs.get(activeInfo.tabId, ({ url, title }: chrome.tabs.Tab) => {
        arriveAtNewPlace(url, title);
        if (ws.readyState === ws.OPEN) {
            console.log('sending');
            ws.send(JSON.stringify({ url, title }));
        } else {
            console.log('deffering');
            waitingForOpen.push(JSON.stringify({ url, title }));
        }
    });
};

chrome.tabs.onActivated.addListener(tabActivated);
