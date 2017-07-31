import { next } from './app/app';
import { fmap, Maybe } from './libs/maybe';
import { initSockets } from './libs/websockets';
import { addPageEvent } from './reducers/pages/actions/action.creators';
import { Page, PageEvent, Place } from './state/state.interface';

(window as any).next = next;

// ASSIGNING LISTENERS

const send = initSockets('ws://localhost:3000', (msg: MessageEvent) => typeof msg === "string" ? "" : next(addPageEvent(msg.data)));

const tabActivated = (activeInfo: chrome.tabs.TabActiveInfo): void => {
    chrome.tabs.get(activeInfo.tabId, ({ url, title }: chrome.tabs.Tab) => {
        send(JSON.stringify({url, title}));
    });
};

chrome.tabs.onActivated.addListener(tabActivated);
