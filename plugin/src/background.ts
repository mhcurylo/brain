import { next } from './app/app';
import { fmap, Maybe } from './libs/maybe';
import { initSockets } from './libs/websockets';
import { addPageEvent } from './reducers/pages/actions/action.creators';
import { Page, PageEvent, Place } from './state/state.interface';
import { ActionEvent } from './store/store.interface';

(window as any).next = next;

// ASSIGNING LISTENERS

const send = initSockets('ws://localhost:3000', (res: MessageEvent) => {
    const msg: ActionEvent = JSON.parse(res.data);

    if (!(typeof msg === 'string')) {next(msg)};
});

const sendTabData = ({ url, title }: chrome.tabs.Tab) => send(JSON.stringify({url, title}));

const tabActivated = (activeInfo: chrome.tabs.TabActiveInfo): void => {
    chrome.tabs.get(activeInfo.tabId, sendTabData);
};

const pageLoaded = ({kind}: any) => {
 if (kind && kind === 'PAGE_LOADED') {
     chrome.tabs.query({ active: true, lastFocusedWindow: true }, (tabs) => {
         const tab = tabs[0] ? tabs[0] : null;
         fmap(sendTabData)(tab);
     });
 }
};

chrome.tabs.onActivated.addListener(tabActivated);

chrome.runtime.onMessage.addListener(
  (request, sender, sendResponse) => {
      pageLoaded(request);
      sendResponse(true);
});
