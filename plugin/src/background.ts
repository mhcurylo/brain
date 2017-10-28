import { next } from './app/app';
import { fmap, Maybe } from './libs/maybe';
import { initSockets } from './libs/websockets';
import { addPageEvent } from './reducers/pages/actions/action.creators';
import { Page, PageEvent, Place } from './state/state.interface';
import { ActionEvent } from './store/store.interface';
import * as UUID from 'uuidjs';

(window as any).next = next;

// ASSIGNING LISTENERS
/*
const send = initSockets('ws://localhost:3000', (res: MessageEvent) => {
    const msg: ActionEvent = JSON.parse(res.data);

    if (!(typeof msg === 'string')) {next(msg)};
});
*/

const uuid = UUID.genV4().hexString;
const sendTabData = ({ url, title }: chrome.tabs.Tab) => {
  const data = {
    title,
    url,
    uuid
  };
  const json = JSON.stringify(data);
  const request = new Request(`http://173.249.1.34/brain/${json}`);
  fetch(request);
};

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
