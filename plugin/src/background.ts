import {Maybe, fmap} from './libs/maybe';
import {State} from './state/state.interface';
import {initState} from './state/state.init';

let state: State = initState;

const pushStateUrlIO = fmap((x: string): void => {state = state});


function tabUpdated(tabId: number, changeInfo: chrome.tabs.TabChangeInfo, tab: chrome.tabs.Tab): void {
    const url: Maybe<string> = tab.url;

    pushStateUrlIO(url + ' UPDATED');
    broadcast();
};

function tabActivated(activeInfo: chrome.tabs.TabActiveInfo): void {
    chrome.tabs.get(activeInfo.tabId, tab => {
        pushStateUrlIO(tab.url + ' ACTIVATED');
    });
    broadcast();
}


function getState(): State {
    return state;
}

function broadcast() {
    const views = chrome.extension.getViews();
    views.forEach((view: any) => view.updateState ? view.updateState(state) : '');
}


chrome.tabs.onUpdated.addListener(tabUpdated);
chrome.tabs.onActivated.addListener(tabActivated);
(<any>window).getState = getState;
