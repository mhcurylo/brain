import {Maybe, fmap} from './lib';

interface State {
    readonly urls: string[];
}

let state: State = {
    urls: ['aaa', 'boo']
}

const pushStateUrlIO = fmap((x: string): void => {state = {urls: [x]}});


function getState(): State {
    return state;
}

function broadcast() {
    const views = chrome.extension.getViews();
    views.forEach((view: any) => view.updateState ? view.updateState(state) : '');
}

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

chrome.tabs.onUpdated.addListener(tabUpdated);
chrome.tabs.onActivated.addListener(tabActivated);
(<any>window).getState = getState;
