import {Maybe, fmap} from './lib';

interface State {
    readonly urls: string[];
}

let state: State = {
    urls: ['aaa', 'boo']
}

const pushStateUrlIO = fmap((x: string): void => {state = {urls: [x]}});

function broadcast() {
    const views = chrome.extension.getViews();
    views.forEach((view: any) => view.updateState ? view.updateState(state) : '');
}

function tabUpdated(tabId: number, changeInfo: chrome.tabs.TabChangeInfo, tab: chrome.tabs.Tab) {
    const url: Maybe<string> = tab.url;

    pushStateUrlIO(url);
};

setInterval(broadcast, 10);

chrome.tabs.onUpdated.addListener(tabUpdated);

