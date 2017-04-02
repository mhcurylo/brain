import { fmap, Maybe } from './libs/maybe';
import { liftReducer } from './reducers/liftReducer';
import { addPageEvent } from './reducers/pages/actions/action.creators';
import { pagesReducer } from './reducers/pages/pages.reducer';
import { initState } from './state/state.init';
import { PageEvent, Place, State} from './state/state.interface';
import { createStore } from './store/store';
import { ActionEvent, ActionStore, MetaState, Subscription } from './store/store.interface';

let store: ActionStore<State> = createStore({
    reducers: [liftReducer(pagesReducer)],
    state: initState,
    subscriptions: [broadcast],
});

const arriveAtNewPlace = (url: Maybe<string>, title: Maybe<string>): void => {
    if (url !== null && url !== undefined && title !== null && title !== undefined) {
        const place: Place = {
            url,
            title,
        }

        const pageEvent: PageEvent = {
            at: place,
            from: place,
            req: place,
            who: 'Usr',
        }

        store = store(addPageEvent(pageEvent));
    }
}

function broadcast<T>(state: T, action: ActionEvent) {
    const views = chrome.extension.getViews();
    views.forEach((view: any) => view.updateState ? view.updateState(state) : '');
}

function forceUpdate(): void {
    store = store({ kind: '', payload: {} })
}

function tabUpdated(tabId: number, changeInfo: chrome.tabs.TabChangeInfo, tab: chrome.tabs.Tab): void {
    const url: Maybe<string> = tab.url;
    const title: Maybe<string> = tab.title;

    arriveAtNewPlace(url, title);
};

function tabActivated(activeInfo: chrome.tabs.TabActiveInfo): void {
    chrome.tabs.get(activeInfo.tabId, (tab) => {
        arriveAtNewPlace(tab.url, tab.title);
    });
}

(<any>window).forceUpdate = forceUpdate;
chrome.tabs.onUpdated.addListener(tabUpdated);
chrome.tabs.onActivated.addListener(tabActivated);
