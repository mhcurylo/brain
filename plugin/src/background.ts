import { fmap, Maybe } from './libs/maybe';
import { liftReducer } from './reducers/liftReducer';
import { addPageEvent } from './reducers/pages/actions/action.creators';
import { pagesReducer } from './reducers/pages/pages.reducer';
import { initState } from './state/state.init';
import { Page, PageEvent, Place, State} from './state/state.interface';
import { createStore } from './store/store';
import { ActionEvent, ActionStore, MetaState, Subscription } from './store/store.interface';

let store: ActionStore<State> = createStore({
    reducers: [liftReducer(pagesReducer)],
    state: initState,
    subscriptions: [broadcast, updateBadge],
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
            when: (new Date()).getTime(),
            who: 'Usr',
        }

        store = store(addPageEvent(pageEvent));
    }
}

function broadcast(state: State, action: ActionEvent) {
    const views = chrome.extension.getViews();
    views.forEach((view: any) => view.updateState ? view.updateState(state) : '');
}

function updateBadge(state: State, action: ActionEvent) {
    chrome.tabs.query({active: true, lastFocusedWindow: true}, (tabs) => {
        const url: Maybe<string> = tabs[0].url;
        const id: Maybe<number> = tabs[0].id;

        if (url && id && state.pages[url]) {
            const page: Page = state.pages[url];
            const shown: number = page.shown;
            const newEvents: number =  page.events.filter((e: PageEvent): boolean => e.when > shown).length;
            const text: string = newEvents ? newEvents.toString() : '';
            chrome.browserAction.setBadgeText({text});
        }
    });
};

function next(action: ActionEvent): void {
    store = store(action);
}

const tabActivated = (activeInfo: chrome.tabs.TabActiveInfo): void =>
    chrome.tabs.get(activeInfo.tabId, (tab) => arriveAtNewPlace(tab.url, tab.title));

(<any>window).next = next;
chrome.tabs.onActivated.addListener(tabActivated);
