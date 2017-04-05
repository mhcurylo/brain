import { fmap, Maybe } from './libs/maybe';
import { liftReducer } from './reducers/liftReducer';
import { addPageEvent } from './reducers/pages/actions/action.creators';
import { pagesReducer } from './reducers/pages/pages.reducer';
import { initState } from './state/state.init';
import { Page, PageEvent, Place, State } from './state/state.interface';
import { createStore } from './store/store';
import { ActionEvent, ActionStore, MetaState, Subscription } from './store/store.interface';
import { updateBadge } from './views/badge.view';

const broadcast = (state: State, action: ActionEvent): void => {
    const views = chrome.extension.getViews();
    views.forEach((view: any) => view.updateState ? view.updateState(state) : '');
}

const next = (action: ActionEvent): void => {
    let store: ActionStore<State> = createStore({
        reducers: [liftReducer(pagesReducer)],
        state: initState,
        subscriptions: [broadcast, updateBadge],
    });

    store = store(action);
}

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

        next((addPageEvent(pageEvent)));
    }
}

const tabActivated = (activeInfo: chrome.tabs.TabActiveInfo): void =>
    chrome.tabs.get(activeInfo.tabId, (tab) => arriveAtNewPlace(tab.url, tab.title));

(<any>window).next = next;
chrome.tabs.onActivated.addListener(tabActivated);
