import { liftReducer } from '../reducers/liftReducer';
import { pagesReducer } from '../reducers/pages/pages.reducer';
import { initState } from '../state/state.init';
import { State } from '../state/state.interface';
import { createStore } from '../store/store';
import { ActionEvent, ActionStore } from '../store/store.interface';
import { updateBadge } from '../views/badge.view';

export const next: ((action: ActionEvent) => void) = (() => {
    const broadcast = (state: State, action: ActionEvent): void => {
        const views = chrome.extension.getViews();
        views.forEach((view: any) => view.updateState ? view.updateState(state) : '');
    }

    let store: ActionStore<State> = createStore({
        reducers: [liftReducer(pagesReducer)],
        state: initState,
        subscriptions: [broadcast, updateBadge],
    });

    return (action: ActionEvent): void => { store = store(action) };
})();

