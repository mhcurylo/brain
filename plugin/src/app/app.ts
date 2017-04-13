import { liftReducer } from '../reducers/liftReducer';
import { pagesReducer } from '../reducers/pages/pages.reducer';
import { initState } from '../state/state.init';
import { State } from '../state/state.interface';
import { createStore } from '../store/store';
import { ActionEvent, ActionStore } from '../store/store.interface';
import { updateBadge } from '../views/badge.view';
import { broadcast } from './broadcast';

export const next: ((action: ActionEvent) => void) = (() => {
    let store: ActionStore<State> = createStore({
        reducers: [liftReducer(pagesReducer)],
        state: initState,
        subscriptions: [broadcast, updateBadge],
    });

    return (action: ActionEvent): void => { store = store(action) };
})();
