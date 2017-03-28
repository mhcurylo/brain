import {Store, MetaState, ActionStore, ActionEvent} from './store.interface';

export function createStore<T>(state: MetaState<T>): ((event: ActionEvent) => ActionStore<T>) {
    return function actionStore<T>(event: ActionEvent): ActionStore<T> {
        const newState = state;

        return createStore(newState);
    };
}

