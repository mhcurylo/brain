import { ActionEvent, ActionStore, MetaState, Reducer, Store } from './store.interface';

function runSubscriptions<T>(state: MetaState<T>, event: ActionEvent): void {
    state.subscriptions.forEach((subscription) => subscription(state.state, event));
}

function runReducers<T>(state: MetaState<T>, event: ActionEvent): MetaState<T> {
    return state.reducers.reduce((p: MetaState<T>, c: Reducer<MetaState<T>>) => c(p, event), state);
}

export function createStore<T>(state: MetaState<T>): ((event: ActionEvent) => ActionStore<T>) {

    return (event: ActionEvent): ActionStore<T> => {
        const newState = runReducers(state, event);
        runSubscriptions(newState, event);

        return createStore(newState);
    }
}
