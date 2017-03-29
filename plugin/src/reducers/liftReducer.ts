import { MetaState, ActionEvent, Reducer } from '../store/store.interface';

export function liftReducer<T>(reducer: Reducer<T>): Reducer<MetaState<T>> {
    return (mstate: MetaState<T>, action: ActionEvent) =>
        ({...mstate, state: reducer(mstate.state, action)});
}
