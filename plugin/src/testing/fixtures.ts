import { should } from 'chai';
import { createStore } from '../store/store';
import { ActionEvent, ActionStore, MetaState, Reducer, Store,  Subscription } from '../store/store.interface';

export interface State {
    count: number;
}

export type MState = MetaState<State>;

export const initState: State = {
    count: 0,
};

export const emptyAction: ActionEvent = { kind: '', payload: {} }
export const incAction: ActionEvent = { kind: 'inc', payload: {} }

export const reducer: Reducer<MState> = (mstate: MState, action: ActionEvent) =>
    action.kind === 'inc' ? { ...mstate, state: { count: mstate.state.count + 1 } } : mstate;

export const createMetaStateFixture = (subscriptions: Array<Subscription<State>>): MetaState<State> => ({
    reducers: [reducer],
    state: initState,
    subscriptions,
});
