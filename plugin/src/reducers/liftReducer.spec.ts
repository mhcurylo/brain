import { should } from 'chai';
import { createStore } from '../store/store';
import { ActionEvent, ActionStore, MetaState, Reducer, Store,  Subscription } from '../store/store.interface';
import { CountState, createMetaStateFixture, emptyAction, incAction, reducer } from '../testing/fixtures';
import { liftReducer } from './liftReducer';

should();

describe('liftReducer', () => {
    let presentState: CountState = { count: 3 };
    let lastAction: ActionEvent = incAction;

    const getStateAndAction: Subscription<CountState> = (state: CountState, action: ActionEvent): void => {
        presentState = state;
        lastAction = action;
    };

    it('should take a reducer working on state T and make it work on metaState T', () => {
        const reducer: Reducer<CountState> = (state: CountState, action: ActionEvent) =>
            action.kind === 'inc' ? { count: state.count + 1 } : state;

        const metaReducer: Reducer<MetaState<CountState>> = liftReducer(reducer);

        const metaState: MetaState<CountState> = {
                ...createMetaStateFixture([getStateAndAction]),
            reducers: [metaReducer],
        };

        const store = createStore(metaState);

        store(incAction)(incAction);

        presentState.should.eql({ count: 2 })
    });
})
