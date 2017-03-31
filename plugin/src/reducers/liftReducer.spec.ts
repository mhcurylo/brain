import { Store, Reducer, Subscription, ActionStore, ActionEvent, MetaState } from '../store/store.interface';
import { emptyAction, incAction, reducer, createMetaStateFixture, State} from '../testing/fixtures';
import { should } from 'chai';
import { liftReducer } from './liftReducer';
import { createStore } from '../store/store';

should();

describe('liftReducer', () => {
    let presentState: State = { count: 3 };
    let lastAction: ActionEvent = incAction;

    const getStateAndAction: Subscription<State> = (state: State, action: ActionEvent): void => { presentState = state; lastAction = action };

    it('should take a reducer working on state T and make it work on metaState T', () => {
        const reducer: Reducer<State> = (state: State, action: ActionEvent) => action.kind === 'inc' ? { count: state.count + 1 } : state;
        const metaReducer: Reducer<MetaState<State>> = liftReducer(reducer);

        const metaState: MetaState<State> = {...createMetaStateFixture([getStateAndAction]), reducers: [metaReducer]};

        const store = createStore(metaState);

        store(incAction)(incAction);

        presentState.should.eql({ count: 2 })
    });
})
