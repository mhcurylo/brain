import { Store, Reducer, Subscription, ActionStore, ActionEvent, MetaState } from './store.interface';
import { createStore } from './store';
import { should } from 'chai';
import { emptyAction, incAction, reducer, createMetaStateFixture} from '../testing/fixtures';

should();

interface State {
    count: number;
}

type MState = MetaState<State>;

describe('Store', () => {
    let presentState: State = { count: 3 };
    let lastAction: ActionEvent = incAction;

    const getStateAndAction: Subscription<State> = (state: State, action: ActionEvent): void => { presentState = state; lastAction = action };
    const metaState: MetaState<State> = createMetaStateFixture([getStateAndAction]);

    beforeEach(() => {
        presentState = { count: 3 };
        lastAction = incAction;
    });

    it('createStore should take a state and return an actionStore accepting actions', () => {
        const store = createStore(metaState);
        const storeCall = () => store(emptyAction)(emptyAction)(emptyAction);

        storeCall.should.not.Throw();
    });

    describe('subscribers', () => {

        it('actionStore should call the subscriber with the new state and last action', () => {
            const store = createStore(metaState);
            store(emptyAction);

            presentState.should.eql({ count: 0 });
            lastAction.should.eql(emptyAction);
        });

        it('actionStore should work with multiple subscriptions', () => {
            const store = createStore({...metaState, subscriptions: [getStateAndAction, getStateAndAction, getStateAndAction]});
            store(emptyAction);

            presentState.should.eql({ count: 0 });
            lastAction.should.eql(emptyAction);
        });

    });

    describe('reducers', () => {

        it('actionStore should apply reducer to the state', () => {
            const store = createStore(metaState);
            store(incAction);

            presentState.should.eql({ count: 1 });
        });

        it('actionStore should return actionStore with the new state', () => {
            const store = createStore(metaState);
            store(incAction)(incAction)(incAction);

            presentState.should.eql({ count: 3 });
        });

        it('actionStore should compose reducers', () => {
            const store = createStore({...metaState, reducers: [reducer, reducer, reducer]});
            store(incAction);

            presentState.should.eql({ count: 3 });
        });
    });
});
