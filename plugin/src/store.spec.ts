import { Store, Reducer, Subscription, ActionStore, ActionEvent, MetaState } from './store.interface';
import { createStore } from './store';
import { should } from 'chai';

should();

interface State {
    count: number;
}

type MState = MetaState<State>;

describe('Store', () => {

    const emptyAction: ActionEvent = { kind: '', payload: {} }
    const incAction: ActionEvent = { kind: 'inc', payload: {} }

    let presentState: State = { count: 3 };
    let lastAction: ActionEvent = incAction;

    const reducer: Reducer<MState> = (mstate: MState, action: ActionEvent) => action.kind === 'inc' ? { ...mstate, state: { count: mstate.state.count + 1 } } : mstate;

    const getStateAndAction: Subscription<MState> = (mstate: MState, action: ActionEvent): void => { presentState = mstate.state; lastAction = action };

    const initState = {
        count: 0
    };

    const metaState: MetaState<State> = {
        reducers: [reducer],
        state: initState,
        subscriptions: [getStateAndAction]
    };

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
