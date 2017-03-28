import {Store, ActionStore, ActionEvent, MetaState} from './store.interface';
import {createStore} from './store';
import {should} from 'chai';

should();


describe('Store', () => {
    const initState = {
        count: 0
    };

    const metaState: MetaState<{count: number}> = {
        actions: [],
        reducers: [],
        state: initState,
        subscribers: []
    };

    const emptyAction: ActionEvent = {kind: '', payload: {}}

    it('createStore should get a state and return a function accepting actions', () => {
        const store = createStore(metaState);
        const storeCall = () => store(emptyAction)(emptyAction)(emptyAction);

        storeCall.should.not.Throw();
    });

});
