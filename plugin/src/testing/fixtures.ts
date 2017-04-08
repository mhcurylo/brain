import { should } from 'chai';
import { PageEventAction, PageShownAction } from '../reducers/pages/actions/actions.interface';
import { Page, PageEvent, Place, State } from '../state/state.interface';
import { createStore } from '../store/store';
import { ActionEvent, ActionStore, MetaState, Reducer, Store, Subscription } from '../store/store.interface';

export interface CountState {
    count: number;
}

export type MState = MetaState<CountState>;

export const initState: CountState = {
    count: 0,
};

export const emptyAction: ActionEvent = { kind: '', payload: {} }
export const incAction: ActionEvent = { kind: 'inc', payload: {} }

export const reducer: Reducer<MState> = (mstate: MState, action: ActionEvent) =>
    action.kind === 'inc' ? { ...mstate, state: { count: mstate.state.count + 1 } } : mstate;

export const createMetaStateFixture = (subscriptions: Array<Subscription<CountState>>): MetaState<CountState> => ({
    reducers: [reducer],
    state: initState,
    subscriptions,
})

export const placeOfErr: Place = {
    title: 'A place of err',
    url: 'http://here.there.er',
}

export const placeOfTher: Place = {
    title: 'A place of Ther',
    url: 'http://there.er',
}

export const placeOfBear: Place = {
    title: 'A place of Bear',
    url: 'http://bear.io',
}

export const pageEventAction: PageEventAction = {
    kind: 'PAGE_EVENT_ACTION',
    payload: {
        at: placeOfErr,
        from: placeOfTher,
        req: placeOfErr,
        when: 13424559,
        who: 'Panda',
    },
}

export const pageShownAction: PageShownAction = {
    kind: 'PAGE_SHOWN_ACTION',
    payload: {
        place: placeOfErr,
        shown: 13424959,
    },
}

export const initStateWithPages: State = {
    pages: {
        'http://here.there.er': {
            at: placeOfErr,
            events: [pageEventAction.payload],
            shown: 13424559,
        },
        'http://there.er': {
            at: placeOfTher,
            events: [],
            shown: 0,
        },
    },
    who: 'meah',
}

export const pandaArrived: PageEvent = {
        at: placeOfErr,
        from: placeOfTher,
        req: placeOfErr,
        when: 13424559,
        who: 'Panda',
};

export const pandaDeparted: PageEvent = {
        at: placeOfTher,
        from: placeOfErr,
        req: placeOfErr,
        when: 13424559,
        who: 'Panda',
};

export const pandaTraveled: PageEvent = {
        at: placeOfBear,
        from: placeOfTher,
        req: placeOfErr,
        when: 13424559,
        who: 'Panda',
};

export const fullBlownPage: Page = {
    at: placeOfErr,
    events: [pandaTraveled, pandaDeparted, pandaArrived],
    shown: 13424560,
}
